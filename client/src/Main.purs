module Main where

import Prelude
import Data.Traversable
import Data.Monoid
import Data.List
import Data.Array as A
import Data.Tuple
import Data.Nullable
import Data.Maybe
import Data.Either
import Data.String as S
import Data.Map as M
import Data.Argonaut.Generic.Aeson
import Data.Argonaut.Parser
import Data.Generic

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception
import Control.Monad.Aff

import DOM as D
import DOM.HTML as D
import DOM.HTML.Window as D
import DOM.HTML.Document as D
import DOM.HTML.Types as D
import DOM.Node.Types as D
import DOM.Node.NonElementParentNode as D
import Thermite as T
import React as R
import React.DOM as R
import React.DOM.Props hiding (span) as R
import ReactDOM as RDOM

import Network.HTTP.Affjax (AJAX, get, post)

data Dir = Dir { name :: String, files :: List String, subdirs :: List Dir }

instance showDir :: Show Dir where
  show (Dir d) =
    "Dir " <> d.name <> "\n"
           <> "Subdirs: \n" <> S.joinWith "," (map show $ toUnfoldable d.subdirs) 
           <> "Files\n" <> S.joinWith "," (toUnfoldable d.files) 

-- the JSON encoder doesn't serialize Lists into a decent JSON format, so
-- we convert it to arrays first before (de)serializing
data DirArray = DirArray { name :: String, files :: Array String, subdirs :: Array DirArray }

derive instance genericDirArray :: Generic DirArray

toDirArray :: Dir -> DirArray
toDirArray (Dir d) =
  DirArray {
    name: d.name,
    files: toUnfoldable d.files,
    subdirs: toUnfoldable $ map toDirArray d.subdirs
  }
  
fromDirArray :: DirArray -> Dir
fromDirArray (DirArray d) =
  Dir {
    name: d.name,
    files: fromFoldable d.files,
    subdirs: fromFoldable $ map fromDirArray d.subdirs
  }

type DirZipper = { name :: String
                 , files :: List String
                 , left :: List Dir
                 , right :: List Dir }

type Focus a =  { left :: List a, focus :: a, right :: List a }

type ExplorerState = { zipper :: List DirZipper, focus :: Dir }

data ExplorerAction = Up | Down Int

fileEndpoint = "/copybox/file"
explorerContainer = "explorer"
dirEndpoint = "/copybox/dir"

explorerSpec :: T.Spec _ ExplorerState _ ExplorerAction
explorerSpec = T.simpleSpec performAction render
  where performAction :: T.PerformAction _ ExplorerState _ ExplorerAction
        performAction Up _ st       = void (T.cotransform pressUp)
        performAction (Down n) _ st = void (T.cotransform (pressDown n))

        -- go up to the parent directory
        pressUp :: ExplorerState -> ExplorerState
        pressUp st =
          case st.zipper of
            Nil -> st
            Cons z zs ->
              let subdirs = getParentSubdirs z.left st.focus z.right in
              let d = Dir { name: z.name, files: z.files, subdirs: subdirs } in
              { zipper: zs, focus: d }
          where getParentSubdirs :: List Dir -> Dir -> List Dir -> List Dir
                getParentSubdirs Nil c r = c:r
                getParentSubdirs (l:ls) c r = getParentSubdirs ls l (c:r)

        -- go down on a subdirectory
        pressDown :: Int -> ExplorerState -> ExplorerState
        pressDown n st =
          case st.focus of
            Dir { subdirs: Nil } -> st
            Dir { subdirs: Cons d ds, files: pfiles, name: pname } -> 
              let res    = getFocus n { left: Nil, focus: d, right: ds } in
              let parent = { name: pname
                           , files: pfiles
                           , left: res.left
                           , right: res.right } in
              { zipper: parent:(st.zipper), focus: res.focus }

          where getFocus :: Int -> Focus Dir -> Focus Dir
                getFocus 0 acc = acc
                getFocus n f =
                  case f.right of
                    Nil -> f
                    Cons r rs -> getFocus (n-1) {
                                   left: (f.focus):(f.left)
                                 , focus: r
                                 , right: rs }

        render :: T.Render ExplorerState _ ExplorerAction
        render dispatch _ st _ =
          let subdirs     = getSubdirs st.focus in
          let nsubdirs    = zip (0 .. ((length subdirs)-1)) subdirs in
          let files       = getFiles st.focus in
          let pathlist    = reverse $ (getName st.focus):(map (\z -> z.name) st.zipper) in
          let pathlist'   = filter (\p -> S.length p > 0) pathlist in
          let path        = S.joinWith "/" $ toUnfoldable $ fileEndpoint:pathlist' in
          let subdirNodes = map (uncurry renderSubdir) nsubdirs in
          let fileNodes   = map (renderFile path) files in
          [
            R.div [R.className "row top30"] [
              R.div [R.className "col-md-1"] [
                R.span [ R.className "press-up", R.onClick (const $ dispatch Up) ] [ R.text "<" ]
              ],
              R.div [R.className "col-md-11"] [
                R.span [R.className "h2 dir-name"] [R.text (getName st.focus)],
                R.text (show (length (getFiles st.focus)) <> " items")
              ]
            ],
            R.div [R.className "row top20"] [
              R.div [R.className "col-md-11 col-md-offset-1"] $ toUnfoldable $ subdirNodes <> fileNodes
            ]
          ]
          where getSubdirs (Dir d)      = d.subdirs
                getFiles (Dir d)        = d.files
                getName (Dir d)         = d.name
                renderSubdir i (Dir d)  =
                  R.div [R.className "dir-item", R.onClick (const $ dispatch $ Down i)] [
                    R.text ("■ " <> d.name)
                  ]
                renderFile path filename =
                  R.div [R.className "file-item"] [
                    R.a [R.href (path <> "/" <> filename)] [
                      R.text ("□ " <> filename)
                    ]
                ]

main = do
  runAff fail success fetchCopyboxData
  where fetchCopyboxData :: forall a. Aff (ajax :: AJAX | a) String
        fetchCopyboxData = do
          res <- get dirEndpoint
          pure res.response

        fail err = do
          log $ message err
        
        withData res k = do
          log res
          case jsonParser res of
            Left err -> do
              log err
              pure unit
            Right json -> do
              case decodeJson json of
                Left err -> do
                  log err
                  pure unit
                Right da -> do
                  k $ fromDirArray da

        withContainer container k = do
          mcontainer <- getContainer container
          case mcontainer of
            Nothing -> pure unit
            Just container -> k container

          where getContainer id = do
                  pdoc <- D.htmlDocumentToNonElementParentNode <$> (D.window >>= D.document)
                  toMaybe <$> D.getElementById (D.ElementId id) pdoc

        runExplorer dir container = do
          let initExplorerState = { zipper:Nil, focus:dir }
          let explorerClass     = T.createClass explorerSpec initExplorerState
          let explorer          = R.createFactory explorerClass {}
          RDOM.render explorer container
          pure unit

        -- surprise! CPS saves the day
        success res = withData res $ \d ->
                      withContainer explorerContainer $ \c ->
                      runExplorer d c

