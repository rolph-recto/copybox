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
import React.DOM.Props as R
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
                getParentSubdirs (Cons l ls) c r = getParentSubdirs ls l (Cons c r)

        -- go down on a subdirectory
        pressDown :: Int -> ExplorerState -> ExplorerState
        pressDown n st =
          case st.focus of
            Dir { subdirs: Nil } -> st
            Dir { subdirs: Cons d ds, name: name, files: files } -> 
              let res    = getFocus n { left: Nil, focus: d, right: ds } in
              let parent = { name: name
                           , files: files
                           , left: res.left
                           , right: res.right } in
              { zipper: Cons parent (st.zipper), focus: res.focus }
          where getFocus :: Int -> Focus Dir -> Focus Dir
                getFocus 0 acc = acc
                getFocus n f =
                  case f.right of
                    Nil -> f
                    Cons r rs -> getFocus (n-1) {
                                   left: Cons (f.focus) (f.left)
                                 , focus: r
                                 , right: rs }

        render :: T.Render ExplorerState _ ExplorerAction
        render dispatch _ st _ =
          let subdirs     = getSubdirs st.focus in
          let nsubdirs    = zip (0 .. ((length subdirs)-1)) subdirs in
          let files       = getFiles st.focus in
          let subdirNodes = map (uncurry renderSubdir) nsubdirs in
          let fileNodes   = map renderFile files in
          [
            R.div [] [
              R.button [ R.onClick (const $ dispatch Up) ] [ R.text "Up" ]
            , R.div [] $ toUnfoldable $ subdirNodes <> fileNodes
            ]
          ]
          where getSubdirs (Dir d)      = d.subdirs
                getFiles (Dir d)        = d.files
                renderSubdir i (Dir d)  =
                  R.div [] [
                    R.a [R.href "#", R.onClick (const $ dispatch $ Down i)] [
                      R.text (show i <> ": " <> d.name)
                    ]
                  ]
                renderFile filename =
                  R.div [] [ R.text filename ]

main = do
  runAff fail success fetchCopyboxData
  where fetchCopyboxData :: forall a. Aff (ajax :: AJAX | a) String
        fetchCopyboxData = do
          res <- get "/test"
          pure res.response

        fail err = do
          log $ message err
        
        withData res k = do
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
          log $ show dir
          pure unit

        -- surprise! CPS saves the day
        success res = withData res $ \d ->
                      withContainer "content" $ \c ->
                      runExplorer d c

