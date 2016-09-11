{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Data.Monoid
import Data.Aeson.Types
import Data.Proxy

import GHC.Generics
import Control.Monad
import Control.Applicative
import Control.Monad.Trans

import System.Directory
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

data Dir = Dir { name :: T.Text, files :: [T.Text], subdirs :: [Dir] }
           deriving (Eq, Show, Generic)

instance ToJSON Dir

type CopyboxAPI = "dir" :> Get '[JSON] Dir
             :<|> "web" :> Raw
             :<|> "file" :> Raw

-- obviously you should change this
rootDir = "/home/rolph/Copy/References"

copyboxServer :: Server CopyboxAPI
copyboxServer = fetchDir "" rootDir
           :<|> serveDirectory "../client/build"
           :<|> serveDirectory rootDir
  where -- recursively traverses a directory and returns its contents
        fetchDir :: FilePath -> FilePath -> Handler Dir
        fetchDir relname absname = do
          dc <- lift $ getDirectoryContents absname
          (files, dirs) <- lift $ foldM getContent ([], []) dc

          let fcs = map T.pack files
          let dirs' = filter (\d -> not (d == "." || d == "..")) dirs
          -- recursive calls to get subdirectories
          dcs <- forM dirs' $ \dir -> fetchDir dir (absname <> "/" <> dir)

          return $ Dir (T.pack relname) fcs dcs
          where getContent (fa, da) name = do
                  let cname = absname <> "/" <> name
                  fexist <- doesFileExist cname
                  if fexist
                  then return (name:fa, da)
                  else do
                    dexist <- doesDirectoryExist cname
                    if dexist
                    then return (fa, name:da)
                    else return (fa, da)

app :: Application
app = serve (Proxy :: Proxy CopyboxAPI) copyboxServer

main :: IO ()
main = run 8081 app
