{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import Data.Monoid
import Data.Aeson.Types
import Data.Proxy
import qualified Data.ConfigFile as CF
import qualified Data.Digest.SHA2 as SHA

import GHC.Generics
import Control.Monad
import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Error

import System.Directory
import Network.Wai.Handler.Warp as W
import Network.Wai.Handler.WarpTLS as WT
import Servant

data Dir = Dir { name :: T.Text, files :: [T.Text], subdirs :: [Dir] }
           deriving (Eq, Show, Generic)

instance ToJSON Dir

type CopyboxAPI = "dir" :> Get '[JSON] Dir
             :<|> "web" :> Raw
             :<|> "file" :> Raw

type CopyboxAPIAuth = BasicAuth "foo-realm" () :> CopyboxAPI

data CopyboxConfig = CopyboxConfig { 
                       copyboxUser      :: String
                     , copyboxPass      :: String
                     , copyboxCert      :: String
                     , copyboxKey       :: String
                     , copyboxClientDir :: String
                     , copyboxRootDir   :: String
                     }

-- from Servant's basic auth tutorial
authCheck :: CopyboxConfig -> BasicAuthCheck ()
authCheck conf =
  let check (BasicAuthData username password) = do
        let hashpass = show $ SHA.sha256Ascii $ B.unpack password
        if B.pack (copyboxUser conf) == username && (copyboxPass conf) == hashpass
        then return $ Authorized ()
        else return Unauthorized
  in BasicAuthCheck check

basicAuthServerContext :: CopyboxConfig -> Context (BasicAuthCheck () ': '[])
basicAuthServerContext conf = (authCheck conf) :. EmptyContext

copyboxServer :: CopyboxConfig -> Server CopyboxAPIAuth
copyboxServer conf _ = fetchDir "" (copyboxRootDir conf)
             :<|> serveDirectory (copyboxClientDir conf)
             :<|> serveDirectory (copyboxRootDir conf)
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

app conf = serveWithContext
             (Proxy :: Proxy CopyboxAPIAuth)
             (basicAuthServerContext conf)
             (copyboxServer conf)

configFile = "copybox.conf"
port = 8081
        
main :: IO ()
main = do
  -- load config file
  res <- runErrorT $ do
    cfile <- join $ liftIO $ CF.readfile CF.emptyCP configFile
    user <- CF.get cfile "DEFAULT" "user"
    pass <- CF.get cfile "DEFAULT" "pass"
    cert <- CF.get cfile "DEFAULT" "cert"
    key <- CF.get cfile "DEFAULT" "key"
    client <- CF.get cfile "DEFAULT" "client"
    root <- CF.get cfile "DEFAULT" "root"
    return $ CopyboxConfig user pass cert key client root

  case res of
    Left err -> do
      print err

    Right conf -> do
      -- set up TLS
      let s = W.setPort port W.defaultSettings 
      let stls = WT.defaultTlsSettings {
        certFile = copyboxCert conf
      , keyFile = copyboxKey conf
      , onInsecure = WT.DenyInsecure "toss off, wanker"
      }
      
      -- run the server
      runTLS stls s (app conf)
