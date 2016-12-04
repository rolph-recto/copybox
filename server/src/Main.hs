{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Default (def)
import qualified Data.String as S
import qualified Data.ByteString.Char8 as B
import qualified Data.List as L
import Data.Monoid
import Data.Aeson.Types
import Data.Proxy
import qualified Data.ConfigFile as CF
import qualified Data.Digest.SHA2 as SHA
import qualified Data.Vault.Lazy as Vault

import GHC.Generics
import Control.Monad
import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Error

import System.Directory
import Network.HTTP.Types.Header as HTTP
import Network.Wai as Wai
import Network.Wai.Handler.Warp as W
import Network.Wai.Handler.WarpTLS as WT
import Network.Wai.Session (withSession, Session)
import Network.Wai.Session.Map (mapStore_)

import Servant
import Servant.Server.Experimental.Auth

data Dir = Dir { name :: String, files :: [String], subdirs :: [Dir] }
           deriving (Eq, Show, Generic)
instance ToJSON Dir

data UserInfo = UserInfo { username :: String, password :: String, sessionKey :: String }
                deriving (Eq, Show, Generic)
instance FromJSON UserInfo
instance ToJSON UserInfo

type CopyboxAPI = "dir" :> Get '[JSON] Dir
             :<|> "web" :> Raw
             :<|> "file" :> Raw
             :<|> Vault :> "logout" :> Get '[JSON] NoContent

type CopyboxAuthAPI = "login" :> Raw
                 :<|> "trylogin" :> Vault :> ReqBody '[JSON] UserInfo :> Post '[JSON] NoContent

type CopyboxSite = "copybox" :> (CopyboxAuthAPI
                            :<|> AuthProtect "copybox-auth" :> CopyboxAPI)

type instance AuthServerData (AuthProtect "copybox-auth") = ()

data CopyboxConfig = CopyboxConfig { 
                       copyboxUser      :: String
                     , copyboxPass      :: String
                     , copyboxCert      :: String
                     , copyboxKey       :: String
                     , copyboxClientDir :: String
                     , copyboxRootDir   :: String
                     , copyboxLoginDir  :: String
                     , copyboxSession   :: Vault.Key (Session IO String String)
                     }

-- from Servant's basic auth tutorial
{-
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
-}

sessionUser = "user"
sessionLogout = "##END##"
authCookie = "copybox-auth-cookie"

redirect :: B.ByteString -> ServantErr
redirect uri = err301 { errHeaders = [(HTTP.hContentLocation, uri)] }

authHandler :: CopyboxConfig -> AuthHandler Request ()
authHandler conf = mkAuthHandler handler
  where handler req = do
          let Just (sessionLookup, sessionInsert) = Vault.lookup (copyboxSession conf) (Wai.vault req)
          case L.lookup authCookie (requestHeaders req) of
            Nothing -> throwError $ redirect "/login"
            -- check if the session key is actually valid
            -- i.e., it must be the same one that the user logged in with
            Just sessionKey -> do
              key <- liftIO $ (maybe sessionLogout id) <$> (sessionLookup sessionUser)
              if key == sessionLogout
              -- user is trying something fishy...
              then throwError $ redirect "/login"
              -- continue to content page
              else return ()

authServerContext :: CopyboxConfig -> Context (AuthHandler Request () ': '[])
authServerContext conf = (authHandler conf) :. EmptyContext

copyboxServer :: CopyboxConfig -> Server CopyboxSite
copyboxServer conf = copyboxAuth :<|> copybox
  where copyboxAuth :: Server CopyboxAuthAPI
        copyboxAuth = login :<|> (tryLogin conf)

        -- (GET response to login page)
        login = serveDirectory (copyboxLoginDir conf)

        -- try to login (POST request from login page)
        tryLogin :: CopyboxConfig -> Vault.Vault -> UserInfo -> Handler NoContent
        tryLogin conf vault userInfo = do
          mSessionVal <- liftIO $ sessionLookup sessionUser
          let sessionVal = maybe sessionLogout id mSessionVal

          -- start new session
          if sessionVal == sessionLogout
          then do
            let hashpass = show $ SHA.sha256Ascii $ password userInfo
            if copyboxUser conf == username userInfo && copyboxPass conf == hashpass
            -- login valid!
            then do
              -- update session key
              liftIO $ sessionInsert sessionUser (sessionKey userInfo)
              -- redirect to content page
              throwError $ redirect "/web"

            -- invalid, go back to login page
            else throwError $ redirect "/login"
                
          -- already logged in, redirect to content page
          else do
            throwError $ redirect "/web"

          where Just (sessionLookup, sessionInsert) = Vault.lookup (copyboxSession conf) vault

        copybox :: AuthServerData (AuthProtect "copybox-auth") -> Server CopyboxAPI
        copybox _ = fetchDir "" (copyboxRootDir conf)
               :<|> serveDirectory (copyboxClientDir conf)
               :<|> serveDirectory (copyboxRootDir conf)
               :<|> logout (copyboxSession conf)
  
        -- recursively traverses a directory and returns its contents
        fetchDir :: FilePath -> FilePath -> Handler Dir
        fetchDir relname absname = do
          dc <- lift $ getDirectoryContents absname
          (fcs, dirs) <- lift $ foldM getContent ([], []) dc

          let dirs' = filter (\d -> not (d == "." || d == "..")) dirs
          -- recursive calls to get subdirectories
          dcs <- forM dirs' $ \dir -> fetchDir dir (absname <> "/" <> dir)

          return $ Dir relname fcs dcs
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

        -- logout from the page
        logout :: Vault.Key (Session IO String String) -> Vault.Vault -> Handler NoContent
        logout session vault = do
          liftIO $ sessionInsert sessionUser sessionLogout
          throwError $ redirect "/login"
          where Just (_, sessionInsert) = Vault.lookup (copyboxSession conf) vault

app conf = serveWithContext
             (Proxy :: Proxy CopyboxSite)
             (authServerContext conf)
             (copyboxServer conf)

configFile = "copybox.conf"
port = 8081
    
main :: IO ()
main = do
  -- create session store in vault
  session <- Vault.newKey
  store <- mapStore_
  
  -- load config file
  res <- runErrorT $ do
    cfile <- join $ liftIO $ CF.readfile CF.emptyCP configFile
    user <- CF.get cfile "DEFAULT" "user"
    pass <- CF.get cfile "DEFAULT" "pass"
    cert <- CF.get cfile "DEFAULT" "cert"
    key <- CF.get cfile "DEFAULT" "key"
    client <- CF.get cfile "DEFAULT" "client"
    root <- CF.get cfile "DEFAULT" "root"
    loginDir <- CF.get cfile "DEFAULT" "loginDir"
    let config = CopyboxConfig {
                   copyboxUser = user
                 , copyboxPass = pass
                 , copyboxCert = cert
                 , copyboxKey  = key
                 , copyboxClientDir = client
                 , copyboxRootDir = root
                 , copyboxLoginDir = loginDir
                 , copyboxSession = session
                 }
    return config

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
      runTLS stls s $ withSession store (S.fromString "SESSION") def session $ app conf
