{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

import Data.Default (def)
import qualified Data.String as S
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import qualified Data.List as L
import Data.Monoid
import Data.Aeson.Types
import Data.Proxy
import qualified Data.ConfigFile as CF
import qualified Data.Digest.SHA2 as SHA
import qualified Data.Vault.Lazy as Vault
import Data.IORef

import Text.Blaze.Html5 ((!), Markup)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import GHC.Generics
import Control.Monad
import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Error

import System.Directory
import System.Random (randomRIO)

import qualified Network.HTTP.Types.Header as HTTP
import Network.Wai as Wai
import Network.Wai.Handler.Warp as W
import Network.Wai.Handler.WarpTLS as WT
import Network.Wai.Session (withSession, Session)
import Network.Wai.Session.Map (mapStore_)

import Servant
import Servant.Server.Experimental.Auth
import Servant.HTML.Blaze

import Debug.Trace

data Dir = Dir { name :: String, files :: [String], subdirs :: [Dir] }
           deriving (Eq, Show, Generic)
instance ToJSON Dir

data UserInfo = UserInfo { username :: String, password :: String }
                deriving (Eq, Show, Generic)
instance FromJSON UserInfo
instance ToJSON UserInfo

instance FromFormUrlEncoded UserInfo where
  fromFormUrlEncoded form = maybe (Left "could not parse form") (Right) $ do
    username <- L.lookup "username" form
    password <- L.lookup "password" form
    return $ UserInfo {
      username = T.unpack username,
      password = T.unpack password
    }

instance ToFormUrlEncoded UserInfo where
  toFormUrlEncoded userInfo = [
    ("user", S.fromString $ username userInfo),
    ("password", S.fromString $ password userInfo)]

type CopyboxAPI =
      "dir" :> Get '[JSON] Dir
 :<|> "web" :> Raw
 :<|> "file" :> Raw
 :<|> Vault :> "logout" :> Get '[HTML] Markup

type CopyboxAuthAPI =
      "login" :> Raw
 :<|> "trylogin" :> Vault :> ReqBody '[FormUrlEncoded] UserInfo
      :> Post '[HTML] (Headers '[Header "set-cookie" String] Markup)

type CopyboxSite = "copybox" :> (CopyboxAuthAPI
                            :<|> AuthProtect "copybox-auth" :> CopyboxAPI)

type instance AuthServerData (AuthProtect "copybox-auth") = ()

type VaultSession = Vault.Key (IORef String)

data CopyboxConfig = CopyboxConfig { 
                       copyboxUser      :: String
                     , copyboxPass      :: String
                     , copyboxCert      :: String
                     , copyboxKey       :: String
                     , copyboxClientDir :: String
                     , copyboxRootDir   :: String
                     , copyboxLoginDir  :: String
                     , copyboxSession   :: VaultSession
                     }

sessionLogout = "##END##"
authCookie = "copybox-auth-cookie"

redirectRequest :: B.ByteString -> ServantErr
redirectRequest uri = err301 { errHeaders = [(HTTP.hLocation, uri)] }

redirectPage :: String -> Markup
redirectPage uri =
  H.docTypeHtml $ do
    H.head $ do
      H.title "redirecting..."
      H.meta ! A.httpEquiv "refresh" ! A.content (H.toValue $ "2; url=" ++ uri)
    H.body $ do
      H.p "You are being redirected."
      H.p $ do
        void "If your browser does not refresh the page click "
        H.a ! A.href (H.toValue uri) $ "here"

authHandler :: CopyboxConfig -> AuthHandler Request ()
authHandler conf = mkAuthHandler handler
  where handler req = do
          key <- liftIO $ readIORef sVal
          case L.lookup HTTP.hCookie $ Wai.requestHeaders req of
            Nothing -> do
              throwError $ redirectRequest "/copybox/login/index.html"

            Just cookie -> do
              return ()

          where Just sVal = Vault.lookup (copyboxSession conf) (Wai.vault req)

authServerContext :: CopyboxConfig -> Context (AuthHandler Request () ': '[])
authServerContext conf = (authHandler conf) :. EmptyContext

copyboxServer :: CopyboxConfig -> Server CopyboxSite
copyboxServer conf = (copyboxAuth conf) :<|> (copybox conf)

copyboxAuth :: CopyboxConfig -> Server CopyboxAuthAPI
copyboxAuth conf = login :<|> (tryLogin conf)
        -- (GET response to login page)
  where login = serveDirectory (copyboxLoginDir conf)

        -- try to login (POST request from login page)
        tryLogin :: CopyboxConfig -> Vault.Vault -> UserInfo -> Handler (Headers '[Header "set-cookie" String] Markup)
        tryLogin conf vault userInfo = do
          -- start new session
          -- even if there is already a session in the vault,
          -- we should clobber that to allow the user to log in
          -- in case the cookie on the client's browser has expired
          let hashpass = show $ SHA.sha256Ascii $ password userInfo
          if copyboxUser conf == username userInfo && copyboxPass conf == hashpass
          -- login valid!
          then do
            -- update session key
            newKey <- liftIO $ do
              newKey <- sequence $ take 30 $ repeat $ randomRIO ('a','z')
              writeIORef sVal newKey
              return newKey

            let cookie = "copybox-auth-cookie=" ++ newKey
            let pageWithHeaders = addHeader cookie $ redirectPage "/copybox/web/index.html"
            return (pageWithHeaders :: Headers '[Header "set-cookie" String] Markup)

          -- invalid, go back to login page
          else do
            let pageWithHeaders = addHeader "" $ redirectPage "/copybox/login/index.html"
            return (pageWithHeaders :: Headers '[Header "set-cookie" String] Markup)
            
          where Just sVal = Vault.lookup (copyboxSession conf) vault

copybox :: CopyboxConfig -> AuthServerData (AuthProtect "copybox-auth") -> Server CopyboxAPI
copybox conf _ =
      fetchDir "" (copyboxRootDir conf)
 :<|> serveDirectory (copyboxClientDir conf)
 :<|> serveDirectory (copyboxRootDir conf)
 :<|> logout (copyboxSession conf)

        -- recursively traverses a directory and returns its contents
  where fetchDir :: FilePath -> FilePath -> Handler Dir
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
        logout :: VaultSession -> Vault.Vault -> Handler Markup
        logout session vault = do
          liftIO $ writeIORef sVal sessionLogout
          return $ redirectPage "/copybox/login/index.html"
          where Just sVal = Vault.lookup session vault

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
  val <- newIORef ""
  
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
    return $ CopyboxConfig {
               copyboxUser = user
             , copyboxPass = pass
             , copyboxCert = cert
             , copyboxKey  = key
             , copyboxClientDir = client
             , copyboxRootDir = root
             , copyboxLoginDir = loginDir
             , copyboxSession = session
             }

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
      runTLS stls s $ withSession session val $ app conf

  where withSession :: VaultSession -> IORef String -> Middleware
        withSession session val app req resp = do
          let vault' = Vault.insert session val (Wai.vault req)
          let req' = req { vault = vault' }
          app req' resp
        
