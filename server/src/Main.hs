{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

import Data.Default (def)
import qualified Data.String as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
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
import Control.Monad.Writer
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

import Prelude hiding (log)

import Debug.Trace

-- pipe syntax for application stolen from OCaml / F#
x |> f = f x

data Dir = Dir { name :: T.Text, files :: [T.Text], subdirs :: [Dir] }
           deriving (Eq, Show, Generic)
instance ToJSON Dir

data UserInfo = UserInfo { username :: T.Text, password :: T.Text }
                deriving (Eq, Show, Generic)
instance FromJSON UserInfo
instance ToJSON UserInfo

instance FromFormUrlEncoded UserInfo where
  fromFormUrlEncoded form = maybe (Left "could not parse form") (Right) $ do
    username <- L.lookup "username" form
    password <- L.lookup "password" form
    return $ UserInfo {
      username = username,
      password = password
    }

instance ToFormUrlEncoded UserInfo where
  toFormUrlEncoded userInfo = [
    ("user", username userInfo),
    ("password",password userInfo)]

type CopyboxAPI =
      "dir" :> Get '[JSON] Dir
 :<|> "web" :> Raw
 :<|> "file" :> Raw
 :<|> Vault :> "logout" :> Get '[HTML] Markup

type CopyboxAuthAPI =
      "login" :> Raw
 :<|> "trylogin" :> Vault :> ReqBody '[FormUrlEncoded] UserInfo
      :> QueryParam "redirect" T.Text
      :> Post '[HTML] (Headers '[Header "set-cookie" T.Text] Markup)

type CopyboxSite = "copybox" :> (CopyboxAuthAPI
                            :<|> AuthProtect "copybox-auth" :> CopyboxAPI
                            :<|> "assets" :> Raw)

type instance AuthServerData (AuthProtect "copybox-auth") = ()

type VaultSession = Vault.Key (IORef T.Text)

data CopyboxConfig = CopyboxConfig { 
                       copyboxUser      :: T.Text
                     , copyboxPass      :: T.Text
                     , copyboxCert      :: T.Text
                     , copyboxKey       :: T.Text
                     , copyboxClientDir :: T.Text
                     , copyboxRootDir   :: T.Text
                     , copyboxLoginDir  :: T.Text
                     , copyboxAssetDir  :: T.Text
                     , copyboxSession   :: VaultSession
                     }

sessionLogout = "##END##"
authCookie = "copybox-auth-cookie"

redirectRequest :: B.ByteString -> ServantErr
redirectRequest uri = err301 { errHeaders = [(HTTP.hLocation, uri)] }

redirectPage :: T.Text -> Markup
redirectPage uri =
  H.docTypeHtml $ do
    H.head $ do
      H.title "redirecting..."
      H.meta ! A.httpEquiv "refresh" ! A.content (H.toValue $ "2; url=" <> uri)
    H.body $ do
      H.p "You are being redirected."
      H.p $ do
        void "If your browser does not refresh the page click "
        H.a ! A.href (H.toValue uri) $ "here"

-- a handler with a logging writer monad
type LoggedHandler a = WriterT [T.Text] Handler a

-- flush log to stdin
runLoggedHandler :: LoggedHandler a -> Handler a
runLoggedHandler lh = do
  (res, loglist) <- runWriterT lh
  liftIO $ forM loglist T.putStrLn
  return res

log :: T.Text -> LoggedHandler ()
log t = tell [t]

authHandler :: CopyboxConfig -> AuthHandler Request ()
authHandler conf = mkAuthHandler handler
  where handler req = runLoggedHandler $ do
          key <- liftIO $ readIORef sVal
          case L.lookup HTTP.hCookie $ Wai.requestHeaders req of
            Nothing -> redirectToLogin

            Just cookieVal -> do
              if T.isInfixOf key (T.decodeUtf8 cookieVal)
              then do
                log $ "Authorized request for " <> (req |> rawPathInfo |> T.decodeUtf8)
                return ()
              else redirectToLogin

          where Just sVal = Vault.lookup (copyboxSession conf) (Wai.vault req)
                redirectToLogin = do
                  log "Unauthorized request, redirecting to login page..."
                  let url = "/copybox/login/index.html?redirect=" <> rawPathInfo req
                  throwError $ redirectRequest url

authServerContext :: CopyboxConfig -> Context (AuthHandler Request () ': '[])
authServerContext conf = (authHandler conf) :. EmptyContext

copyboxServer :: CopyboxConfig -> Server CopyboxSite
copyboxServer conf = (copyboxAuth conf)
                :<|> (copybox conf)
                :<|> (serveDirectory $ T.unpack $ copyboxAssetDir conf)

copyboxAuth :: CopyboxConfig -> Server CopyboxAuthAPI
copyboxAuth conf = login :<|> (tryLogin conf)
        -- (GET response to login page)
  where login = serveDirectory $ T.unpack $ copyboxLoginDir conf

        -- try to login (POST request from login page)
        tryLogin :: CopyboxConfig -> Vault.Vault -> UserInfo -> Maybe T.Text ->
                    Handler (Headers '[Header "set-cookie" T.Text] Markup)
        tryLogin conf vault userInfo redirParam = runLoggedHandler $ do
          -- start new session
          -- even if there is already a session in the vault,
          -- we should clobber that to allow the user to log in
          -- in case the cookie on the client's browser has expired
          let hashpass = userInfo |> password |> T.unpack |> SHA.sha256Ascii |> show |> T.pack
          -- let hashpass = T.pack $ show $ SHA.sha256Ascii $ T.unpack $ password userInfo
          if copyboxUser conf == username userInfo && copyboxPass conf == hashpass
          -- login valid!
          then do
            -- update session key
            newKey <- liftIO $ do
              newKey <- T.pack <$> ('a','z') |> randomRIO |> repeat |> take 40 |> sequence
              -- newKey <- T.pack <$> (sequence $ take 30 $ repeat $ randomRIO ('a','z'))
              writeIORef sVal newKey
              return newKey

            let cookie = T.pack "copybox-auth-cookie=" <> newKey
            let redirectUrl = maybe "/copybox/web/index.html" id redirParam
            let pageWithHeaders = addHeader cookie $ redirectPage redirectUrl
            log $ "logged in as " <> username userInfo <> ", redirecting to " <> redirectUrl <> " ..."
            return (pageWithHeaders :: Headers '[Header "set-cookie" T.Text] Markup)

          -- invalid, go back to login page
          else do
            log "invalid login"
            let pageWithHeaders = addHeader "" $ redirectPage "/copybox/login/index.html"
            return (pageWithHeaders :: Headers '[Header "set-cookie" T.Text] Markup)
            
          where Just sVal = Vault.lookup (copyboxSession conf) vault

copybox :: CopyboxConfig -> AuthServerData (AuthProtect "copybox-auth") -> Server CopyboxAPI
copybox conf _ =
      fetchDir "" (T.unpack $ copyboxRootDir conf)
 :<|> serveDirectory (T.unpack $ copyboxClientDir conf)
 :<|> serveDirectory (T.unpack $ copyboxRootDir conf)
 :<|> logout (copyboxSession conf)

        -- recursively traverses a directory and returns its contents
  where fetchDir :: FilePath -> FilePath -> Handler Dir
        fetchDir relname absname = runLoggedHandler $ do
          dc <- liftIO $ getDirectoryContents absname
          (fcs, dirs) <- liftIO $ foldM getContent ([], []) dc

          let dirs' = filter (\d -> not (d == "." || d == "..")) dirs
          -- recursive calls to get subdirectories
          dcs <- lift $ forM dirs' $ \dir -> fetchDir dir (absname <> "/" <> dir)

          return $ Dir (T.pack relname) (map T.pack fcs) dcs
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
        logout session vault = runLoggedHandler $ do
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
    user <- T.pack <$> CF.get cfile "DEFAULT" "user"
    pass <- T.pack <$> CF.get cfile "DEFAULT" "pass"
    cert <- T.pack <$> CF.get cfile "DEFAULT" "cert"
    key <- T.pack <$> CF.get cfile "DEFAULT" "key"
    client <- T.pack <$> CF.get cfile "DEFAULT" "client"
    root <- T.pack <$> CF.get cfile "DEFAULT" "root"
    loginDir <- T.pack <$> CF.get cfile "DEFAULT" "logindir"
    assetDir <- T.pack <$> CF.get cfile "DEFAULT" "assetdir"
    return $ CopyboxConfig {
               copyboxUser = user
             , copyboxPass = pass
             , copyboxCert = cert
             , copyboxKey  = key
             , copyboxClientDir = client
             , copyboxRootDir = root
             , copyboxLoginDir = loginDir
             , copyboxSession = session
             , copyboxAssetDir = assetDir
             }

  case res of
    Left err -> do
      print err

    Right conf -> do
      -- set up TLS
      let s = W.setPort port W.defaultSettings 
      let stls = WT.defaultTlsSettings {
        certFile = T.unpack $ copyboxCert conf
      , keyFile = T.unpack $ copyboxKey conf
      , onInsecure = WT.DenyInsecure "toss off, wanker"
      }
      
      -- run the server
      runTLS stls s $ withSession session val $ app conf

  where withSession :: VaultSession -> IORef T.Text -> Middleware
        withSession session val app req resp = do
          let vault' = Vault.insert session val (Wai.vault req)
          let req' = req { vault = vault' }
          app req' resp
        
