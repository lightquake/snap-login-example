{-# LANGUAGE OverloadedStrings #-}

{-|

This is where all the routes and handlers are defined for your site. The
'app' function is the initializer that combines everything together and
is exported by this module.

-}

module Site
  ( app
  ) where

import           Control.Applicative
import           Control.Monad.State
import           Data.ByteString (ByteString)
import           Data.Maybe
import qualified Data.Text.Encoding as T

import           Database.HDBC.Sqlite3

import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth hiding (session)
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Hdbc
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe

import           Text.Templating.Heist

import           Application
import           SLE.Auth
import           SLE.Model
import           SLE.Splices

------------------------------------------------------------------------------
-- | Renders the front page of the sample site.
--
-- The 'ifTop' is required to limit this to the top of a route.
-- Otherwise, the way the route table is currently set up, this action
-- would be given every request.
indexGET :: Handler App (AuthManager App) ()
indexGET = do
  -- maybe get the splice that holds the user's message
  splice <- join <$> withCurrentUser messageSplice
  -- if we got it, splice it in. otherwise add 'not found'.
  let message = liftHeist $ fromMaybe (textSplice "not found") splice
  ifTop $ renderWithSplices "index" [("username", usernameSplice), ("message", message)]

-- | Handle POSTing to the index page; i.e., setting the message.
indexPOST :: Handler App (AuthManager App) ()
indexPOST = do
  message <- getParam "message"
  when (isJust message) . withCurrentUser_ . flip setMessage
    . T.decodeUtf8 . fromJust $ message
  redirect "/"
  
index :: Handler App (AuthManager App) ()
index = method GET indexGET <|> method POST indexPOST

-----------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, AppHandler ())]
routes = [ ("/", with auth index)
         , ("/register", registerH)
         , ("/login", loginH)
         , ("/logout", with auth logout >> redirect "/")
         , ("", with heist heistServe)
         , ("", serveDirectory "resources/static")
         ]

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "A snaplet example application." Nothing $ do
    h <- nestSnaplet "heist" heist $ heistInit "resources/templates"
    am <- nestSnaplet "authmanager" auth $ initJsonFileAuthManager defAuthSettings session "user.json"
    sm <- nestSnaplet "sessionmanager" session $ initCookieSessionManager "site_key.txt" "_cookie" Nothing
    conn <- nestSnaplet "hdbc" hdbc $ hdbcInit $ connectSqlite3 "data.db"
    addRoutes routes
    addAuthSplices auth
    wrapHandlers (<|> with hdbc initializeModel)
    return $ App h am sm conn