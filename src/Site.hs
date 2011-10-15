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
import           Control.Monad.Trans
import           Control.Monad.State
import           Data.ByteString (ByteString)
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time.Clock

import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth hiding (session)
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Snaplet.Heist
import           Snap.Util.FileServe

import           Text.Templating.Heist
import           Text.XmlHtml hiding (render)

import           Application
import           SLE.Auth
import           SLE.Splices

------------------------------------------------------------------------------
-- | Renders the front page of the sample site.
--
-- The 'ifTop' is required to limit this to the top of a route.
-- Otherwise, the way the route table is currently set up, this action
-- would be given every request.
index :: Handler App (AuthManager App) ()
index = ifTop $ renderWithSplices "index" [("username", usernameSplice)]


-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/", with auth index)
         , ("/register", registerH)
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
    addRoutes routes
    addAuthSplices auth
    return $ App h am sm


