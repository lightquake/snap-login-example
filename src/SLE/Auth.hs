{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

{-| This is where we handle logic related to authenticating users,
  such as logging them in, registering them, etc.
-}

module SLE.Auth
       ( Registration
       , registerH
       , loginH )
where

import           Control.Applicative
import           Control.Arrow (second)
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Trans
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import           Data.Maybe (isJust)
import           Data.Monoid
import qualified Data.Text as T

import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Heist

import           Text.Digestive.Snap.Heist
import           Text.Digestive
import           Text.Templating.Heist
import qualified Text.XmlHtml as X

import           Application (App, AppHandler, auth)
import           SLE.Splices (errorBind)

data Registration = Registration T.Text ByteString deriving (Eq, Show)


------------------------------------------------------------------------------
-- | Auth-related forms.

registerForm :: SnapForm (AppHandler) T.Text HeistView Registration
registerForm = Registration
               <$> (T.pack <$> input "username" Nothing `validateMany` [nonEmpty, notInUse]) <++ errors
               <*> (B.pack <$> input "password" Nothing `validate` nonEmpty) <++ errors

  where
    nonEmpty :: (Monad m) => Validator m T.Text String
    nonEmpty = check "Field must not be empty." $ not . null

    notInUse :: Validator AppHandler T.Text String
    notInUse = checkM "This username already exists." $ liftM not . with auth . userExists . T.pack


loginForm :: SnapForm AppHandler T.Text HeistView (Handler b (AuthManager b) (Either AuthFailure AuthUser))
loginForm = loginByUsername
               <$> (B.pack <$> input "username" Nothing `validate` nonEmpty) <++ errors
               <*> (ClearText . B.pack <$> input "password" Nothing `validate` nonEmpty) <++ errors
               <*> pure False -- this indicates not to create a remember token
  where
    nonEmpty :: (Monad m) => Validator m T.Text String
    nonEmpty = check "Field must not be empty." $ not . null


userExists :: T.Text -> Handler app (AuthManager app) Bool
userExists username = do
  (AuthManager r _ _ _ _ _ _ _) <- get
  liftIO $ isJust <$> lookupByLogin r username

------------------------------------------------------------------------------
-- | Auth-related handlers.

registerH :: AppHandler ()
registerH = do
  -- run the registerForm
  result <- eitherSnapForm registerForm "registerForm"
  case result of
    -- we errored; splices is the list of error splices
    Left splices -> do
      -- bind error splices in so the elements that errored have the error class
      let splices' = splices ++ map errorBind ["username", "password"]
      renderWithSplices "register" $ map (second liftHeist) splices'
    -- we succeeded and got a registration to process
    Right (Registration username password) -> do
      with auth $ createUser username password >>= forceLogin
      redirect "/"

loginH :: AppHandler ()
loginH = withSplices [blankBind] $ do
  -- run the login form
  result <- eitherSnapForm loginForm "loginForm"
  case result of
    Left splices -> do
      -- use the list of error splices to render the login form, which
      -- had a validation error.
      let splices' = splices ++ map errorBind ["username", "password"]
      renderWithSplices "login" $ map (second liftHeist) splices'
    Right result -> do
      -- the values were valid, but we still might redirect back if
      -- login fails (Left case). only redirect to / if login succeeded
      -- (Right case).
      r <- with auth result
      case r of
        Left failure ->
          withSplices [failureBind] $ render "login"
        Right success ->
          redirect "/"
  where 
    -- since binds are applied in reverse order, we apply blankBlind
    -- globally and then failureBind in the case where we have an
    -- actual login failure.
    failureBind = ("auth-error", liftHeist runChildren)
    blankBind = ("auth-error", return [X.TextNode ""])