{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

{-| This is where we handle logic related to authenticating users,
  such as logging them in, registering them, etc.
-}

module SLE.Auth
       ( Registration
       , registerH )
where

import           Control.Applicative
import           Control.Arrow (second)
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Trans
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import           Data.Maybe (isJust)
import qualified Data.Text as T

import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Heist

import           Text.Digestive.Snap.Heist
import           Text.Digestive

import           Application (App, auth)

data Registration = Registration T.Text ByteString deriving (Eq, Show)

------------------------------------------------------------------------------
-- | Auth-related forms.

registerForm :: SnapForm (Handler App App) T.Text HeistView Registration
registerForm = Registration
               <$> (T.pack <$> input "username" Nothing `validateMany` [nonEmpty, notInUse]) <++ errors
               <*> (B.pack <$> input "password" Nothing `validate` nonEmpty) <++ errors
  where
    nonEmpty :: (Monad m) => Validator m T.Text String
    nonEmpty = check "Field must not be empty" $ not . null

    notInUse :: Validator (Handler App App) T.Text String
    notInUse = checkM "User exists" $ liftM not . with auth . userExists . T.pack

userExists :: T.Text -> Handler b (AuthManager b) Bool
userExists username = do
  (AuthManager r _ _ _ _ _ _ _) <- get
  liftIO $ isJust <$> lookupByLogin r username

------------------------------------------------------------------------------
-- | Auth-related handlers.

registerH :: Handler App App ()
registerH = do
  -- run the registerForm
  result <- eitherSnapForm registerForm "registerForm"
  case result of
    -- we errored; splices is the list of error splices
    Left splices ->
      renderWithSplices "register" $ map (second liftHeist) splices
    -- we succeeded and got a registration to process
    Right (Registration username password) -> do
      with auth $ createUser username password >>= forceLogin
      redirect "/"