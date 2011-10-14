{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

{-| This is where we handle logic related to authenticating users,
  such as logging them in, registering them, etc.
-}

module SLE.Auth
       ( Registration
       , registerForm )
where

import           Application (App, auth)

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import           Data.Maybe (isJust)
import qualified Data.Text as T

import           Snap.Snaplet
import           Snap.Snaplet.Auth

import           Text.Digestive.Snap.Heist
import           Text.Digestive

data Registration = Registration T.Text ByteString deriving (Eq, Show)

registerForm :: SnapForm (Handler App App) T.Text HeistView Registration
registerForm = Registration
               <$> (T.pack <$> input "username" Nothing `validateMany` [nonEmpty, notInUse]) <++ errors
               <*> (B.pack <$> input "password" Nothing `validate` nonEmpty) <++ errors
  where nonEmpty = check "Field must not be empty" $ not . null
        notInUse = checkM "User exists" $ liftM not . with auth . userExists . T.pack

userExists :: T.Text -> Handler b (AuthManager b) Bool
userExists username = do
  (AuthManager r _ _ _ _ _ _ _) <- get
  liftIO $ isJust <$> lookupByLogin r username
