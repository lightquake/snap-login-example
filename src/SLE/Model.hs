{-# LANGUAGE ViewPatterns, OverloadedStrings #-}

module SLE.Model 
       ( getMessage )
where

import qualified Data.Text as T
import Data.Text.Encoding

import Snap.Snaplet.Auth
import Snap.Snaplet.Hdbc

getMessage :: HasHdbc m c => AuthUser -> m (Maybe T.Text)
getMessage (userId -> Nothing) = return Nothing
getMessage (userId -> Just uid) = do
  results <- quickQuery "select message from messages where userid = ?" [toSql . unUid $ uid]
  return $ if null results then Nothing else Just . fromSql $ head . head $ results