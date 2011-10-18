{-# LANGUAGE ViewPatterns, OverloadedStrings #-}

module SLE.Model 
       ( getMessage 
       , setMessage )
where

import qualified Data.Text as T
import Data.Text.Encoding

import Snap.Snaplet.Auth
import Snap.Snaplet.Hdbc

-- Get the message for the given user.
getMessage :: HasHdbc m c => AuthUser -> m (Maybe T.Text)
getMessage (userId -> Nothing) = return Nothing
getMessage (userId -> Just uid) = do
  results <- quickQuery "select message from messages where userid = ?" [toSql . unUid $ uid]
  return $ if null results then Nothing else Just . fromSql $ head . head $ results

setMessage (userId -> Nothing) _ = return ()
setMessage (userId -> Just uid) message = do
  quickQuery "delete from messages where userid = ?" [uid']
  quickQuery "insert into messages (userid, message) values (?, ?)" [uid', toSql message]
  commit
  where uid' = toSql . unUid $ uid
  