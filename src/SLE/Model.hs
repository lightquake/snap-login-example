{-# LANGUAGE ViewPatterns, OverloadedStrings #-}

module SLE.Model 
       ( getMessage 
       , setMessage 
       , initializeModel )
where

import qualified Data.Text as T

import Snap.Snaplet.Auth
import Snap.Snaplet.Hdbc

-- Get the message for the given user.
getMessage :: HasHdbc m c => AuthUser -> m (Maybe T.Text)
getMessage (userId -> Nothing) = return Nothing
getMessage (userId -> Just uid) = do
  results <- quickQuery "select message from messages where userid = ?" [toSql . unUid $ uid]
  return $ if null results then Nothing else Just . fromSql $ head . head $ results

-- Set the message for the given user.
setMessage :: HasHdbc m c => AuthUser -> T.Text -> m ()
setMessage (userId -> Nothing) _ = return ()
setMessage (userId -> Just uid) message = do
  quickQuery "delete from messages where userid = ?" [uid']
  quickQuery "insert into messages (userid, message) values (?, ?)" [uid', toSql message]
  commit
  where uid' = toSql . unUid $ uid
  
-- Create the SQL table to hold the model
initializeModel :: HasHdbc m c => m ()
initializeModel = do 
  quickQuery "create table if not exists messages (userid int primary key, message text)" []
  commit