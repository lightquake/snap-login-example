{-# LANGUAGE OverloadedStrings #-}

{-| Various splices that SLE uses. -}

module SLE.Splices 
       ( errorBind 
       , usernameSplice )
where
  
import           Data.Monoid        (mappend)
import qualified Data.Text as T

import           Snap.Snaplet       (withTop)
import           Snap.Snaplet.Auth  (currentUser, userLogin)
import           Snap.Snaplet.Heist (liftHandler, liftHeist)

import           Text.Templating.Heist
import qualified Text.XmlHtml as X


-- Splices "error" in if the corresponding field has errors, and
-- nothing otherwise.
errorBind :: (Monad m) => T.Text -> (T.Text, Splice m)
errorBind field = (field <> "-error", splice)
  where splice = do
          -- we run the splice on
          -- <field-errors><error/></field-errors>; if we get nodes
          -- back, then there were errors.
          let node = X.Element (field <> "-errors") []  [X.Element "error" [] []]
          errorList <- runNodeList [node]
          case errorList of
            [] -> textSplice ""
            _  -> textSplice " error"

-- The username of the currently logged-in user.
usernameSplice auth = do
  user <- liftHandler $ withTop auth currentUser
  maybe (return []) (liftHeist . textSplice . userLogin) user

(<>) = mappend