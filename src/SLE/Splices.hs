{-# LANGUAGE OverloadedStrings #-}

{-| Various splices that SLE uses. -}

module SLE.Splices 
       ( errorBind 
       , usernameSplice )
where
  
import           Data.Monoid        (mappend)
import qualified Data.Text as T

import           Snap.Snaplet       (withTop)
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Heist

import           Text.Templating.Heist
import qualified Text.XmlHtml as X


-- Splices "error" in if the corresponding field has errors, and
-- nothing otherwise.
errorBind :: (Monad m) => T.Text -> (T.Text, Splice m)
errorBind field = (field <> "-error", splice)
  where splice = do
          -- we run the splice on
          -- <field-errors><error/></field-errors>; if we get nodes
          -- back, then there were errors. Kind of a hack at the
          -- moment, but I don't know a better way to do it.
          let node = X.Element (field <> "-errors") []  [X.Element "error" [] []]
          errorList <- runNodeList [node]
          -- the space before error is because we don't have a space
          -- in the actual class
          textSplice $ if null errorList then "" else " error"

-- The username of the currently logged-in user; evaluates to an empty
-- node if there is none.
usernameSplice :: SnapletSplice b (AuthManager b)
usernameSplice = do
  user <- liftHandler currentUser
  maybe (return []) (liftHeist . textSplice . userLogin) user

-- Text is a Monoid, so we can use <> to concat them as opposed to `T.append`.
(<>) = mappend