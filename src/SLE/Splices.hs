{-# LANGUAGE OverloadedStrings #-}

{-| Various splices that SLE uses. -}

module SLE.Splices 
       ( errorBind )
where
  
import           Data.Monoid (mappend)
import qualified Data.Text as T

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
            otherwise -> textSplice " error"
            
(<>) = mappend