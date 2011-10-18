{-# LANGUAGE ViewPatterns, OverloadedStrings #-}

module SLE.Model 
       ( getMessage )
where

import Data.Text

import Snap.Snaplet.Auth

getMessage :: AuthUser -> Maybe Text
getMessage (userId -> Nothing) = Nothing
getMessage (userId -> Just uid) = Just "a message"
