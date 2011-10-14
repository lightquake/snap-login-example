{-# LANGUAGE TemplateHaskell #-}

{-

This module defines our application's state type and an alias for its handler
monad.

-}

module Application where

import Data.Lens.Template
import Data.Time.Clock

import Snap.Snaplet
import Snap.Snaplet.Auth
import Snap.Snaplet.Heist
import Snap.Snaplet.Session

data App = App
    { _heist :: Snaplet (Heist App)
    , _auth :: Snaplet (AuthManager App)
    , _session :: Snaplet SessionManager
    }

type AppHandler = Handler App App

makeLens ''App

instance HasHeist App where
    heistLens = subSnaplet heist

