{-# LANGUAGE TemplateHaskell, TypeSynonymInstances, MultiParamTypeClasses, FlexibleInstances #-}

{-

This module defines our application's state type and an alias for its handler
monad.

-}

module Application where

import Control.Monad.State
import Control.Monad.IO.Control

import Database.HDBC.Sqlite3

import Data.Lens.Template
import Data.Time.Clock

import Snap.Snaplet
import Snap.Snaplet.Auth
import Snap.Snaplet.Hdbc
import Snap.Snaplet.Heist
import Snap.Snaplet.Session

data App = App
    { _heist :: Snaplet (Heist App)
    , _auth :: Snaplet (AuthManager App)
    , _session :: Snaplet SessionManager
    , _hdbc :: Snaplet (HdbcSnaplet Connection)
    }


makeLens ''App

instance HasHdbc (Handler App subapp) Connection where
  getPool = withTop hdbc $ gets hdbcPool

type AppHandler = Handler App App

instance HasHdbc AppHandler Connection where
  getPool = with hdbc $ gets hdbcPool

instance HasHeist App where
    heistLens = subSnaplet heist

