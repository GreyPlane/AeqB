{-# LANGUAGE NoImplicitPrelude #-}

module Types where

import RIO
import RIO.Process

-- | Command line arguments
data AppOptions = AppOptions
  { optionsVerbose :: !Bool,
    optionsProgramFile :: !String
  }

data App = App
  { appLogFunc :: !LogFunc,
    appProcessContext :: !ProcessContext,
    appOptions :: !AppOptions
    -- Add other app-specific configuration information here
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x {appLogFunc = y})

instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x {appProcessContext = y})

class HasAppOptions env where
  appOptionsL :: Lens' env AppOptions

instance HasAppOptions AppOptions where
  appOptionsL = id

instance HasAppOptions App where
  appOptionsL = lens appOptions (\x y -> x {appOptions = y})
