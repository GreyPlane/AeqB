module Exception where

import Import

data MalformedProgram = MalformedProgram
  deriving (Show, Typeable)

instance Exception MalformedProgram
