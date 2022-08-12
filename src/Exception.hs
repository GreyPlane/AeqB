module Exception where

import Import

newtype MalformedProgram = MalformedProgram Text
  deriving (Show, Typeable)

instance Exception MalformedProgram
