{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Run (run) where

import Exception
import HaskellBackend.Interpreter (compile, eval)
import Import
import qualified Parser as P
import RIO.ByteString (getLine)
import RIO.FilePath (takeFileName)
import RIO.Text.Lazy (toStrict)

run :: RIO App ()
run =
  do
    logInfo "We're inside the application!"
    build >>= go
  where
    go p = do
      input <- decodeUtf8Lenient <$> getLine
      unless (input == ":quit") $
        logInfo (display $ p input) >> go p

processFile :: String -> Text -> RIO env (Text -> Text)
processFile filename content =
  let result = eval . compile <$> P.parse filename content
   in case result of
        Left (ParseErrorBundle _ _) -> throwIO MalformedProgram
        Right f -> return f

build :: HasAppOptions env => RIO env (Text -> Text)
build = do
  options <- view appOptionsL
  let path = optionsProgramFile options
  let filename = takeFileName path
  withLazyFileUtf8 path (processFile filename . toStrict)