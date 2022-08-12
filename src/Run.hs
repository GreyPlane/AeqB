{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Run (run) where

import Exception
import HaskellBackend.Interpreter (compile, eval)
import Import
import qualified Parser as P
import RIO.ByteString (getLine)
import RIO.FilePath (takeFileName)
import qualified RIO.Text as T
import RIO.Text.Lazy (toStrict)
import System.IO (putStrLn)
import Text.RawString.QQ (r)

logo :: String
logo =
  [r|
     _              ____    _                      
    / \     _____  | __ )  | |    __ _ _ __   __ _ 
   / _ \   |_____| |  _ \  | |   / _` | '_ \ / _` |
  / ___ \  |_____| | |_) | | |__| (_| | | | | (_| |
 /_/   \_\         |____/  |_____\__,_|_| |_|\__, |
                                             |___/                         
Input your lines
|]

run :: RIO App ()
run =
  do
    liftIO $ putStrLn logo
    catch
      (build >>= go)
      ( \case
          MalformedProgram message -> do
            logError $ display message
            exitFailure
      )
  where
    go program = do
      input <- decodeUtf8Lenient <$> getLine
      unless (input == ":quit") $
        logInfo (display $ T.concat ["> ", program input]) >> go program

processFile :: String -> Text -> RIO env (Text -> Text)
processFile filename content =
  let result = eval . compile <$> P.parse filename content
   in case result of
        Left errorBundle -> throwIO $ MalformedProgram $ T.pack $ errorBundlePretty errorBundle
        Right f -> return f

build :: HasAppOptions env => RIO env (Text -> Text)
build = do
  options <- view appOptionsL
  let path = optionsProgramFile options
  let filename = takeFileName path
  withLazyFileUtf8 path (processFile filename . toStrict)
