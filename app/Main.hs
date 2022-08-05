{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Import
import Options.Applicative.Simple
import qualified Paths_AeqB
import RIO.Process
import Run

optParser :: Parser AppOptions
optParser =
  AppOptions
    <$> switch
      ( long "verbose"
          <> short 'v'
          <> help "Verbose output?"
      )
      <*> strOption
        ( long "file"
            <> short 'f'
            <> metavar "FIELNAME"
            <> help "Input program file"
        )

main :: IO ()
main =
  do
    (options, ()) <-
      simpleOptions
        $(simpleVersion Paths_AeqB.version)
        "A=B"
        "simple text substitution language"
        optParser
        empty
    lo <- logOptionsHandle stderr (optionsVerbose options)
    pc <- mkDefaultProcessContext
    withLogFunc lo $ \lf ->
      let app =
            App
              { appLogFunc = lf,
                appProcessContext = pc,
                appOptions = options
              }
       in runRIO app run
