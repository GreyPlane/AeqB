{-# LANGUAGE NoImplicitPrelude #-}

-- | Silly utility module, used to demonstrate how to write a test
-- case.
module Util
  ( replaceFirst,
    replaceAppend,
    replacePrepend,
    replacePrefix,
    replacePrefixAppened,
    replaceSuffix,
    replaceSuffixPrepend,
    Position (Start, End),
    matchByPosition,
    replaceByPosition,
  )
where

import AST (Position (End, Start))
import RIO
import RIO.Text (isInfixOf, isPrefixOf, isSuffixOf)
import qualified RIO.Text as T
import RIO.Text.Partial (breakOn)

replaceFirst :: Text -> Text -> Text -> Text
replaceFirst pattern subsitution text
  | T.null back = text
  | otherwise = T.concat [front, subsitution, T.drop (T.length pattern) back]
  where
    (front, back) = breakOn pattern text

replacePrepend :: Text -> Text -> Text -> Text
replacePrepend pattern subsitution text
  | T.null back = text
  | otherwise = T.concat [subsitution, front, T.drop (T.length pattern) back]
  where
    (front, back) = breakOn pattern text

replaceAppend :: Text -> Text -> Text -> Text
replaceAppend pattern subsitution text
  | T.null back = text
  | otherwise = T.concat [front, T.drop (T.length pattern) back, subsitution]
  where
    (front, back) = breakOn pattern text

replacePrefix :: Text -> Text -> Text -> Text
replacePrefix pattern subsitution text
  | T.isPrefixOf pattern text = T.concat [subsitution, T.drop (T.length pattern) text]
  | otherwise = text

replacePrefixAppened :: Text -> Text -> Text -> Text
replacePrefixAppened pattern subsitution text
  | T.isPrefixOf pattern text = T.concat [T.drop (T.length pattern) text, subsitution]
  | otherwise = text

replaceSuffix :: Text -> Text -> Text -> Text
replaceSuffix pattern subsitution text
  | T.isSuffixOf pattern text = T.concat [T.dropEnd (T.length pattern) text, subsitution]
  | otherwise = text

replaceSuffixPrepend :: Text -> Text -> Text -> Text
replaceSuffixPrepend pattern subsitution text
  | T.isSuffixOf pattern text = T.concat [subsitution, T.dropEnd (T.length pattern) text]
  | otherwise = text

matchByPosition :: Text -> Text -> Maybe Position -> Bool
matchByPosition text pattern Nothing = pattern `isInfixOf` text
matchByPosition text pattern (Just Start) = pattern `isPrefixOf` text
matchByPosition text pattern (Just End) = pattern `isSuffixOf` text

replaceByPosition :: (Maybe Position, Text) -> (Maybe Position, Text) -> Text -> Text
replaceByPosition (Nothing, pattern) (Nothing, subsitution) text = replaceFirst pattern subsitution text
replaceByPosition (Just Start, pattern) (Nothing, subsitution) text = replacePrefix pattern subsitution text
replaceByPosition (Just End, pattern) (Nothing, subsitution) text = replaceSuffix pattern subsitution text
replaceByPosition (Nothing, pattern) (Just Start, subsitution) text = replacePrepend pattern subsitution text
replaceByPosition (Nothing, pattern) (Just End, subsitution) text = replaceAppend pattern subsitution text
replaceByPosition (Just Start, pattern) (Just Start, subsitution) text = replacePrefix pattern subsitution text
replaceByPosition (Just Start, pattern) (Just End, subsitution) text = replacePrefixAppened pattern subsitution text
replaceByPosition (Just End, pattern) (Just Start, subsitution) text = replaceSuffixPrepend pattern subsitution text
replaceByPosition (Just End, pattern) (Just End, subsitution) text = replaceSuffix pattern subsitution text