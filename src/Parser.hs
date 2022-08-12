{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Parser (Parser.parse) where

import AST (Current, PatternAttr (..), Subsitute, SubsitutionAttr (..), current, subsitute)
import Import
import RIO.Text (pack)
import Util (Position (End, Start))

type Parser a = Parsec Void Text a

position :: Parser Position
position =
  Start <$ string "start"
    <|> End <$ string "end"
    <?> "position keyword"

subsitutionAttr :: Parser SubsitutionAttr
subsitutionAttr = (Return <$ string "return") <|> (SP <$> position) <?> "subsitution attribute"

patternAttr :: Parser PatternAttr
patternAttr = (Once <$ string "once") <|> (PP <$> position) <?> "pattern attribute"

keyword :: Parser a -> Parser a
keyword = between (char '(') (char ')')

line :: Parser (Int -> Free Subsitute ())
line = do
  pattr <- optional $ keyword patternAttr
  p <- pack <$> manyTill asciiChar (char '=')
  sattr <- optional $ keyword subsitutionAttr
  s <- pack <$> manyTill asciiChar (eof <|> void eol)
  return $ \lineNum -> subsitute lineNum (pattr, p) (sattr, s)

program :: Parser (Free (Current :+: Subsitute) Text)
program = (>> current) . hoistFree inj . traverse (\(l, f) -> f l) . zip [0 ..] <$> some line

parse ::
  String ->
  Text ->
  Either
    (ParseErrorBundle Text Void)
    (Free (Current :+: Subsitute) Text)
parse = runParser program