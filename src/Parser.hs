{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Parser where

import AST (ControlFlow (Return), Current, Subsitute, current, subsitute)
import HaskellBackend.Interpreter (compile, eval)
import Import
import RIO.Text (pack)
import Util (Position (End, Start))

type Parser a = Parsec Void Text a

position :: Parser Position
position =
  Start <$ string "start"
    <|> End <$ string "end"
    <?> "position keyword"

control :: Parser ControlFlow
control = (Return <$ string "return") <?> "control keyword"

keyword :: Parser a -> Parser a
keyword = between (char '(') (char ')')

stringLit :: Parser Text
stringLit = pack <$> someTill asciiChar (char '=')

line :: Parser (Free (Current :+: Subsitute) ())
line = do
  lk <- optional $ keyword position
  l <- pack <$> someTill letterChar (char '=')
  rk <- optional $ keyword $ (Left <$> position) <|> (Right <$> control)
  r <- pack <$> someTill letterChar (eof <|> void eol)
  return $ subsitute (lk, l) (rk, r)

-- return $ pack $ show lk ++ show l ++ show rk ++ show r

program :: Parser (Free (Current :+: Subsitute) Text)
program = (>> current) . sequence <$> some line

-- >>> ttt
-- "JsLoveKoishi"
test = runParser program "code.js" "(start)x=y\ny=JsX\nX=Love\n(end)x=Koishi"

ttt = case test of
  Left peb -> undefined
  Right monad -> eval (compile monad) "xx"
