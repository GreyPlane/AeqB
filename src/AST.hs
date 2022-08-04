{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AST (compile, eval, Subsitute, subsitute, Current, current, ControlFlow (..), RhsKeywords, LhsKeywords) where

import Import
import Util

data ControlFlow = Return deriving (Show)

type RhsKeywords = Either Position ControlFlow

type LhsKeywords = Position

data MachineState r = MachineState
  { -- current text of program
    _output :: Text,
    -- if program should keep running from the start after abort
    _continue :: Bool,
    -- refer the continuation for abort the program
    _ptr :: MachineState r -> Cont (MachineState r) r
  }

output :: Lens' (MachineState a) Text
output = lens _output (\x y -> x {_output = y})

ptr :: Lens' (MachineState a) (MachineState a -> Cont (MachineState a) a)
ptr = lens _ptr (\x y -> x {_ptr = y})

continue :: Lens' (MachineState a) Bool
continue = lens _continue (\x y -> x {_continue = y})

data Current e = Current (Text -> e) deriving (Functor)

current :: (Current :<: f) => Free f Text
current = inject (Current Pure)

data Subsitute e = Subsitute (Maybe LhsKeywords, Text) (Maybe RhsKeywords, Text) e deriving (Functor)

subsitute :: (Subsitute :<: f) => (Maybe LhsKeywords, Text) -> (Maybe RhsKeywords, Text) -> Free f ()
subsitute a b = inject (Subsitute a b (Pure ()))

type Executable a = StateT (MachineState a) (Cont (MachineState a)) a

class Functor f => Eval f where
  hAlgebra :: f (Executable a) -> Executable a

instance (Eval f, Eval g) => Eval (f :+: g) where
  hAlgebra (Inl r) = hAlgebra r
  hAlgebra (Inr r) = hAlgebra r

instance Eval Current where
  hAlgebra (Current k) = modify (set continue False) >> get >>= k . view output

overState :: (Text -> Text) -> (Bool -> Bool) -> MachineState a -> MachineState a
overState o c = over output o . over continue c

instance Eval Subsitute where
  hAlgebra (Subsitute (patternKeyword, pattern) (subsitutionKeyword, subsitution) k) = get >>= f
    where
      f s =
        let isMatched = matchByPosition (patternKeyword, s ^. output) pattern
         in if isMatched
              then lift $ s ^. ptr $ overState updateOutput updateContinue s
              else k
      updateOutput = case subsitutionKeyword of
        Just (Right Return) -> const subsitution
        Just (Left subsitutionPosition) -> replaceByPosition (patternKeyword, pattern) (Just subsitutionPosition, subsitution)
        Nothing -> replaceByPosition (patternKeyword, pattern) (Nothing, subsitution)
      updateContinue = case subsitutionKeyword of
        Just (Right Return) -> const False
        _ -> const True

compile :: Eval f => Free f a -> Executable a
compile = iterM hAlgebra

sampleProgram :: Free (Current :+: Subsitute) Text
sampleProgram = do
  -- subsitute (Nothing, "a") (Just (Right Return), "b")
  -- subsitute (Nothing, "b") (Just $ Right Return, "n")
  -- subsitute (Nothing, "c") (Just $ Right Return, "x")
  subsitute (Nothing, "a") (Nothing, "w")
  subsitute (Nothing, "w") (Nothing, "c")
  subsitute (Just End, "b") (Just $ Left Start, "haha")
  current

-- >>> (eval "aaaaaaab")
-- "hchcccccccc"

eval :: Executable Text -> Text -> Text
eval program input = runCont eval'' id ^. output
  where
    eval' = execStateT program
    eval'' = eval'''' (eval' . MachineState input False) eval'''
    eval''' s =
      eval'''' (\goto -> eval' $ s & ptr .~ goto) eval'''
    eval'''' evalWithEscape k =
      do
        s <- callCC evalWithEscape
        if s ^. continue then k s else pure s
