{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AST
  ( MachineState (MachineState),
    output,
    ptr,
    continue,
    Subsitute (Subsitute),
    subsitute,
    Current (Current),
    current,
    ControlFlow (Return),
    Position (Start, End),
    RhsKeywords,
    LhsKeywords,
  )
where

import Import

data Position = Start | End deriving (Show)

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