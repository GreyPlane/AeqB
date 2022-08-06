{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AST
  ( MachineState (MachineState),
    output,
    ptr,
    continue,
    executed,
    Subsitute (Subsitute),
    subsitute,
    Current (Current),
    current,
    Position (Start, End),
    PatternAttr (PP, Once),
    SubsitutionAttr (SP, Return),
  )
where

import Import

data Position = Start | End deriving (Show)

data PatternAttr = PP Position | Once

data SubsitutionAttr = SP Position | Return

data MachineState r = MachineState
  { -- current text of program
    _output :: Text,
    -- if program should keep running from the start after abort
    _continue :: Bool,
    -- record if certain line of program has been executed once
    _executed :: IntMap Bool,
    -- refer the continuation for abort the program
    _ptr :: MachineState r -> Cont (MachineState r) r
  }

output :: Lens' (MachineState a) Text
output = lens _output (\x y -> x {_output = y})

ptr :: Lens' (MachineState a) (MachineState a -> Cont (MachineState a) a)
ptr = lens _ptr (\x y -> x {_ptr = y})

continue :: Lens' (MachineState a) Bool
continue = lens _continue (\x y -> x {_continue = y})

executed :: Lens' (MachineState a) (IntMap Bool)
executed = lens _executed (\x y -> x {_executed = y})

data Current e = Current (Text -> e) deriving (Functor)

current :: (Current :<: f) => Free f Text
current = inject (Current Pure)

data Subsitute e = Subsitute Int (Maybe PatternAttr, Text) (Maybe SubsitutionAttr, Text) e deriving (Functor)

subsitute :: (Subsitute :<: f) => Int -> (Maybe PatternAttr, Text) -> (Maybe SubsitutionAttr, Text) -> Free f ()
subsitute lineNum pattern subsitution = inject (Subsitute lineNum pattern subsitution (Pure ()))