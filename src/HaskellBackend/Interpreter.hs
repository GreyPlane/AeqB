{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HaskellBackend.Interpreter (eval, compile) where

import AST
import Import
import Util

type Executable a = StateT (MachineState a) (Cont (MachineState a)) a

class Functor f => Interpret f where
  hAlgebra :: f (Executable a) -> Executable a

instance (Interpret f, Interpret g) => Interpret (f :+: g) where
  hAlgebra (Inl r) = hAlgebra r
  hAlgebra (Inr r) = hAlgebra r

instance Interpret Current where
  hAlgebra (Current k) = modify (set continue False) >> get >>= k . view output

overState :: (Text -> Text) -> (Bool -> Bool) -> MachineState a -> MachineState a
overState o c = over output o . over continue c

instance Interpret Subsitute where
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

compile :: Interpret f => Free f a -> Executable a
compile = iterM hAlgebra

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
