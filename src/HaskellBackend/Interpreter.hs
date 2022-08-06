{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HaskellBackend.Interpreter (eval, compile) where

import AST
import Data.IntMap
import qualified Data.IntMap as IM
import Import
import Util (matchByPosition, replaceByPosition)

type Executable a = StateT (MachineState a) (Cont (MachineState a)) a

class Functor f => Interpret f where
  hAlgebra :: f (Executable a) -> Executable a

instance (Interpret f, Interpret g) => Interpret (f :+: g) where
  hAlgebra (Inl r) = hAlgebra r
  hAlgebra (Inr r) = hAlgebra r

instance Interpret Current where
  hAlgebra (Current k) = modify resetState >> get >>= k . view output
    where
      resetState = set continue False . set executed IM.empty

updateState :: (Text -> Text) -> Bool -> (IntMap Bool -> IntMap Bool) -> MachineState a -> MachineState a
updateState o c e = over output o . set continue c . over executed e

instance Interpret Subsitute where
  hAlgebra (Subsitute lineNum (patternAttr, pattern) (subsitutionAttr, subsitution) k) = get >>= f
    where
      f s =
        let shouldRun = matcher s
         in if shouldRun
              then lift $ s ^. ptr $ updateState updateOutput updateContinue updateExecuted s
              else k
      matcher mstate =
        let ctext = mstate ^. output
            erecord = mstate ^. executed
            executedOnce = fromMaybe False $ erecord !? lineNum
            matchedByPosition = matchByPosition ctext pattern
         in case patternAttr of
              Nothing -> matchedByPosition Nothing
              Just Once -> not executedOnce && matchedByPosition Nothing
              Just (PP pos) -> matchedByPosition (Just pos)
      replacer = case patternAttr of
        Just Once -> replaceByPosition (Nothing, pattern)
        Nothing -> replaceByPosition (Nothing, pattern)
        Just (PP ppos) -> replaceByPosition (Just ppos, pattern)
      updateOutput = case subsitutionAttr of
        Just Return -> const subsitution
        Just (SP pos) -> replacer (Just pos, subsitution)
        Nothing -> replacer (Nothing, subsitution)
      updateContinue = case subsitutionAttr of
        Just Return -> False
        _ -> True
      updateExecuted = case patternAttr of
        Just Once -> insert lineNum True
        _ -> id

compile :: Interpret f => Free f a -> Executable a
compile = iterM hAlgebra

eval :: Executable Text -> Text -> Text
eval program input = runCont eval'' id ^. output
  where
    eval' = execStateT program
    eval'' = eval'''' (eval' . MachineState input False IM.empty) eval'''
    eval''' s =
      eval'''' (\goto -> eval' $ s & ptr .~ goto) eval'''
    eval'''' evalWithEscape k =
      do
        s <- callCC evalWithEscape
        if s ^. continue then k s else pure s
