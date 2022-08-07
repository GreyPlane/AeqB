{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HaskellBackend.Render (render) where

import AST
import Import
import qualified RIO.Text as T

class Functor f => Render f where
  hAlgebra :: f Text -> Text

instance (Render f, Render g) => Render (f :+: g) where
  hAlgebra (Inl r) = hAlgebra r
  hAlgebra (Inr r) = hAlgebra r

instance Render Current where
  hAlgebra (Current k) = k ""

instance Render Subsitute where
  hAlgebra (Subsitute _ (patternAttr, pattern) (subsitutionAttr, subsitution) k) =
    T.concat
      [ maybe "" renderPatternAttr patternAttr,
        pattern,
        "=",
        maybe "" renderSubsitutionAttr subsitutionAttr,
        subsitution,
        if T.null k then k else T.concat ["\n", k]
      ]
    where
      renderSubsitutionAttr :: SubsitutionAttr -> Text
      renderSubsitutionAttr = \case
        SP po -> "(" <> renderPosition po <> ")"
        Return -> "(" <> "return" <> ")"
      renderPatternAttr :: PatternAttr -> Text
      renderPatternAttr = \case
        PP po -> "(" <> renderPosition po <> ")"
        Once -> "(" <> "once" <> ")"
      renderPosition :: Position -> Text
      renderPosition = \case
        Start -> "start"
        End -> "end"

render :: Render f => Free f Text -> Text
render = iter hAlgebra