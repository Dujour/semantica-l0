-- | Implements Syntax and Types terms to L0 Language

module Syntax where

data Terms =
    TRUE | FALSE
  | ZERO | SUCC Terms
  | IF (Terms, Terms, Terms)
  | ISZERO Terms | PRED Terms
  deriving (Show)
