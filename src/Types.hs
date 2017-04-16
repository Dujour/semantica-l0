-- | Implements type system for L0 language

module Types where

import           Syntax

data Types =
    NAT
  | BOOL
  | INVALIDTYPE
  deriving (Eq, Show)

typeInfer :: Terms -> Types


-- | Type rules for "base" values
typeInfer ZERO  = NAT
typeInfer TRUE  = BOOL
typeInfer FALSE = BOOL


-- | Type rule to SUCC statement
typeInfer (SUCC t) = case typeInfer t of
  NAT -> NAT
  _   -> INVALIDTYPE


-- | Type rule to PRED statement
typeInfer (PRED t) = case typeInfer t of
  NAT -> NAT
  _   -> INVALIDTYPE


-- | Type rule to IS-ZERO statement
typeInfer (ISZERO t) = case typeInfer t of
  NAT -> BOOL
  _   -> INVALIDTYPE


-- | Type rule to IF statement
typeInfer (IF(t1,t2,t3)) =
  case typeInfer t1 of
    BOOL ->
      if typeTermTwo == typeTermThree
      then typeTermTwo
      else INVALIDTYPE
       where
         typeTermTwo = typeInfer t2
         typeTermThree = typeInfer t3

    _    -> INVALIDTYPE
