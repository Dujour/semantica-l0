-- | Implements a Operational Semantic (small step) for L0

module Semantic where

import           Syntax


-- | Auxiliary Function that verify if a term is a Numeric Value
isNumericValue :: Terms -> Bool
isNumericValue (SUCC term) = isNumericValue term
isNumericValue ZERO        = True
isNumericValue _           = False

step :: Terms -> Maybe Terms

-- | Step rules to IF statement
step ( IF (TRUE  , t2 , _  ) ) = Just t2
step ( IF (FALSE , _  , t3 ) ) = Just t3
step ( IF (t1    , t2 , t3 ) ) = case step t1 of
                                   Just t1' -> Just $ IF (t1', t2, t3)
                                   Nothing  -> Just t1

-- | Step rules to numeric values operations

-- | Step rules to SUCC statement
step ( SUCC t )                = case step t of
                                   Just t' -> Just $ SUCC t'
                                   Nothing -> Just t

-- | Step rules to PRED statement
step ( PRED ZERO )             = Just ZERO
step ( PRED ( SUCC nv ))       = Just nv
step ( PRED t )                = case step t of
                                   Just t' -> Just $ PRED t'
                                   Nothing -> Just t

-- | Step rules to ISZERO statement
step (ISZERO ZERO)             = Just TRUE
step ( ISZERO (SUCC nv))       = if isNumericValue nv then Just FALSE else Nothing
step ( ISZERO t)               = case step t of
                                   Just t' -> Just $ ISZERO t'
                                   Nothing -> Just t

-- | No rules applies
step _ = Nothing

eval :: Terms -> Terms
eval t =
  case step t of
    Just t' -> eval t'
    Nothing -> t
