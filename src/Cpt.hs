module Cpt
    ( Cpt(..)
    ) where

data Cpt = Number Int
        | Symbol String
        | List [ Cpt ]
        | Boolean Bool
        | Array [ Cpt ]
        | Function [ Cpt ]
        | Argument [ Cpt ]
        | Instruction [ Cpt ]
        | Condition [ Cpt ]
        deriving Show

instance Eq Cpt where
  (Number x) == (Number y) = x == y
  (Symbol x) == (Symbol y) = x == y
  (List xs) == (List ys) = xs == ys
  (Boolean x) == (Boolean y) = x == y
  _ == _ = False
