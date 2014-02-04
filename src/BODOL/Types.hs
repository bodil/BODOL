{-# LANGUAGE GADTs #-}

module BODOL.Types where

class (Show t, Eq t) => LType t

--- Datatype for all runtime types

data LCons a b where
  LCons :: (LType a, LType d) => a -> d -> LCons a d
instance LType (LCons a d)
instance Eq (LCons a d) where (==) (LCons a1 d1) (LCons a2 d2) = (a1 == a2) && (d1 == d2)
instance Show (LCons a d) where show (LCons a d) = "(" ++ show a ++ " . " ++ show d ++ ")"

data LSymbol = LSymbol String deriving (Eq, Show)
instance LType LSymbol

data LString = LString String deriving (Eq)
instance LType LString
instance Show LString where show (LString str) = "\"" ++ str ++ "\""

data LNumber = LNumber String deriving (Eq, Show) -- TODO obviously not a string, insert numeric tower pls
instance LType LNumber

data LBool = LBool Bool deriving (Eq)
instance LType LBool
instance Show LBool where show (LBool True) = "#t"
                          show (LBool False) = "#f"

car :: (LType a, LType b) => LCons a b -> a
car (LCons a _) = a

cdr :: (LType a, LType b) => LCons a b -> b
cdr (LCons _ d) = d

cons :: (LType a, LType b) => a -> b -> LCons a b
cons = LCons
