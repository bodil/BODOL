module BODOL.Types where

--- Datatype for all runtime types

data LCons = LCons LType LType
instance Show LCons where show (LCons a d) = "(" ++ show a ++ " . " ++ show d ++ ")"

data LSymbol = LSymbol String
instance Show LSymbol where show (LSymbol sym) = sym

data LString = LString String
instance Show LString where show (LString str) = "\"" ++ str ++ "\""

data LNumber = LNumber String -- TODO obviously not a string, insert numeric tower pls
instance Show LNumber where show (LNumber num) = num

data LBoolean = LBoolean Bool
instance Show LBoolean where show (LBoolean True) = "#t"
                             show (LBoolean False) = "#f"

data LType = LTCons LCons -- nonono this is horrid, make it a typeclass maybe
           | LTSymbol LSymbol
           | LTString LString
           | LTNumber LNumber
           | LTBoolean LBoolean

instance Show LType where
  show (LTCons val) = show val
  show (LTSymbol val) = show val
  show (LTString val) = show val
  show (LTNumber val) = show val
  show (LTBoolean val) = show val

car :: LCons -> LType
car (LCons a _) = a

cdr :: LCons -> LType
cdr (LCons _ d) = d

cons :: LType -> LType -> LCons
cons = LCons
