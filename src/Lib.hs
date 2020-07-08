module Lib
    ( someFunc
    )
where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Name
    = Const String
    | Bound Int
    | Unquoted Int
    deriving(Show, Eq)

data TermUp 
    = Ann TermDown Type 
    | Var Int 
    | Par Name 
    | TermUp :@: TermDown
    deriving(Show, Eq)

data TermDown 
    = Inf TermUp 
    | Lam TermDown 
    deriving(Show, Eq)

data Type 
    = TPar Name 
    | Fun Type Type 
    deriving(Show, Eq)
