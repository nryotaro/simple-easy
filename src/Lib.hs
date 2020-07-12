module Lib
    ( someFunc
    )
where

import           Control.Monad
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

data Value
    = VLam (Value -> Value)
    | VNeutral Neutral

data Neutral
    = NPar Name
    | NApp Neutral Value

vpar :: Name -> Value
vpar n = VNeutral (NPar n)


type Env = [Value]

evalUp :: TermUp -> Env -> Value
evalUp (Ann e _  ) d = evalDown e d
evalUp (Par x    ) d = vpar x
evalUp (Var i    ) d = d !! i
evalUp (e1 :@: e2) d = vapp (evalUp e1 d) (evalDown e2 d)

vapp :: Value -> Value -> Value
vapp (VLam     f) v = f v
vapp (VNeutral n) v = VNeutral (NApp n v)

evalDown :: TermDown -> Env -> Value
evalDown (Inf i) d = evalUp i d
evalDown (Lam e) d = VLam (\x -> evalDown e (x : d))

data Kind = Star deriving(Show)

data Info = HasKind Kind | HasType Type deriving(Show)

type Context = [(Name, Info)]

type Result a = Either String a

throwError :: String -> Result a
throwError = Left

kindDown :: Context -> Type -> Kind -> Result ()
kindDown context (TPar x) Star = case lookup x context of
    Just (HasKind Star) -> Right ()
    Nothing             -> throwError "unknown identifier"


kindDown context (Fun k0 k1) Star = do
    kindDown context k0 Star
    kindDown context k1 Star

typeUp0 :: Context -> TermUp -> Result Type
typeUp0 = typeUp 0

typeUp :: Int -> Context -> TermUp -> Result Type
typeUp i context (Ann e t) = do
    kindDown context t Star
    typeDown i context e t
    return t

typeDown :: Int -> Context -> TermDown -> Type -> Result ()
typeDown i context (Inf e) t = do
    t' <- typeUp i context e
    unless (t == t') (throwError "type mismatch")

typeDown i context (Lam e) (Fun t t') = typeDown
    (i + 1)
    ((Bound i, HasType t) : context)
    (substDown 0 (Par (Bound i)) e)
    t'


substUp :: Int -> TermUp -> TermUp -> TermUp
substUp i r (Ann e t) = Ann (substDown i r e) t
substUp i r (Var j) = if i == j then r else Var j
substUp i r (Par y) = Par y
substUp i r (e1 :@: e2) = substUp i r e1 :@: substDown i r e2

substDown :: Int -> TermUp -> TermDown -> TermDown
substDown i r (Inf e) = Inf (substUp i r e)
substDown i r (Lam e) = Lam (substDown (i+1) r e)
