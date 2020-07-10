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

kindDown :: Context -> Type -> Kind -> Either String ()
kindDown context (TPar x) Star = case lookup x context of
    Just (HasKind Star) -> Right ()
    Nothing             -> Left "unknown identifier"


kindDown context (Fun k0 k1) Star = do
    kindDown context k0 Star
    kindDown context k1 Star
