module Expression (
  BExpr (..),
  BOp (..),
  Expr (..),
  LOp (..),
  Op (..),
  Name,
  Parametric(..),
  Scope,
  Ty (..),
  Val,
  Var,
  eval,
  getType,
  getTypeFromVal,
  isCapitalized,
  natMax,
  test
) where

import Base.Char (isUpper)
import qualified Base.Char as C (isNumber)
import Base.Errors (panic)
import Base.List (find, nub)
import Prelude hiding (negate)

class Parametric a where
  replace :: Var -> Val -> a -> a
  typeCheck :: Scope -> a -> Either String a
  vEnums :: a -> [Val]

data Expr
  = Arithmetic Expr Op Expr
  | EEnum String
  | ENat Int
  | Var Var

data Op
  = Div
  | Minus
  | Mul
  | Plus
  deriving (Eq)

data BExpr
  = Comparison Expr BOp Expr
  | Not BExpr
  | Binary BExpr LOp BExpr

data BOp
  = Eq
  | Neq
  | Lt
  | Geq

data LOp
  = And
  | Or

data Ty = Enum | Nat | Tup [Ty] deriving (Eq, Show)

type Name = String
type Scope = [(Name, Ty)]
type Val = String
type Var = String

instance Parametric Expr where
  replace _ _ e@(EEnum _) = e
  replace _ _ e@(ENat _) = e
  replace x v (Arithmetic e1 op e2) =
    let (e1', e2') = (replace x v e1, replace x v e2) in
    case (e1', e2') of
      (EEnum _, _) -> panic "`replace` caused type mismatch"
      (_, EEnum _) -> panic "`replace` caused type mismatch"
      _ -> Arithmetic e1' op e2'
  replace x v (Var y) =
    if x == y then
      if getTypeFromVal v == Just Enum then
        EEnum v
      else if getTypeFromVal v == Just Nat then
        ENat (read v)
      else
        panic ("unknown type for " ++ v)
    else
      Var y

  typeCheck scope e = getType scope e >> return e

  vEnums (Arithmetic e1 _ e2) = nub (vEnums e1 ++ vEnums e2)
  vEnums (EEnum v) = [v]
  vEnums (ENat _) = []
  vEnums (Var _) = []

instance Show Expr where
  show (Arithmetic e1 op e2) =
    unwords [wrap e1, show op, wrap e2]
    where
      wrap e =
        case e of
          Arithmetic _ op' _ | op `elem` [Div, Mul],
                               op' `elem` [Minus, Plus] -> "(" ++ show e ++ ")"
          _ -> show e
  show (EEnum v) = v
  show (ENat n) = show n
  show (Var x) = x

instance Show Op where
  show Div = "/"
  show Minus = "-"
  show Mul = "\\cdot"
  show Plus = "+"

instance Parametric BExpr where
  replace x v (Comparison e1 op e2) = Comparison (replace x v e1) op (replace x v e2)
  replace x v (Not b) = Not (replace x v b)
  replace x v (Binary b1 op b2) = Binary (replace x v b1) op (replace x v b2)

  typeCheck scope e@(Comparison e1 op e2) =
    do t1 <- getType scope e1
       t2 <- getType scope e2
       case (t1, op, t2) of
        (Nat, _, Nat) -> return e
        (Enum, Eq, Enum) -> return e
        (Enum, Neq, Enum) -> return e
        _ -> Left ("type mismatch " ++ show [t1, t2] ++ " in " ++ show e)
  typeCheck scope (Not e) = typeCheck scope e
  typeCheck scope e@(Binary e1 _ e2) =
    typeCheck scope e1 >> typeCheck scope e2 >> return e

  vEnums (Comparison e1 _ e2) = nub (vEnums e1 ++ vEnums e2)
  vEnums (Not e) = vEnums e
  vEnums (Binary e1 _ e2) = nub (vEnums e1 ++ vEnums e2)

instance Show BExpr where
  show (Comparison e1 op e2) = unwords [show e1, show op, show e2]
  show (Not b) = "\\neg{" ++ show b ++ "}"
  show (Binary b1 op b2) = unwords [wrap op b1, show op, wrap op b2]
    where
      wrap :: LOp -> BExpr -> String
      wrap And b@(Binary _ Or _) = "(" ++ show b ++ ")"
      wrap _ b = show b

instance Show BOp where
  show Eq = "="
  show Neq = "\\neq"
  show Lt = "<"
  show Geq = "\\geq"

instance Show LOp where
  show And = "\\land"
  show Or = "\\lor"

eval :: (Int, Int) -> Expr -> Val
eval bounds e =
  case e of
    Arithmetic e1 op e2 ->
      let (v1, v2) = (eval bounds e1, eval bounds e2) in
      let (t1, t2) = (getTypeFromVal v1, getTypeFromVal v2) in
      case (t1, t2) of
        (Nothing, _) -> panic ("unknown type for " ++ v1)
        (_, Nothing) -> panic ("unknown type for " ++ v2)
        (Just Nat, Just Nat) ->
          case op of
            Div | read v2 == (0 :: Int) -> panic "division by zero in `eval`"
            Div                         -> safe div v1 v2
            Minus                       -> safe (-) v1 v2
            Mul                         -> safe (*) v1 v2
            Plus                        -> safe (+) v1 v2
        _ -> panic "non numeric pair in `eval`"
    ENat n  -> show (enforceBounds n)
    EEnum v -> v
    Var _   -> panic "can't evaluate variables in `eval`"
  where
    enforceBounds :: Int -> Int
    enforceBounds n
      | uncurry (>) bounds = panic "invalid bounds in `enforceBounds`"
      | n < fst bounds = max (fst bounds) 0
      | otherwise = min (min n (snd bounds)) natMax

    safe :: (Int -> Int -> Int) -> Val -> Val -> Val
    safe f v1 v2 = show (enforceBounds (f (enforceBounds (read v1)) (enforceBounds (read v2))))

getType :: Scope -> Expr -> Either String Ty
getType scope e@(Arithmetic e1 _ e2) =
  do t1 <- getType scope e1
     t2 <- getType scope e2
     case (t1, t2) of
      (Nat, Nat) -> return Nat
      _ -> Left ("type mismatch [Nat, Nat] / " ++ show [t1, t2] ++ " in " ++ show e)
getType _ (EEnum _) = return Enum
getType _ (ENat _) = return Nat
getType scope (Var x) =
  case find (\(y, _) -> x == y) scope of
    Nothing -> Left ("Couldn't solve " ++ x ++ " within scope")
    Just (_, t) -> return t

getTypeFromVal :: Val -> Maybe Ty
getTypeFromVal v
  | isCapitalized v  = return Enum
  | isNumber v = return Nat
  | otherwise = Nothing

isCapitalized :: String -> Bool
isCapitalized s = not (null s) && isUpper (head s) && all (`elem` ['a'..'z'] ++ ['0'..'9'] ++ ['_']) (tail s) 

isNumber :: String -> Bool
isNumber s = not (null s) && all C.isNumber s

natMax :: Int
natMax = 125

negate :: BExpr -> BExpr
negate (Comparison e1 op e2) = Comparison e1 (case op of
                                                Eq -> Neq
                                                Neq -> Eq
                                                Lt -> Geq
                                                Geq -> Lt) e2
negate (Not b) = b
negate (Binary b1 op b2) = Binary (negate b1) (case op of
                                                 And -> Or
                                                 Or  -> And) (negate b2)

test :: (Int, Int) -> BExpr -> Bool
test bounds (Comparison e1 op e2) =
  let (v1, v2) = (eval bounds e1, eval bounds e2) in
  let (t1, t2) = (getTypeFromVal v1, getTypeFromVal v2) in
  case (t1, t2) of 
    (Nothing, _) -> panic ("unknown type for " ++ v1)
    (_, Nothing) -> panic ("unknown type for " ++ v2)
    (Just Enum, Just Enum) ->
      case op of
        Eq  -> v1 == v2
        Neq -> v1 /= v2
        _   -> panic "enum. values used in comparison within `test`"
    (Just Nat, Just Nat) ->
      let n1 :: Int
          n1 = read v1 in
      let n2 :: Int
          n2 = read v2 in
      (case op of
        Eq  -> (==)
        Neq -> (/=)
        Lt  -> (<)
        Geq -> (>=)) n1 n2
    _ -> panic "type mismatch in `test`"

test bounds (Not e) = test bounds (negate e)

test bounds (Binary e1 op e2) = (case op of
                                   And -> (&&)
                                   Or  -> (||)) (test bounds e1) (test bounds e2)