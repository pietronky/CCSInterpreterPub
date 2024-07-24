module Context (
  Context,
  Definitions,
  checkMain,
  forceGetMain,
  prettyDefs,
  universe ) where

import Base.Errors (panic)
import Base.List (intercalate)
import Base.Map (Map, toList)
import qualified Base.Map as M (lookup)
import Base.Monads (fromJust)
import Expression (Val, Var, Ty)
import Process (Proc (..))

type Context = ((Int, Int), [Val], Definitions)
type Definitions = Map String ([(Var, Ty)], Proc)

checkMain :: Definitions -> Either String ()
checkMain defs =
  case M.lookup "Main" defs of
    Nothing -> Left "no \"Main\" process was found"
    Just ([], _) -> return ()
    Just (_, _) -> Left "\"Main\" process must be non-parametric"

forceGetMain :: Definitions -> Proc
forceGetMain defs =
  case checkMain defs of
    Left msg -> panic msg
    Right () -> snd (fromJust (M.lookup "Main" defs))

prettyDefs :: Definitions -> String
prettyDefs defs =
  let pretty s= if null s then ""
                else "(" ++ intercalate ", " (map (\(x, t) -> x ++ " : " ++ show t) s) ++ ")" in
  intercalate "\n" (map (\(pN, (sign, p)) -> pN ++ pretty sign ++ " = " ++ show p ++ ";") (toList defs))

universe :: Context -> [Val]
universe (bs, vals, _) = [show n | n <- [fst bs..snd bs]] ++ vals
