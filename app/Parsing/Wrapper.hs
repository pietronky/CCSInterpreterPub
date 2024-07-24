module Parsing.Wrapper (parseCCSEnvironment) where

import Base.Errors (panic)
import Base.List (count, exists, intercalate, nub)
import Base.Map (fromList, toList)
import qualified Base.Map as M (lookup)
import Base.Monads (fromLeft, isLeft)
import Context (Context, Definitions)
import Expression (Name, Parametric (typeCheck, vEnums), Scope, Ty (..), Var)
import Parsing.Lexer (runAlex)
import Parsing.Parser (happyParseCCSEnvironment)
import Process (Chan, Proc)
import Translation (chans)

type ParseResult = ((Int, Int), [(String, [(Var, Ty)], Proc)])
type ParseResult' = ((Int, Int), Definitions)

checkChans :: ParseResult' -> Either String ParseResult'
checkChans res@(_, defs) =
  let wronglyUsedChans p =
        let maybeCs = chans defs p in
        case maybeCs of
          Nothing -> panic "assertion `isJust (chans defs p)` failed (in `Wrapper.checkChans`)"
          Just cs -> nub (map fst (filter (\(c, isVP) -> exists (\(c', isVP') -> c == c' && isVP == not isVP') cs) cs)) in
  let psWithWronglyUsedChans = filter (\(_, cs) -> not (null cs)) (map (\(pN, (_, p)) -> (pN, wronglyUsedChans p)) (toList defs)) in
  if not (null psWithWronglyUsedChans) then
    let pretty :: (String, [Chan]) -> String
        pretty (pN, cs) =
          let msg c = "chan. " ++ c ++ " used both as standard and v.p (in \"" ++ pN ++ "\")" in
          intercalate "\n" (map msg cs) in
    Left (intercalate "\n" (map pretty psWithWronglyUsedChans))
  else
    Right res

checkNames :: ParseResult -> Either String ParseResult'
checkNames (bounds, pList) =
  let pNames = map (\(pN, _, _) -> pN) pList in
  let dupPNames = filter (\(_, occurrences) -> occurrences > 1) (map (\pN -> (pN, count (== pN) pNames)) (nub pNames)) in
  if not (null dupPNames) then
    let pretty :: (String, Int) -> String
        pretty (pN, occurrences) = "const. \"" ++ pN ++ "\" declared " ++ show occurrences ++ " times" in
    Left (intercalate "\n" (map pretty dupPNames))
  else
    Right (bounds, fromList (map (\(pN, sign, p) -> (pN, (sign, p))) pList))

checkTypes :: ParseResult' -> Either String ParseResult'
checkTypes res@(_, defs) =
  let psWithBadTypes = filter (isLeft . snd) (map (\(pN, (_, p)) -> (pN, buildScopeFor pN >>= (`typeCheck` p))) (toList defs)) in
  if not (null psWithBadTypes) then
    Left (intercalate "\n" (map (\(pN, left) -> fromLeft left ++ " (in " ++ pN ++ ")") psWithBadTypes))
  else
    Right res
  where
  buildScopeFor :: Name -> Either String Scope
  buildScopeFor name =
    case M.lookup name defs of
      Nothing -> Left ("couldn't solve " ++ name ++ " within environment")
      Just (sign, _) -> 
        return (sign ++ map (\(pN, (sign', _)) -> (pN, Tup (map snd sign'))) (toList defs))

parseCCSEnvironment :: String -> Either String Context
parseCCSEnvironment sourceCode =
  runAlex sourceCode happyParseCCSEnvironment
  >>= checkNames
  >>= checkTypes
  >>= checkChans
  >>= \(bounds, defs) -> return (bounds,
                                 nub (concatMap (\(_, (_, p)) -> vEnums p) (toList defs)),
                                 defs)
