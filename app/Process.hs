module Process (
  Act (..),
  Chan,
  Name,
  Proc (..),
  consts,
) where

import Base.Errors (panic)
import Base.List (find, findIndex, intercalate, nub)
import Base.Monads (fromLeft, fromRight, isLeft)
import Expression (BExpr, Expr (..), Name, Parametric (..), Var, Ty (..), getType)

type Chan = String

data Act
  = In (Chan, Maybe (Var, Ty))
  | Out (Chan, Maybe Expr)
  | Tau

data Proc
  = ActPrefx Act Proc
  | Choice [Proc]
  | Cond BExpr Proc
  | Const Name (Maybe [Expr])
  | Inaction
  | Par Proc Proc
  | Relabeling Proc [(Chan, Chan)]
  | Restriction Proc [(Chan, Bool)]

instance Show Act where
  show (In (c, maybeXT)) = c ++ case maybeXT of
                                  Nothing -> ""
                                  Just (x, t) -> "(" ++ x ++ " : " ++ show t ++ ")"
  show (Out (c, maybeE)) = "\\overline{" ++ c ++ "}" ++ case maybeE of
                                                          Nothing -> ""
                                                          Just e -> "(" ++ show e ++ ")"
  show Tau = "\\tau"

instance Parametric Proc where
  replace x n p@(ActPrefx a p') =
    case a of
      Out (c, Just e) -> ActPrefx (Out (c, Just (replace x n e))) (replace x n p')
      In (_, Just (y, _)) | x == y -> p
      _ -> ActPrefx a (replace x n p')

  replace x n (Choice ps) = Choice (map (replace x n) ps)

  replace x n (Cond e p) = Cond (replace x n e) (replace x n p)

  replace x n (Const k maybeEs) = Const k (fmap (map (replace x n)) maybeEs)

  replace _ _ Inaction = Inaction

  replace x n (Par p1 p2) = Par (replace x n p1) (replace x n p2)

  replace x n (Relabeling p' changes) = Relabeling (replace x n p') changes

  replace x n (Restriction p' cs) = Restriction (replace x n p') cs

  typeCheck scope p@(ActPrefx a p') =
    case a of
      In (_, Just (x, t)) -> typeCheck ((x, t) : scope) p' >> return p
      Out (_, Just e) -> typeCheck scope e >> typeCheck scope p' >> return p
      _ -> typeCheck scope p' >> return p
  typeCheck scope p@(Choice ps) = mapM_ (typeCheck scope) ps >> return p
  typeCheck scope p@(Cond e p') = typeCheck scope e >> typeCheck scope p' >> return p
  typeCheck scope p@(Const k maybeEs) =
    case find (\(k', _) -> k == k') scope of
      Nothing -> Left ("couldn't find " ++ k ++ " within scope")
      Just (_, Tup ts) ->
        case fmap (map (getType scope)) maybeEs of
          Nothing -> if null ts then return p else Left ("type mismatch [] / " ++ show ts ++ " for " ++ k)
          Just eithers ->
            let maybeI = findIndex isLeft eithers in
            case maybeI of
              Nothing ->
                let ts' = map fromRight eithers in
                if ts == ts' then
                  return p
                else
                  Left ("type mismatch " ++ show ts' ++ " / " ++ show ts ++ " for " ++ k)
              Just i ->
                Left (fromLeft (eithers !! i))
      Just _ -> panic "asserion `isTuple ...` failed (in `Process.typeCheck`)"
  typeCheck _ p@Inaction = return p
  typeCheck scope p@(Par p1 p2) = typeCheck scope p1 >> typeCheck scope p2 >> return p
  typeCheck scope p@(Relabeling p1 _) = typeCheck scope p1 >> return p
  typeCheck scope p@(Restriction p1 _) = typeCheck scope p1 >> return p

  vEnums (ActPrefx a p') =
    case a of
      Out (_, Just e) -> nub (vEnums e ++ vEnums p')
      _ -> vEnums p'

  vEnums (Choice ps) = nub (concatMap vEnums ps)

  vEnums (Cond e p) = nub (vEnums e ++ vEnums p)

  vEnums (Const _ maybeEs) = case maybeEs of
                               Nothing -> []
                               Just es -> nub (concatMap vEnums es)

  vEnums Inaction = []

  vEnums (Par p1 p2) = nub (vEnums p1 ++ vEnums p2)

  vEnums (Relabeling p _) = vEnums p

  vEnums (Restriction p _) = vEnums p

instance Show Proc where
  show (ActPrefx a p') = show a ++ "." ++ wrap p'
  show (Choice ps) = case ps of
                       [] -> show Inaction
                       [p] -> show p
                       _ -> intercalate " + " (map wrap ps)
  show (Cond e p') = unwords ["if", show e, "then", wrap p']
  show (Const k maybeEs) = k ++ case maybeEs of
                                  Nothing -> ""
                                  Just es -> "(" ++ intercalate ", " (map show es) ++ ")"
  show Inaction = "0"
  show (Par p1 p2) = unwords [wrap p1, "|", wrap p2]
  show (Relabeling p' changes) = wrap p' ++ "[" ++ intercalate ", " (map (\(nu, ol) -> nu ++ "/" ++ ol) changes) ++ "]"
  show (Restriction p' cs) =
    let prettyChan :: (Chan, Bool) -> String
        prettyChan (c, isInput) = if isInput then c else "\\overline{" ++ c ++ "}" in
    unwords [wrap p', "\\", "{" ++ intercalate ", " (map prettyChan cs) ++ "}"]

consts :: Proc -> [String]
consts (ActPrefx _ p') = consts p'
consts (Choice ps) = nub (concatMap consts ps)
consts (Cond _ p') = consts p'
consts (Const k _) = [k]
consts Inaction = []
consts (Par p1 p2) = nub (nub (consts p1) ++ nub (consts p2))
consts (Relabeling p' _) = consts p'
consts (Restriction p' _) = consts p'

wrap :: Proc -> String
wrap p@(ActPrefx _ _) = show p
wrap p@(Choice ps) =
  case ps of
    []  -> show Inaction
    [p'] -> show p'
    _ -> "(" ++ show p ++ ")"
wrap p@(Par _ _) = "(" ++ show p ++ ")"
wrap p = show p
