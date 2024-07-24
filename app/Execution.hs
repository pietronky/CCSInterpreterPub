module Execution (Step, nextStep, prettyStep) where

import Base.Errors (panic)
import Base.List (find, findIndex)
import qualified Base.Map as M (lookup)
import Base.System (newStdGen, randomR)
import Context (Definitions)
import Process (Act (..), Proc (..))

type Step = (Maybe Act, Proc)

prettyStep :: Step -> String
prettyStep (maybeA, p) =
  let a = maybe "" show maybeA in
  (if a == "" then "" else " " ++ a ++ " \n") ++ replicate (2 + length a) '-' ++ "> " ++ show p

nextStep :: Definitions -> Proc -> IO (Either String Step)
nextStep env p =
  case nextSteps env p of
    [] -> return (Left "deadlock state reached")
    steps -> do
      newGen <- newStdGen
      return (Right (steps !! fst (randomR (0, length steps - 1) newGen)))

nextSteps :: Definitions -> Proc -> [Step]
nextSteps _ (ActPrefx a p') =
  case a of
    In (_, Nothing) -> [(Just a, p')]
    Out (_, Nothing) -> [(Just a, p')]
    Tau -> [(Just a, p')]
    _ -> panic "unexpected CCS-VP sub-process (in Execution.nextSteps)"

nextSteps env (Choice ps) = concatMap (nextSteps env) ps

nextSteps _ (Cond _ _) = panic "unexpected CCS-VP sub-process (in Execution.nextSteps)"

nextSteps env (Const k maybeEs) =
  case maybeEs of
    Just _ -> panic "unexpected CCS-VP sub-process (in Execution.nextSteps)"
    Nothing ->
      case M.lookup k env of
        Nothing -> panic ("unexpected unsolvable constant \"" ++ k ++ "\" (in Execution.nextSteps)")
        Just (_, p) -> [(Nothing, p)]

nextSteps _ Inaction = []

nextSteps env (Par p1 p2) =
  let (ss1, ss2) = (nextSteps env p1, nextSteps env p2) in
  let (ss1', ss2') = (map (\(maybeA1, p1') -> 
                            case maybeA1 of
                              Nothing -> (Nothing, Par p1' p2)
                              Just a1 -> (Just a1, Par p1' p2)) ss1, map (\(maybeA2, p2') -> case maybeA2 of
                                                                                               Nothing -> (Nothing, Par p1 p2')
                                                                                               Just a2 -> (Just a2, Par p1 p2')) ss2) in
  let (areSwapped, longest, shortest) = if length ss1 > length ss2 then 
                                          (False, ss1, ss2) 
                                        else 
                                          (True, ss2, ss1) in
  let ss3' = foldl (\acc step1@(_, p1') -> 
                     case findIndex (\step2 -> canSync (step1, step2)) shortest of
                       Nothing -> acc
                       Just i -> (Just Tau, let (fromLongest, fromShortest) = (p1', snd (shortest !! i)) in
                                            let (first, second) = if areSwapped then 
                                                                    (fromShortest, fromLongest) 
                                                                  else 
                                                                    (fromLongest, fromShortest) in
                                            Par first second) : acc)  [] longest in
  ss1' ++ ss2' ++ ss3'
  where
  canSync :: (Step, Step) -> Bool
  canSync ((Just (In  (c1, _)), _), (Just (Out (c2, _)), _)) = c1 == c2
  canSync ((Just (Out (c1, _)), _), (Just (In  (c2, _)), _)) = c1 == c2
  canSync _ = False

nextSteps env (Relabeling p1 changes) =
  let nuName c = maybe c fst (find (\(_, ol) -> ol == c) changes) in
  map (\(maybeA, p1') -> 
        case maybeA of
          Nothing           -> (Nothing,                        Relabeling p1' changes)
          Just (In  (c, _)) -> (Just (In  (nuName c, Nothing)), Relabeling p1' changes)
          Just (Out (c, _)) -> (Just (Out (nuName c, Nothing)), Relabeling p1' changes)
          Just Tau          -> (Just Tau,                       Relabeling p1' changes)) (nextSteps env p1 )

nextSteps env (Restriction p1 cs) =
  map (\(maybeA, p1') -> (maybeA, Restriction p1' cs)) (filter (\(maybeA, _) -> 
                                                                 case maybeA of
                                                                   Nothing           -> True
                                                                   Just (In  (c, _)) -> c `notElem` map fst cs
                                                                   Just (Out (c, _)) -> c `notElem` map fst cs
                                                                   Just Tau          -> True) (nextSteps env p1))