module Translation (chans, translDefs) where

import Base.Errors (panic)
import Base.List ((\\), elemIndex, exists, find, intercalate, nub)
import Base.Map (member, restrictKeys)
import qualified Base.Map as M (insert, lookup)
import Base.Monads (concatMapM, first, fromJust, isJust)
import Base.Set (Set, singleton)
import qualified Base.Set as S (insert)
import Context (Context, Definitions, universe)
import Control.Exception (assert)
import Expression (Parametric (..), Val, eval, getTypeFromVal, isCapitalized, test)
import Process (Act(..), Name, Proc(..), consts, Chan)

args :: String -> [String]
args s =
  case elemIndex ',' s of
    Nothing -> [s]
    Just i ->
      let prefix = take i s in
      prefix : args (drop (i+1) s)

isValidConstName :: String -> Bool
isValidConstName s =
  isCapitalized s || (let maybeI = fmap (\n -> n - 1) (elemIndex '{' s) in
                      case maybeI of
                        Nothing -> False
                        Just i | i < 0 || not (isCapitalized (take i s)) || s !! i /= '_' || last s /= '}' -> False
                        Just i -> all (isJust . getTypeFromVal) (args (init (drop (i+2) s))))

-- | Returns a list of pairs composed by a `Chan` 
--   and a flag set to `True` if and only if
--   the channel is value passing
chans :: Definitions -> Proc -> Maybe [(Chan, Bool)]
chans defs = chans' []
  where
  chans' :: [Name] -> Proc -> Maybe [(Chan, Bool)]
  chans' checked p =
    let chans'Checked = chans' checked in
    case p of
      (ActPrefx a p') ->
        case a of
          In  (c, maybeX) -> do
            csp' <- chans'Checked p'
            return (nub ((c, isJust maybeX) : csp'))
          Out (c, maybeE) -> do
            csp' <- chans'Checked p'
            return (nub ((c, isJust maybeE) : csp'))
          Tau -> chans'Checked p'
      (Choice ps) -> do 
        concatMapM chans'Checked ps
      (Cond _ p') -> chans'Checked p'
      (Const k _) ->
        if k `elem` checked then
          return []
        else
          case M.lookup k defs of
            Nothing | not (wasParametric k) -> Nothing
            Nothing ->
              let olK = (fst . originalConst) k in
              case M.lookup olK defs of
                Nothing -> Nothing
                Just (_, p') -> chans' (k : olK : checked) p'
            Just (_, p') -> chans' (k : checked) p'
      Inaction -> return []
      Par p1 p2 -> do
        csp1 <- chans'Checked p1
        csp2 <- chans'Checked p2
        return (nub (csp1 ++ csp2))
      Relabeling p' changes ->
        let nuName c = if not (exists (\(_, ol) -> c == ol) changes) then
                         c
                       else
                         fst (fromJust (find (\(_, ol) -> c == ol) changes)) in
        do 
          csp' <- chans'Checked p'
          return (map (first nuName) csp')
      Restriction p' _ -> chans'Checked p'

originalConst :: Name -> (Name, [Val])
originalConst k =
  if not (wasParametric k) then
    panic ("const. " ++ k ++ " was not parametric")
  else
    (init (takeWhile (/= '{') k), args (init (tail (dropWhile (/= '{') k))))

translDefs :: Context -> Definitions
translDefs ctx@(bounds, _, defs) =
  assert (member "Main" defs && null (fst (fromJust (M.lookup "Main" defs)))) $ -- assert that "Main" exists and that it has empty signature
  let (translated, newDefs') = translDefs' (singleton "Main", defs) "Main" in
  restrictKeys newDefs' translated
  where
    translDefs' :: (Set Name, Definitions) -> Name -> (Set Name, Definitions)
    translDefs' (translated, defs') name =
      assert (member name defs') $
      let tP = transl (snd (fromJust (M.lookup name defs'))) in
      let ks = consts tP in
      if all (`elem` translated) ks then
        (translated, M.insert name ([], tP) defs')
      else
        let constsToTranslate = filter (`notElem` translated) ks in
        let exParametrics = filter wasParametric constsToTranslate in
        assert (all ((`member` defs') . fst . originalConst) exParametrics) $ -- assert that all left parametric constants exist
        let newDefs = foldl (\currDefs k ->
                                let (oldK, vs) = originalConst k in
                                let (sign, def) = fromJust (M.lookup oldK currDefs) in
                                assert (length sign == length vs) $
                                let replacements = zip (map fst sign) vs in
                                M.insert k ([], foldl (\currDef (x, v) ->
                                                        replace x v currDef) def replacements) currDefs) (M.insert name ([], tP) defs') exParametrics in
        foldl (\(ts, nDs) k -> translDefs' (S.insert k ts, nDs) k) (translated, newDefs) constsToTranslate

    transl :: Proc -> Proc
    transl p =
      case p of
        ActPrefx a p' ->
          case a of
            In (c, Just (x, t)) ->
              let validValues = filter (\v' -> fromJust (getTypeFromVal v') == t) (universe ctx) in
              let choices = [transl (replace x v p') | v <- validValues] in
              Choice [ActPrefx (In (c ++ "_{" ++ v ++ "}", Nothing)) choice | (v, choice) <- zip validValues choices]
            Out (c, Just e) ->
              ActPrefx (Out (c ++ "_{" ++ eval bounds e ++ "}", Nothing)) (transl p')
            _ -> ActPrefx a (transl p')
        Choice ps -> Choice (map transl ps)
        Cond e p' ->
          if test bounds e then
            transl p'
          else
            Inaction
        Const k maybeEs ->
          case fmap (map (eval bounds)) maybeEs of
            Nothing -> p
            Just vs -> Const (k ++ "_{" ++ intercalate "," vs ++ "}") Nothing
        Inaction -> p
        Par p1 p2 -> Par (transl p1) (transl p2)
        Relabeling p' changes ->
          let maybeCsp' = chans defs p' in
          case maybeCsp' of
            Nothing -> panic "assertion `isJust (chans defs p)` failed (in `Translation.transl`)"
            Just csp' ->
              let vpChansChanges = filter (\(_, ol) -> ol `elem` map fst (filter snd csp')) changes in
              let translatedVpChansChanges = concatMap (\(nu, ol) -> [(nu ++ "_{" ++ v ++ "}",
                                                                       ol ++ "_{" ++ v ++ "}") | v <- universe ctx]) vpChansChanges in
              Relabeling (transl p') ((changes \\ vpChansChanges) ++ translatedVpChansChanges)
        Restriction p' cs ->
          let maybeCsp' = chans defs p' in
            case maybeCsp' of
              Nothing -> panic "assertion `isJust (chans defs p)` failed (in `Translation.transl`)"
              Just csp' ->
                let restrictedVpInchans   = filter    (\(c, _) -> c `elem` map fst (filter snd csp'))                           (filter snd cs) in
                let restrictedVpOutchans  = filter    (\(c, _) -> c `elem` map fst (filter snd csp'))                           (filter (not . snd) cs) in
                let newRestrictedInChans  = concatMap (\(c, isInput) -> [(c ++ "_{" ++ v ++ "}", isInput) | v <- universe ctx]) restrictedVpInchans in
                let newRestrictedOutChans = concatMap (\(c, isInput) -> [(c ++ "_{" ++ v ++ "}", isInput) | v <- universe ctx]) restrictedVpOutchans in
                Restriction (transl p') (((cs \\ restrictedVpInchans) \\ restrictedVpOutchans) ++
                                                newRestrictedInChans ++ newRestrictedOutChans)

wasParametric :: String -> Bool
wasParametric s = isValidConstName s && isJust (elemIndex '{' s)