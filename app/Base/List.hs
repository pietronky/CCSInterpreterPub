module Base.List ((\\), count, elemIndex, exists, find, findIndex, intercalate, nub) where

import Base.Monads (isJust)
import Data.List (elemIndex, find, findIndex, intercalate, nub, (\\))
import GHC.Utils.Misc (count)

-- | Returns `True` if and only if the input list has an element satisfying the input predicate
exists :: (a -> Bool) -> [a] -> Bool
exists predicate list = isJust (find predicate list)
