module Base.Monads
  ( concatMapM,
    first,
    fromJust,
    fromLeft,
    fromRight,
    isJust,
    isLeft,
    isNothing,
    isRight,
  )
where

import Base.Errors (panic)
import Data.Bifunctor (first)
import Data.Either (isLeft, isRight)
import Data.Maybe (fromJust, isJust, isNothing)
import GHC.Utils.Monad (concatMapM)

fromLeft :: Either a b -> a
fromLeft (Left a) = a
fromLeft _ = panic "`Right` data constructor in `fromLeft`"

fromRight :: Either a b -> b
fromRight (Right b) = b
fromRight _ = panic "`Left` data constructor in `fromRight`"
