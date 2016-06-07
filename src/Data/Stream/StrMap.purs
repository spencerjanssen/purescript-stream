module Data.Stream.StrMap (stream, unstream) where

import Data.Stream
import Data.Stream.Array as Arr
import Control.Monad.Eff (runPure)
import Data.StrMap (runST, keys, StrMap)
import Data.StrMap.ST (poke, new)
import Data.StrMap.Unsafe (unsafeIndex)
import Data.Stream.Operations (eachEff)
import Data.Tuple (Tuple(Tuple))
import Prelude (void, pure, ($), map, bind)

stream :: forall a. StrMap a -> Stream (Tuple String a)
stream arr = map (\i -> Tuple i $ unsafeIndex arr i) $ Arr.stream $ keys arr

unstream :: forall a. Stream (Tuple String a) -> StrMap a
unstream s = runPure (runST do
    res <- new
    eachEff (\(Tuple k v) -> void $ poke res k v) s
    pure res)
