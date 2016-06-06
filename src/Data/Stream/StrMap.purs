module Data.Stream.StrMap (stream, unstream) where

import Data.Stream
import Data.Stream.Array as Arr
import Control.Monad.Eff (Eff, runPure)
import Data.StrMap (runST, keys, StrMap)
import Data.StrMap.ST (poke, new)
import Data.StrMap.Unsafe (unsafeIndex)
import Data.Tuple (Tuple(Tuple))
import Prelude (void, Unit, pure, ($), map, bind)

stream :: forall a. StrMap a -> Stream (Tuple String a)
stream arr = map (\i -> Tuple i $ unsafeIndex arr i) $ Arr.stream $ keys arr

unstream :: forall a. Stream (Tuple String a) -> StrMap a
unstream (Stream comb) = comb \next s0 -> runPure (runST (do
    res <- new
    playeff next s0 (\(Tuple k v) -> void $ poke res k v)
    pure res
    ))

foreign import playeff :: forall s a eff. (s -> Step s a) -> s -> (a -> Eff eff Unit) -> Eff eff Unit
