module Data.Stream.StrMap where

import Data.Stream
import Data.Stream.Array as Arr
import Data.StrMap (keys, fromFoldable, StrMap)
import Data.StrMap.Unsafe (unsafeIndex)
import Data.Tuple (Tuple(Tuple))
import Prelude ((<<<), ($), map)

stream :: forall a. StrMap a -> Stream (Tuple String a)
stream arr = map (\i -> Tuple i $ unsafeIndex arr i) $ Arr.stream $ keys arr

unstream :: forall a. Stream (Tuple String a) -> StrMap a
unstream = fromFoldable <<< Arr.unstream
