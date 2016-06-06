module Data.Stream.Array where

import Data.Stream
import Data.Array as Array
import Data.Array.Unsafe (unsafeIndex)
import Prelude ((+), (>=), (<>))

-- todo make this efficient
unstream :: forall a. Stream a -> Array a
unstream (Stream comb) = comb \next s0 ->
    let go s arr = case next s of
                    Done -> arr
                    Yield x s' -> go s' (arr <> [x])
                    Skip s' -> go s' arr
    in go s0 []

stream :: forall a. Array a -> Stream a
stream arr = Stream \comb -> comb next 0
 where
    alen = Array.length arr
    next i = if i >= alen
                then Done
                else Yield (unsafeIndex arr i) (i+1)
