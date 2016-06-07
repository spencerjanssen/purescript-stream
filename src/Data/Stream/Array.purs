module Data.Stream.Array where

import Data.Stream
import Data.Array as Array
import Control.Monad.Eff (runPure)
import Data.Array.ST (pushSTArray, emptySTArray, runSTArray)
import Data.Array.Unsafe (unsafeIndex)
import Data.Stream.Operations (eachEff)
import Prelude (void, (+), (>=), (<<<), bind, pure)

unstream :: forall a. Stream a -> Array a
unstream s = runPure (runSTArray do
    res <- emptySTArray
    eachEff (void <<< pushSTArray res) s
    pure res
    )

stream :: forall a. Array a -> Stream a
stream arr = Stream \comb -> comb next 0
 where
    alen = Array.length arr
    next i = if i >= alen
                then Done
                else Yield (unsafeIndex arr i) (i+1)
