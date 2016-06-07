module Data.Stream.Operations where

import Data.Stream
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Data.Either (Either(Right, Left))
import Data.Tuple (Tuple(Tuple))
import Prelude (Unit, unit, pure, otherwise, (+), (==), (-))

repeat :: forall a. a -> Stream a
repeat val = Stream \f -> f next init
 where
    next _ = Yield val unit
    init = unit

take :: forall a. Int -> Stream a -> Stream a
take n (Stream comb) = comb \next s0 ->
    let next' {i, s} =
            if i == 0
                then Done
                else case next s of
                        Yield x s' -> Yield x {i: i-1, s: s'}
                        Skip s' -> Skip {i, s: s'}
                        Done -> Done
    in Stream \f -> f next' {i: n, s: s0}

length :: forall a. Stream a -> Int
length (Stream comb) = comb \next s0 ->
    let go s i = case next s of
                    Yield _ s' -> go s' (i+1)
                    Skip s' -> go s' i
                    Done -> i
    in go s0 0

filter :: forall a. (a -> Boolean) -> Stream a -> Stream a
filter p (Stream comb) = comb \next s0 ->
    let next' s = case next s of
                    Yield x s' | p x -> Yield x s'
                               | otherwise -> Skip s'
                    Skip s' -> Skip s'
                    Done -> Done
    in Stream \f -> f next' s0

append :: forall a. Stream a -> Stream a -> Stream a
append (Stream comb1) (Stream comb2) = comb1 \next1 s1 -> comb2 \next2 s2 ->
    let next' (Left (Tuple s t)) = case next1 s of
            Yield x s' -> Yield x (Left (Tuple s' t))
            Skip s' -> Skip (Left (Tuple s' t))
            Done -> Skip (Right t)
        next' (Right s) = case next2 s of
            Yield x s' -> Yield x (Right s')
            Skip s' -> Skip (Right s')
            Done -> Done
    in Stream \f -> f next' (Left (Tuple s1 s2))

foldl :: forall a b. (b -> a -> b) -> b -> Stream a -> b
foldl f y0 (Stream comb) = comb \next s0 ->
    let go y s = case next s of
                    Yield x s' -> go (f y x) s'
                    Skip s' -> go y s'
                    Done -> y
    in go y0 s0

eachEff :: forall a eff. (a -> Eff eff Unit) -> Stream a -> Eff eff Unit
eachEff f (Stream comb) = comb \next s0 ->
    let go s = case next s of
                Yield x s' ->
                    case unsafePerformEff (f x) of _ -> go s'
                Skip s' -> go s'
                Done -> pure unit
    in go s0
