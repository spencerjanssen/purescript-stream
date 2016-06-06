module Data.Stream where

import Prelude

data Step s a
    = Done
    | Yield a s
    | Skip s

instance functorStep :: Functor (Step s) where
    map f (Yield x s) = Yield (f x) s
    map _ (Skip s) = Skip s
    map _ Done = Done

-- church style because I couldn't figure out how to make Exists work
newtype Stream a = Stream (forall x. (forall s. (s -> Step s a) -> s -> x) -> x)

instance functorStream :: Functor Stream where
    map f (Stream comb) = comb \next s0 ->
        let next' s = f <$> next s
        in Stream \f -> f next' s0
