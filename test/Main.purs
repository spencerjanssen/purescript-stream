module Test.Main where

import Prelude
import Data.Stream.Operations as StreamOp
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Random (RANDOM)
import Data.Array (filter)
import Data.Stream (Stream)
import Data.Stream.Array (stream, unstream)
import Test.QuickCheck (Result, (===))
import Test.Unit (test)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)
import Test.Unit.QuickCheck (quickCheck)

fnEqual :: forall a b. (Eq b, Show b) => (a -> b) -> (a -> b) -> a -> Result
fnEqual f g x = f x === g x

arrayModel :: forall b. (Eq b, Show b)
    => (Array Int -> Array b) -> (Stream Int -> Stream b) -> Array Int -> Result
arrayModel f g = fnEqual f (unstream <<< g <<< stream)

unstreamStream :: Array Int -> Result
unstreamStream = fnEqual id (unstream <<< stream)

main :: forall eff. Eff (console :: CONSOLE, testOutput :: TESTOUTPUT, random :: RANDOM | eff) Unit
main = runTest do
    test "unstream <<< stream" do
        quickCheck unstreamStream
    test "filter" do
        quickCheck \f arr -> arrayModel (filter f) (StreamOp.filter f) arr
    test "map" do
        quickCheck \f -> arrayModel (map (f :: Int -> Int)) (map f)
    test "append" do
        quickCheck \(xs :: Array Int) ys -> xs <> ys === unstream (StreamOp.append (stream xs) (stream ys))
    test "length/take/repeat" do
        quickCheck $ let n = 100 in n === (StreamOp.length $ StreamOp.take n $ StreamOp.repeat unit)