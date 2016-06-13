module Test.Main where

import Prelude
import Data.Foldable as Foldable
import Data.Stream.Operations as StreamOp
import Data.Stream.StrMap as SM
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Random (RANDOM)
import Data.Array (filter)
import Data.Array as Array
import Data.StrMap (fromFoldable, StrMap)
import Data.Stream (Stream)
import Data.Stream.Array (stream, unstream)
import Data.Stream.Operations (zip, foldl)
import Data.Tuple (Tuple)
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

arrayToStrMap :: Array (Tuple String Int)-> StrMap Int
arrayToStrMap = fromFoldable

unstreamStreamStrMap :: Array (Tuple String Int)-> Result
unstreamStreamStrMap arr =
    let sm = arrayToStrMap arr
    in fnEqual id (SM.unstream <<< SM.stream) sm

foldlArray :: (Int -> String -> Int) -> Int -> Array String -> Result
foldlArray f y = fnEqual (foldl f y <<< stream) (Foldable.foldl f y)

zipProp :: Array Int -> Array Int -> Result
zipProp xs ys = unstream (zip (stream xs) (stream ys)) === Array.zip xs ys

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
    test "StrMap unstream <<< stream" do
        quickCheck unstreamStreamStrMap
    test "Array foldl" do
        quickCheck foldlArray
    test "Array zip" do
        quickCheck zipProp
