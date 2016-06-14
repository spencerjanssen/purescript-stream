module Test.Bench where

import Prelude
import Data.Array as Array
import Data.Stream.Operations as Stream
import Benchotron.BenchmarkJS (BENCHMARK)
import Benchotron.Core (Benchmark, benchFn, mkBenchmark)
import Benchotron.UI.Console (runSuite)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Data.Array ((..))
import Data.Date (Now)
import Data.Date.Locale (Locale)
import Data.Stream.Array (stream)
import Node.FS (FS)
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen (vectorOf)

benchLength :: Benchmark
benchLength = mkBenchmark
    { slug: "length"
    , title: "Length"
    , sizes: (1..5) <#> (_ * 100)
    , sizeInterpretation: "Number of elements in the array"
    , inputsPerSize: 1
    , gen: \n -> vectorOf n arbitrary
    , functions:
        [ benchFn "array" Array.length
        , benchFn "stream" \(arr :: Array Int) -> Stream.length $ stream arr
        ]
    }

main :: Eff (err :: EXCEPTION, fs :: FS, now :: Now, locale :: Locale, console :: CONSOLE, random :: RANDOM, benchmark :: BENCHMARK) Unit
main = runSuite [benchLength]
