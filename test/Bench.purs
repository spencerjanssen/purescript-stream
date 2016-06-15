module Test.Bench where

import Prelude
import Data.Array as Array
import Data.Foldable as Foldable
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
import Data.Int (even)
import Data.Stream.Array (stream)
import Node.FS (FS)
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen (vectorOf)

foreign import foldlfilt :: forall a b. (a -> Boolean) -> (b -> a -> b) -> b -> Array a -> b

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

benchFoldlSum :: Benchmark
benchFoldlSum = mkBenchmark
    { slug: "sum"
    , title: "sum"
    , sizes: (1..5) <#> (_ * 100)
    , sizeInterpretation: "Number of elements in the array"
    , inputsPerSize: 1
    , gen: \n -> vectorOf n arbitrary
    , functions:
        [ benchFn "array" $ Foldable.foldl (+) 0
        , benchFn "stream" \(arr :: Array Int) -> Stream.foldl (+) 0 $ stream arr
        ]
    }

benchSumFilter :: Benchmark
benchSumFilter = mkBenchmark
    { slug: "sum_filter"
    , title: "Sum <<< Filter"
    , sizes: (1..5) <#> (_ * 1000)
    , sizeInterpretation: "Number of elements in the array"
    , inputsPerSize: 1
    , gen: \n -> vectorOf n arbitrary
    , functions:
        [ benchFn "array" $ Foldable.foldl (+) 0 <<< Array.filter even
        , benchFn "stream" \(arr :: Array Int) -> Stream.foldl (+) 0 $ Stream.filter even $ stream arr
        , benchFn "hand rolled" $ foldlfilt even (+) 0
        ]
    }

main :: Eff (err :: EXCEPTION, fs :: FS, now :: Now, locale :: Locale, console :: CONSOLE, random :: RANDOM, benchmark :: BENCHMARK) Unit
main = runSuite
    [ benchLength
    , benchFoldlSum
    , benchSumFilter
    ]
