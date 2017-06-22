{-# LANGUAGE FlexibleContexts #-}

module Lib
    ( exampleRun
    ) where

import Model
import Models
import Data.Fixed

toYM :: Double -> [Double]
toYM x = [fromIntegral n, r] where (n, r) = divMod' x 12

devSet1 = [([2, 3], 27)
         ,([1, 0], 12)
         ,([1, 1], 13)
         ,([5, 11], 71)
         ]

trainingSet1 = map (\x -> (toYM x, x)) [1..200]
  -- [([21, 7], 259)
  -- ,([1, 0], 12)
  -- ,([3, 8], 44)
  -- ,([11, 11], 143)
  -- ]

devSet2 = [([2, 3], False)
         ,([1, 0], True)
         ,([1, 1], False)
         ,([5, 11], False)
         ]

trainingSet2 = map (\x -> (toYM x, toYM x!!1 == 0)) [1..100]

try :: Model a i o => a i o -> (i, o) -> IO ()
try m (i, o) = putStrLn out where
  out = "Predicted: "++show (predict m i)++" for "++show i++" -> "++show o

test :: (Model a i o, Show x) => (a i o -> [(i, o)] -> x) -> a i o -> [(i, o)] -> [(i, o)] -> IO ()
test acc model' training dev = do
  let model = trainSet model' training
  logModel model
  let tA = acc model training
  let dA = acc model dev
  putStrLn $ "Training Accuracy: "++show tA
  putStrLn $ "DevSet Accuracy: "++show dA
  mapM_ (try model) dev


exampleRun :: IO ()
exampleRun = do
  putStrLn "Running Jungell"
  --test getRegressionAccuracy
    --(blank :: LinReg [Double] Double) trainingSet1 devSet1
  test getRegressionAccuracy
    (blank :: Perceptron [Double] Double) trainingSet1 devSet1
  --test getClassificationAccuracy
    --(blank :: LinReg [Double] Bool) trainingSet2 devSet2
  test getClassificationAccuracy
    (blank :: Perceptron [Double] Bool) trainingSet2 devSet2
