{-# LANGUAGE GADTs, MultiParamTypeClasses, FlexibleInstances, InstanceSigs, ScopedTypeVariables #-}
module Model where
import Debug.Trace
import Utils
import Data.List

data Accuracy a i o where
  -- correct / total
  Classification :: Model a i o => Int -> Int -> Accuracy a i o
  -- mse, mae -- Add median
  Regression :: Model a i o => o -> Int -> Accuracy a i o

instance (Show o) => Show (Accuracy a i o) where
  show (Classification correct total) = "Classified "++show correct++" of "++show total++" instances."
  show (Regression mse total) = "MSE: "++show mse++" for "++show total++" instances."

class (Show (a i o), Show i, Show o, Eq o) => Model a i o where
  predict :: a i o -> i -> o
  train :: a i o -> (i, o) -> a i o
  train m f = trainSet m [f]
  trainSet :: a i o -> [(i, o)] -> a i o
  blank :: a i o
  logModel :: a i o -> IO ()
  logModel = print
  -- save :: a i o -> FileHandle or something
  -- load :: FileHandle or something -> a i o

getClassificationAccuracy :: (Model a i o) => a i o -> [(i, o)] -> Accuracy a i o
getClassificationAccuracy model tData = Classification correct total where
  total :: Int
  total = length tData
  correct :: Int
  correct = length $ filter (\(i, o) -> predict model i == o) tData

getRegressionAccuracy :: (RealFrac o, Model a i o) => a i o -> [(i, o)] -> Accuracy a i o
getRegressionAccuracy model tData = Regression mseAv total where
  total :: Int
  total = length tData
  mseAv = mse / fromIntegral total
  mse = sum $ map (\(i, o) -> let x = predict model i - o in x*x) tData
