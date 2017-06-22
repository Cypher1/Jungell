{-# LANGUAGE GADTs, MultiParamTypeClasses, FlexibleInstances, InstanceSigs, ScopedTypeVariables #-}
module Models where
import Model
import Utils
import Debug.Trace
import Data.List
import Control.Arrow

data LinReg t o where
  LinReg :: [t] -> t -> LinReg [t] o

instance (Show g, Show o) => Show (LinReg [g] o) where
  show (LinReg g o) = show g++" * X + "++show o

instance (Real a, Show a, Fractional a) => Model LinReg [a] a where
  predict :: LinReg [a] a -> [a] -> a
  predict (LinReg bias offset) input = offset + dotV bias input
  trainSet (LinReg _ _) ldata = trace out (LinReg gradient intercept) where
    out = ""-- "Correlation: "++show top++"\n/"++show bot++"\nGradiant: "++show gradient++"\nIntercept: "++show intercept++"\nX bar: "++show avgX++"\nY bar: "++show avgY
    xs' = getXs ldata
    avgX = averageVector xs'
    xOff = transpose $ map (`subV` avgX) xs'
    ys' = getYs ldata
    avgY = average ys'
    yOff = map (\x -> x - avgY) ys'
    top = map (dotV yOff) xOff
    bot = map (\x -> dotV x x) xOff
    gradient = divV top bot
    intercept = avgY - dotV gradient avgX
  blank = LinReg [] 0

instance (Show a, Real a, Fractional a) => Model LinReg [a] Bool where
  predict (LinReg b o) i = 0.5 < predict m' i where
    m' :: LinReg [a] a
    m' = LinReg b o
  trainSet (LinReg b o) d = LinReg b' o' where
    (b', o') = case m'' of LinReg g o -> (g, o)
    m'' = trainSet m' (map (second bTof) d)
    m' :: LinReg [a] a
    m' = LinReg b o
  blank = LinReg [] 0

data Perceptron i o where
  Perceptron :: i {- weights -} -> Perceptron i o

instance (Show a, Show b) => Show (Perceptron [a] b) where
  show (Perceptron []) = "Untrained Perceptron"
  show (Perceptron (t:w)) = "Perceptron: "++show w++" * X + "++show t

maxIters :: Int
maxIters = 1000000

instance (Show a, Real a, Fractional a) => Model Perceptron [a] a where
  predict :: Perceptron [a] a -> [a] -> a
  predict (Perceptron w) input = dotV w (1:input)
  trainSet m ldata = trainSet' maxIters m (cycle ldata) where
    trainSet' :: Int -> Perceptron [a] a -> [([a], a)] -> Perceptron [a] a
    trainSet' 0 m _ = m
    trainSet' _ m [] = m
    trainSet' it m@(Perceptron w) ((i, o):is) = trained where
      -- trace (show i++" -> "++show o++"\nP: "++show p++"\n W: "++show w) trained where
      trained = trainSet' (it-1) (Perceptron w') is
      learningRate :: a
      learningRate = (0.001/fromIntegral maxIters)*fromIntegral it
      e :: a
      e = learningRate * (predict m i - o)
      w' :: [a]
      w' = subV w $ scaleV e (1:i)
  blank = Perceptron []

instance (Show a, Real a, Fractional a) => Model Perceptron [a] Bool where
  predict (Perceptron w) i = 0 < predict m' i where
    m' :: Perceptron [a] a
    m' = Perceptron w
  trainSet (Perceptron w) d = Perceptron w' where
    (Perceptron w') = trainSet m' (map (second bTof) d)
    m' :: Perceptron [a] a
    m' = Perceptron w
  blank = Perceptron []

bTof :: (Real a, Fractional a) => Bool -> a
bTof True = 1.0
bTof False = -1.0
