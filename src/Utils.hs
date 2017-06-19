module Utils where

import Data.List

average :: (Real a, Fractional b) => [a] -> b
average xs = realToFrac (sum xs) / genericLength xs

averageVector :: (Real a, Fractional b) => [[a]] -> [b]
averageVector xs = map average $ transpose xs

getXs :: [(i, o)] -> [i]
getXs = map fst
getYs :: [(i, o)] -> [o]
getYs = map snd

dotV :: Num a => [a] -> [a] -> a
dotV x y = sum $ scaleVV x y

divV :: Fractional a => [a] -> [a] -> [a]
divV = zipWith (/)

scaleV :: Num a => a -> [a] -> [a]
scaleV s = map (s*)

scaleVV :: Num a => [a] -> [a] -> [a]
scaleVV = zipWithDefault (*)

subV :: Num a => [a] -> [a] -> [a]
subV = zipWithDefault (-)

addV :: Num a => [a] -> [a] -> [a]
addV = zipWithDefault (+)

zipWithDefault :: (Num a, Num b) => (a -> b -> c) -> [a] -> [b] -> [c]
zipWithDefault = zipWithPadding 0 0

zipWithPadding :: (Num a, Num b) => a -> b -> (a -> b -> c) -> [a] -> [b] -> [c]
zipWithPadding a b f (x:xs) (y:ys) = f' : zipWithPadding a b f xs ys where
  f' = f x y
zipWithPadding a _ f []     ys     = map (f a) ys
zipWithPadding _ b f xs     []     = map (`f` b) xs
