module Lib (
  Net () 
  ) where

import System.Random
import Control.Monad.Trans.State
import Data.List

data Net = Net {biases :: [[Double]], weights :: [[[Double]]]} deriving (Show, Eq, Ord, Read)
type Input = [Double]
type Output = [Double]

randomNet :: [Integer] -> IO Net
randomNet d = getStdGen >>= pure . net d

net :: RandomGen r => [Integer] -> r -> Net
net d = evalState $ do
          b <- sequence [sequence [rand | v <- [1..n]] | n <- tail d]
          w <- sequence [sequence [sequence [rand | j <- [1..y]] | v <- [1..x]] | (y, x) <- (zip <*> tail) d]
          return $ Net {biases = b, weights = w}
  where rand = state $ randomR (-1.0, 1.0)

run :: Net -> Input -> Output
run Net {biases = b, weights = w} input | length (head . head  $ w) /= length input = error "Invalid Input Size"
                                        | any ((||) <$> (>1.0) <*> (<0.0)) input = error "Invalid Input Values"
                                        | otherwise = foldl' ($$) input (zip w b)
  where i $$ (w, b) = zipWith ((sig .) . (+)) (sum . zipWith (*) i <$> w) b

cost :: Net -> Input -> Output -> Double
cost n input result = sum $ (**2) <$> zipWith (-) (run n input) result

train :: Double -> [Input] -> [Output] -> Net -> Net
train e i o n | e <= 0 = error "Invalid Eta Value"
              | otherwise = n {biases = b, weights = w}
  where results = [prop x y | (x, y) <- zip i o]
        b = undefined
        w = undefined

zipmat :: (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]
zipmat _ [] [] = []
zipmat f (x:xs) (y:ys) = zipWith f x y : zipmat f xs ys

zipmat3 :: (a -> b -> c) -> [[[a]]] -> [[[b]]] -> [[[c]]]
zipmat3 _ [] [] = []
zipmat3 f (x:xs) (y:ys) = zipmat f x y : zipmat3 f xs ys

prop :: Input -> Output -> ([[[Double]]], [[Double]])
prop = undefined

sig :: Double -> Double
sig x = 1 / (1 + exp (-x))

sigd :: Double -> Double
sigd = do x <- sig
          pure $ x * (x - 1)
