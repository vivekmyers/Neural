module Lib
    ( Net () 
    ) where

newtype Net = Net {layers :: [Layer]}
data Layer = Layer {weights :: [[Double]], biases :: [Double]}
type Input = [Double]
type Result = [Double]

run :: Net -> Input -> Result
run Net {layers = n} input = foldr ($$) input n
  where (Layer w b) $$ i = zipWith ((sig .) . (+)) (sum . zipWith (*) i <$> w) b

cost :: Net -> Input -> Result -> Double
cost n input result = sum $ (**2) <$> zipWith (-) (run n input) result

sig :: Double -> Double
sig x = 1 / (1 + exp (-x))

