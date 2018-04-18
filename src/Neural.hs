module Neural (
  Net () ,
  net,
  dNet,
  rNet,
  run,
  (<<+),
  TrainingData (..),
  Input,
  Output,
  train
  ) where

import System.Random
import Control.Monad.Trans.State
import Data.List

data Net = Net {biases :: [[Double]], weights :: [[[Double]]], generator :: StdGen} deriving (Show, Read)

type Input = [Double]
type Output = [Double]

dNet :: [Integer] -> Net
dNet d =  net d $ mkStdGen 0

rNet :: [Integer] -> IO Net
rNet d = getStdGen >>= pure . net d

net :: [Integer] -> StdGen -> Net
net d | length d < 2 || any (<1) d = error "Invalid Dimensions"
      | otherwise = evalState $ do
          b <- sequence [sequence [rand | v <- [1..n]] | n <- tail d]
          w <- sequence [sequence [sequence [rand | j <- [1..y]] | v <- [1..x]] | (y, x) <- (zip <*> tail) d]
          r <- get
          return $ Net {biases = b, weights = w, generator = r}
  where rand = state $ randomR (-1.0, 1.0)

run :: Net -> Input -> Output
run Net {biases = b, weights = w} input | length (head . head  $ w) /= length input = error "Invalid Input Size"
                                        | any ((||) <$> (>1.0) <*> (<0.0)) input = error "Invalid Input Values"
                                        | otherwise = foldl' ($$) input (zip w b)
  where i $$ (w, b) = zipWith ((sig .) . (+)) (i .* w) b

(.*) :: [Double] -> [[Double]] -> [Double]
i .* w = sum . zipWith (*) i <$> w

data TrainingData = TrainingData {
  rate :: Double,
  batch :: Int,
  epochs :: Int,
  stochastic :: Bool,
  input :: [Input],
  output :: [Output]
} deriving (Show, Eq, Read)

train = TrainingData 0.1 1 (2 ^ 16) True [] []

(<<+) :: Net -> TrainingData -> Net
n <<+ TrainingData {
  rate = r,
  batch = b,
  epochs = e,
  stochastic = s,
  input = i,
  output = o
} | s = trainStoch r b e i o n
  | otherwise = trainDet r e i o n

trainStoch :: Double -> Int -> Int -> [Input] -> [Output] -> Net -> Net
trainStoch r s e i o n = flip evalState (generator n) $ do
                           v <- sequence $ take e $ repeat $ pick s $ zip i o
                           return $ foldl' f n v
  where f n l = let (a, b) = unzip l
                in gradient r a b n
        pick i d = sequence $ replicate i rand
          where rand = do i <- state $ randomR (0, length d - 1)
                          return $ d !! i

trainDet :: Double -> Int -> [Input] -> [Output] -> Net -> Net
trainDet r e i o n = iterate (gradient r i o) n !! e

gradient :: Double -> [Input] -> [Output] -> Net -> Net
gradient e i o n | e <= 0 = error "Invalid Training Rate"
                 | length i /= length o = error "Invalid Training Data"
                 | Net {biases = ob, weights = ow} <- n = n {biases = zipmat (-) ob b, weights = zipmat3 (-) ow w}
  where (nws, nbs) = unzip [prop x y n | (x, y) <- zip i o]
        bb = foldl1' (zipmat (+)) nbs
        ww = foldl1' (zipmat3 (+)) nws
        dn = e / fromIntegral (length i)
        b = mapmat (*dn) bb
        w = mapmat3 (*dn) ww

prop :: Input -> Output -> Net -> ([[[Double]]], [[Double]])
prop i o Net {biases = b, weights = w} | length o /= length (last b) = error "Invalid Output Size"
                                       | otherwise = (map transpose nwsr, nbsr)
  where (as, zss) = propf i b w 
        dc = zipWith (*) (zipWith (-) (last as) o) (map sigd (last zss))
        nw = [map (*v1) dc | v1 <- last $ init as]
        (nbsr, nwsr) = propb dc nw w (init zss) (init . init $ as)

propb :: [Double] -> [[Double]] -> [[[Double]]] -> [[Double]] -> [[Double]] -> ([[Double]], [[[Double]]])
propb cc ww a b c = foldr acc ([cc], [ww]) $ zipr3 a b c
  where acc (wx, zx, ax) (nbs, nws) = let dd = zipWith (*) (sigd <$> zx) (head nbs .* transpose wx)
                                          nnw = [map (*v1) dd | v1 <- ax]
                                      in (dd:nbs, nnw:nws)

propf :: [Double] -> [[Double]] -> [[[Double]]] -> ([[Double]], [[Double]])
propf i b w = foldl f ([i], []) (zip b w)
  where f (a, zs) (b', w') = let z = zipWith (+) (last a .* w') b'
                             in ((a ++ [map sig z]), zs ++ [z])
        
sig :: Double -> Double
sig x = 1 / (1 + exp (-x))

sigd :: Double -> Double
sigd = do x <- sig
          pure $ x * (1 - x)
          
mapmat :: (a -> b) -> [[a]] -> [[b]]
mapmat _ [] = []
mapmat f (x:xs) = map f x : mapmat f xs

mapmat3 :: (a -> b) -> [[[a]]] -> [[[b]]]
mapmat3 _ [] = []
mapmat3 f (x:xs) = mapmat f x : mapmat3 f xs

zipmat :: (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]
zipmat _ [] [] = []
zipmat f (x:xs) (y:ys) = zipWith f x y : zipmat f xs ys

zipmat3 :: (a -> b -> c) -> [[[a]]] -> [[[b]]] -> [[[c]]]
zipmat3 _ [] [] = []
zipmat3 f (x:xs) (y:ys) = zipmat f x y : zipmat3 f xs ys

zipr3 a b c = r $ zip3 (r a) (r b) (r c)
  where r = reverse
