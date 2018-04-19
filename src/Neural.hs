module Neural (
  Net (),
  Input,
  Output,
  save,
  load,
  dNet,
  rNet,
  net,
  run,
  TrainingData (
      input,
      output,
      iterations,
      batch,
      rate,
      stochastic
      ),
  (<<+),
  train
  ) where

import System.Random
import Control.Monad.Trans.State
import Data.List
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C


data Net = Net {
  biases :: [[Double]],
  weights :: [[[Double]]],
  generator :: StdGen
  } deriving (Show, Read)

type Input = [Double]
type Output = [Double]

save :: FilePath -> Net -> IO ()
save f n = B.writeFile f . C.pack $ show n 

load :: FilePath -> IO Net
load f = do n <- B.readFile f
            return . read $ C.unpack n

dNet :: [Integer] -> Net
dNet d =  net d $ mkStdGen 0

rNet :: [Integer] -> IO Net
rNet d = getStdGen >>= pure . net d

net :: [Integer] -> StdGen -> Net
net d | length d < 2 || any (<1) d = error "Invalid Dimensions"
      | otherwise = evalState $ do
          b <- s [s [rand | _ <- [1..i]] | i <- tail d]
          w <- s [s [s [rand | _ <- [1..j]] | _ <- [1..i]] | (j, i) <- (zip <*> tail) d]
          r <- get
          return $ Net {biases = b, weights = w, generator = r}
  where rand = state $ randomR (-1.0, 1.0)
        s = sequence

run :: Net -> Input -> Output
run Net {
  biases = b,
  weights = w
  } input | length (head . head  $ w) /= length input = error "Invalid Input Size"
          | any ((||) <$> (>1.0) <*> (<0.0)) input = error "Invalid Input Values"
          | otherwise = foldl' f input (zip w b)
  where f i (w, b) = zipWith ((sig .) . (+)) (i .* w) b

(.*) :: [Double] -> [[Double]] -> [Double]
i .* w = sum . zipWith (*) i <$> w

data TrainingData = TrainingData {
  rate :: Double,
  batch :: Int,
  iterations :: Int,
  stochastic :: Bool,
  input :: [Input],
  output :: [Output]
  } deriving (Show, Eq, Read)

train :: TrainingData
train = TrainingData 0.125 8 (2 ^ 16) True [] []

(<<+) :: Net -> TrainingData -> Net
n <<+ TrainingData {
  rate = r,
  batch = b,
  iterations = e,
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
                 | Net {biases = b', weights = w'} <- n = n {
                     biases = zipmat (-) b' b,
                     weights = zipmat3 (-) w' w
                     }
  where (nws, nbs) = unzip [prop x y n | (x, y) <- zip i o]
        dn = e / fromIntegral (length i)
        b = mapmat (*dn) $ foldl1' (zipmat (+)) nbs
        w = mapmat3 (*dn) $ foldl1' (zipmat3 (+)) nws

prop :: Input -> Output -> Net -> ([[[Double]]], [[Double]])
prop i o Net {biases = b, weights = w} | length o /= length (last b) = error "Invalid Output Size"
                                       | otherwise = (nws, nbs)
  where as = propf i b w 
        dc = zipWith (*) (zipWith (-) (last as) o) (map sigd (last as))
        nw = [map (*a) $ last $ init as | a <- dc]
        (nbs, nws) = propb dc nw w (init as) (init . init $ as)

propb :: [Double] -> [[Double]] -> [[[Double]]] -> [[Double]] -> [[Double]] -> ([[Double]], [[[Double]]])
propb nb nw ws zs as = foldr f ([nb], [nw]) $ zipr3 ws zs as
  where f (w, z, a) (nbs, nws) = let nb = zipWith (*) (sigd <$> z) (head nbs .* transpose w)
                                     nw = [map (*i) a | i <- nb]
                                 in (nb:nbs, nw:nws)

propf :: [Double] -> [[Double]] -> [[[Double]]] -> [[Double]]
propf i bs ws = reverse $ foldl f [i] (zip bs ws)
  where f a (b, w) = let z = zipWith (+) (head a .* w) b
                     in (map sig z : a)
        
sig :: Double -> Double
sig x = 1 / (1 + exp (-x))

sigd :: Double -> Double
sigd x = x * (1 - x)
          
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

zipr3 :: [a] -> [b] -> [c] -> [(a, b, c)]
zipr3 a b c = r $ zip3 (r a) (r b) (r c)
  where r = reverse
