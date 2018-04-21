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
      epochs,
      batch,
      rate,
      momentum
      ),
  (<<+),
  train
  ) where

import Data.List
import System.Random
import Control.Monad.Trans.State
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
          | otherwise = let l = zip w b
                        in f act1 (foldl' (f act2) input (init l)) (last l)
  where f g i (w, b) = zipWith ((g .) . (+)) (i .* w) b

(.*) :: [Double] -> [[Double]] -> [Double]
i .* w = sum . zipWith (*) i <$> w

data TrainingData = TrainingData {
  rate :: Double,
  momentum :: Double,
  batch :: Int,
  epochs :: Int,
  input :: [Input],
  output :: [Output]
  } deriving (Show, Eq, Read)

train :: TrainingData
train = TrainingData 0.125 0 8 0 [] []

(<<+) :: Net -> TrainingData -> Net
n <<+ TrainingData {
  rate = r,
  momentum = m,
  batch = b,
  epochs = e,
  input = i,
  output = o
  } | m < 0 || m >= 1 = error "Invalid Momentum"
    | e < 0 = error "Invalid Number of Epochs"
    | b <= 0 = error "Invalid Batch Size"
    | r <= 0 = error "Invalid Learning Rate"
    | e == 0 = stochastic m r b (max 1 $ (2 ^ 16) `div` (length i + 1)) i o n
    | otherwise = stochastic m r b e i o n

stochastic :: Double -> Double -> Int -> Int -> [Input] -> [Output] -> Net -> Net
stochastic m r s e i o n = flip evalState (generator n) $ do
                             v <- sequence $ replicate e $ batches s $ zip i o
                             return . snd . foldl' f (Nothing, n) $ concat v
  where f (v, n) l = let (a, b) = unzip l
                     in gradient v m r a b n

batches :: Int -> [(Input, Output)] -> State StdGen [[(Input, Output)]]
batches s [] = return []
batches s v = do r <- sequence . replicate s $ state (randomR (0.0, 1.0)) :: State StdGen [Double]
                 let h = map snd . sort . zip r $ take s v
                 hs <- batches s (drop s v)
                 return $ h:hs

gradient :: Maybe ([[Double]], [[[Double]]]) -> Double -> Double -> [Input] -> [Output] -> Net -> (Maybe ([[Double]], [[[Double]]]), Net)
gradient v m e i o n | e <= 0 = error "Invalid Training Rate"
                     | length i /= length o = error "Invalid Training Data"
                     | v == Nothing, Net {biases = b', weights = w'} <- n =
                         (,) (Just (mapmat negate b, mapmat3 negate w)) n {
                           biases = zipmat (-) b' b,
                           weights = zipmat3 (-) w' w
                           }
                     | Just (b'', w'') <- v, Net {biases = b', weights = w'} <- n =
                         let vb = zipmat (+) (mapmat (*m) b'') (mapmat negate b)
                             vw =  zipmat3 (+) (mapmat3 (*m) w'') (mapmat3 negate w)
                         in (,) (Just (vb, vw)) n {
                           biases = zipmat (+) vb b',
                           weights = zipmat3 (+) vw w'
                           }
  where (nws, nbs) = unzip [prop x y n | (x, y) <- zip i o]
        dn = e / fromIntegral (length i)
        b = mapmat (*dn) $ foldl1' (zipmat (+)) nbs
        w = mapmat3 (*dn) $ foldl1' (zipmat3 (+)) nws

prop :: Input -> Output -> Net -> ([[[Double]]], [[Double]])
prop i o Net {biases = b, weights = w} | length o /= length (last b) = error "Invalid Output Size"
                                       | otherwise = (nws, nbs)
  where as = propf i b w 
        dc = zipWith (*) (zipWith (-) (last as) o) (map act1d (last as))
        nw = [map (*a) $ last $ init as | a <- dc]
        (nbs, nws) = propb dc nw w (init as) (init . init $ as)

propb :: [Double] -> [[Double]] -> [[[Double]]] -> [[Double]] -> [[Double]] -> ([[Double]], [[[Double]]])
propb nb nw ws zs as = foldr f ([nb], [nw]) $ zipr3 ws zs as
  where f (w, z, a) (nbs, nws) = let nb = zipWith (*) (act2d <$> z) (head nbs .* transpose w)
                                     nw = [map (*i) a | i <- nb]
                                 in (nb:nbs, nw:nws)

propf :: [Double] -> [[Double]] -> [[[Double]]] -> [[Double]]
propf i bs ws = let l = (zip bs ws)
                    r = foldl' (f act2) [i] (init l)
                in reverse $ f act1 r (last l)
  where f g a (b, w) = let z = zipWith (+) (head a .* w) b
                       in (map g z : a)
        
act1 :: Double -> Double
act1 x = 1 / (1 + exp (-x))

act1d :: Double -> Double
act1d x = x * (1 - x)

act2 :: Double -> Double
act2 = tanh

act2d :: Double -> Double
act2d x = 1 - x ** 2
          
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
