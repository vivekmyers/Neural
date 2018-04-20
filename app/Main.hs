module Main where

import Neural
import System.IO
import Control.Monad
import System.Environment
import Text.Printf
import qualified Data.ByteString.Lazy as B

main :: IO ()
main = do (arg:values) <- (++[[]]) <$> getArgs
          case arg of
            "init"  -> do let layers = map read $ init values
                          n <- rNet $ layers
                          save "DigitNetwork.nnhs" n
                          putStrLn "New Network Initialized"
            "view"  -> do let imgs = read <$> init values
                          input <- B.readFile "t10k-images.idx3-ubyte"
                          n <- load "DigitNetwork.nnhs"
                          forM_ imgs $ \img -> do
                            putStrLn $ visual (getImage input img)
                            putStr "Computed: "
                            print $ disp (run n $ getX input img)
            "train" -> do n <- load "DigitNetwork.nnhs"
                          [input, output] <- mapM B.readFile [
                            "train-images.idx3-ubyte",
                            "train-labels.idx1-ubyte"
                            ]
                          save "DigitNetwork.nnhs" $ n <<+ train {
                            input = pInput input 60000,
                            output = pOutput output 60000,
                            epochs = read $ head values,
                            batch = 10,
                            rate = 0.5
                            }
                          putStrLn "Training Complete"
            "run"   -> do n <- load "DigitNetwork.nnhs"
                          [input, output] <- mapM B.readFile [
                            "t10k-images.idx3-ubyte",
                            "t10k-labels.idx1-ubyte"
                            ]
                          let results = run n <$> pInput input 10000
                              ar = disp <$> results
                              ad = disp <$> pOutput output 10000
                              count = length . filter (uncurry (==)) $ zip ar ad
                          putStrLn . ("Accuracy: "++) . (++"%") $ printf "%.2f" (fromIntegral count / 100 :: Double)
            _ -> putStrLn "Invalid Options"
            

visual [] = []
visual xs = map sh (take 28 xs) ++ "\n" ++ visual (drop 28 xs)
  where sh x | x > 128 = '#'
             | otherwise = ' '

pInput i n = getX i <$> [0..n - 1]
pOutput o n = getY o <$> [0..n - 1]

getImage s n = fromIntegral . B.index s . (n*28^2 + 16 +) <$> [0..28^2 - 1]
getX     s n = (/ 256) <$> getImage s n
getLabel s n = fromIntegral $ B.index s (n + 8)
getY     s n = fromIntegral . fromEnum . (getLabel s n ==) <$> [0..9]

mi :: Integer -> Integer -> Double -> [Double] -> Integer
mi i _ _ [] = i
mi i c n (x:xs) = if x > n then mi c (c + 1) x xs else mi i (c + 1) n xs

disp = mi 0 0 0
