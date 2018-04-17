module Main where

import Neural
import System.IO
import Text.Printf

main :: IO ()
main = do let t = dNet [3,5,9,2] <<+ train {
            input = [[0,1,0],[1,1,0],[1,0,0],[1,1,1],[1,0,1]],
            output = [[0,1],[1,0],[0,1],[1,0],[1,0]],
            epochs = 50000,
            batch = 2,
            rate = 1
          }
          writeFile "Network.nnhs" (show t)
          let f = run t
          print . r $ f[0,0,1]
          print . r $ f[1,1,1]
          print . r $ f[1,1,0]
          print . r $ f[1,0,1]

r :: [Double] -> [String]
r = map (printf "%f" . ((/100.0) . fromIntegral . round . (*100) :: Double -> Double))
