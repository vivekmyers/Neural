import Neural
import System.IO
import Text.Printf
import System.Random

main :: IO ()
main = do let t = dNet [2,5,5,1] <<+ train {
            input = [[0,1],[1,0],[0,0],[1,1]],
            output = [[1],[1],[1],[0]],
            rate = 1,
            epochs = 100000,
            momentum = 0.5
          }
          putStrLn []
          let f = run t
          print . r $ f [0,0]
          print . r $ f [1,0]
          print . r $ f [0,1]
          print . r $ f [1,1]

r :: [Double] -> [String]
r = map (printf "%f" . ((/100.0) . fromIntegral . round . (*100) :: Double -> Double))

