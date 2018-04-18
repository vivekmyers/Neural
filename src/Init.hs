module Init where

import Neural
import System.IO
import System.Environment

main :: IO ()
main = do arg <- read . head <$> getArgs :: IO [Integer]
          nt <- rNet ([784] ++ arg ++ [10])
          save "DigitNetwork.nnhs" nt

