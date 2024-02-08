import System.Environment (getArgs)
import Data.Char (toUpper)

main = do
  [file] <- getArgs
  s <- readFile file
  writeFile file (map toUpper s)



