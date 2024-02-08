import System.Environment (getArgs)
main = do
  [file] <- getArgs
  s <- readFile file
  putStr s

