import System.Environment (getArgs)

main = do
  [src, dest] <- getArgs
  s <- readFile src
  writeFile dest s


