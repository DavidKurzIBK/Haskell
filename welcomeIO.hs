main = do
  putStrLn "Greetings! Please tell me your name."
  name <- getLine
  putStrLn $ "Welcome to Haskell's IO, " ++ name ++ "!"

