import Data.List (sort)
main = interact (unlines . sort . lines)
