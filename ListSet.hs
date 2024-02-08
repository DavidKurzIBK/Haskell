module ListSet (Set, insert, delete, foldSet, empty, member) where

newtype Set a = Set [a] deriving (Show)

insert :: (Eq a) => a -> Set a -> Set a
insert x (Set xs) = Set (x : xs)

delete :: (Eq a) => a -> Set a -> Set a
delete x (Set xs) = Set (filter (/= x) xs)

foldSet :: (a -> b -> b) -> b -> Set a -> b
foldSet f e (Set xs) = foldr f e xs

member :: (Eq a) => a -> Set a -> Bool
member x (Set xs) = x `elem` xs

empty :: Set a
empty = Set []
