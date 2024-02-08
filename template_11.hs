

-- program 1
[] ++ ys = ys
(x : xs) ++ ys = x : (xs ++ ys)
filter f [] = []
filter f (x : xs)
| f x = x : filter f xs
| otherwise = filter f xs
smaller p xs = filter (\x -> x < p) xs
bigger p xs = filter (\x -> x >= p) xs
qsort [] = []
qsort (x:[]) = [x]
qsort (x:xs) = qsort (smaller x xs) ++ x : qsort (bigger x xs)


-- program 2
double x = x + x
take 0 _ = []
take _ [] = []
take n (x : xs) = x : take (n - 1) xs
map f [] = []
map f (x : xs) = f x : map f xs





-- Exercise 1
-- Datentypen und Funktionen, die mit gerichteten Graphen und Bäumen arbeiten

type Graph a = [(a, a)]                           -- gerichteten Graphen mit Kanten zwischen Werten vom Typ a.
type RootedGraph a = (a, Graph a)                 -- gerichteter Graph zusammen mit einem Startknoten vom Typ a
data Tree a = Node a [Tree a] deriving (Eq, Show) -- Baum, jeder Knoten einen Wert vom Typ a und eine Liste von Teilbäumen enthält.

graphFigure1 :: RootedGraph Int
graphFigure1 = (1, [(1,1), (1,2), (1,3), (1,4), (2,1), (3,1), (4,1)]) 
-- gerichteter Graph mit Startknoten 1, und Kanten zu Knoten 1, 2, 3 und 4.


-- Fkt nimmt RootedGraph und erstellt einen Baum (Tree) basierend auf diesem Graphen
-- n... wurzelknoten, kinder durch unwind erstellte Teilbäume
-- ausgehend Startknotentupel (n, g) wird Baum konstruiert, bei dem jeder Knoten direkten Nachfolger als Teilbäume enthält.
unwind :: Eq a => RootedGraph a -> Tree a
unwind (n, g) = Node n (map (\ s -> unwind (s, g)) successors)  -- Fkt map und rekursiven Anwendung unwind auf die Nachfolger erreicht.
	where successors = map snd (filter ( (== n) . fst) g)       
--  filter ( (== n) . fst) g ... filtert alle Kanten (m, n) aus Graphen g, bei denen Startknoten m gleich aktuellen Knoten n ist, gibt Liste von direkten Nachfolgern (successors) des aktuellen Knotens n zurück.
-- map snd...extrahiert zweiten Elemente aus Tupeln in Liste successors, was Liste direkten Nachfolger ohne die Startknoten darstellt
-- map (\s -> unwind (s, g)) successors ... Fkt unwind rekursiv auf jede Kante in Liste direkten Nachfolger, Teilbäume jedes direkten Nachfolgers entstehen.


-- (node x ts)... eingabebaum mit knoten, x der aktuelle wert des knotens und ts die liste der teilbäume (kinder) 
prune :: Int -> Tree a -> Tree a
prune = prune 0 (Node x ts) = Node x []               -- prune 0: ktuelle Knoten beibehalten, aber Teilbäume entfernt
prune n (Node x ts) = Node x (map (prune (n - 1)) ts) -- nimmt Tiefe n und Baum und entfernt alle Teilbäume, die tiefer als n sind rekursiv mit einer Tiefe von (n - 1)


-- narrow: erstellt neuen Baum, der durch Verengen des ursprünglichen Baums auf die ersten n Teilbäume entsteht. 
-- nur ersten n Kinder jedes Knotens im Baum beibehalten werden, und alle weiteren Kinder werden ignoriert.
-- Tiefe n und einen Baum und behält nur die ersten n Teilbäume durch Verwendung von take n auf die Liste der Teilbäume erreicht.
-- (Node x ts)... eingabebaum mit knoten, x der aktuelle wert des knotens und ts die liste der teilbäume (kinder) 
narrow :: Int -> Tree a -> Tree a
narrow n (Node x ts) = Node x $ map (narrow n) $ take n ts
-- take n ts...ersten n Teilbäume aus Liste ts; wenn n größer als Anzahl der Teilbäume, werden alle Teilbäume ausgewählt
-- map (narrow n)...narrow rekursiv auf jeden der ausgewählten Teilbäume an.


-- unendlichen Baum: Startnoten: Wert 1, jeder Knoten einen Wert und Teilbäume und Kinderknoten durch Multiplik des Werts des aktuellen Knotens mit aufeinanderfolgenden Zahlen erzeugt.
mults :: Tree Integer
mults = go 1  -- initialisiert Baum mit Wurzelknoten Wert 1.
	where go i = Node i $ map go $ map (i*) [2..]  --  rekursive Funktion go: nimmt Parameter i und erstellt Knoten mit Wert i. 
    -- Kinder des Knotens durch Anwendung map go auf Liste von Zahlen map (i*) [2..] gebildet.
    -- (map (i*) [2..]) alle Vielfachen von i ab 2. Jedes Element Liste verwendet, um rekursiv Kinderknoten zu erstellen


-- Tests
firsts :: Tree a -> [a]
firsts (Node x []) = [x]
firsts (Node x (t:_)) = x : firsts t

tests = do
  test "narrow" "Node 1 [Node 2 []]" (narrow 1 $ Node 1 [Node 2 [], Node 3 []])
  test "prune+unwind" "Node 1 [Node 1 [Node 1 [],Node 2 [],Node 3 [],Node 4 []],Node 2 [Node 1 []],Node 3 [Node 1 []],Node 4 [Node 1 []]]" (prune 2 $ unwind graphFigure1)
  test "prune+unwind" "Node 1 []" (prune 0 $ unwind graphFigure1)
  test "mults" "[1,2,4,8,16,32,64,128,256,512]" (take 10 $ firsts $ mults)

test name e c = do
  putStr ("*** " ++ name ++ ": ")
  if show c == e then putStrLn "OK"
  else putStrLn ("ERROR; expected '" ++ e ++ "', but found '" ++ show c ++ "'")