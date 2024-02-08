-- im modul Logic sind folgende Elemente definiert: 

module Logic (State, Move, Player, initState, showPlayer, showState, winningPlayer, validMoves, dropTile) where

-- import Data.List
-- import Data.Maybe

-- typ der als alias für spezielle int festgelegt 
type Tile = Int -- 0, 1, or 2
type Player = Int -- 1 and 2
type Move = Int -- column number


-- neuer Datentyp State definiert: State übernimmt Argumente: Wert des Typs Player und Tile als Liste (Reihe von Spielsteinen am Spielfeld)
data State = State Player [[Tile]] deriving (Show, Read) -- list of rows

-- die Konstante empty mit dem Typ von Tile wird die Zahl null zugewiesen; 
empty :: Tile
empty = 0

-- 2 konstanten mit typ integer als Zeilen und spalten des Spielfelds
numRows, numCols :: Int
numRows = 6
numCols = 7

-- Konstante mit Typ vom Player wird zahl 1 zugew, 
startPlayer :: Player
startPlayer = 1


-- Fkt mit Typ State wird mit Startspieler und rechteckigen Listen für Zeilen und Spalten des Spielfelds erstellt..alle felder sind Null
initState :: State
initState = State startPlayer (replicate numRows (replicate numCols empty))

-- Fkt mit Typ State vom Spiel gibt als output [Move] also Spielzüge in Liste
validMoves :: State -> [Move]
validMoves (State _ rows) = map fst . filter ((== empty) . snd) . zip [0 .. numCols - 1] $ head rows
-- zuerst wird rows aus dem State-Wert extrahiert, Startspielerwert dabei ignoriert. 
-- zip [0 .. numCols - 1] $ head rows von [0 bis numCols - 1] mit 1sten Zeile des Spielfelds rows gepaart. Ergebnis: Liste von Paaren (Spaltennummer, Zellenwert).
-- filter ((== empty) . snd): Liste der Paare gefiltert, diejenigen behalten wo zweites Element (snd) gleich von empty ist. also nur leere Zellen berücksichtigt.
-- map fst: Spaltennummer (erstes Paar des Paares) für jedes verlbeib. Paar extrahiert. 

-- Fkt ertstellt X strings für Spieler 1 und O strings für Spieler 2
showPlayer :: Player -> String
showPlayer 1 = "X"
showPlayer 2 = "O"

-- Fkt mit Typ Tile gibt char zurück
showTile :: Tile -> Char
showTile t = if t == empty then '.' else head $ showPlayer t
-- bedingte Anweisung: wenn t leer (Zustand leere Zelle) gibt Fkt Punkt zurück. 
--                     wenn t nicht leer ,Fkt showPlayer auf t angewendet und mit head erste Zeichen der kette extrahiert



showState :: State -> String
showState (State player rows) =               --  Spielzustand gemustert, um aktuellen Spieler (player) und Spielfeld (rows) zu extrahieren
  unlines $                                   -- fügt Liste von zeichenketten zu einer einzigen zusammen. 
    map (head . show) [0 .. numCols - 1]      -- Liste von Zeichenketten die Spaltennummern räpresent. 
                                              -- fkt (head . show) auf 0 bis numCols - 1: um 1ste Zeichen Zeichenkette der Spaltennummer zu                                                  extrahieren.
      : map (map showTile) rows -- jede Zeile Spielfelds Funktion showTile auf jedes Element angewendet -> Liste von Zeichenketten erstellen
      ++ ["\nPlayer " ++ showPlayer player ++ " to go"]  -- fügt zeichenkette am ende Liste mit aktuellen Spieler 



otherPlayer :: Player -> Player
otherPlayer = (3 -)  -- auf Fkt angewande Fkt rechnet 3 - x



dropTile :: Move -> State -> State
dropTile col (State player rows) =  -- nimmt eine Spaltennummer (col) und einen Spielzustand (State) entgegen.
  State
    (otherPlayer player)
    (reverse $ dropAux $ reverse rows)  -- modifizierte Spielzustand: (player) gewechselt & dropAux auf umgek Zeilen (reverse rows) angew
  where
    dropAux (row : rows) =      -- modifizier umgekehrten Zeilen 
      case splitAt col row of   -- Spalte col von der aktuellen Zeile zu trennen.
        (first, t : last) ->    -- Ergebnis: Tupel (first, t : last), first col Elemente sind, t Element an Stelle col und last die restl
          if t == empty         -- ob Elem t leeres Spielfeld
            then (first ++ player : last) : rows  -- neues Spielfeld erstellt, indem Spielerstein player in die Spalte eingefügt wird.
            else row : dropAux rows               -- wenn nicht aktuelle Zeile unverändert 

winningRow :: Player -> [Tile] -> Bool   -- Tkt mit Typ Player und Tile als Liste gibt Bool aus
winningRow player [] = False             -- leere liste .. string rückgabe
winningRow player row =                   
  take 4 row == replicate 4 player       -- ersten 4 Elemente Zeile gleich 4mal  Spielerwert entsprechen: gewinnbedingung TRUE
    || winningRow player (tail row)      -- obige Bedingung nicht: Fkt rekursiv mit (tail row) aufge, Überprüfung nächst 4 Elemente forts.
    
   
    
    
-- transponiert Liste (zeilen & reihen tausch)
transpose ([] : _) = []             -- fall einer leeren Liste; 
transpose xs = map head xs : transpose (map tail xs) -- immer neue listen erstellen aus head der xs liste, dann rekursiv aufgerufn mit tail 



diagonals :: [[Tile]] -> [[Tile]]
diagonals [] = []                   -- fall leerer listen
diagonals rows@(row : remRows) =    -- aktuelle Zeile (row) und die verbleibenden Zeilen (remRows) des Spielfelds gemustert
	if length row < winNum || length rows < winNum then -- Länge aktn Zeile oder Anzahl verbl Zeilen kleiner gewünschte Länge (winNum) , keine Diagonalen mehr, und Funkt gibt leere Liste.
		[]
	else
		checkableCs ++ diagonals remRows  -- andernf: Liste der Diagonalen zu bisher gefundenen hinzugefügt: rekurauf die verbl Zeilen 
	where
		winNum = 4  -- gewünschte Länge der Diagonale
		numCheckableCs = numCols - winNum -- Spalten berechnen um genügend um Diagonale zu bilden
		extract = zipWith (!!) rows       -- Liste von Indizes mit entspr. Elem. der Zeilen
		checkableCs = concatMap (\c -> let ps = map (+ c) [0 .. winNum-1] -- Diag. für überpr. Spalte c indiz ps erstell und Diag hinzugef
				in [extract ps, extract $ reverse ps])  -- \lambda call; 
			[0 .. numCheckableCs] -- 

--  TODO 3: check for winning diagonals
winningPlayer :: State -> Maybe Player
winningPlayer (State player rows) =    -- 
  let prevPlayer = otherPlayer player  -- vorherige Spieler als andere Spieler festgelegt
      longRows = rows ++ transpose rows ++ diagonals rows -- Zeilen mit Transp. zeilen undd diagonalen kombiniert
   in if any (winningRow prevPlayer) longRows             -- Zeilen, Spalten oder Diagonalen ine gewinnende Reihe 
        then Just prevPlayer                              -- ja, dann hat prevPlayer gewonnen, 
        else Nothing                                      -- sonst gibt es keinen sieger