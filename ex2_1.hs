module Main (main) where

import Logic      -- importiert das Logic-Modul mit allen funkt darin
import System.IO  -- für Ein/Ausgabe 
import Text.Read  -- sicherstellen um strings zu lesen 

file :: FilePath        -- definiert einen fixen pfad zur connect4.txt datei
file = "connect4.txt"   

startPlayer :: Player   -- initialisiert startplayer mit 1
startPlayer = 1

-- TODO 2: offer users the option to load game from a .txt file
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering        -- stellt buffering mode auf standard output 'NoBuffering'
  putStrLn "Welcome to Connect Four"      -- print
  newOrLoad "(n)ew game or (l)oad game: " -- eingabeaufforderung mit fkt aufruf nowOrLoad 
  game initState                          -- ruft game fkt auf mit initState status


-- input char von oben (entweder n oder l) : output string je nach IO
newOrLoad :: [Char] -> IO ()
newOrLoad str = do
	putStr str            -- übergibt die string eingabe von oben
	answer <- getLine     -- liest string ein
	if elem answer ["n","new"] then newGame startPlayer   -- wenn n oder new im string wird die fkt newGame mit startPlayer instanz von obn
	else if elem answer ["l","load"] then loadGame        -- l oder load im string, wird fkt loadGame fkt aufgerufen
	else newOrLoad "unknown answer, please type \"n\" or \"l\": "  -- wenn keine obere bedingung zutrifft dass als ausgabe und nochmal rekusiv ebendiese fkt nochmal aufgerufen 


loadGame :: IO()
	loadGame = do
	stateStr <- readFile file                 -- liest file connect4.txt
	let (startPlayer,state) = read stateStr   -- Pattern-Matching: rückga. von read statStr in Bestandt aufget und startPlayer, state zugew. 
	game startPlayer state                    -- ruft nun game fkt unter startPlayer und state auf
    

newGame :: Player -> IO()
newGame startPlayer = game startPlayer (initState startPlayer) -- startet neues spiel mit init vom startPlayer (angeg. Startspieler) und initSate (anfangszust d. spiels)


-- nimmt aktuellen Spieler und gameState (argumente)
game :: Player -> State -> IO ()
game state = do
  putStrLn $ showState state    -- print von aktuellen gameState
  case winningPlayer state of   -- überprüft den spiel-status falls ein spieler das spiel gewinnt
    Just player -> putStrLn $ showPlayer player ++ " wins!"   -- falls ja, schließt loop hier mit print ab!! 
    Nothing ->                                                -- falls nicht kommt man in move loop: 
      let moves = validMoves state                            -- validMoves fkt auf aktuellen Spiel-status angewandt, result. Liste von 
                                                              -- gültigen Zügen moves zugewiesemn 
       in if null moves                                       -- if-bed. prüft ob liste von gültigen Zügen leer? 
            then putStrLn "Game ends in draw."                -- falls ja, wird print ausgeführt
            else do                                           -- andernfalls, print mit user input, 
              putStr $ "Choose one of " ++ show moves ++ ": " -- TODO 2: allow user to save game in a .txt file
              hFlush stdout -- flush print buffer
			  moveStr <- getLine                              -- liest user-input von oben ein!!                         
			  let move = (read moveStr :: Move)               -- Eing. Str von oben in Wert des Typs Move umgew. und move zugew.
              game (dropTile move state)                      -- zug (move) aus benutzereingabe extrahiert, fkt dropfile aufgerufen um aktuellen Spielstand aktualisieren und Zug auf Spiel durchführen, dann aktuelle Spielzustand game zugew. 

-- 
saveGame :: Player -> State -> IO ()      -- 
saveGame startPlayer state = do           -- 
	writeFile file $ show (startPlayer, state)   -- aktueller Spielstand mit zugehörigen Spieler wird zu String-Paar konvertiert
                                                 -- gespeichert dann in file.
	putStrLn "Game saved. Good Bye!"             -- danach Str ausgabe
