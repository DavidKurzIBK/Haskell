module Main where

import System.Directory
import System.FilePath
import System.IO
import Data.List (isSuffixOf)

{- Main: ruft die anderen Funktionen in der richtigen Reihenfolge auf.-}

main :: IO ()
main = do
	files <- getFilesInCurrentDirectory
	let txtFiles = filterTxtfiles files
	putStrLn $ formatFiles txtFiles
	interactWithFiles txtFiles

  -- TODO: extend main function from exercise 1
  
getFilesInCurrentDirectory :: IO [FilePath]
getFilesInCurrentDirectory = do
	currentDirectory <- getCurrentDirectory
	files <- getDirectoryContents currentDirectory
	return $ filter (`notElem` [".", ".."]) files

  

filterTxtfiles :: [FilePath] -> [FilePath]
filterTxtfiles = filter (".txt" `isSuffixOf`)


formatFiles :: [FilePath] -> String
formatFiles fs = unlines $ zipWith format [1..] fs
	where
		format :: Int -> FilePath -> String
		format n file = show n ++ ": " ++ file
        


-- bis hierhin gleich wie ex_1_2.hs
-- Funktion interactWithFiles aufgerufen, um die Benutzerinteraktion zu ermöglichen
-- input ist FilePath liste -> output ein IO() input/output eingabeaufforderung


interactWithFiles :: [FilePath] -> IO ()
interactWithFiles files = do
  putStrLn "Input 0 to quit or a file number to view the file contents:"  -- gleich mal output; 
  input <- getLine                                                        -- liest user-input von Zeile darüber ein!! 
  case read input of                                                      -- case unterteilt die User-Input eingabe in 2 Fälle: 
    0 -> putStrLn "Program quits."                                        -- bei 0 als Eingabe wird programm beendet mit Ausgabestring
    n | n > 0 && n <= length files -> do                                  -- überprüfung bei 0 < n < nummer vom letzten file: 
      let selectedFile = files !! (n - 1)                                 -- weist selectedFile den Wert der Datei an Position (n - 1) zu 
                                                                          -- Indexierung von Liste (files) in Haskell beginnt bei Null.
      contents <- readFile selectedFile                                   -- liest Inhalt der Datei mit Pfad selectedFile als string
      putStrLn $ "Contents of " ++ selectedFile ++ ":\n" ++ contents      -- gibt den Dateinamen und dessen Inhalt auf Konsole aus!
      putStrLn "Press enter to restart"                                   -- ausgabe
      _ <- getLine                                                        -- ignoriert eingabe von zeile darüber.. ausgefürt wenn enter
      main                                                                -- wenn enter:hauptprogramm erneut gestartet (rekusiver aufruf)
    _ -> putStrLn "Invalid input. Please try again." >> interactWithFiles files 
    
   -- letze Zeile erreicht, eingeg Zahl weder 0 noch gültige Dateinummer ist.
   -- dann Ausgabe: Eingabe ungültig ist, und Fkt interactWithFiles wird rekursiv mit Liste von Dateien (files) aufgerufen. 
    
    
    