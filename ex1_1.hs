{-
Programm filtert und formatiert dateien im aktuellen Ordner bevor es etw. ausgibt
-}

module Main where

{- 
benötigten Module werden importiert:
-}

import System.Directory       -- für Verzeichnisoperationen
import System.IO              -- für Ein-/Ausgabeoperationen
import Data.List (isSuffixOf) -- für Funktionen zur Arbeit mit Listen



{-
 To build:
 1) navigate to folder Sheet10 in the terminal
 2) run the command: ghc --make ex1_1.hs
 3) call ./ex1_1 or ./ex1_1.exe on Windows
-}


{- 
Hauptteil des Programms: 
-> ruft getFilesInCurrentDirectory auf, um Liste der Dateien im aktuellen Verzeichnis zu erhalten.
-> Dann Dateien nach der Filterung von .txt-Dateien gespeichert.
-> schließlich die formatierte Ausgabe mit putStr auf Konsole ausgegeben.
-} 


main :: IO ()
main = do
  files <- getFilesInCurrentDirectory -- TODO: get files in current directory = input, then filter txt files and print formatted values
  let txtFiles = filterTxtfiles files
  putStrLn $ formatFiles txtFiles       -- $-Zeichen wie putStrLn ..gibt am ende vom (formatFiles txtFiles)
  


getFilesInCurrentDirectory :: IO [FilePath]        -- [FilePath] ist immer Liste von Dateipfaden im aktuellen Ordner
getFilesInCurrentDirectory = do
  currentDirectory <- getCurrentDirectory          -- um das aktuelle Verzeichnis zu erhalten
  files <- getDirectoryContents currentDirectory   -- um Liste aller Dateien im aktuellen Verzeichnis zu erhalten.
  return $ filter (`notElem` [".", ".."]) files    -- Ergebnisse gefiltert, um spezielle Einträge mit . und .. zu entfernen.
                                                   -- return notwendig um filter Ergebnis in IO-Monad zu heben


filterTxtfiles :: [FilePath] -> [FilePath]         
filterTxtfiles = filter (".txt" `isSuffixOf`)      -- filtert übergebene Liste von Dateipfaden und gibt nur zurück, deren Endung .txt


formatFiles :: [FilePath] -> String
formatFiles = unlines $ zipWith format [1..] -- zipWith: kombiniert Nummern [1..] mit Dateipfaden paarweise in der Liste fs (kann man weglasssen
                                                   -- unlines: fügt Zeilenumbrüche zw. formattierten zeichenketten ein. 
	where
		format :: Int -> FilePath -> String        -- format: als wird auf jedes Paar (Nummer und Dateipfad angewendet 
		format n file = show n ++ ": " ++ file     -- Formatierten String enthält nummer und dateipfad jeder datei. 
        
        
        
        