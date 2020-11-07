{-          Imports             -}
import Prelude hiding (null, lookup, map, filter)
import qualified Data.Map as Map
import Data.List (intercalate, intersperse)
import System.IO
    ( Handle,
      IOMode(ReadMode),
      hClose,
      hIsEOF,
      hGetLine,
      openFile )
import DataDef (HypMap)
import Functions (dict2String, split2string, string2DictEntry, separarYalinear)
import System.Directory (doesFileExist)
import Data.Char as Char (ord)

{-          main             -}
main :: IO ()
main = do 
       mainloop (Map.fromList [])

{-          mainloop             -}

--Iteration:
mainloop :: HypMap -> IO ()
mainloop dict = do
  putStr ">> "
  commandLine <- getLine
  let commands = words commandLine
  let comand = take 1 commands
  case comand of
     ["load"] -> do 
                  let fileName = commands !! 1
                  fileExists <- doesFileExist fileName
                  if (fileExists) then  do 
                                          inh <- openFile fileName ReadMode
                                          newDict <- loadDict inh ((Map.fromList []),0)
                                          hClose inh
                                          putStrLn $ "Diccionario cargado (" ++ show(snd(newDict)) ++ " palabras cargadas)"
                                          mainloop (fst(newDict))
                  else do
                        putStrLn $ "Error: El archivo " ++ fileName ++ " no existe"      
                        mainloop (dict)
     ["show"] -> do 
                  putStrLn "Diccionario actual: "
                  putStrLn (unlines [(fst x) ++ " " ++ (intercalate "-" (snd x)) | x <- Map.toList dict])
                  mainloop dict
     ["ins"] -> do 
                  let newEntryLine = (commands !! 1) ++ " " ++ (commands !! 2)
                  let newEntry = string2DictEntry newEntryLine
                  let newDict = (Map.insert (fst newEntry) (snd newEntry) dict)
                  putStrLn ("Palabra " ++ (fst newEntry) ++ " agregada") 
                  mainloop newDict
     ["save"] -> do 
                  let fileName = commands !! 1
                  saveDict fileName dict
                  putStrLn ("Diccionario guardado en el archivo: " ++ fileName ++ " (" ++ show(Map.size dict) ++ " palabras cargadas)")  
                  mainloop dict
     ["split"] -> do
                  let text = concat (intersperse " " (drop 4 commands))
                  putStrLn $ "Texto ajustado: "
                  putStrLn (split2string (separarYalinear (read (commands !! 1) :: Int) (commands !! 2) (commands !! 3) dict text))
                  mainloop dict
     ["splitf"] -> do
                  let inputFileName = commands !! 4
                  fileExists <- doesFileExist inputFileName
                  if (fileExists) then  do 
                                          inh <- openFile inputFileName ReadMode
                                          text <- hGetLine inh
                                          let resultString = (split2string (separarYalinear (read (commands !! 1) :: Int) (commands !! 2) (commands !! 3) dict (normalizeText text)))
                                          if (length commands == 5) then do putStrLn $ "Texto ajustado: " ++ "\n"
                                                                            putStrLn resultString
                                                else do let outputFileName = commands !! 5
                                                        writeFile outputFileName (denormalizeText resultString)
                                                        putStrLn  ("Resultado guardado en " ++ outputFileName)
                  else do
                        putStrLn $ "Error: El archivo " ++ inputFileName ++ " no existe"                           
                  mainloop dict
     ["exit"] -> do
                  putStrLn "Saliendo..."
     _    -> do
            putStrLn $ "Comando desconocido en la instrucción: '" ++ commandLine ++ "'" 
            mainloop dict


--MainLoop Functions:
loadDict :: Handle -> (HypMap,Int) -> IO (HypMap, Int)
loadDict inh dict = do
      ineof <- hIsEOF inh
      if ineof then return dict
               else do 
                       inpStr <- hGetLine inh
                       let newEntry = string2DictEntry (normalizeText inpStr) 
                       loadDict inh ((Map.insert (fst newEntry) (snd newEntry) (fst dict)), (snd dict) + 1) 

saveDict:: String -> HypMap -> IO ()
saveDict fileName dict = writeFile fileName (dict2String (Map.toList dict))

normalizeText :: String -> String
normalizeText [] = []
normalizeText (x:xs)  | (Char.ord x == 9500 && Char.ord (head xs) == 237) = "á" ++ (normalizeText (drop 1 xs))
                      | (Char.ord x == 9500 && Char.ord (head xs) == 9474) = "ó" ++ (normalizeText (drop 1 xs))
                      | (Char.ord x == 9500 && Char.ord (head xs) == 161) = "í" ++ (normalizeText (drop 1 xs))
                      | (Char.ord x == 9500 && Char.ord (head xs) == 9553) = "ú" ++ (normalizeText (drop 1 xs))
                      | (Char.ord x == 9500 && Char.ord (head xs) == 8976) = "é" ++ (normalizeText (drop 1 xs))
                      | otherwise = [x] ++ normalizeText xs

denormalizeText :: String -> String
denormalizeText [] = []
denormalizeText (x:xs) | (x == 'á') = "\9500\237" ++ denormalizeText xs
                       | (x == 'ó') = "\9500\9474" ++ denormalizeText xs
                       | (x == 'é') = "\9500\8976" ++ denormalizeText xs
                       | (x == 'ú') = "\9500\9553" ++ denormalizeText xs
                       | (x == 'í') = "\9500\161" ++ denormalizeText xs
                       | otherwise = [x] ++ denormalizeText xs