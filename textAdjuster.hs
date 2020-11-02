
import Prelude hiding (null, lookup, map, filter)
import qualified Data.Map as Map
import Data.List ((\\), intercalate, intersperse)
import System.IO
    ( Handle,
      IOMode(ReadMode),
      hClose,
      hIsEOF,
      hGetLine,
      openFile )
import DataDef (HypMap)
import Functions (separarYalinear)

main :: IO ()
main = do 
       mainloop (Map.fromList [])

mainloop :: HypMap -> IO ()
mainloop dict = do
  putStr ">> "
  commandLine <- getLine
  let commands = words commandLine
  let comand = take 1 commands
  case comand of
     ["load"] -> do 
                  let fileName = commands !! 1
                  inh <- openFile fileName ReadMode
                  newDict <- loadDict inh ((Map.fromList []),0)
                  hClose inh
                  putStrLn $ "Diccionario cargado ( " ++ show(snd(newDict)) ++ " palabras cargadas)"
                  mainloop (fst(newDict))
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
                  putStrLn ("Palabra " ++ (fst newEntry) ++ " agregada")  
                  mainloop dict
     ["split"] -> do
                  let text = commandLine \\ (concat ((intersperse " " [(commands !! 0),(commands !! 1),(commands !! 2),(commands !! 3)])))
                  putStrLn (split2string (separarYalinear (read (commands !! 1) :: Int) (commands !! 2) (commands !! 3) dict text))
                  mainloop dict
     ["splitf"] -> do
                  let inputFileName = commands !! 4
                  inh <- openFile inputFileName ReadMode
                  text <- hGetLine inh
                  let resultString = (split2string (separarYalinear (read (commands !! 1) :: Int) (commands !! 2) (commands !! 3) dict text))
                  if (length commands == 5) then putStrLn resultString
                        else do let outputFileName = commands !! 5
                                writeFile outputFileName resultString                            
                  mainloop dict
     ["exit"] -> do
                  putStrLn "Saliendo..."
     _    -> do
            putStrLn $ "Comando desconocido: '" ++ commandLine ++ "'" 
            mainloop dict

loadDict :: Handle -> (HypMap,Int) -> IO (HypMap, Int)
loadDict inh dict = do
      ineof <- hIsEOF inh
      if ineof then return dict
               else do 
                       inpStr <- hGetLine inh
                       let newEntry = string2DictEntry inpStr 
                       loadDict inh ((Map.insert (fst newEntry) (snd newEntry) (fst dict)), (snd dict) + 1) 

saveDict:: String -> HypMap -> IO ()
saveDict fileName dict = writeFile fileName (dict2String (Map.toList dict))

dict2String :: [(String, [String])] -> String
dict2String dict | (length dict == 0) = [] 
                 |otherwise = newLine ++ dict2String  (tail dict)
      where newLine = fst (head dict) ++ " " ++ concat (intersperse "-" (snd (head dict))) ++ "\n"

string2DictEntry::String -> (String, [String])
string2DictEntry line = ((head separated), divison )
      where
            separated = words line
            divison = words (fmap (\c -> if c=='-' then ' '; else c) (last separated))

split2string::[String] -> String
split2string split | (take 1 split  == []) = "Comando inválido" 
                   | otherwise = concat (intersperse "\n" split) 