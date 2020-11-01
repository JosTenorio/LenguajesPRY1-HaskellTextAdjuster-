import Prelude hiding (null, lookup, map, filter)
import qualified Data.Map as Map
import Data.List (intersperse)
import System.IO
    ( Handle,
      IOMode(ReadMode, ReadWriteMode),
      hClose,
      hIsEOF,
      hGetLine,
      openFile,
      hPutStrLn )
import Monad (when)
import DataDef (HypMap)
import Functions ()

-- El Estado es una "Map" (hash String -> Int) que
-- para cada palabra da el número de veces que la ha encontrado
type Estado = Map.Map [Char] Int

-- main crea un Estado vacío e invoca a mainloop
-- el cual recibe el Estado como parámetro
main :: IO ()
main = do 
       mainloop (Map.fromList [])

-- Ciclo de ejecución:
--     recibe un Estado
--     lee un comando
--     ejecuta un comando que produce un nuevo Estado
--     se invoca recursivamente con el nuevo Estado.
mainloop :: HypMap -> IO ()
mainloop dict = do
  putStr ">> "
  inpStr <- getLine
  let tokens  = words inpStr
  let comand = tokens!!0
  when (isPreffixOf "load" comand)  (do putStrLn ">>> Nombre archivo de entrada: "
                                       fileName <- getLine
                                       inh <- openFile fileName ReadMode
                                       newDict <- loadDict inh ((Map.fromList []),0)
                                       hClose inh
                                       putStrLn $ "Diccionario cargado ( " ++ show(snd(newDict)) ++ " palabras cargadas)"
                                       mainloop (fst(newDict)))




  when (isPreffixOf "show" comand)  (do putStrLn "Diccionario actual: "
                                                 putStrLn (show (dict))
                                                 mainloop dict)
  when (isPreffixOf "ins" comand) (do putStrLn ">>> Nueva entrada del diccionario: "
                                      newEntryLine <- getLine
                                      putStrLn ">>> "
                                      let newEntry = string2DictEntry newEntryLine
                                      mainloop (Map.insert (fst newEntry) (snd newEntry) dict)
     "save" -> do
                  putStrLn ">>> Nombre archivo de salida: "
                  fileName <- getLine
                  saveDict fileName dict 
                  mainloop dict
     "exit" -> do
                  putStrLn "Saliendo..."
     else then do
                 putStrLn $ "Comando desconocido ("++ comand ++"): '" ++ inpStr ++ "'" 
                 mainloop dict

-- función que implementa el comando contar_linea
contar_linea :: [String] -> Estado -> (Estado, String) 
contar_linea tokens estado = (foldl contar_token estado tokens, "contar_linea" )

contar_token :: Estado -> String -> Estado
contar_token estado tok = case Map.lookup tok estado of
                               Nothing -> Map.insert tok 1 estado
                               Just valor -> Map.insert tok (valor+1) estado
  
-- función que implementa el comando borrar
cmd_borrar::[String] -> Estado -> (Estado, String)
cmd_borrar [] estado = (estado, "No se especificó qué borrar")
cmd_borrar (v:_) estado = if Map.member v estado 
                               then (Map.delete v estado, v ++ " borrado")
                               else (estado, v ++ " no aparece")

-- función que maneja un comando desconocido
cmd_desconocido ::
      String -> String -> Estado -> (Bool, Estado, String)
cmd_desconocido cmd comando estado = (False,estado,mensaje)
  where mensaje = "Comando desconocido ("++ cmd ++"): '" 
                                         ++ comando ++ "'"

-- función que implementa el comando imp
cmd_imp :: Estado -> (Estado, String)
cmd_imp estado = (estado, show estado)

-- función que implementa leer un archivo línea por línea
-- y contar las palabras de cada línea
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

-- descargar :: Handle -> [(String,Int)] -> IO ()
descargar outh [] = return ()
descargar outh ((k,v):kvs) = do hPutStrLn outh $ k ++ " " ++ (show v)
                                descargar outh kvs



{-
"guardar" -> do
               putStrLn ">>> Nombre archivo salida: "
               nombreArchivo <- getLine
               outh <- openFile nombreArchivo WriteMode
               descargar outh (sort (toList estado))
               hClose outh
               mainloop estado     
     "clin" -> do
                let (nuevoestado, salida)= contar_linea (tail tokens) estado
                putStrLn salida
                mainloop nuevoestado
     "borrar" -> do
                   let (nuevoestado, salida)= cmd_borrar (tail tokens) estado
                   putStrLn salida
                   mainloop nuevoestado
     "imp" -> do
                 let (nuevoestado, salida) = cmd_imp estado
                 putStrLn salida
                 mainloop nuevoestado

-}