import Prelude hiding (null, lookup, map, filter)
import Data.Map.Lazy hiding (sort,map,foldl)
import Data.Char
import Data.List (sort,map)
import System.IO

-- El Estado es una "Map" (hash String -> Int) que
-- para cada palabra da el número de veces que la ha encontrado
type Estado = Map [Char] Int

-- main crea un Estado vacío e invoca a mainloop
-- el cual recibe el Estado como parámetro
main :: IO ()
main = do 
       mainloop (fromList[])

-- Ciclo de ejecución:
--     recibe un Estado
--     lee un comando
--     ejecuta un comando que produce un nuevo Estado
--     se invoca recursivamente con el nuevo Estado.
mainloop :: Estado -> IO ()
mainloop estado = do
  putStr ">> "
  inpStr <- getLine
  let tokens  = words inpStr
  let comando = tokens!!0
  
  case comando of
     "leer" -> do
               putStrLn ">>> Nombre archivo entrada: "
               nombreArchivo <- getLine
               inh <- openFile nombreArchivo ReadMode
               nuevoestado <- cargar inh estado
               hClose inh
               putStrLn $ "Archivo " ++ nombreArchivo ++ " fue cargado"
               mainloop nuevoestado
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
     "fin" -> do
                 putStrLn "Saliendo..."
     _     -> do
                 putStrLn $ "Comando desconocido ("++ comando ++"): '" ++ inpStr ++ "'" 
                 mainloop estado

-- función que implementa el comando contar_linea
contar_linea :: [String] -> Estado -> (Estado, String) 
contar_linea tokens estado = (foldl contar_token estado tokens, "contar_linea" )

contar_token :: Estado -> String -> Estado
contar_token estado tok = case lookup tok estado of
                               Nothing -> insert tok 1 estado
                               Just valor -> insert tok (valor+1) estado
  
-- función que implementa el comando borrar
cmd_borrar::[String] -> Estado -> (Estado, String)
cmd_borrar [] estado = (estado, "No se especificó qué borrar")
cmd_borrar (v:_) estado = if member v estado 
                               then (delete v estado, v ++ " borrado")
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
cargar :: Handle -> Estado -> IO Estado
cargar inh estado = do
      ineof <- hIsEOF inh
      if ineof then return estado
               else do inpStr <- hGetLine inh
                       let nuevoestado = foldl contar_token estado (words (map toLower inpStr))
                       cargar inh nuevoestado


-- descargar :: Handle -> [(String,Int)] -> IO ()
descargar outh [] = return ()
descargar outh ((k,v):kvs) = do hPutStrLn outh $ k ++ " " ++ (show v)
                                descargar outh kvs
