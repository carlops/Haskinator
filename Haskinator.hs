{-- Haskinator.hs
    Este Archivo contiene el cliente para el funcionamiento de Haskinator
    Autores: Carlo Polisano 09-10672
	     Alejandro Guevara 09-10971
--}
module Haskinator (main) where

import Data.Maybe
import Control.Applicative
import Oraculo

-- hSetBuffering stdin LineBuffering

-- Recibe un Maybe Oraculo y checkea que la entrada no este 'vacia'
-- Luego se apoya en la funcion 'recorrer' para recorrer el arbol
predecir :: Maybe Oraculo -> IO()
predecir Nothing = do
		putStrLn "\n"
		putStrLn "El Oraculo se encuentra vacio, por favor responda a las siguientes peticiones."
		putStrLn "Ingrese una pregunta:"
		pregunta <- getLine
		putStrLn "Ingrese la respuesta posivita a la pregunta ingresada"
		resp1 <- getLine
		putStrLn "Ahora ingrese la respuesta negativa"
		resp2 <- getLine
		putStrLn "Oraculo creado!, ahora si puedo adivinar todos tus pensamientos!"	
		menu (Just (crearPregunta (pregunta) (crearPrediccion resp1) (crearPrediccion resp2)))
		
predecir ora = do 
		aux <- recorrer ora Nothing
		menu aux

{-- Recibe dos Maybe Oraculos, el primero representa la rama por la cual nos 
    encontramos dentro del arbol, y el segundo representa el padre de la
    rama actual
--}
recorrer :: Maybe Oraculo -> Maybe Oraculo -> IO(Maybe Oraculo)
recorrer (Just (Pregunta s ora1 ora2)) padre = do
		putStrLn "\n"
		putStrLn s
		putStrLn "Responda escribiendo si o no"
		respuesta <- getLine
		if (respuesta == "si") then do
			aux <- recorrer (Just ora1) (Just (Pregunta s ora1 ora2))
			return (Just (crearPregunta s (fromJust(aux)) ora2))
		else do 
			aux <- recorrer (Just ora2) (Just (Pregunta s ora1 ora2))
			return (Just (crearPregunta s ora1 (fromJust(aux))))

recorrer (Just (Prediccion s)) padre = do
		putStrLn "\n"
		putStrLn "Pude ver en tu mente que lo que buscas es..."
		putStrLn s
		putStrLn "\n"
		putStrLn "la respuesta fue acertada?"
		putStrLn "Responda escribiendo si, de lo contrario escriba cualquier cosa"
		respuesta <- getLine
		if (respuesta == "si") then return (Just (Prediccion s))
		else do
			putStrLn "\n"
			putStrLn "Ingrese la respuesta correcta"
			resp <- getLine
		        putStrLn "Ingrese una pregunta que distinga su respuesta"
		        resp1 <- getLine
			return (Just (insertar (Prediccion s) padre resp resp1))

insertar :: Oraculo -> Maybe Oraculo -> String -> String -> Oraculo
insertar (Prediccion s) Nothing resp preg = (crearPregunta preg (crearPrediccion resp) (crearPrediccion s))
insertar (Prediccion s) (Just (Pregunta p izq der)) resp preg = 
		(crearPregunta preg (crearPrediccion resp) (crearPrediccion s))

persistir :: Maybe Oraculo -> IO()
persistir ora = do
		putStrLn "\n"
		putStrLn "Introduzca un nombre para el archivo donde se guardara el oraculo"
		namefile <- getLine
		writeFile namefile (show ora)
		putStrLn "¡El Oraculo ahora perdurara por siglos!"
		menu ora

cargar :: IO()
cargar = do
	putStrLn "\n"
	putStrLn "Introduzca un nombre del archivo que desear cargar"
	namefile <- getLine
	aux <- readFile namefile
	putStrLn "Ya tengo mas conocimientos del pasado!"
	putStrLn "\nPresione Enter para continuar"
	a <- getLine
	menu (read aux :: (Maybe Oraculo))

preguntaCrucial :: Maybe Oraculo -> IO()
preguntaCrucial Nothing = do putStrLn "\n"
			     putStrLn "No se pueden realizar consultas, Oraculo vacio"
			     menu Nothing
preguntaCrucial (Just ora) = do
		putStrLn "\n"
		putStrLn "Introduzca Primera prediccion: "
		pred1 <- getLine
		putStrLn "Introduzca Segunda prediccion: "
		pred2 <- getLine
		putStrLn "\n"
		let cadena1 = obtenerCadena ora pred1
		let cadena2 = obtenerCadena ora pred2
		let auxfun = (\c1 c2 -> [x | x <- c1, any (\a -> if ((fst a) == (fst x) && (snd a) /= (snd x)) then True else False) c2])
		let filtrado = ((auxfun) <$> cadena1 <*> cadena2)
		if (cadena1==Nothing || cadena2==Nothing) then putStrLn "Consulta Invalida"
		else if  (filtrado == Just []) then putStrLn "Consulta Invalida"
		else do
			putStrLn "La Pregunta Crucial para ello es: "
			putStrLn (fromJust (fmap (fst . head) filtrado))
		putStrLn "Presione Enter para continuar"
		a <- getLine
		menu (Just ora)
				
consultarEstadistica :: Maybe Oraculo -> IO()
consultarEstadistica Nothing = do 
				  putStrLn "\n" 
				  putStrLn "No se pueden realizar consultas, Oraculo vacio"
				  menu Nothing
consultarEstadistica (Just ora) = do
			let tripleta = obtenerEstadistica ora
			putStrLn " \n"
			putStrLn "Minimo Numero de Preguntas: "
			putStrLn (show $extFirst tripleta)
			putStrLn "Maximo Numero de Preguntas: "
			putStrLn (show $extSecond tripleta)
			putStrLn "Promedio de Preguntas a Realizar: "
			putStrLn (show $extThird tripleta)
			putStrLn "Presione Enter para continuar"
			aux <- getLine
			menu (Just ora)

extFirst :: (a,b,c) -> a
extFirst (a,_,_) = a

extSecond :: (a,b,c) -> b
extSecond (_,b,_) = b

extThird :: (a,b,c) -> c
extThird (_,_,c) = c

main :: IO()
main = menu Nothing

menu :: Maybe Oraculo -> IO ()
menu orac = do

	 putStrLn "\n\n _______________________________ "
	 putStrLn " "
	 putStrLn "     Bienvenido a Haskinator       "
	 putStrLn " "
	 putStrLn " _______________________________ "
	 putStrLn ""
	 putStrLn "   1. Crear un Oraculo nuevo."
	 putStrLn ""
	 putStrLn "   2. Prediccion."
	 putStrLn ""
	 putStrLn "   3. Persistir."
	 putStrLn ""
	 putStrLn "   4. Cargar."
	 putStrLn ""
	 putStrLn "   5. Consultar pregunta crucial."
	 putStrLn ""
	 putStrLn "   6. Consultar estadisticas."
	 putStrLn ""
	 putStrLn "   7. Salir.\n"

	 entrada <- getLine

	 case entrada of
	     "1" -> menu Nothing
	     "2" -> predecir orac
	     "3" -> persistir orac
	     "4" -> cargar
	     "5" -> preguntaCrucial orac
	     "6" -> consultarEstadistica orac
	     "7" -> return ()
	     -- _ -> do 

