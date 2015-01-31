import Data.Maybe
import Control.Applicative

data Oraculo = Prediccion String | Pregunta String Oraculo Oraculo deriving Read

instance Show Oraculo where
	show (Prediccion str)  = "(Pred: " ++ str ++ ")"
	show (Pregunta str izq der) = "(Preg: " ++ str ++ " (" ++ show izq ++ "," ++ show der ++")"
{--
instance Read Oraculo where
	read strCompleto = readOra strCompleto where
		readOra ('!':'{':ys) = crearPrediccion (takeWhile (/='}') ys) readOra (tail . dropWhile(/='}') ys)
		readOra ('¿':ys) = crearPregunta ys  (readOra xs) 
--}
{--
instance Read Oraculo where
	readsPrec d str = readParen (d > 10) (\str -> if first str == "Pred: " then crearPrediccion (readToQuotes second str)
		else if first str == "¿:" 
		then crearPregunta (readToQuotes second str) (readsPrec d (fst resto str)) (readsPrec d (snd resto str))
		else error "not readable") 
--}

leer str = if first str == "Preg: " then crearPrediccion (readToQuotes (second str))
	else if first str == "¿:" 
	then crearPregunta (readToQuotes $ second str) (leer (fst (resto str))) (leer (snd (resto str)))
	else crearPrediccion ("<" ++ (readToQuotes (second str)) ++">") 

first =  takeWhile (/=' ') 
second =  tail . dropWhile (/='\"') 
readToQuotes (x:xs) = takeWhile (/='\"') xs 
dropToQuotes (x:xs) = tail $ dropWhile (/='\"') xs
tupla s = read s :: (String, String) 
--resto :: String -> (String,String)
resto str = tupla (dropToQuotes (second str))
--Haskinator = Oraculo

crearPrediccion :: String -> Oraculo
crearPrediccion pred = Prediccion pred

crearPregunta :: String -> Oraculo -> Oraculo -> Oraculo 
crearPregunta pred pos neg = Pregunta pred pos neg

prediccion :: Oraculo -> String
prediccion (Prediccion pred) = pred
prediccion _ = error("Se esperaba prediccion")

pregunta :: Oraculo -> String
pregunta (Pregunta pred _ _) = pred
pregunta _ = error("Se esperaba pregunta") 

positivo :: Oraculo -> Oraculo
positivo (Pregunta _ pos _ )= pos 
positivo _ = error("Se esperaba prgunta")

negativo :: Oraculo -> Oraculo
negativo (Pregunta _ _ neg) = neg 
negativo _ = error("Se esperaba pregunta")

obtenerCadena :: Oraculo -> String -> Maybe [(String,Bool)]
obtenerCadena (Prediccion predAct) pred = if pred == predAct then Just [] else Nothing
obtenerCadena (Pregunta preg izq der) pred = 
	let xs=(obtenerCadena izq pred) in 
	if xs/=Nothing then fmap ((preg,True):) xs 
	else 
		let ys=(obtenerCadena der pred) in
		if ys/=Nothing then fmap ((preg,False):) ys 
		else Nothing

obtenerEstadistica :: Oraculo -> (Integer, Integer, Integer)
obtenerEstadistica ora = (extFirst res,extSecond res, promedio)
		where res = (estadistica ora (99999,0,(1,1)) 1)
		      x = (fst (extThird res))
		      y = (snd (extThird res))
		      promedio = div x y
extFirst :: (a,b,c) -> a
extFirst (a,_,_) = a

extSecond :: (a,b,c) -> b
extSecond (_,b,_) = b

extThird :: (a,b,c) -> c
extThird (_,_,c) = c


estadistica :: Oraculo -> (Integer, Integer,(Integer,Integer)) -> Integer -> (Integer, Integer,(Integer,Integer))
estadistica (Prediccion s) (x,y,z) n = (minimo,maximo,(suma,cantidad))
			where minimo = if x>n then n else x
			      maximo = if y<n then n else y
			      suma = (fst z) + n
			      cantidad = (snd z) + 1
estadistica (Pregunta s ora1 ora2) tripleta piso = compara (estadistica ora1 tripleta (piso+1)) (estadistica ora2 tripleta (piso+1))

compara :: (Integer, Integer,(Integer,Integer)) -> (Integer, Integer,(Integer,Integer)) -> (Integer, Integer,(Integer,Integer))
compara (x,y,z) (a,b,c) = (min x a,max y b,((fst z)+(fst c),(snd z)+(snd c)))
 
-------------------- IO ------------------------------------
persistir :: Maybe Oraculo -> IO()
persistir ora = do
		putStrLn "Introduzca un nombre para el archivo donde se guardara el oraculo"
		namefile <- getLine
		writeFile namefile (show ora)
		putStrLn "Persistencia completada"
{--
cargar :: Maybe Oraculo
cargar = do
	putStrLn "Introduzca un nombre del archivo que desear cargar"
	namefile <- getLine
	ora = Just (read (readFile name)) 
	putStrLn "Carga completada"
	return ora
 --}

-- Falta acomodar cuando una misma pregunta esta en caminos distintos 

preguntaCrucial :: Maybe Oraculo -> IO()
preguntaCrucial Nothing = putStrLn "No se pueden realizar consultas, Oraculo vacio"
preguntaCrucial (Just ora) = do
		putStrLn "Introduzca Primera prediccion: "
		pred1 <- getLine
		putStrLn "Introduzca Segunda prediccion: "
		pred2 <- getLine
		let cadena1 = obtenerCadena ora pred1
		let cadena2 = obtenerCadena ora pred2
		let filtrado = (((\c1 c2 -> [x | x <- c1, any (\a -> (fst a) == (fst x)) c2])) <$> cadena1 <*> cadena2)
		if (cadena1==Nothing || cadena2==Nothing) then putStrLn "Consulta Invalida1"
		else if  (filtrado == Just []) then putStrLn "Consulta Invalida2"
		else putStrLn (fromJust (fmap (fst . last) filtrado))
				
consultarEstadistica :: Maybe Oraculo -> IO()
consultarEstadistica Nothing = putStrLn "No se pueden realizar consultas, Oraculo vacio"
consultarEstadistica (Just ora) = do
			let tripleta = obtenerEstadistica ora
			putStrLn "Minimo Numero de Preguntas: "
			putStrLn (show $extFirst tripleta)
			putStrLn "Maximo Numero de Preguntas: "
			putStrLn (show $extSecond tripleta)
			putStrLn "Promedio de Preguntas a Realizar: "
			putStrLn (show $extThird tripleta)
main = printmenu

printmenu = do 
	putStrLn ("1  crear un oráculo nuevo ")
	putStrLn ("2: Predicción ")
	putStrLn ("3: Persistir ")
	putStrLn ("4: Cargar ")
	putStrLn ("5: Consultar pregunta crucial ")
	putStrLn ("6: Consultar estadísticas ")

menu orac = do 
	printmenu
	{--n <- getChar
	case n of 1 -> putChar(n) 
		  2 -> putChar(n)
		  3 -> putChar(n)
		  4 -> putChar(n)
		  5 -> putChar(n)
		  6 -> putChar(n)
		  _ -> putStr("Opcion no válida, inserte un número entre 1 y 6\n")
	main--}

