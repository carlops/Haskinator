import Data.Maybe

data Oraculo = Prediccion String | Pregunta String Oraculo Oraculo deriving Read

instance Show Oraculo where
	show (Prediccion str)  = "{" ++ str ++ "}"
	show (Pregunta str izq der) = "¿" ++ str ++ "<" ++ show izq ++ "|" ++ show der ++">"
{--
instance Read Oraculo where
	read strCompleto = readOra strCompleto where
		readOra ('!':'{':ys) = crearPrediccion (takeWhile (/='}') ys) readOra (tail . dropWhile(/='}') ys)
		readOra ('¿':ys) = crearPregunta ys  (readOra xs)
		readOra  () = 
--}
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

preguntaCrucial :: Maybe Oraculo -> IO()
preguntaCrucial Nothing = putStrLn "No se pueden realizar consultas, Oraculo vacio"
preguntaCrucial Just ora = do
				putStrLn "Introduzca Primera prediccion: "
			    	pred1 <- getLine
				putStrLn "Introduzca Segunda prediccion: "
			    	pred2 <- getLine
				cadena1 <- obtenerCadena ora pred1
				cadena2 <- obtenerCadena ora pred2
				if (cadena1==Nothing || cadena2==Nothing) then putStrLn "Consulta Invalida"
				else  putStrLn "Consulta Invalida"
		{--			filtrado <- ((\c1 c2 -> [x | x <- c1, any (\a -> (fst a) == (fst x)) c2])) <$> cadena1 <*> cadena2
					if  (filtrado == Just []) then
					 	putStrLn "Consulta Invalida"
					else
						putStrLn fromJust (fmap (fst . last) filtrado)

--}


main = printmenu

printmenu = do 
	putStrLn ("1  crear un oráculo nuevo ")
	putStrLn ("2: Predicción ")
	putStrLn ("3: Persistir ")
	putStrLn ("4: Cargar ")
	putStrLn ("5: Consultar pregunta crucial ")
	putStrLn ("6: Consultar estadísticas ")

menu = do 
	printMenu
	{--n <- getChar
	case n of 1 -> putChar(n) 
		  2 -> putChar(n)
		  3 -> putChar(n)
		  4 -> putChar(n)
		  5 -> putChar(n)
		  6 -> putChar(n)
		  _ -> putStr("Opcion no válida, inserte un número entre 1 y 6\n")
	main--}

