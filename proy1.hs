data Pregunta = Pregunta {preg :: String, hijos :: Either String String}
data Oraculo = String | Oraculo Pregunta 
-- data Oraculo = preg (String, Either Oraculo Oraculo) | String prediccion deriving (Show)
{--
main = do
        putStrLn "Pregunta:"
        p <- getLine
        if null p
		then return() 
		else do
		putStrLn ("Tu pregunta: " ++ p)
		main
--}
main = interact menu

printMenu = do 
	putStr ("1  Crear un oráculo nuevo ")
	putStr ("2: Predicción ")
	putStr ("3: Persistir ")
	putStr ("4: Cargar ")
	putStr ("5: Consultar pregunta crucial ")
	putStr ("6: Consultar estadísticas ")

menu = do 
	printMenu
	n <- getChar
	case n of 1 -> putChar(n) 
		  2 -> putChar(n)
		  3 -> putChar(n)
		  4 -> putChar(n)
		  5 -> putChar(n)
		  6 -> putChar(n)
		  _ -> putStr("Opcion no válida, inserte un número entre 1 y 6\n")
	main
