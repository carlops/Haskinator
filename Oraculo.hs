{-- Modulo Oraculo.hs
    Contiene las funciones implementadas para el funcionamiento de Haskinator.hs
    Autores: Carlo Polisado 09-10672
	     Alejandro Guevara 09-10971
--}
module Oraculo  where

data Oraculo = Prediccion String | Pregunta String Oraculo Oraculo deriving (Read,Show)

{--
instance Show Oraculo where
	show (Prediccion str)  = "(Pred: " ++ str ++ ")"
	show (Pregunta str izq der) = "(Preg: " ++ str ++ " (" ++ show izq ++ "," ++ show der ++")"
--}
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
		where res = (estadistica ora (99999,0,(0,0)) 0)
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
 
