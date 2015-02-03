{-- Modulo Oraculo.hs
    Contiene las funciones implementadas para el funcionamiento de Haskinator.hs
    Autores: Carlo Polisado 09-10672
	     Alejandro Guevara 09-10971
--}
module Oraculo (Oraculo(Prediccion,Pregunta),crearPrediccion,crearPregunta, prediccion,pregunta,positivo,negativo,obtenerCadena,obtenerEstadistica)  where

import Data.List

data Oraculo = Prediccion String | Pregunta String Oraculo Oraculo deriving (Read,Show)

{--
instance Show Oraculo where
	show (Prediccion str)  = "Pred:\"" ++ str ++ "\""
	show (Pregunta str izq der) = "(Preg:" ++ str ++ "{" ++ show izq ++ "}{" ++ show der ++"}"
--}

{--
instance Read Oraculo where
	readsPrec d str = (leer str,[]):[]

leer str = if isPrefixOf "Pred:" str then crearPrediccion $ (readToQuotes . dropToQuotes) str 
	else if isPrefixOf "¿:" str 
	then crearPregunta str2 (leer (readPregunta str2 0)) (leer (readPregunta (drop (length (readPregunta str2 0)) str2) 0))
	else crearPrediccion str2
	where str2 = (readToQuotes . dropToQuotes) str
--}

readPregunta ('}':xs) 1 = "}" 
readPregunta ('}':xs) n = '}':(readPregunta xs (n-1)) 
readPregunta ('{':xs) n = '{':(readPregunta xs (n+1))  
readPregunta (x:xs) n = x:(readPregunta xs n)
readPregunta [] n = []
 
first =  takeWhile (/=' ') 
second =  tail . dropWhile (/='\"') 
readToQuotes (x:xs) = takeWhile (/='\"') xs 
readToQuotes [] = []
dropToQuotes (x:xs) = dropWhile (/='\"') xs
dropToQuotes [] = []
tupla s = read s :: (String, String) 
--resto :: String -> (String,String)
resto str = tupla (dropToQuotes (second str))

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

obtenerEstadistica :: Oraculo -> (Integer, Integer, Float)
obtenerEstadistica ora = (extFirst res,extSecond res, promedio)
		where res = (estadistica ora (99999,0,(0,0)) 0)
		      x = (fst (extThird res))
		      y = (snd (extThird res))
		      promedio = fromIntegral x / fromIntegral y
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
 
