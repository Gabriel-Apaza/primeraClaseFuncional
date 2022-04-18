module Library where
import PdePreludat

doble :: Number -> Number -- defino doble como un numero
doble numero = 2 * numero

siguiente :: Number -> Number -- siguiente es una funcion que va de numeros a numeros
siguiente nro = nro + 1


calcular nro | even nro = siguiente nro -- si es par, devuelve el siguiente
             | otherwise = doble nro -- reutilizo la funcion doble, si es inpar devuelve el doble


mes :: (Number,Number,Number) -> Number -- declaro el tipo de mes
mes (_, unMes, _) = unMes -- devuelve el segundo elemento | "_" es variable anonima

--1) Definir la función calcular’, que recibe una tupla de 2 elementos, y
-- devuelve una nueva tupla según las siguientes reglas:
-- ● si el primer elemento es par lo duplica; si no lo deja como está
-- ● si el segundo elemento es impar le suma 1; si no deja como está
-- >calcular’ (4,5) --resultado --> (8,6)



calcular' :: (Number,Number) -> (Number, Number)
calcular' (numero1, numero2) = (primerElemento numero1, segundoElemento numero2) -- declaro 2 funciones

primerElemento :: Number -> Number
primerElemento nro | even nro = doble nro -- si es par, lo duplica
                   | otherwise = nro -- si es inpar, deja el numero como esta

segundoElemento :: Number -> Number
segundoElemento nro | odd nro = siguiente nro -- funcion odd, para el caso de inpar
                    | otherwise = nro


-- 2) Definir las funciones boolenas estándar. Sin usar las funciones predefinidas.
-- 2.1) Definir la función and’
-- 2.2) Definir la función or’.


and' :: Bool -> Bool -> Bool -- los primeros 2 Bool son los valores del dominio, el ultimo es la imagen
and' unBool otroBool | not unBool = False -- si unBool no es verdadero, entonces es falso
                     | not otroBool = False -- si otroBool no es verdadero, entonces es falso
                     | otherwise = True

-- otra forma de hacerlo:
and'' :: Bool -> Bool -> Bool
and'' unBool otroBool | unBool = otroBool -- si unBool es V, evalua otroBool. Si otroBool es V, va a devolver V, sino Falso
                      | otherwise = False -- si unBool es F, entonces devuelve Falso

-- otra forma de hacerlo: (usamos pater maching)
and''' :: Bool -> Bool -> Bool
and''' True unBool = unBool -- si el valor es un True, entonces devuelve el valor de verdad de unBool
and''' False _ = False -- tambien podria ser _ _ = False, porque arriba ya estoy asumiendo que no es True
-- ejemplo: and''' (even 4) (odd 9) --resultado--> True

or' :: Bool -> Bool -> Bool
or' True _ = True -- si es True la primera, y el segundo lo que sea, devuelve True
or' _ True = True -- si es lo que sea la primera y el segundo True, devuelve True
or' _ _ = False -- lo que sea y lo que sea, devuelve False

-- otra forma: 
or''' True _ = True
or''' _ unBool = unBool

-- otra forma:
or'' :: Bool -> Bool -> Bool
or'' False False = False -- tambien puede ser: False unBool = unBool
or'' _ _ = True