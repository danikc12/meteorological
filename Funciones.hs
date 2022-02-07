module Funciones
(transforma_datos,
transforma_datos_aux,
muestra_datos,
llamada_facil,
datos_grafica_4,
datos_grafica_5,
) where


import Data.List
import Data.Maybe
import Control.Monad
import Text.CSV

import Data.Map (Map)
import qualified Data.Map as Map
-- Cambiar las funciones de la fecha, cambiar funciones de mayor ascenso, mirar los comentarios que faltan y tipo fecha

-- ============================================================================================================
-- ===================================== CREACIÓN Y FORMATEO DE LOS DATOS =====================================
-- ============================================================================================================

-- El tipo de dato Metereología constará de seis parámetros, un indicador y una fecha seguido de
-- la temperatura mínima con su hora de medición así como la temperatura máxima y su hora.

data Meteorologia = Meteorologia { ind :: Int
                                    , fecha :: String
                                    , tmin :: Float
                                    , horatmin :: String
                                    , tmax :: Float
                                    , horatmax :: String 
                                    } deriving Show

-- El tipo de dato Fecha constará de tres parámetros, día, mes y año.

data Fecha = Fecha { dia :: Int
                        , mes :: Int
                        , anyo :: Int
                        } deriving Show

-- ============================================================================================================

-- Definimos la función transforma_datos que tomará como entrada una lista de listas de tipo Field que nos dev-
-- -uelve el lector en el main y, eliminando la primera línea que no son datos, transforma los datos gracias a 
-- su función auxiliar que utilizará el constructor de Meteorología para crear cada dato.

transforma_datos :: [[Field]]->[Meteorologia]
transforma_datos ls =  map f (drop 1 ls)
    where f x = transforma_datos_aux x

-- Cabe destacar que aquí se usa la función auxiliar "parsea_float" para convertir los datos tipo Float que se 
-- encuentran con "," en el fichero en vez de con ".".

transforma_datos_aux :: [Field] -> Meteorologia 
transforma_datos_aux ls = Meteorologia {ind = read (head ls) :: Int
                                    , fecha = show (ls !! 1)
                                    , tmin = parsea_float (show (ls !! 2))  
                                    , horatmin = show (ls !! 3) 
                                    , tmax = parsea_float (show (ls !! 4)) 
                                    , horatmax = show (ls !! 5)
                                    } 

-- Además creamos la función to_fecha para poder pasar los datos de tipo String a tipo Fecha.

to_fecha :: String -> Fecha
to_fecha [_,y1,y2,y3,y4,_,m1,m2,_,d1,d2,_] = Fecha {anyo = read a::Int,
                                                        mes = read m::Int,
                                                        dia = read d::Int}
    where
        a = y1:y2:y3:y4:"" 
        m = m1:m2:""
        d = d1:d2:"" 

-- ============================================================================================================   

-- La función muestra_datos se crea para mostrar de una manera más visual el tipo Meteorología, por ello recibe 
-- un tipo Meteorología y devuelve un String.

muestra_datos :: Meteorologia -> String
muestra_datos x = "[ind: "++ show (ind x) ++
                ", fecha: "++ fecha x ++
                ", tmin: "++ show (tmin x) ++
                ", horatmin: "++ horatmin x ++
                ", tmax: "++ show (tmax x)++
                ", horatmax: "++ horatmax x ++"]\n"

-- ============================================================================================================

-- La función llamada_facil recibe la lista de Meteorología que se lee del fichero y un número, este es el ind-
-- -icador que referencia a las demás funciones y devuelve el resultado de dicha función por con consola, por 
-- lo tanto, tipo Int. Recibe también un String auxiliar para el elemento 14 que lo necesita como parámetro.

llamada_facil :: [Meteorologia]->Int->String->String
llamada_facil xs t s 
    | t==1 = show $ mayor_diferencia_temperaturas xs
    | t==2 = show $ mayor_ascenso_tmax xs
    | t==3 = show $ mayor_ascenso_tmin xs
    | t==4 = show $ mayor_descenso_tmax xs
    | t==5 = show $ mayor_descenso_tmin xs
    | t==6 = show $ tmax_mas_alta_2 xs
    | t==7 = show $ tmax_mas_baja_2 xs
    | t==8 = show $ tmin_mas_alta_2 xs
    | t==9 = show $ tmin_mas_baja_2 xs
    | t==10 = show $ fecha_tmax_alta_anual_2 xs 
    | t==11 = show $ fecha_tmax_baja_anual_2 xs
    | t==12 = show $ fecha_tmin_alta_anual_2 xs
    | t==13 = show $ fecha_tmin_baja_anual_2 xs
    | t==14 = aux_filtra_fecha $ filtra_fecha xs s
    | t==15 = show $ media_tmax xs
    | t==16 = show $ media_tmin xs
    | otherwise = "Lo siento, esa opci0n no esta disponible."

-- Esta función auxiliar nos permite una mejor impresión de los datos para la funcón filtra-fecha

aux_filtra_fecha :: [Meteorologia] -> String
aux_filtra_fecha xs = unwords [ muestra_datos x | x <-xs ]

-- ============================================================================================================
-- ================================ CREACIÓN DE DATOS PARA LAS GRÁFICAS =======================================
-- ============================================================================================================

-- Tenemos tres funciones, todas ellas reciben la lista de Meteorología que leemos y devuelven una lista de tu-
-- -plas cuyo primer elemento es la fecha y el segundo es la temperatura mínima y máxima respectivamente. La 
-- tercera función en vez de usar la fecha, utiliza el indicador.

datos_grafica :: [Meteorologia]->[(String,Float)]
datos_grafica ls = [(fecha x , tmin x)| x<-ls]

-- Como nota en esta función eliminamos los elementos que se imprimen de más en la fecha en la función anterior.
datos_grafica_2 :: [Meteorologia]->[(String,Float)]
datos_grafica_2 = foldr (\x y -> [(tail ( init (fecha x)), tmax x)]++y) []     

datos_grafica_3 :: [Meteorologia]->[(Int,Float)]
datos_grafica_3 = foldr (\x y -> [(ind x, tmin x)]++y) []        

datos_grafica_4 :: [Meteorologia]->[(Float)]
datos_grafica_4 = foldr (\x y -> [(tmin x)]++y) []        

datos_grafica_5 :: [Meteorologia]->[(Float)]
datos_grafica_5 = foldr (\x y -> [(tmax x)]++y) []  



-- ============================================================================================================
-- =========================================== FUNCIONES GENERALES ============================================
-- ============================================================================================================

-- La siguiente función recibe la lista de Meteorología y devuelve la mayor diferencias de temperaturas máxima 
-- y mínima recorriendo toda la lista. Usa la función auxiliar "maximo".

mayor_diferencia_temperaturas :: [Meteorologia] -> Float
mayor_diferencia_temperaturas ls = maximo $ [(tmax x) -(tmin x)| x <- ls]

-- ============================================================================================================

-- Las siguientes cuatro funciones reciben la lista de Meteorología y devuelven un dato Meteorología y su aquel 
-- es comparar las temperaturas con la siguiente para encontrar la mayor diferencia de estas. Las hemos creado
-- de manera recursivas con acumulador restringiendo en las dos primeras guardas para evitar los fallos del fi-
-- -chero.

mayor_ascenso_tmax ::[Meteorologia] -> Meteorologia
mayor_ascenso_tmax [] = error "Lista vacía"
mayor_ascenso_tmax [x] = x
mayor_ascenso_tmax xs = mayor_ascenso_tmax_acum xs (head xs) 0.0

mayor_ascenso_tmax_acum :: [Meteorologia] -> Meteorologia -> Float -> Meteorologia
mayor_ascenso_tmax_acum [] m n= error "Lista vacía"
mayor_ascenso_tmax_acum [x] m n = m
mayor_ascenso_tmax_acum (x:xs) m n
    | tmax x == 0.0 = mayor_ascenso_tmax_acum xs m n
    | tmax (head xs) == 0.0 = mayor_ascenso_tmax_acum (tail xs) m n
    | resta > n = mayor_ascenso_tmax_acum xs (head xs) resta
    | otherwise = mayor_ascenso_tmax_acum xs m n
        where
            resta= (tmax (head xs) - tmax x)

-- -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

mayor_ascenso_tmin ::[Meteorologia] -> Meteorologia
mayor_ascenso_tmin [] = error "Lista vacía"
mayor_ascenso_tmin [x] = x
mayor_ascenso_tmin xs = mayor_ascenso_tmin_acum xs (head xs) 0.0

mayor_ascenso_tmin_acum :: [Meteorologia] -> Meteorologia -> Float -> Meteorologia
mayor_ascenso_tmin_acum [] m n= error "Lista vacía"
mayor_ascenso_tmin_acum [x] m n = m
mayor_ascenso_tmin_acum (x:xs) m n
    | tmin x == 0.0 = mayor_ascenso_tmin_acum xs m n
    | tmin (head xs) == 0.0 = mayor_ascenso_tmin_acum (tail xs) m n
    | resta > n = mayor_ascenso_tmin_acum xs (head xs) resta
    | otherwise = mayor_ascenso_tmin_acum xs m n
        where
            resta= (tmin (head xs) - tmin x)

-- -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

mayor_descenso_tmax ::[Meteorologia] -> Meteorologia
mayor_descenso_tmax [] = error "Lista vacía"
mayor_descenso_tmax [x] = x
mayor_descenso_tmax xs = mayor_descenso_tmax_acum xs (head xs) 0.0

mayor_descenso_tmax_acum :: [Meteorologia] -> Meteorologia -> Float -> Meteorologia
mayor_descenso_tmax_acum [] m n= error "Lista vacía"
mayor_descenso_tmax_acum [x] m n = m
mayor_descenso_tmax_acum (x:xs) m n
    | tmax x == 0.0 = mayor_descenso_tmax_acum xs m n
    | tmax (head xs) == 0.0 = mayor_descenso_tmax_acum (tail xs) m n
    | resta < n = mayor_descenso_tmax_acum xs (head xs) resta
    | otherwise = mayor_descenso_tmax_acum xs m n
        where
            resta= (tmax (head xs) - tmax x)

-- -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

mayor_descenso_tmin ::[Meteorologia] -> Meteorologia
mayor_descenso_tmin [] = error "Lista vacía"
mayor_descenso_tmin [x] = x
mayor_descenso_tmin xs = mayor_descenso_tmin_acum xs (head xs) 0.0

mayor_descenso_tmin_acum :: [Meteorologia] -> Meteorologia -> Float -> Meteorologia
mayor_descenso_tmin_acum [] m n= error "Lista vacía"
mayor_descenso_tmin_acum [x] m n = m
mayor_descenso_tmin_acum (x:xs) m n
    | tmin x == 0.0 = mayor_descenso_tmin_acum xs m n
    | tmin (head xs) == 0.0 = mayor_descenso_tmin_acum (tail xs) m n
    | resta < n = mayor_descenso_tmin_acum xs (head xs) resta
    | otherwise = mayor_descenso_tmin_acum xs m n
        where
            resta= (tmin (head xs) - tmin x)

-- ============================================================================================================

-- Las siguiente funciones solo se encargan de buscar las temperaturas más altas y más bajas con la diferencia
-- entre ellas de que unas utilizan las funciones maximun y minimun de haskell y otras utilizan las funciones 
-- auxiliares "maximo" y "minimo".

-- -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

-- Temperatura Máxima más alta Registrada 
tmax_mas_alta :: [Meteorologia] -> Float
tmax_mas_alta ls = maximo $ [tmax x| x <- ls]

tmax_mas_alta_2 :: [Meteorologia] -> Float
tmax_mas_alta_2 ls = maximum [ tmax x | x <- ls ]

-- -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

-- Temperatura Máxima más baja Registrada
tmax_mas_baja :: [Meteorologia] -> Float
tmax_mas_baja ls = minimo $ [tmax x| x <- ls, tmax x /= 0.0 ]

tmax_mas_baja_2 :: [Meteorologia] -> Float
tmax_mas_baja_2 ls = minimum [ tmax x | x <- ls, tmax x /= 0.0 ]

-- -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

-- Temperatura Mínima más alta Registrada
tmin_mas_alta :: [Meteorologia] -> Float
tmin_mas_alta ls = maximo $ [tmin x| x <- ls]

tmin_mas_alta_2 :: [Meteorologia] -> Float
tmin_mas_alta_2 ls = maximum [ tmin x | x <- ls ]

-- -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

--Temperatura Mínima más baja Registrada
tmin_mas_baja :: [Meteorologia] -> Float
tmin_mas_baja ls = minimo $ [tmin x| x <- ls]

tmin_mas_baja_2 :: [Meteorologia] -> Float
tmin_mas_baja_2 ls = minimum [ tmin x | x <- ls ]

-- ============================================================================================================

-- Ahora tenemos unas funciones cuyo propósito es adquirir la fecha de las temperaturas máximas y mínimas más 
-- altas y bajas del año, por ello reciben la lista de Meteorología y devuelven la fecha tipo String. Para con-
-- -seguirlo obtenemos por comprensión la lista de fechas y gracias al índice adquirido con el uso de las func-
-- -iones anteriores podemos conseguir la fecha que buscamos.

fecha_tmax_alta_anual :: [Meteorologia] -> String
fecha_tmax_alta_anual ls = [ fecha x | x <- ls ] !! index
    where x = tmax_mas_alta_2 ls
          xs = [ tmax x | x <- ls ]
          index = fromMaybe (-1) $ elemIndex x xs

fecha_tmax_alta_anual_2 :: [Meteorologia] -> Fecha
fecha_tmax_alta_anual_2 xs = to_fecha $ fecha $ foldr (\x y -> if (tmax x) > (tmax y) 
    then x else y ) (head xs) xs

-- -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

fecha_tmax_baja_anual :: [Meteorologia] -> String
fecha_tmax_baja_anual ls = [ fecha x | x <- ls ] !! index
    where x = tmax_mas_baja_2 ls
          xs = [ tmax x | x <- ls ]
          index = fromMaybe (-1) $ elemIndex x xs

fecha_tmax_baja_anual_2 :: [Meteorologia] -> Fecha
fecha_tmax_baja_anual_2 xs = to_fecha $ fecha $ foldr (\x y -> if (tmax x) < (tmax y) && (tmax x) > 0.0 
    then x else y ) (head xs) xs

-- -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

fecha_tmin_alta_anual :: [Meteorologia] -> String
fecha_tmin_alta_anual ls = [ fecha x | x <- ls ] !! index
    where x = tmin_mas_alta_2 ls
          xs = [ tmin x | x <- ls ]
          index = fromMaybe (-1) $ elemIndex x xs

fecha_tmin_alta_anual_2 :: [Meteorologia] -> Fecha
fecha_tmin_alta_anual_2 xs = to_fecha $ fecha $ foldr (\x y -> if (tmin x) > (tmin y) && (tmin x) > 0.0 
    then x else y ) (head xs) xs

-- -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

fecha_tmin_baja_anual :: [Meteorologia] -> String
fecha_tmin_baja_anual ls = [ fecha x | x <- ls ] !! index
    where x = tmin_mas_baja_2 ls
          xs = [ tmin x | x <- ls ]
          index = fromMaybe (-1) $ elemIndex x xs

fecha_tmin_baja_anual_2 :: [Meteorologia] -> Fecha
fecha_tmin_baja_anual_2 xs = to_fecha $ fecha $ foldr (\x y -> if (tmin x) < (tmin y) && (tmin x) > 0.0 
    then x else y ) (head xs) xs

-- ============================================================================================================

-- La función filtra_fecha es una lista por comprensión que recibe la lista de Meteorología y el mes, devolvien-
-- -do otra lista de Meteorología pero solo con los datos del mes que hemos pedido.

filtra_fecha :: [Meteorologia] -> String -> [Meteorologia]
filtra_fecha ls m = [ x | x <- ls, (isSubsequenceOf m (fecha x)) == True ]

-- ============================================================================================================

-- A continuación tenemos dos funciones que se encargan de realizar la media de las temperaturas máxima y míni- 
-- -ma usando las funciones auxiliares, además tenemos que eliminar unos datos "wrong" ya que hay algunos días
-- de los que no hay información.

-- Por concretar se hace la suma de todas las temperatura y se divide entre el tamaño de la lista, en ambos ca-
-- -sos eliminando la información inexistente.

media_tmax :: [Meteorologia] -> Float
media_tmax ls = (suma_temp_max ls) / fromIntegral ((length ls) - wrong)
    where wrong = num_incompleta ls

media_tmin :: [Meteorologia] -> Float
media_tmin ls = (sum [ tmin x | x <- ls ]) / fromIntegral ((length ls) - wrong)
    where wrong = num_incompleta_2 ls

-- Número de registros incompletos
num_incompleta :: [Meteorologia] -> Int
num_incompleta ls = length [ x | x <- ls, tmax x == 0.0 ]

num_incompleta_2 :: [Meteorologia] -> Int
num_incompleta_2 ls = length [ x | x <- ls, tmin x == 0.0 ]

-- Suma de temperaturas
suma_temp_max :: [Meteorologia]-> Float
suma_temp_max ls = sum [ tmax x | x <- ls ]

suma_temp_max_2 :: [Meteorologia]-> Float
suma_temp_max_2 = foldr (\x y -> (tmax x) + y) 0.0

-- ============================================================================================================
-- ============================================= USO DEL Data.Map =============================================
-- ============================================================================================================

-- Aquí tenemos el intento fallido del uso de los Map que prentendía convertirse en un mejor acceso a los datos
-- gracias a la forma de los diccionarios pero no se ha conseguido. 

{-
myMap :: Integer -> Map Integer [Integer]
myMap n = Map.fromList (map makePair [1..n])
    where makePair x = (x, [x])

mapa1 :: [Meteorologia] -> Map Int Meteorologia
mapa1 xs = Map.fromList (map f xs)
    where f x =((ind x), x) 

mapa2 :: [Meteorologia] -> Maybe [Float] 
mapa2 xs = Map.lookup 34 (mapa1 xs)



mapa3 :: Map Int [Float] -> Map Int Meteorologia
mapa3 m xs = Map.filter este_e_bueno m


mapa4 :: [Meteorologia]->Map Int Meteorologia
mapa4 xs = mapa3 (mapa1 xs)

este_e_bueno :: Meteorologia -> Bool
este_e_bueno x = ( tmin x /=0.0 || tmax x /=0.0 )

-}

-- ============================================================================================================
-- =========================================== FUNCIONES AUXILIARES ===========================================
-- ============================================================================================================

-- -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.- Máximo de una lista de tipo generico -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-

maximo:: (Ord a) => [a] -> a
maximo [] = error "Lista vacía"
maximo [x] = x
maximo (x:xs)
    | x > f = x
    | otherwise = f
        where
            f= maximo xs

-- -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.- Mínimo de una lista de tipo generico -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-

minimo:: (Ord a) => [a] -> a
minimo [] = error "Lista vacía"
minimo [x] = x
minimo (x:xs)
    | x < f = x
    | otherwise = f
        where
            f= minimo xs

-- -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.--.- Corrector de dato tipo Float -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-

-- parsea_float utiliza "elem" para comprobar si contiene la coma, si es así con la función la cambiamos por un 
-- punto y el tail init elimina las comillas paa poder devolver un tipo Float. Para poder lidiar con los fallos 
-- del fichero devolvemos 0.0 si no hay ningun elemento. 

parsea_float :: String -> Float
parsea_float x 
    | elem ',' x = read $ f $ tail $ init x 
    | otherwise = 0.0
        where                      
            f = map (\c -> if c == ',' then '.' else c)




























