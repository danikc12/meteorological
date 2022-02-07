
-- PD 2021-22. Análisis de Datos Meteorológicos de Sevilla
-- Juan Rafael Barragán y Daniel Koh
-- Universidad de Sevilla

-- =====================================================================

-- ---------------------------------------------------------------------
-- --------------------------- Introducción ----------------------------
-- ---------------------------------------------------------------------

-- Cargar datos desde los JSON descargados del portal de datos de AEMET

-- ---------------------------------------------------------------------
-- ----------------------- Librerías auxiliares ------------------------
-- ---------------------------------------------------------------------

import Text.CSV
import Funciones
import Control.Monad

-- Función principal para el programa.

main :: IO ()
main = do

    let fileName = "ejemplos/Sevilla-aeropuerto-2021.csv"
    -- Leemos:
    input <- readFile fileName

    -- Parseamos:
    let csv = parseCSV fileName input
        filas = case csv of
            (Right lineas) -> lineas
            (Left err) -> []
        filasValidas = filter (\x -> length x == 6) filas
        (cabecera,registros) = case filasValidas of
            [] -> ([],[])
            (cab:regs) -> (cab,regs)

    --putStrLn $ muestra_datos $ transforma_datos_aux xs

    if null filasValidas then
        putStrLn "El fichero no es un CSV valido o carece de contenido"
    else do
        putStrLn "¿Cual quiere que sea el nombre del archivo?"
        nombre <- getLine
        writeFile (nombre++".txt") "-.-.-.-.-.-.-.-.-.-.-.- Fichero nuevo -.-.-.-.-.-.-.-.-.-.-.-\n"

    -- En este momento del main es donde podemos declarar qué función vamos a querer lanzar.
        
        putStrLn "t=1 => Mayor diferencia de temperaturas"
        putStrLn "t=2 => Mayor ascenso de Temperatura Maxima"
        putStrLn "t=3 => Mayor ascenso de Temperatura Minima"
        putStrLn "t=4 => Mayor descenso de Temperatura Maxima"
        putStrLn "t=5 => Mayor descenso de Temperatura Minima"
        putStrLn "t=6 => Temperatura Maxima mas alta"
        putStrLn "t=7 => Temperatura Maxima mas baja"
        putStrLn "t=8 => Temperatura Minima mas alta"
        putStrLn "t=9 => Temperatura Minima mas baja"
        putStrLn "t=10 => Fecha de la Temperatura Maxima mas alta "  
        putStrLn "t=11 => Fecha de la Temperatura Maxima mas baja"
        putStrLn "t=12 => Fecha de la Temperatura Minima mas alta"
        putStrLn "t=13 => Fecha de la Temperatura Minima mas baja"
        putStrLn "t=14 => Filtrar por fecha"
        putStrLn "t=15 => Media Temperatura Maxima"
        putStrLn "t=16 => Media Temperatura Minima\n"

        forever $ do
        putStrLn "¿Que funcion quiere ejecutar?"
        numero <- getLine

        let ls = transforma_datos filasValidas

        let mes = ""
        let num =(read numero :: Int)
        if (1<=num && num <=16) then do
            if num == 14 then do
                putStrLn "Introduzca el mes en dos digitos"
                entrada <- getLine 
                let num_mes =(read entrada :: Int)
                if (1<=num_mes && num_mes <=12) then do
                    let mes = "-" ++ entrada ++ "-"
                    
                    appendFile (nombre++".txt") ("La salida para la ejecucion de la funcion numero "++ numero ++ " y mes: "++entrada++" ha sido:\n")  
                    appendFile (nombre++".txt") ( (llamada_facil ls num mes) ++"\n\n")
                    --putStrLn $ llamada_facil ls (read numero :: Int) mes
                    putStrLn ""
                else do
                    putStrLn "\nMes no válido"
            else do
                appendFile (nombre++".txt") ("La salida para la ejecucion de la funcion numero "++ numero ++ " ha sido:\n")
                appendFile (nombre++".txt") ((llamada_facil ls num mes) ++"\n\n")
                --putStrLn $ llamada_facil ls (read numero :: Int) mes
                putStrLn ""
            putStrLn ""
        else putStrLn "\nNumero de ejecución no valido"

        putStrLn "Se ha terminado la ejecución.\n"

