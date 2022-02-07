import Text.CSV
import Funciones
import Graphics.EasyPlot


-- Instalar GNUplot y EasyPlot
-- Para la ventana ejecutable, cambiar el archivo gnuplot.exe por 

-- ============================================================================================================
-- ===================================== INSTALLING GNUPLOT AND EASYPLOT ======================================
-- ============================================================================================================

-- Download and install from http://www.gnuplot.info/
-- Then you needed to use "cabal install gnuplot"

-- For 8.10.2, putting: "cabal new-install easyplot" in the command line
-- For 8.6.5, you needed to use the older "cabal install easyplot".



-- ============================================================================================================
-- ===================================== POSIBLE ERROR CON LA LIBRERIA GNUPLOT ================================
-- ============================================================================================================
-- ERROR: ghc.exe: pgnuplot: readCreateProcessWithExitCode: does not exist (No such file or directory)
-- which can be solved by
-- 1. allowing gnuplot to run from the console after setting the PATH variable, and
-- 2. making a copy of wgnuplot_pipes.exe in the gnuplot/bin directory and renaming it to pgnuplot.exe
--    These files are located here:     C:\Program Files\gnuplot\bin




-- ============================================================================================================
-- ===================================== LECTURA Y FORMATEO DE LOS DATOS ======================================
-- ============================================================================================================

-- Función principal para el programa.

figura :: IO ()
figura = do

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
      
    if null filasValidas then
        putStrLn "El fichero no es un CSV valido o carece de contenido"
    else do

    -- En este momento del main es donde podemos declarar qué función vamos a querer lanzar.
        
        -- Lista de Meteorologías
        let ls = transforma_datos filasValidas

        -- Valores que nos interesa (temperaturas mínimas y máximas)                                
        let minimos = datos_grafica_4 ls
        let minData = zip [1..] minimos

        let maximos = datos_grafica_5 ls
        let maxData = zip [1..] maximos   

        -- Generar un PNG con la gráfica
        putStrLn "\nSe va a proceder a guardar la gráfica."
        putStrLn "Introduzca el nombre del fichero: "
        nameGraph <- getLine

        plot (PNG (nameGraph++".png")) [ Data2D [Title "Temperaturas min", Color Blue] [] (minData)
         , Data2D [Title "Temperaturas max", Color Red] [] (maxData) ]

        -- Si queremos una ventana desplegable
        --plot' [Interactive] Windows [ Data2D [Title "Temperaturas max", Color Red] [] (maxData)
        -- , Data2D [Title "Temperaturas min", Color Blue] [] (minData) ]
    

        putStr "Se ha gurdado la gráfica en "
        putStrLn (nameGraph++".png")


    putStr "\nFinalizado la carga de "
    putStrLn fileName
    
