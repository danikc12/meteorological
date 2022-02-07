# meteorological

===========================================================================

	MINI PROYECTO PARA PROGRAMACIÓN DECLARATIVA
	Programación Declarativa. Curso 2021/2022.

	Proyecto realizado por los alumnos:
		- Barragán Franco, Juan Rafael
		- Koh Chaves, Daniel

===========================================================================

1. Detalles técnicos y requisitos

	1.1 - Detalles técnicos:

	Aplicación desarrollada con Haskell 8.6.5, usando las librerías externas:
		- Text.CSV
		- GNUplot
		- Easyplot
		- Control.Monad

	Pruebas y desarrollo realizado en Visual Studio (versión 1.63.2). 


	1.2 - Requisitos:

	Software compatible con equipos con sistema operativo Windows 10,
	con arquitectura de 64 bits.

	OBLIGATORIO: en el directorio raíz de la aplicación debe encontrarse
	los siguientes archivos:

	- load.hs
	- Funciones.hs
	- Grafica.hs
	- ejemplos

	ejemplos es un directorio donde se encuentran los datasets en CSV.
	El formato del nombre es "Sevilla-aeropuerto-2021.csv"
	Fechas disponibles, 2018, 2019, 2020 y 2021.


===========================================================================

2. Cómo usar la aplicación principal

	2.1 - Abrir Visual Studio:

	Cargar el directorio con todos los archivos

	2.2 - Seleccionar load.hs:

	Una vez seleccionado load.hs, pulsar en Load GHCi

	2.3 - Ejecutar la función main:

	En el terminal escribir "main" y pulsar enter.
	Nos pedirá darle un nombre al archivo donde se guardarán 
	los resultados

	2.4 - Ejecutar funciones:

	Tras haber cargado correctamente el archivo y ejecutado el main, nos 
	pedirá introducir un número entre 1 y 16, que corresponden a 
	funciones diferentes.

	2.5 - Cerrar el programa:

	La aplicación se puede cerrar tras dar por finalizadas las operaciones
	que deseemos pulsando enter o directamente cerrando Visual Studio.
	

===========================================================================

3. Cómo usar la aplicación de gráficas

	3.1 - Abrir Visual Studio:

	Cargar el directorio con todos los archivos

	3.2 - Seleccionar Grafica.hs:

	Una vez seleccionado correctamente Grafica.hs, pulsar en Load GHCi

	3.3 - Ejecutar la función figura:

	En el terminal escribir "figura" y pulsar enter.
	Nos pedirá darle un nombre al archivo donde se guardarán 
	los resultados	

	3.4 - Cerrar el programa:

	La aplicación ha generado la imagen y se ha guardado con el nombre
	que hemos introducido. Para terminar pulsando enter o directamente 
	cerrando Visual Studio.
	
	
===========================================================================

4. Obtener más datos

	4.1 - Descargar datos de AEMET
	Hemos utilizado el dataset de la categoría “climatologías diarias”, 
	de la estación de Sevilla - Aeropuerto, obtenidos de la página oficial de 
	OpenData de la AEMET. 
	https://opendata.aemet.es/centrodedescargas/productosAEMET

	4.2 - Usamos JSONaCSV.ipynb
	Ejecutamos este archivo con Jupyter Notebook que usa Python y su librería
	Pandas para filtrar los datos para obtener sólo los datos que vamos a usar.
	Solo hay que modificar los nombres de los archivos que vamos a usar.
