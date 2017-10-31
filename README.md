# Factores-Mejora-Generacional
Modelización de la mortalidad nacional y cálculo de factores

  * Datos.zip
    Contiene los datos del INE para la construcción de los factores de mejora generacionales en el ámbito nacional.
    
  * Factores de mejora - Guia R.pdf
    Guía donde se datalla cada script para la construcción de los factores de mejora generacionales.
    
  * Factores_mejora.pdf
    Metodología para la construcción de factores de mejora generacionales mediante los modelos Lee-Carter, CBD y Pspline.
    
  * Factores Mejora Generacionales.R
    Programa principal para la construcción de factores de mejora generacionales. Este se desglosa en:
    
        * Script:
             * 01. Carga de Datos.R
             * 02. Ajuste LeeCarter 40 a 89.R
             * 03. Ajuste CBD 40 a 89.R
             * 04. Ajuste Pspline 40 a 89.R
             * 05. Ajuste LeeCarter 90 a 95.R
             * 06. Calculo Factores Mejora.R
             * 07. Suavizado Factores Mejora y Proyeccion.R
        
        * Funciones auxiliares:
             * my.predict.R
