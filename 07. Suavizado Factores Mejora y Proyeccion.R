
#SUAVIZADO FACTORES DE MEJORA
#----------------------------

x=40:edad_max_modelo

#Suavizado de los factores de mejora para asegurar una transición suave y que decrecen para las edades avanzadas
FM_MEDIO_ESP=gam(FM_MEDIO_ESP_BRUTOS~s(x,k=5))$fitted

#Igualmente se suavizan los factores estresados
FM_0.995_ESP=gam(FM_0.995_ESP_BRUTOS~s(x,k=5))$fitted


#Proyección de los factores hasta los 120 años
#---------------------------------------------

#Selecciona el factor de mejora medio de las edades de 85 a 95 años, que siguen una tendencia lineal decreciente 
FM85_95=(FM_MEDIO_ESP[46:(edad_max_modelo-39)])

#y se le ajusta una línea recta con respecto a la edad
linea=lm(FM85_95~c(85:edad_max_modelo))

#Con la pendiente de esa línea se proyecta hasta los 120
y=linea$coeff[1]+linea$coeff[2]*((edad_max_modelo+1):proyeccion_modelo)

FM_MEDIO_ESP_120=c(FM_MEDIO_ESP,y)

#Para la proyección de los factores estresados se procede de la misma forma
FM_0.995_85_95=(FM_0.995_ESP[46:(edad_max_modelo-39)])

#y se ajusta una línea recta con respecto a la edad
linea_0.995=lm(FM_0.995_85_95~c(85:edad_max_modelo))

#Con la pendiente de esa línea se proyecta hasta los 120
y_0.995=linea_0.995$coeff[1]+linea_0.995$coeff[2]*((edad_max_modelo+1):proyeccion_modelo)

FM_0.995_ESP_120=c(FM_0.995_ESP,y_0.995)


