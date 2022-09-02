#1.04 Transformado (innecesariamente) las variables
#El objetivo de esta sección es analizar el efecto que tiene sobre el arbol de decision
#Variables Colineales
#Normalizacion de Variables
#Transformada logarítmica
#Outliers


rm( list=ls())
gc()

library( "data.table")   #cargo la libreria  data.table
library( "rpart")  #cargo la libreria  rpart
library( "rpart.plot")

options(repr.plot.width=20, repr.plot.height=10) 
setwd("D:\\gdrive\\UBA2022\\")  #Aqui se debe poner la ruta de la PC local

dataset <- fread("./datasets/competencia1_2022.csv")   #cargo el dataset

#genero el modelo
modelo <-  rpart(  formula= "clase_ternaria ~ ." ,
                   data= dataset[ foto_mes==202101],
                   xval= 0,
                   cp= -1,
                   maxdepth= 2 )

#imprimo el modelo graficamente
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0, tweak=1.1, cex=1.2)

#La variable mas importante que aparece es ctrx_quarter
#
#
#
#variables colineales
#agrego al dataset tres variables colineales con ctrx_quarter

dataset[ foto_mes==202101 , ctrx_quarter_dos    :=  2*ctrx_quarter ]
dataset[ foto_mes==202101 , ctrx_quarter_tres   :=  3*ctrx_quarter ]
dataset[ foto_mes==202101 , ctrx_quarter_cuatro :=  4*ctrx_quarter ]

#y vuelvo a correr el arbol de decision

#genero el modelo
modelo <-  rpart(  formula= "clase_ternaria ~ ." ,
                   data= dataset[ foto_mes==202101],
                   xval= 0,
                   cp= -1,
                   maxdepth= 2 )

#imprimo el modelo graficamente
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0, tweak=1.1, cex=1.2)


#
# SORPRENDENTE , el arbol de decision es inmune a las colinearidad de variables
# El arbol de decisión no ha cambiado. Su forma es exactamente igual, las cantidades en los nodos idéntica al arbol original.
#
#


################################
#
# Normalizacion de variables
##################################

# Analizo la variable ctrx_quarter

min( dataset[ foto_mes==202101 , ctrx_quarter] )

max( dataset[ foto_mes==202101, ctrx_quarter] )

boxplot(  dataset[ foto_mes==202101, ctrx_quarter])

hist( dataset[ foto_mes==202101, ctrx_quarter] )

plot( density( dataset[ foto_mes==202101, ctrx_quarter] ) )

# Normailzo ctrx_quarter

dataset[ foto_mes==202101, ctrx_quarter_normalizado := scale(ctrx_quarter)]

# confirmo que me quedó normalizada

plot( density( dataset[foto_mes==202101, ctrx_quarter_normalizado] ) )

# Confirmado, la variable está normalizada, ahora corremos nuevamente el arbol de decision

#genero el modelo
modelo <-  rpart(  formula= "clase_ternaria ~ ." ,
                   data= dataset[ foto_mes==202101],
                   xval= 0,
                   cp= -1,
                   maxdepth= 2 )

#imprimo el modelo graficamente
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0, tweak=1.1, cex=1.2)

# SORPRENDENTE , el arbol de decision es inmune a las normalizacion de variables
# El arbol de decisión no ha cambiado. Su forma es exactamente igual, las cantidades en los nodos idéntica al arbol original.
#


#################################
# Transformación logaritmica
#################################

# Primero vuelvo a cargar el dataset

dataset <- fread("./datasets/competencia1_2022.csv")   #cargo el dataset

# hago la transformación logarítmica

dataset[ foto_mes==202101 , ctrx_quarter_log :=log(ctrx_quarter+1)]  #sumo el uno porque no quiero infinitos

# Ahora veo el boxplot

boxplot(  dataset[foto_mes==202101 , ctrx_quarter_log])

plot( density( dataset[ foto_mes==202101, ctrx_quarter_log] ) )

# Finalmente, el arbol de decision

# Pero antes, ELIMINO del dataset la variable ctrx_quarter , para que solo juegue ctrx_quarter_log

dataset[ , ctrx_quarter := NULL ]

#genero el modelo
modelo <-  rpart(  formula= "clase_ternaria ~ ." ,
                   data= dataset[foto_mes==202101],
                   xval= 0,
                   cp= -1,
                   maxdepth= 2 )

#imprimo el modelo graficamente
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0, tweak=1.1, cex=1.2)

# SORPRENDENTE , el arbol de decision es inmune a la transformada logaritmica
# El arbol de decisión no ha cambiado. Su forma es exactamente igual, las cantidades en los nodos idéntica al arbol original.
# Por supuesto, el arbol original cortaba en ctrx_quarter < 14 y 
# ahora corta en ctrx_quarter < 2.673 porque obviamente alteré esa variable, 
# pero en realidad está cortando en el mismo punto.

##########################
# Outliers
#########################

# Ahora fabrico outliers y veo como se comporta el arbol

# Primero vuelvo a cargar el dataset

dataset <- fread("./datasets/competencia1_2022.csv")   #cargo el dataset

#el boxplot original

boxplot(  dataset[ foto_mes==202101 , ctrx_quarter])

#cuento cuantos registros hay con ctrx_quarter > 1500

dataset[ foto_mes==202101 & ctrx_quarter > 1500, .N]

## Ahora, a esos 12 valores los transformo en outliers extremos

dataset[ foto_mes==202101 & ctrx_quarter > 1500,  ctrx_quarter := ctrx_quarter * 1000]

#compruebo que sean outliers extremos

boxplot(  dataset[ foto_mes==202101 , ctrx_quarter])

# Finalmente, nuevamente arbol de decision

#genero el modelo
modelo <-  rpart(  formula= "clase_ternaria ~ ." ,
                   data= dataset[foto_mes==202101],
                   xval= 0,
                   cp= -1,
                   maxdepth= 2 )

#imprimo el modelo graficamente
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0, tweak=1.1, cex=1.2)

# SORPRENDENTE, el arbol de decision es inmune a los outliers

# El arbol de decisión no ha cambiado. Su forma es exactamente igual, las cantidades en los nodos idéntica al arbol original.


# Si usted considera que fueron muy conservador convertir en outliers a tan solo 12 registros a que sean outliers, 
# pruebe con  ctrx_quarter > 1000
