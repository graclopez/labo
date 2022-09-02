# 1.03 Construyendo un arbol
# Se construye un arbol de decisión, se ven distintas formas de pasar los parámetros y distintas formas de dibujarlo.
# Se muestran funcionalidades básicas de0la libreria  *data.table*


library( "data.table")   #cargo la libreria  data.table
library( "rpart")  #cargo la libreria  rpart

options(repr.plot.width=25, repr.plot.height=25)  #para que los gráficos me salgan legibles

setwd("C:\\Maestria\\DMEyF\\") #Aqui se debe poner la ruta de la PC local
dataset <- fread("./datasets/competencia1_2022.csv")

# Ahora entreno un arbol de decision
# "clase_ternaria ~ ." significa predecir clase_ternaria utilizando todo el resto de las variables del dataset

modelo <-  rpart(  formula= "clase_ternaria ~ ." ,
                   data= dataset[ foto_mes==202101] )

# Imprimo el modelo con la muy básica funcion print() de la libreria rpart
print( modelo)

# Esta impresión no es gráfica. No me sirve.
# a pesar que no me sirve, he encontrado una piedra en el camino, me está generando un arbol con un solo nodo, con solo la raiz
#
# Busco bibliografía y encuentro que existe la libreria rpart.plot que grafica arboles generados con la libreria rpart

library("rpart.plot")

rpart.plot::prp(modelo)

# Me ha salido una impresión del arbol, que es un solo nodo, pero solo dice continua.
# leo la documentacion de la librería rpart.plot   
# https://cran.r-project.org/web/packages/rpart.plot/rpart.plot.pdf

prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0, tweak=1.3)


# Ha salido solo la raiz del arbol
# Los tres numeros que muestra en el nodo con la cantidad de BAJA+1, BAJA+2 y CONTINUA, en ese orden, alfabetico.
# la cantidad de CONTINUA la está mostrando en notacion científica

# cambio hiperparámetros del arbol para salga algo mas que un solo nodo

# El hiperparámetro cp complexity limita el split de los nodos.
# El default es cp=0.05
# Pruebo con cp=0.0 a ver si "se abre el arbol"

# Leo la documentación de la libreria rpart https://cran.r-project.org/web/packages/rpart/rpart.pdf 
# y veo que existe un hiperparámetro de la funcion rpart llamado xval que es para hacer cross validation, 
# que por default viene seteado en xval=10 . 
# No me interesa en este momento que haga cross validation, para evitarlo voy a poner xval=0


#########################################################

#genero el modelo
modelo <-  rpart(  formula= "clase_ternaria ~ ." ,
                   data= dataset[ foto_mes==202101],
                   xval= 0,
                   cp= 0.0 )

#imprimo el modelo graficamente
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0, tweak=1.3)

#genero el modelo
modelo <-  rpart(  formula= "clase_ternaria ~ ." ,
                   data= dataset[ foto_mes==202101],
                   xval= 0,
                   cp= 0.0,
                   maxdepth= 2 )

#imprimo el modelo graficamente
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0, tweak=1.3, cex=1.2)

# Esperaba ver un albol de profundidad 2 sin embargo, por alguna misteriosa razón, se ha generado un arbol con un solo nodo.
# Corto por lo sano, y establezco cp=-1 para que siempre se abra el arbol

#genero el modelo
modelo <-  rpart(  formula= "clase_ternaria ~ ." ,
                   data= dataset[ foto_mes==202101],
                   xval= 0,
                   cp= -1,
                   maxdepth= 2 )

#imprimo el modelo graficamente
options(repr.plot.width=20, repr.plot.height=10) 
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0, tweak=1.1, cex=1.2)

# Disgresión : aprendo a borrar la memoria

# Listo los objetos que estan en la memoria de R en este momento
ls

# creo una variable a ver que sucede
a <-  1

# me vuelvo a fijar que objtetos estan en la memoria, deberia aparecer la nueva variable a
ls()

# tal cual esperaba, aparece el nuevo objeto, la variable a

# Me fijo cuanta memoria esta disponible
gc()

# borro TODOS los objetos que estan en la memoria de R
rm( list=ls())

# Me vuelvo a fijar cuanta memoria hay disponible
gc()

# llama a gc() garbaje collection, que me va a liberar mas aún la memoria
# https://cran.r-project.org/web/packages/profmem/vignettes/profmem.html
#
# Ahora si, limpie bore todos los objetos de R y limpie la memoria
