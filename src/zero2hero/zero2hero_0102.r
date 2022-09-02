library( "data.table")   #cargo la libreria  data.table

dataset <- fread( "C:\\Maestria\\DMEyF\\datasets\\competencia1_2022.csv")

# Cantidad de registros. 
# Cantidad de columnas

nrow( dataset )
ncol( dataset )

# Otra forma de ver la cantidad de registros del dataset, al estilo data table
# El .N es la cantidad de registros y va en la segunda poscion dataset[ 1, 2, 3 ]

dataset[ , .N ]

# Ahora hacemos la apertura por el periodo, el campo foto_mes
dataset[ , .N, foto_mes ]

# nombre de las columnas del dataset
colnames( dataset )

# Exploración de clase_ternaria.
dataset[  , .N, list( foto_mes, clase_ternaria) ]

# varias formas de contar los BAJA+2
# todas las formas dan el mismo resultado
nrow(  dataset[ clase_ternaria=="BAJA+2" ])

dataset[ clase_ternaria=="BAJA+2", .N ] #el autentico estilo data.table

dataset[  , sum(clase_ternaria=="BAJA+2")]

# Conteo de proporcion de BAJA+2 en el dataset
dataset[ foto_mes==202101  ,  sum(clase_ternaria=="BAJA+2")/.N]

# Conteo de la proporcion de BAJA+2 en un predicado
dataset[ foto_mes==202101 & ctrx_quarter < 20  ,  sum(clase_ternaria=="BAJA+2")/.N]

dataset[ foto_mes==202101 & ctrx_quarter < 20  ,  sum(clase_ternaria=="BAJA+2")/.N]  /dataset[ foto_mes==202101  ,  sum(clase_ternaria=="BAJA+2")/.N]

dataset[ foto_mes==202101 & ctrx_quarter < 20 , .N]

dataset[ foto_mes==202101  & clase_ternaria=="BAJA+2",  .N]

dataset[ foto_mes==202101  & clase_ternaria=="BAJA+2" & ctrx_quarter < 20,  .N]

dataset[ foto_mes==202101 , .N]

# Agregado de la columna ganancia al dataset
# Primero le asigno a TODOS los registros el valor de -2000
# la asignacion se hace con el :=

dataset[ foto_mes==202101, ganancia := -2000]

# y finalmente a los BAJA+2 les asigno 78000
dataset[ foto_mes==202101 & clase_ternaria=="BAJA+2", ganancia := 78000]

# Calculo la ganancia que tendria una campaña en donde envío estímulo a TODOS los clientes
dataset[ foto_mes==202101 , sum(ganancia)]

# Ganancias de predicados univariados
# Calculo la ganancia de un predicado simple ctrx_quarter < 20

dataset[ foto_mes==202101 & ctrx_quarter < 20,  sum( ganancia )  ]

# Ahora la ganancia de *ctrx_quarter < 4 
dataset[ foto_mes==202101 & ctrx_quarter < 4,  sum( ganancia )  ]

# Ahora, en forma brutal e ineficiente, busco donde esta el mejor corte de ctrx_quarter
# Ya resolveremos esto en forma inteligente más adelante
#
# cat() convierte sus argumentos a string, los concatena separandolos con el parametro sep= string y luego los imprime.
# separador default es el espacio

for(  transacciones  in   0:50)
{
   cat(  transacciones, dataset[  foto_mes==202101 & ctrx_quarter < transacciones,  sum( ganancia )  ] , "\n")    
}

dataset[  foto_mes==202101 & ctrx_quarter < 20 & mpasivos_margen < 29.8 ,  sum( ganancia )  ]

install.packages("ggplot2", dependencies = TRUE)
library("ggplot2") #cargo la libreria ggplot2

campo <- "cliente_antiguedad" 
ggplot(dataset[ foto_mes==202101] , aes_string(x = campo)) + geom_density(trim=TRUE, na.rm=TRUE) + facet_grid( "clase_ternaria~ .")

# los gráficos salen muy pequeños, busco la documentacion:
# https://blog.revolutionanalytics.com/2015/09/resizing-plots-in-the-r-kernel-for-jupyter-notebooks.html
# y agrando los graficos
options(repr.plot.width=15, repr.plot.height=15)

campo <- "cliente_antiguedad" 
ggplot(dataset[ foto_mes==202101], aes_string(x = campo)) + geom_density(trim=TRUE, na.rm=TRUE) + facet_grid( "clase_ternaria~ .")
