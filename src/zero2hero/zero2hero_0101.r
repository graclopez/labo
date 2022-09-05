setwd("C:\\Maestria\\DMEyF\\")  #Establezco el Working Directory

Sys.time()

t0  <- Sys.time()
dataset <- read.csv("./datasets/competencia1_2022.csv")
t1  <- Sys.time()
delta  <- as.numeric(  t1 - t0, units = "secs")  #calculo la diferencia de tiempos
print( delta) #imprimo

t0  <- Sys.time()
dataset <- read.csv("./datasets/competencia1_2022.csv")
t1  <- Sys.time()
delta  <- as.numeric(  t1 - t0, units = "secs")  #calculo la diferencia de tiempos
print(delta) #imprimo

library( "data.table")   #cargo la libreria  data.table

t0  <- Sys.time()
dataset <- fread("./datasets/competencia1_2022.csv")
t1  <- Sys.time()
delta  <- as.numeric(  t1 - t0, units = "secs")  #calculo la diferencia de tiempos
print(delta) #imprimo

t0  <- Sys.time()
dataset <- fread("./datasets/competencia1_2022.csv")
t1  <- Sys.time()
delta  <- as.numeric(  t1 - t0, units = "secs")  #calculo la diferencia de tiempos
print(delta) #imprimo
