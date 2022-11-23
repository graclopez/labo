print("*")

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

setwd( "~/buckets/b1/" )  #cambiar por la carpeta local

#leo el dataset original
# pero podria leer cualquiera que tenga Feature Engineering
#dataset  <- fread( "./datasets/competencia3_2022.csv.gz", stringsAsFactors= TRUE)
#dataset  <- fread( "./exp/FINAL_02_FEH_RF_3M_Can_15/dataset.csv.gz", stringsAsFactors= TRUE)

#print(ncol(dataset))



dataset  <- fread( "./datasets/competenciaFINAL_2022.csv.gz", stringsAsFactors= TRUE)

print("**")
dataset[foto_mes==202101 & clase_ternaria=="BAJA+2"  , .N]
