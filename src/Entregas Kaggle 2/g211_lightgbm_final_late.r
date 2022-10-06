# para correr el Google Cloud
#   8 vCPU
#  64 GB memoria RAM
# 256 GB espacio en disco

# son varios archivos, subirlos INTELIGENTEMENTE a Kaggle

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("lightgbm")


#defino los parametros de la corrida, en una lista, la variable global  PARAM
#  muy pronto esto se leera desde un archivo formato .yaml
PARAM <- list()
PARAM$experimento  <- "HT_LGBM_2_late"

PARAM$input$dataset       <- "./datasets/competencia2_2022.csv.gz"
PARAM$input$training      <- c( 202103 )
PARAM$input$future        <- c( 202105 )

PARAM$finalmodel$max_bin           <-     31
PARAM$finalmodel$learning_rate     <-     0.0056112702   #0.0142501265
PARAM$finalmodel$num_iterations    <-    1216  #615
PARAM$finalmodel$num_leaves        <-   82  #784
PARAM$finalmodel$min_data_in_leaf  <-   5923  #5628
PARAM$finalmodel$feature_fraction  <-    0.9621205704  #0.8382482539
PARAM$finalmodel$semilla           <- 851159

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa
# Poner la carpeta de la materia de SU computadora local
setwd("C:\\Maestria\\dmeyf\\")

# semillas
semillas <- c(851159,773567,807797,216617,324757)

#cargo el dataset donde voy a entrenar
dataset  <- fread(PARAM$input$dataset, stringsAsFactors= TRUE)


#--------------------------------------

#paso la clase a binaria que tome valores {0,1}  enteros
#set trabaja con la clase  POS = { BAJA+1, BAJA+2 } 
#esta estrategia es MUY importante
dataset[ , clase01 := ifelse( clase_ternaria %in%  c("BAJA+2","BAJA+1"), 1L, 0L) ]

########################################################################################################
# Experimiento BASICO. Misma FE que en competencia 1

dataset[, cprestamos_personales := ifelse(is.na(cprestamos_personales), 0, cprestamos_personales)] 
dataset[, cprestamos_prendarios := ifelse(is.na(cprestamos_prendarios), 0, cprestamos_prendarios)] 
dataset[, cprestamos_hipotecarios := ifelse(is.na(cprestamos_hipotecarios),  0, cprestamos_hipotecarios)] 
dataset[, suma_cprestamos := cprestamos_hipotecarios + cprestamos_prendarios + cprestamos_personales] 

dataset[, mprestamos_personales := ifelse(is.na(mprestamos_personales), 0, mprestamos_personales)] 
dataset[, mprestamos_prendarios := ifelse(is.na(mprestamos_prendarios), 0, mprestamos_prendarios)] 
dataset[, mprestamos_hipotecarios := ifelse(is.na(mprestamos_hipotecarios), 0, mprestamos_hipotecarios)]
dataset[, suma_mprestamos := mprestamos_hipotecarios + mprestamos_prendarios + mprestamos_personales] 

dataset[, cpayroll_trx := ifelse(is.na(cpayroll_trx), 0, cpayroll_trx)] 
dataset[, cpayroll2_trx := ifelse(is.na(cpayroll2_trx), 0, cpayroll2_trx)] 
dataset[, suma_cpayroll := cpayroll_trx + cpayroll2_trx] 

dataset[, mpayroll:= ifelse(is.na(mpayroll), 0, mpayroll)] 
dataset[, mpayroll2 := ifelse(is.na(mpayroll2), 0, mpayroll2)] 
dataset[, suma_mpayroll := mpayroll + mpayroll2] 

dataset[, ccajeros_propios_descuentos := ifelse(is.na(ccajeros_propios_descuentos), 0, ccajeros_propios_descuentos)] 
dataset[, ctarjeta_visa_descuentos := ifelse(is.na(ctarjeta_visa_descuentos), 0, ctarjeta_visa_descuentos)] 
dataset[, ctarjeta_master_descuentos := ifelse(is.na(ctarjeta_master_descuentos), 0, ctarjeta_master_descuentos)] 
dataset[, suma_cdescuentos := ccajeros_propios_descuentos + ctarjeta_visa_descuentos + ctarjeta_master_descuentos] 
                  
dataset[, ccomisiones_mantenimiento := ifelse(is.na(ccomisiones_mantenimiento), 0, ccomisiones_mantenimiento)] 
dataset[, ccomisiones_otras := ifelse(is.na(ccomisiones_otras), 0, ccomisiones_otras)] 
dataset[, suma_ccomisiones := ccomisiones_otras + ccomisiones_otras] 

dataset[, mcajeros_propios_descuentos := ifelse(is.na(mcajeros_propios_descuentos), 0, mcajeros_propios_descuentos)] 
dataset[, mtarjeta_visa_descuentos := ifelse(is.na(mtarjeta_visa_descuentos), 0, mtarjeta_visa_descuentos)] 
dataset[, mtarjeta_master_descuentos := ifelse(is.na(mtarjeta_master_descuentos), 0, mtarjeta_master_descuentos)] 
dataset[, suma_mdescuentos := mcajeros_propios_descuentos + mtarjeta_visa_descuentos + mtarjeta_master_descuentos] 

dataset[, mcomisiones_mantenimiento := ifelse(is.na(mcomisiones_mantenimiento), 0, mcomisiones_mantenimiento)] 
dataset[, mcomisiones_otras := ifelse(is.na(mcomisiones_otras), 0, mcomisiones_otras)] 
dataset[, suma_mcomisiones := mcomisiones_mantenimiento + mcomisiones_otras] 

dataset[, mcuentas_saldo := ifelse(is.na(mcuentas_saldo), 0, mcuentas_saldo)] 
dataset[, Master_mlimitecompra := ifelse(is.na(Master_mlimitecompra), 0, Master_mlimitecompra)] 
dataset[, Visa_mlimitecompra := ifelse(is.na(Visa_mlimitecompra), 0, Visa_mlimitecompra)] 


## proporcion entre prestamos y haberes. Si haberes cero -> entre prestamos y saldo cuentas
dataset[, p_prestamos := ifelse(suma_mpayroll==0, ifelse(mcuentas_saldo<=0,0,suma_mprestamos/mcuentas_saldo), suma_mprestamos/suma_mpayroll)] 

## proporcion entre limite compra visa  y haberes. Si cero -> entre limite y saldo cuentas
dataset[, p_Visa_mlimitecompra := ifelse(suma_mpayroll==0, ifelse(mcuentas_saldo<=0,0,Visa_mlimitecompra/mcuentas_saldo), Visa_mlimitecompra/suma_mpayroll)] 

## proporcion entre limite compra mc  y haberes. Si cero -> entre limite y saldo cuentas
dataset[, p_Master_mlimitecompra := ifelse(suma_mpayroll==0, ifelse(mcuentas_saldo<=0,0,Master_mlimitecompra/mcuentas_saldo), Master_mlimitecompra/suma_mpayroll)] 


# le agrego Datadrifing comp 1 ajustado a comp 2
dataset[foto_mes==202105 & Visa_fultimo_cierre== 5, Visa_fultimo_cierre :=  1 ]
 dataset[foto_mes==202105 & Visa_fultimo_cierre > 5, Visa_fultimo_cierre := Visa_fultimo_cierre - 5 ]

 dataset[ foto_mes==202105 & Master_fultimo_cierre== 5, Master_fultimo_cierre :=  1 ]
 dataset[ foto_mes==202105 & Master_fultimo_cierre > 5, Master_fultimo_cierre := Master_fultimo_cierre -5]

saco_variables <- c("cprestamos_personales","cprestamos_prendarios","cprestamos_hipotecarios",
                    "mprestamos_personales","mprestamos_prendarios","mprestamos_hipotecarios",
                    "cpayroll_trx","cpayroll2_trx",
                    "mpayroll","mpayroll2",
                    "ccajeros_propios_descuentos","ctarjeta_visa_descuentos","ctarjeta_master_descuentos",
                    "ccomisiones_mantenimiento","ccomisiones_otras",
                    "mcajeros_propios_descuentos","mtarjeta_visa_descuentos","mtarjeta_master_descuentos",
                    "mcomisiones_mantenimiento","mcomisiones_otras",
                    "clase_ternaria","clase01")


#--------------------------------------

#los campos que se van a utilizar
#campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01") )
campos_buenos  <- setdiff( colnames(dataset), saco_variables )

#--------------------------------------


#establezco donde entreno
dataset[ , train  := 0L ]
dataset[ foto_mes %in% PARAM$input$training, train  := 1L ]

#--------------------------------------
#creo las carpetas donde van los resultados
#creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( paste0("./exp/", PARAM$experimento, "/" ), showWarnings = FALSE )
setwd( paste0("./exp/", PARAM$experimento, "/" ) )   #Establezco el Working Directory DEL EXPERIMENTO



#dejo los datos en el formato que necesita LightGBM
dtrain  <- lgb.Dataset( data= data.matrix(  dataset[ train==1L, campos_buenos, with=FALSE]),
                        label= dataset[ train==1L, clase01] )

#genero el modelo
#estos hiperparametros  salieron de una laaarga Optmizacion Bayesiana
modelo  <- lgb.train( data= dtrain,
                      param= list( objective=          "binary",
                                   max_bin=            PARAM$finalmodel$max_bin,
                                   learning_rate=      PARAM$finalmodel$learning_rate,
                                   num_iterations=     PARAM$finalmodel$num_iterations,
                                   num_leaves=         PARAM$finalmodel$num_leaves,
                                   min_data_in_leaf=   PARAM$finalmodel$min_data_in_leaf,
                                   feature_fraction=   PARAM$finalmodel$feature_fraction,
                                   seed=               PARAM$finalmodel$semilla
                                  )
                    )

#--------------------------------------
#ahora imprimo la importancia de variables
tb_importancia  <-  as.data.table( lgb.importance(modelo) ) 
archivo_importancia  <- "impo.txt"

fwrite( tb_importancia, 
        file= archivo_importancia, 
        sep= "\t" )

#--------------------------------------


#aplico el modelo a los datos sin clase
dapply  <- dataset[ foto_mes== PARAM$input$future ]

#aplico el modelo a los datos nuevos
prediccion  <- predict( modelo, 
                        data.matrix( dapply[, campos_buenos, with=FALSE ])                                 )

#genero la tabla de entrega
tb_entrega  <-  dapply[ , list( numero_de_cliente, foto_mes ) ]
tb_entrega[  , prob := prediccion ]

#grabo las probabilidad del modelo
fwrite( tb_entrega,
        file= "prediccion.txt",
        sep= "\t" )

#ordeno por probabilidad descendente
setorder( tb_entrega, -prob )


#genero archivos con los  "envios" mejores
#deben subirse "inteligentemente" a Kaggle para no malgastar submits
cortes <- seq( 5000, 12000, by=500 )
for( envios  in  cortes )
{
  tb_entrega[  , Predicted := 0L ]
  tb_entrega[ 1:envios, Predicted := 1L ]

  fwrite( tb_entrega[ , list(numero_de_cliente, Predicted)], 
          file= paste0(  PARAM$experimento, "_", envios, ".csv" ),
          sep= "," )
}

#--------------------------------------

quit( save= "no" )

