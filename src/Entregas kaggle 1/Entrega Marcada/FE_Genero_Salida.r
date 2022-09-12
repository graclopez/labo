#Aplicacion de los mejores hiperparametros encontrados en una bayesiana
#Utilizando clase_binaria =  [  SI = { "BAJA+1", "BAJA+2"} ,  NO="CONTINUA ]

#cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")


#Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("C:\\Maestria\\dmeyf\\")  #Establezco el Working Directory

#cargo el dataset
dataset  <- fread("./datasets/competencia1_2022.csv" )


#creo la clase_binaria SI={ BAJA+1, BAJA+2 }    NO={ CONTINUA }
dataset[ foto_mes==202101, 
         clase_binaria :=  ifelse( clase_ternaria=="CONTINUA", "NO", "SI" ) ]


dtrain  <- dataset[ foto_mes==202101 ]  #defino donde voy a entrenar
dapply  <- dataset[ foto_mes==202103 ]  #defino donde voy a aplicar el modelo


#####################################################################
# FE
#####################################################################
dtrain[, cprestamos_personales := ifelse(is.na(cprestamos_personales), 0, cprestamos_personales)] 
dtrain[, cprestamos_prendarios := ifelse(is.na(cprestamos_prendarios), 0, cprestamos_prendarios)] 
dtrain[, cprestamos_hipotecarios := ifelse(is.na(cprestamos_hipotecarios),  0, cprestamos_hipotecarios)] 
dtrain[, suma_cprestamos := cprestamos_hipotecarios + cprestamos_prendarios + cprestamos_personales] 

dtrain[, mprestamos_personales := ifelse(is.na(mprestamos_personales), 0, mprestamos_personales)] 
dtrain[, mprestamos_prendarios := ifelse(is.na(mprestamos_prendarios), 0, mprestamos_prendarios)] 
dtrain[, mprestamos_hipotecarios := ifelse(is.na(mprestamos_hipotecarios), 0, mprestamos_hipotecarios)]
dtrain[, suma_mprestamos := mprestamos_hipotecarios + mprestamos_prendarios + mprestamos_personales] 

dtrain[, cpayroll_trx := ifelse(is.na(cpayroll_trx), 0, cpayroll_trx)] 
dtrain[, cpayroll2_trx := ifelse(is.na(cpayroll2_trx), 0, cpayroll2_trx)] 
dtrain[, suma_cpayroll := cpayroll_trx + cpayroll2_trx] 

dtrain[, mpayroll:= ifelse(is.na(mpayroll), 0, mpayroll)] 
dtrain[, mpayroll2 := ifelse(is.na(mpayroll2), 0, mpayroll2)] 
dtrain[, suma_mpayroll := mpayroll + mpayroll2] 

dtrain[, ccajeros_propios_descuentos := ifelse(is.na(ccajeros_propios_descuentos), 0, ccajeros_propios_descuentos)] 
dtrain[, ctarjeta_visa_descuentos := ifelse(is.na(ctarjeta_visa_descuentos), 0, ctarjeta_visa_descuentos)] 
dtrain[, ctarjeta_master_descuentos := ifelse(is.na(ctarjeta_master_descuentos), 0, ctarjeta_master_descuentos)] 
dtrain[, suma_cdescuentos := ccajeros_propios_descuentos + ctarjeta_visa_descuentos + ctarjeta_master_descuentos] 
                  
dtrain[, ccomisiones_mantenimiento := ifelse(is.na(ccomisiones_mantenimiento), 0, ccomisiones_mantenimiento)] 
dtrain[, ccomisiones_otras := ifelse(is.na(ccomisiones_otras), 0, ccomisiones_otras)] 
dtrain[, suma_ccomisiones := ccomisiones_otras + ccomisiones_otras] 

dtrain[, mcajeros_propios_descuentos := ifelse(is.na(mcajeros_propios_descuentos), 0, mcajeros_propios_descuentos)] 
dtrain[, mtarjeta_visa_descuentos := ifelse(is.na(mtarjeta_visa_descuentos), 0, mtarjeta_visa_descuentos)] 
dtrain[, mtarjeta_master_descuentos := ifelse(is.na(mtarjeta_master_descuentos), 0, mtarjeta_master_descuentos)] 
dtrain[, suma_mdescuentos := mcajeros_propios_descuentos + mtarjeta_visa_descuentos + mtarjeta_master_descuentos] 

dtrain[, mcomisiones_mantenimiento := ifelse(is.na(mcomisiones_mantenimiento), 0, mcomisiones_mantenimiento)] 
dtrain[, mcomisiones_otras := ifelse(is.na(mcomisiones_otras), 0, mcomisiones_otras)] 
dtrain[, suma_mcomisiones := mcomisiones_mantenimiento + mcomisiones_otras] 

dtrain[, mcuentas_saldo := ifelse(is.na(mcuentas_saldo), 0, mcuentas_saldo)] 
dtrain[, Master_mlimitecompra := ifelse(is.na(Master_mlimitecompra), 0, Master_mlimitecompra)] 
dtrain[, Visa_mlimitecompra := ifelse(is.na(Visa_mlimitecompra), 0, Visa_mlimitecompra)] 


## proporcion entre prestamos y haberes. Si haberes cero -> entre prestamos y saldo cuentas
dtrain[, p_prestamos := ifelse(suma_mpayroll==0, ifelse(mcuentas_saldo<=0,0,suma_mprestamos/mcuentas_saldo), suma_mprestamos/suma_mpayroll)] 

## proporcion entre limite compra visa  y haberes. Si cero -> entre limite y saldo cuentas
dtrain[, p_Visa_mlimitecompra := ifelse(suma_mpayroll==0, ifelse(mcuentas_saldo<=0,0,Visa_mlimitecompra/mcuentas_saldo), Visa_mlimitecompra/suma_mpayroll)] 

## proporcion entre limite compra mc  y haberes. Si cero -> entre limite y saldo cuentas
dtrain[, p_Master_mlimitecompra := ifelse(suma_mpayroll==0, ifelse(mcuentas_saldo<=0,0,Master_mlimitecompra/mcuentas_saldo), Master_mlimitecompra/suma_mpayroll)] 



dapply[, cprestamos_personales := ifelse(is.na(cprestamos_personales), 0, cprestamos_personales)] 
dapply[, cprestamos_prendarios := ifelse(is.na(cprestamos_prendarios), 0, cprestamos_prendarios)] 
dapply[, cprestamos_hipotecarios := ifelse(is.na(cprestamos_hipotecarios),  0, cprestamos_hipotecarios)] 
dapply[, suma_cprestamos := cprestamos_hipotecarios + cprestamos_prendarios + cprestamos_personales] 

dapply[, mprestamos_personales := ifelse(is.na(mprestamos_personales), 0, mprestamos_personales)] 
dapply[, mprestamos_prendarios := ifelse(is.na(mprestamos_prendarios), 0, mprestamos_prendarios)] 
dapply[, mprestamos_hipotecarios := ifelse(is.na(mprestamos_hipotecarios), 0, mprestamos_hipotecarios)]
dapply[, suma_mprestamos := mprestamos_hipotecarios + mprestamos_prendarios + mprestamos_personales] 

dapply[, cpayroll_trx := ifelse(is.na(cpayroll_trx), 0, cpayroll_trx)] 
dapply[, cpayroll2_trx := ifelse(is.na(cpayroll2_trx), 0, cpayroll2_trx)] 
dapply[, suma_cpayroll := cpayroll_trx + cpayroll2_trx] 

dapply[, mpayroll:= ifelse(is.na(mpayroll), 0, mpayroll)] 
dapply[, mpayroll2 := ifelse(is.na(mpayroll2), 0, mpayroll2)] 
dapply[, suma_mpayroll := mpayroll + mpayroll2] 

dapply[, ccajeros_propios_descuentos := ifelse(is.na(ccajeros_propios_descuentos), 0, ccajeros_propios_descuentos)] 
dapply[, ctarjeta_visa_descuentos := ifelse(is.na(ctarjeta_visa_descuentos), 0, ctarjeta_visa_descuentos)] 
dapply[, ctarjeta_master_descuentos := ifelse(is.na(ctarjeta_master_descuentos), 0, ctarjeta_master_descuentos)] 
dapply[, suma_cdescuentos := ccajeros_propios_descuentos + ctarjeta_visa_descuentos + ctarjeta_master_descuentos] 
                  
dapply[, ccomisiones_mantenimiento := ifelse(is.na(ccomisiones_mantenimiento), 0, ccomisiones_mantenimiento)] 
dapply[, ccomisiones_otras := ifelse(is.na(ccomisiones_otras), 0, ccomisiones_otras)] 
dapply[, suma_ccomisiones := ccomisiones_otras + ccomisiones_otras] 

dapply[, mcajeros_propios_descuentos := ifelse(is.na(mcajeros_propios_descuentos), 0, mcajeros_propios_descuentos)] 
dapply[, mtarjeta_visa_descuentos := ifelse(is.na(mtarjeta_visa_descuentos), 0, mtarjeta_visa_descuentos)] 
dapply[, mtarjeta_master_descuentos := ifelse(is.na(mtarjeta_master_descuentos), 0, mtarjeta_master_descuentos)] 
dapply[, suma_mdescuentos := mcajeros_propios_descuentos + mtarjeta_visa_descuentos + mtarjeta_master_descuentos] 

dapply[, mcomisiones_mantenimiento := ifelse(is.na(mcomisiones_mantenimiento), 0, mcomisiones_mantenimiento)] 
dapply[, mcomisiones_otras := ifelse(is.na(mcomisiones_otras), 0, mcomisiones_otras)] 
dapply[, suma_mcomisiones := mcomisiones_mantenimiento + mcomisiones_otras] 

dapply[, mcuentas_saldo := ifelse(is.na(mcuentas_saldo), 0, mcuentas_saldo)] 
dapply[, Master_mlimitecompra := ifelse(is.na(Master_mlimitecompra), 0, Master_mlimitecompra)] 
dapply[, Visa_mlimitecompra := ifelse(is.na(Visa_mlimitecompra), 0, Visa_mlimitecompra)] 


## proporcion entre prestamos y haberes. Si haberes cero -> entre prestamos y saldo cuentas
dapply[, p_prestamos := ifelse(suma_mpayroll==0, ifelse(mcuentas_saldo<=0,0,suma_mprestamos/mcuentas_saldo), suma_mprestamos/suma_mpayroll)] 

## proporcion entre limite compra visa  y haberes. Si cero -> entre limite y saldo cuentas
dapply[, p_Visa_mlimitecompra := ifelse(suma_mpayroll==0, ifelse(mcuentas_saldo<=0,0,Visa_mlimitecompra/mcuentas_saldo), Visa_mlimitecompra/suma_mpayroll)] 

## proporcion entre limite compra mc  y haberes. Si cero -> entre limite y saldo cuentas
dapply[, p_Master_mlimitecompra := ifelse(suma_mpayroll==0, ifelse(mcuentas_saldo<=0,0,Master_mlimitecompra/mcuentas_saldo), Master_mlimitecompra/suma_mpayroll)] 

dtrain[,c("cprestamos_personales","cprestamos_prendarios","cprestamos_hipotecarios",
                    "mprestamos_personales","mprestamos_prendarios","mprestamos_hipotecarios",
                    "cpayroll_trx","cpayroll2_trx",
                    "mpayroll","mpayroll2",
                    "ccajeros_propios_descuentos","ctarjeta_visa_descuentos","ctarjeta_master_descuentos",
                    "ccomisiones_mantenimiento","ccomisiones_otras",
                    "mcajeros_propios_descuentos","mtarjeta_visa_descuentos","mtarjeta_master_descuentos",
                    "mcomisiones_mantenimiento","mcomisiones_otras"):=NULL]

dapply[,c("cprestamos_personales","cprestamos_prendarios","cprestamos_hipotecarios",
                    "mprestamos_personales","mprestamos_prendarios","mprestamos_hipotecarios",
                    "cpayroll_trx","cpayroll2_trx",
                    "mpayroll","mpayroll2",
                    "ccajeros_propios_descuentos","ctarjeta_visa_descuentos","ctarjeta_master_descuentos",
                    "ccomisiones_mantenimiento","ccomisiones_otras",
                    "mcajeros_propios_descuentos","mtarjeta_visa_descuentos","mtarjeta_master_descuentos",
                    "mcomisiones_mantenimiento","mcomisiones_otras"):=NULL]

# Entreno el modelo
# obviamente rpart no puede ve  clase_ternaria para predecir  clase_binaria
#  #no utilizo Visa_mpagado ni  mcomisiones_mantenimiento por drifting

modelo  <- rpart(formula=   "clase_binaria ~ . -clase_ternaria",
                 data=      dtrain,  #los datos donde voy a entrenar
                 xval=         5,
                 cp=          -0.37,#  -0.89
                 minsplit=  1092,   # 621
                 minbucket=  278,   # 309
                 maxdepth=     8 )  #  12


#----------------------------------------------------------------------------
# habilitar esta seccion si el Fiscal General  Alejandro Bolaños  lo autoriza
#----------------------------------------------------------------------------

# corrijo manualmente el drifting de  Visa_fultimo_cierre
 dapply[ Visa_fultimo_cierre== 1, Visa_fultimo_cierre :=  4 ]
 dapply[ Visa_fultimo_cierre== 7, Visa_fultimo_cierre := 11 ]
 dapply[ Visa_fultimo_cierre==21, Visa_fultimo_cierre := 25 ]
 dapply[ Visa_fultimo_cierre==14, Visa_fultimo_cierre := 18 ]
 dapply[ Visa_fultimo_cierre==28, Visa_fultimo_cierre := 32 ]
 dapply[ Visa_fultimo_cierre==35, Visa_fultimo_cierre := 39 ]
 dapply[ Visa_fultimo_cierre> 39, Visa_fultimo_cierre := Visa_fultimo_cierre + 4 ]

# corrijo manualmente el drifting de  Visa_fultimo_cierre
 dapply[ Master_fultimo_cierre== 1, Master_fultimo_cierre :=  4 ]
 dapply[ Master_fultimo_cierre== 7, Master_fultimo_cierre := 11 ]
 dapply[ Master_fultimo_cierre==21, Master_fultimo_cierre := 25 ]
 dapply[ Master_fultimo_cierre==14, Master_fultimo_cierre := 18 ]
 dapply[ Master_fultimo_cierre==28, Master_fultimo_cierre := 32 ]
 dapply[ Master_fultimo_cierre==35, Master_fultimo_cierre := 39 ]
 dapply[ Master_fultimo_cierre> 39, Master_fultimo_cierre := Master_fultimo_cierre + 4 ]


#aplico el modelo a los datos nuevos
prediccion  <- predict( object=  modelo,
                        newdata= dapply,
                        type = "prob")

#prediccion es una matriz con DOS columnas, llamadas "NO", "SI"
#cada columna es el vector de probabilidades 

#agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dfinal  <- copy( dapply[ , list(numero_de_cliente) ] )
dfinal[ , prob_SI := prediccion[ , "SI"] ]


# por favor cambiar por una semilla propia
# que sino el Fiscal General va a impugnar la prediccion
set.seed(851159)  
dfinal[ , azar := runif( nrow(dapply) ) ]

# ordeno en forma descentente, y cuando coincide la probabilidad, al azar
setorder( dfinal, -prob_SI, azar )


#dir.create( "./exp/FEComp1/" )


for( corte  in  c( 7500, 8000, 8500, 9000, 9500, 10000, 10500, 11000 ) )
{
  #le envio a los  corte  mejores,  de mayor probabilidad de prob_SI
  dfinal[ , Predicted := 0L ]
  dfinal[ 1:corte , Predicted := 1L ]


  fwrite( dfinal[ , list(numero_de_cliente, Predicted) ], #solo los campos para Kaggle
           file= paste0( "./exp/FEComp1/FE_002_",  corte, ".csv"),
           sep=  "," )
}


