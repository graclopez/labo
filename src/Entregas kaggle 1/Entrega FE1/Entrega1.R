##
## Sobre Campos
##
## ---------------------------
## Step 1: Cargando los datos y las librerías
## ---------------------------
##
## Genius is one percent inspiration and 99 percent perspiration
## --- ~~Thomas Edison~~ Kate Sanborn

# Limpiamos el entorno
rm(list = ls())
gc(verbose = FALSE)

# Librerías necesarias
require("data.table")
require("rpart")
require("ggplot2")
require("dplyr")

# Poner la carpeta de la materia de SU computadora local
setwd("C:\\Maestria\\dmeyf\\")
# Poner sus semillas
semillas <- c(851159,773567,807797,216617,324757)


# Cargamos el dataset
dataset <- fread("./datasets/competencia1_2022.csv")

# Nos quedamos solo con el 202101
dataset <- dataset[foto_mes == 202101]

# Creamos una clase binaria
dataset[, clase_binaria := ifelse(
                            clase_ternaria == "BAJA+2",
                                "evento",
                                "noevento"
                            )]
# Borramos el target viejo
dataset[, clase_ternaria := NULL]


set.seed(semillas[1])

# Particionamos de forma estratificada
in_training <- caret::createDataPartition(dataset$clase_binaria,
                     p = 0.70, list = FALSE)
dtrain  <-  dataset[in_training, ]
dtest   <-  dataset[-in_training, ]

calcular_ganancia <- function(modelo, test) {
    pred_testing <- predict(modelo, test, type = "prob")
    sum(
        (pred_testing[, "evento"] >= 0.025) * ifelse(test$clase_binaria == "evento",
                                         78000, -2000) / 0.3
    )
}

## ---------------------------
## Step 2: Importancia de variables
## ---------------------------

# Antes de empezar vamos a ver la importancia de variables
modelo <- rpart(clase_binaria ~ .,
                data = dtrain,
                xval = 0,
                cp = -1,
                minsplit = 20,
                minbucket = 10,
                maxdepth = 5)

calcular_ganancia(modelo, dtest)

print(modelo$variable.importance)

########################################################
# 
# Lleno con cero las variables nulas para luego sumar
#
########################################################
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
dtrain[, p_prestamos := ifelse(suma_mpayroll==0, ifelse(mcuentas_saldo==0,0,suma_mprestamos/mcuentas_saldo), suma_mprestamos/suma_mpayroll)] 

## proporcion entre limite compra visa  y haberes. Si cero -> entre limite y saldo cuentas
dtrain[, p_Visa_mlimitecompra := ifelse(suma_mpayroll==0, ifelse(mcuentas_saldo==0,0,Visa_mlimitecompra/mcuentas_saldo), Visa_mlimitecompra/suma_mpayroll)] 

## proporcion entre limite compra mc  y haberes. Si cero -> entre limite y saldo cuentas
dtrain[, p_Master_mlimitecompra := ifelse(suma_mpayroll==0, ifelse(mcuentas_saldo==0,0,Master_mlimitecompra/mcuentas_saldo), Master_mlimitecompra/suma_mpayroll)] 


## proporcion entre comisiones y descuentos


dtest[, cprestamos_personales := ifelse(is.na(cprestamos_personales), 0, cprestamos_personales)] 
dtest[, cprestamos_prendarios := ifelse(is.na(cprestamos_prendarios), 0, cprestamos_prendarios)] 
dtest[, cprestamos_hipotecarios := ifelse(is.na(cprestamos_hipotecarios), 0, cprestamos_hipotecarios)] 
dtest[, suma_cprestamos := cprestamos_hipotecarios + cprestamos_prendarios + cprestamos_personales] 

dtest[, mprestamos_personales := ifelse(is.na(mprestamos_personales), 0, mprestamos_personales)] 
dtest[, mprestamos_prendarios := ifelse(is.na(mprestamos_prendarios), 0, mprestamos_prendarios)] 
dtest[, mprestamos_hipotecarios := ifelse(is.na(mprestamos_hipotecarios), 0, mprestamos_hipotecarios)] 
dtest[, suma_mprestamos := mprestamos_hipotecarios + mprestamos_prendarios + mprestamos_personales] 

dtest[, cpayroll_trx := ifelse(is.na(cpayroll_trx), 0, cpayroll_trx)] 
dtest[, cpayroll2_trx := ifelse(is.na(cpayroll2_trx), 0, cpayroll2_trx)] 
dtest[, suma_cpayroll := cpayroll_trx + cpayroll2_trx] 

dtest[, mpayroll := ifelse(is.na(mpayroll), 0, mpayroll)] 
dtest[, mpayroll2 := ifelse(is.na(mpayroll2), 0, mpayroll2)] 
dtest[, suma_mpayroll := mpayroll + mpayroll2]

dtest[, ccajeros_propios_descuentos := ifelse(is.na(ccajeros_propios_descuentos), 0, ccajeros_propios_descuentos)] 
dtest[, ctarjeta_visa_descuentos := ifelse(is.na(ctarjeta_visa_descuentos), 0, ctarjeta_visa_descuentos)] 
dtest[, ctarjeta_master_descuentos := ifelse(is.na(ctarjeta_master_descuentos), 0, ctarjeta_master_descuentos)] 
dtest[, suma_cdescuentos := ccajeros_propios_descuentos + ctarjeta_visa_descuentos + ctarjeta_master_descuentos] 

dtest[, ccomisiones_mantenimiento := ifelse(is.na(ccomisiones_mantenimiento), 0, ccomisiones_mantenimiento)] 
dtest[, ccomisiones_otras := ifelse(is.na(ccomisiones_otras), 0, ccomisiones_otras)] 
dtest[, suma_ccomisiones := ccomisiones_otras + ccomisiones_otras] 

dtest[, mcajeros_propios_descuentos := ifelse(is.na(mcajeros_propios_descuentos), 0, mcajeros_propios_descuentos)] 
dtest[, mtarjeta_visa_descuentos := ifelse(is.na(mtarjeta_visa_descuentos), 0, mtarjeta_visa_descuentos)] 
dtest[, mtarjeta_master_descuentos := ifelse(is.na(mtarjeta_master_descuentos), 0, mtarjeta_master_descuentos)] 
dtest[, suma_mdescuentos := mcajeros_propios_descuentos + mtarjeta_visa_descuentos + mtarjeta_master_descuentos] 

dtest[, mcomisiones_mantenimiento := ifelse(is.na(mcomisiones_mantenimiento), 0, mcomisiones_mantenimiento)] 
dtest[, mcomisiones_otras := ifelse(is.na(mcomisiones_otras), 0, mcomisiones_otras)] 
dtest[, suma_mcomisiones := mcomisiones_mantenimiento + mcomisiones_otras] 

dtest[, mcuentas_saldo := ifelse(is.na(mcuentas_saldo), 0, mcuentas_saldo)] 
dtest[, Master_mlimitecompra := ifelse(is.na(Master_mlimitecompra), 0, Master_mlimitecompra)] 
dtest[, Visa_mlimitecompra := ifelse(is.na(Visa_mlimitecompra), 0, Visa_mlimitecompra)] 


## proporcion entre prestamos y haberes. Si haberes cero -> entre prestamos y saldo cuentas
dtest[, p_prestamos := ifelse(suma_mpayroll==0, ifelse(mcuentas_saldo==0,0,suma_mprestamos/mcuentas_saldo), suma_mprestamos/suma_mpayroll)] 

## proporcion entre limite compra visa  y haberes. Si cero -> entre limite y saldo cuentas
dtest[, p_Visa_mlimitecompra := ifelse(suma_mpayroll==0, ifelse(mcuentas_saldo==0,0,Visa_mlimitecompra/mcuentas_saldo), Visa_mlimitecompra/suma_mpayroll)] 

## proporcion entre limite compra mc  y haberes. Si cero -> entre limite y saldo cuentas
dtest[, p_Master_mlimitecompra := ifelse(suma_mpayroll==0, ifelse(mcuentas_saldo==0,0,Master_mlimitecompra/mcuentas_saldo), Master_mlimitecompra/suma_mpayroll)] 


modelo2 <- rpart(clase_binaria ~ .,
                data = dtrain,
                xval = 0,
                cp = -1,
                minsplit = 20,
                minbucket = 10,
                maxdepth = 5)

calcular_ganancia(modelo2, dtest)

#print(modelo2$variable.importance)
#summary(dtrain$suma_mpayroll)
#View(dtrain[r_suma_mprestamos==10])
#ggplot(dtrain, aes(x = suma_mpayroll)) +
#    geom_density()
#summary(dtrain[r_suma_mprestamos==10]$suma_mprestamos)


########################################
# Saco las variables que no me interesan del modelo
##########################################
saco_variables <- c("ctrx_quarter",
                    "active_quarter",
                    "r_mprestamos_personales",
                    "cprestamos_personales",
                    "r_mactivos_margen",
                    "r_mcuentas_saldo",
                    "ccomisiones_otras",
                    "r_mcuenta_corriente",
                    "cdescubierto_preacordado") 

#campos <- paste(saco_variables, collapse = " - ")
#formula <- paste0( "clase_binaria ~ . - ", campos )

dprueba <- dtrain[, numero_de_cliente:=NULL]
View(dprueba)
modelo5 <- rpart(formula,
                    data = dtrain,
                    xval = 0,
                    cp = -1,
                    minsplit = 20,
                    minbucket = 10,
                    maxdepth = 5)

print(modelo5$variable.importance)



###################################################################
# guardo el dataset que luego voy a aplicar BO  y luego predecir
###################################################################
#dir.create( "./exp/KA4120" )

fwrite( dataset, #solo los campos para Kaggle
           file= paste0( "./prueba.csv"),
           sep=  "," )
