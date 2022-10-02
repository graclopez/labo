
#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rlist")



#Aqui se debe poner la carpeta de la computadora local
setwd("C:\\Maestria\\dmeyf\\")   #Establezco el Working Directory


FE_campos  <- function()
{
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
                  
# El campo mcomisiones ya trae esta suma. 
#dataset[foto_mes %in% ls_meses, ccomisiones_mantenimiento := ifelse(is.na(ccomisiones_mantenimiento), 0, ccomisiones_mantenimiento)] 
#dataset[foto_mes %in% ls_meses, ccomisiones_otras := ifelse(is.na(ccomisiones_otras), 0, ccomisiones_otras)] 
#dataset[foto_mes %in% ls_meses, suma_ccomisiones := ccomisiones_otras + ccomisiones_otras] 


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

## proporcion entre descuentos y comisiones
dataset[, p_desc_com := ifelse(mcomisiones==0,0,suma_mdescuentos/mcomisiones)] 


}


#cargo el dataset donde voy a entrenar el modelo
dataset  <- fread("./datasets/competencia2_2022.csv.gz")
FE_campos()

#Guardar el archivo con las nuevas columnas
data.table::fwrite(dataset, file = "./datasets/comp2_FE3_2022.csv.gz")



#dt_nuevo  <- fread("./datasets/comp2_FE1_2022.csv.gz")
#colnames(dt_nuevo)


#View(dt_nuevo[foto_mes %in% meses & mcomisiones>0, c("mcomisiones","mcomisiones_mantenimiento","mcomisiones_otras","norm_mcomisiones")])

#p <- dataset[foto_mes %in% meses, mean(mcomisiones)]
#mx <- dataset[foto_mes %in% meses, max(mcomisiones)]
#mi <- dataset[foto_mes %in% meses, min(mcomisiones)]
#p
#mx
#mi
#mx -mi

