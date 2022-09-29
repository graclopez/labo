
#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rlist")



#Aqui se debe poner la carpeta de la computadora local
setwd("C:\\Maestria\\dmeyf\\")   #Establezco el Working Directory


FE_campos  <- function(mes)
{

dataset[foto_mes==mes, cprestamos_personales := ifelse(is.na(cprestamos_personales), 0, cprestamos_personales)] 
dataset[foto_mes==mes, cprestamos_prendarios := ifelse(is.na(cprestamos_prendarios), 0, cprestamos_prendarios)] 
dataset[foto_mes==mes, cprestamos_hipotecarios := ifelse(is.na(cprestamos_hipotecarios),  0, cprestamos_hipotecarios)] 
dataset[foto_mes==mes, suma_cprestamos := cprestamos_hipotecarios + cprestamos_prendarios + cprestamos_personales] 
# rescale(base$Income) ¿va a tomar mean, min y max del periodo?

dataset[foto_mes==mes, mprestamos_personales := ifelse(is.na(mprestamos_personales), 0, mprestamos_personales)] 
dataset[foto_mes==mes, mprestamos_prendarios := ifelse(is.na(mprestamos_prendarios), 0, mprestamos_prendarios)] 
dataset[foto_mes==mes, mprestamos_hipotecarios := ifelse(is.na(mprestamos_hipotecarios), 0, mprestamos_hipotecarios)]
dataset[foto_mes==mes, suma_mprestamos := mprestamos_hipotecarios + mprestamos_prendarios + mprestamos_personales] 
media <- mean(dataset[foto_mes==mes, suma_mprestamos])
mini <- min(dataset[foto_mes==mes, suma_mprestamos])
maxi <- max(dataset[foto_mes==mes, suma_mprestamos])
dataset[foto_mes==mes, suma_mprestamos := (suma_mprestamos - media)/(maxi-mini)] 

dataset[foto_mes==mes, cpayroll_trx := ifelse(is.na(cpayroll_trx), 0, cpayroll_trx)] 
dataset[foto_mes==mes, cpayroll2_trx := ifelse(is.na(cpayroll2_trx), 0, cpayroll2_trx)] 
dataset[foto_mes==mes, suma_cpayroll := cpayroll_trx + cpayroll2_trx] 

dataset[foto_mes==mes, mpayroll:= ifelse(is.na(mpayroll), 0, mpayroll)] 
dataset[foto_mes==mes, mpayroll2 := ifelse(is.na(mpayroll2), 0, mpayroll2)] 
dataset[foto_mes==mes, suma_mpayroll := mpayroll + mpayroll2] 
media <- mean(dataset[foto_mes==mes, suma_mpayroll])
mini <- min(dataset[foto_mes==mes, suma_mpayroll])
maxi <- max(dataset[foto_mes==mes, suma_mpayroll])
dataset[foto_mes==mes, suma_mpayroll := (suma_mpayroll - media)/(maxi-mini)] 

dataset[foto_mes==mes, ccajeros_propios_descuentos := ifelse(is.na(ccajeros_propios_descuentos), 0, ccajeros_propios_descuentos)] 
dataset[foto_mes==mes, ctarjeta_visa_descuentos := ifelse(is.na(ctarjeta_visa_descuentos), 0, ctarjeta_visa_descuentos)] 
dataset[foto_mes==mes, ctarjeta_master_descuentos := ifelse(is.na(ctarjeta_master_descuentos), 0, ctarjeta_master_descuentos)] 
dataset[foto_mes==mes, suma_cdescuentos := ccajeros_propios_descuentos + ctarjeta_visa_descuentos + ctarjeta_master_descuentos] 
                  
dataset[foto_mes==mes, ccomisiones_mantenimiento := ifelse(is.na(ccomisiones_mantenimiento), 0, ccomisiones_mantenimiento)] 
dataset[foto_mes==mes, ccomisiones_otras := ifelse(is.na(ccomisiones_otras), 0, ccomisiones_otras)] 
dataset[foto_mes==mes, suma_ccomisiones := ccomisiones_otras + ccomisiones_otras] 

dataset[foto_mes==mes, mcajeros_propios_descuentos := ifelse(is.na(mcajeros_propios_descuentos), 0, mcajeros_propios_descuentos)] 
dataset[foto_mes==mes, mtarjeta_visa_descuentos := ifelse(is.na(mtarjeta_visa_descuentos), 0, mtarjeta_visa_descuentos)] 
dataset[foto_mes==mes, mtarjeta_master_descuentos := ifelse(is.na(mtarjeta_master_descuentos), 0, mtarjeta_master_descuentos)] 
dataset[foto_mes==mes, suma_mdescuentos := mcajeros_propios_descuentos + mtarjeta_visa_descuentos + mtarjeta_master_descuentos] 
media <- mean(dataset[foto_mes==mes, suma_mdescuentos])
mini <- min(dataset[foto_mes==mes, suma_mdescuentos])
maxi <- max(dataset[foto_mes==mes, suma_mdescuentos])
dataset[foto_mes==mes, suma_mdescuentos := (suma_mdescuentos - media)/(maxi-mini)] 

dataset[foto_mes==mes, mcomisiones_mantenimiento := ifelse(is.na(mcomisiones_mantenimiento), 0, mcomisiones_mantenimiento)] 
dataset[foto_mes==mes, mcomisiones_otras := ifelse(is.na(mcomisiones_otras), 0, mcomisiones_otras)] 
dataset[foto_mes==mes, suma_mcomisiones := mcomisiones_mantenimiento + mcomisiones_otras] 
media <- mean(dataset[foto_mes==mes, suma_mcomisiones])
mini <- min(dataset[foto_mes==mes, suma_mcomisiones])
maxi <- max(dataset[foto_mes==mes, suma_mcomisiones])
dataset[foto_mes==mes, suma_mcomisiones := (suma_mcomisiones - media)/(maxi-mini)] 

dataset[foto_mes==mes, mrentabilidad := ifelse(is.na(mrentabilidad), 0, mrentabilidad)] 
media <- mean(dataset[foto_mes==mes, mrentabilidad])
mini <- min(dataset[foto_mes==mes, mrentabilidad])
maxi <- max(dataset[foto_mes==mes, mrentabilidad])
dataset[foto_mes==mes, mrentabilidad := (mrentabilidad - media)/(maxi-mini)] 

dataset[foto_mes==mes, mrentabilidad_annual := ifelse(is.na(mrentabilidad_annual), 0, mrentabilidad_annual)] 
media <- mean(dataset[foto_mes==mes, mrentabilidad_annual])
mini <- min(dataset[foto_mes==mes, mrentabilidad_annual])
maxi <- max(dataset[foto_mes==mes, mrentabilidad_annual])
dataset[foto_mes==mes, mrentabilidad_annual := (mrentabilidad_annual - media)/(maxi-mini)] 


dataset[foto_mes==mes, mcuenta_corriente_adicional := ifelse(is.na(mcuenta_corriente_adicional), 0, mcuenta_corriente_adicional)] 
dataset[foto_mes==mes, mcuenta_corriente := ifelse(is.na(mcuenta_corriente), 0, mcuenta_corriente)] 
dataset[foto_mes==mes, mcaja_ahorro := ifelse(is.na(mcaja_ahorro), 0, mcaja_ahorro)] 
dataset[foto_mes==mes, mcaja_ahorro_adicional := ifelse(is.na(mcaja_ahorro_adicional), 0, mcaja_ahorro_adicional)] 
dataset[foto_mes==mes, mcaja_ahorro_dolares := ifelse(is.na(mcaja_ahorro_dolares), 0, mcaja_ahorro_dolares)] 
dataset[foto_mes==mes, suma_msaldo := mcuenta_corriente_adicional+mcuenta_corriente+mcaja_ahorro+mcaja_ahorro_adicional+mcaja_ahorro_dolares] 
media <- mean(dataset[foto_mes==mes, suma_msaldo])
mini <- min(dataset[foto_mes==mes, suma_msaldo])
maxi <- max(dataset[foto_mes==mes, suma_msaldo])
dataset[foto_mes==mes, suma_msaldo := (suma_msaldo - media)/(maxi-mini)] 

dataset[foto_mes==mes, mactivos_margen := ifelse(is.na(mactivos_margen), 0, mactivos_margen)] 
media <- mean(dataset[foto_mes==mes, mactivos_margen])
mini <- min(dataset[foto_mes==mes, mactivos_margen])
maxi <- max(dataset[foto_mes==mes, mactivos_margen])
dataset[foto_mes==mes, mactivos_margen := (mactivos_margen - media)/(maxi-mini)] 

dataset[foto_mes==mes, mpasivos_margen := ifelse(is.na(mpasivos_margen), 0, mpasivos_margen)] 
media <- mean(dataset[foto_mes==mes, mpasivos_margen])
mini <- min(dataset[foto_mes==mes, mpasivos_margen])
maxi <- max(dataset[foto_mes==mes, mpasivos_margen])
dataset[foto_mes==mes, mpasivos_margen := (mpasivos_margen - media)/(maxi-mini)] 


dataset[foto_mes==mes, mautoservicio := ifelse(is.na(mautoservicio), 0, mautoservicio)] 
media <- mean(dataset[foto_mes==mes, mautoservicio])
mini <- min(dataset[foto_mes==mes, mautoservicio])
maxi <- max(dataset[foto_mes==mes, mautoservicio])
dataset[foto_mes==mes, mautoservicio := (mautoservicio - media)/(maxi-mini)] 


dataset[foto_mes==mes, mtarjeta_visa_consumo := ifelse(is.na(mtarjeta_visa_consumo), 0, mtarjeta_visa_consumo)] 
media <- mean(dataset[foto_mes==mes, mtarjeta_visa_consumo])
mini <- min(dataset[foto_mes==mes, mtarjeta_visa_consumo])
maxi <- max(dataset[foto_mes==mes, mtarjeta_visa_consumo])
dataset[foto_mes==mes, mtarjeta_visa_consumo := (mtarjeta_visa_consumo - media)/(maxi-mini)] 


dataset[foto_mes==mes, mtarjeta_master_consumo := ifelse(is.na(mtarjeta_master_consumo), 0, mtarjeta_master_consumo)] 
media <- mean(dataset[foto_mes==mes, mtarjeta_master_consumo])
mini <- min(dataset[foto_mes==mes, mtarjeta_master_consumo])
maxi <- max(dataset[foto_mes==mes, mtarjeta_master_consumo])
dataset[foto_mes==mes, mtarjeta_master_consumo := (mtarjeta_master_consumo - media)/(maxi-mini)] 

dataset[foto_mes==mes, mplazo_fijo_dolares := ifelse(is.na(mplazo_fijo_dolares), 0, mplazo_fijo_dolares)] 
media <- mean(dataset[foto_mes==mes, mplazo_fijo_dolares])
mini <- min(dataset[foto_mes==mes, mplazo_fijo_dolares])
maxi <- max(dataset[foto_mes==mes, mplazo_fijo_dolares])
dataset[foto_mes==mes, mplazo_fijo_dolares := (mplazo_fijo_dolares - media)/(maxi-mini)] 


dataset[foto_mes==mes, mplazo_fijo_pesos := ifelse(is.na(mplazo_fijo_pesos), 0, mplazo_fijo_pesos)] 
media <- mean(dataset[foto_mes==mes, mplazo_fijo_pesos])
mini <- min(dataset[foto_mes==mes, mplazo_fijo_pesos])
maxi <- max(dataset[foto_mes==mes, mplazo_fijo_pesos])
dataset[foto_mes==mes, mplazo_fijo_pesos := (mplazo_fijo_pesos - media)/(maxi-mini)] 


dataset[foto_mes==mes, minversion1_pesos := ifelse(is.na(minversion1_pesos), 0, minversion1_pesos)] 
media <- mean(dataset[foto_mes==mes, minversion1_pesos])
mini <- min(dataset[foto_mes==mes, minversion1_pesos])
maxi <- max(dataset[foto_mes==mes, minversion1_pesos])
dataset[foto_mes==mes, minversion1_pesos := (minversion1_pesos - media)/(maxi-mini)] 

dataset[foto_mes==mes, minversion1_dolares := ifelse(is.na(minversion1_dolares), 0, minversion1_dolares)] 
media <- mean(dataset[foto_mes==mes, minversion1_dolares])
mini <- min(dataset[foto_mes==mes, minversion1_dolares])
maxi <- max(dataset[foto_mes==mes, minversion1_dolares])
dataset[foto_mes==mes, minversion1_dolares := (minversion1_dolares - media)/(maxi-mini)] 

dataset[foto_mes==mes, minversion2 := ifelse(is.na(minversion2), 0, minversion2)] 
media <- mean(dataset[foto_mes==mes, minversion2])
mini <- min(dataset[foto_mes==mes, minversion2])
maxi <- max(dataset[foto_mes==mes, minversion2])
dataset[foto_mes==mes, minversion2 := (minversion2 - media)/(maxi-mini)] 


dataset[foto_mes==mes, mcuenta_debitos_automaticos := ifelse(is.na(mcuenta_debitos_automaticos), 0, mcuenta_debitos_automaticos)] 
media <- mean(dataset[foto_mes==mes, mcuenta_debitos_automaticos])
mini <- min(dataset[foto_mes==mes, mcuenta_debitos_automaticos])
maxi <- max(dataset[foto_mes==mes, mcuenta_debitos_automaticos])
dataset[foto_mes==mes, mcuenta_debitos_automaticos := (mcuenta_debitos_automaticos - media)/(maxi-mini)] 


dataset[foto_mes==mes, mtarjeta_visa_debitos_automaticos := ifelse(is.na(mtarjeta_visa_debitos_automaticos), 0, mtarjeta_visa_debitos_automaticos)] 
media <- mean(dataset[foto_mes==mes, mtarjeta_visa_debitos_automaticos])
mini <- min(dataset[foto_mes==mes, mtarjeta_visa_debitos_automaticos])
maxi <- max(dataset[foto_mes==mes, mtarjeta_visa_debitos_automaticos])
dataset[foto_mes==mes, mtarjeta_visa_debitos_automaticos := (mtarjeta_visa_debitos_automaticos - media)/(maxi-mini)] 


dataset[foto_mes==mes, mttarjeta_master_debitos_automaticos := ifelse(is.na(mttarjeta_master_debitos_automaticos), 0, mttarjeta_master_debitos_automaticos)] 
media <- mean(dataset[foto_mes==mes, mttarjeta_master_debitos_automaticos])
mini <- min(dataset[foto_mes==mes, mttarjeta_master_debitos_automaticos])
maxi <- max(dataset[foto_mes==mes, mttarjeta_master_debitos_automaticos])
dataset[foto_mes==mes, mttarjeta_master_debitos_automaticos := (mttarjeta_master_debitos_automaticos - media)/(maxi-mini)]


dataset[foto_mes==mes, mpagodeservicios := ifelse(is.na(mpagodeservicios), 0, mpagodeservicios)] 
media <- mean(dataset[foto_mes==mes, mpagodeservicios])
mini <- min(dataset[foto_mes==mes, mpagodeservicios])
maxi <- max(dataset[foto_mes==mes, mpagodeservicios])
dataset[foto_mes==mes, mpagodeservicios := (mpagodeservicios - media)/(maxi-mini)]

dataset[foto_mes==mes, mpagomiscuentas := ifelse(is.na(mpagomiscuentas), 0, mpagomiscuentas)] 
media <- mean(dataset[foto_mes==mes, mpagomiscuentas])
mini <- min(dataset[foto_mes==mes, mpagomiscuentas])
maxi <- max(dataset[foto_mes==mes, mpagomiscuentas])
dataset[foto_mes==mes, mpagomiscuentas := (mpagomiscuentas - media)/(maxi-mini)]


dataset[foto_mes==mes, mforex_buy := ifelse(is.na(mforex_buy), 0, mforex_buy)] 
media <- mean(dataset[foto_mes==mes, mforex_buy])
mini <- min(dataset[foto_mes==mes, mforex_buy])
maxi <- max(dataset[foto_mes==mes, mforex_buy])
dataset[foto_mes==mes, mforex_buy := (mforex_buy - media)/(maxi-mini)]


dataset[foto_mes==mes, mforex_sell := ifelse(is.na(mforex_sell), 0, mforex_sell)] 
media <- mean(dataset[foto_mes==mes, mforex_sell])
mini <- min(dataset[foto_mes==mes, mforex_sell])
maxi <- max(dataset[foto_mes==mes, mforex_sell])
dataset[foto_mes==mes, mforex_sell := (mforex_sell - media)/(maxi-mini)]


dataset[foto_mes==mes, mtransferencias_recibidas := ifelse(is.na(mtransferencias_recibidas), 0, mtransferencias_recibidas)] 
media <- mean(dataset[foto_mes==mes, mtransferencias_recibidas])
mini <- min(dataset[foto_mes==mes, mtransferencias_recibidas])
maxi <- max(dataset[foto_mes==mes, mtransferencias_recibidas])
dataset[foto_mes==mes, mtransferencias_recibidas := (mtransferencias_recibidas - media)/(maxi-mini)]


dataset[foto_mes==mes, mtransferencias_emitidas := ifelse(is.na(mtransferencias_emitidas), 0, mtransferencias_emitidas)] 
media <- mean(dataset[foto_mes==mes, mtransferencias_emitidas])
mini <- min(dataset[foto_mes==mes, mtransferencias_emitidas])
maxi <- max(dataset[foto_mes==mes, mtransferencias_emitidas])
dataset[foto_mes==mes, mtransferencias_emitidas := (mtransferencias_emitidas - media)/(maxi-mini)]


dataset[foto_mes==mes, mextraccion_autoservicio := ifelse(is.na(mextraccion_autoservicio), 0, mextraccion_autoservicio)] 
media <- mean(dataset[foto_mes==mes, mextraccion_autoservicio])
mini <- min(dataset[foto_mes==mes, mextraccion_autoservicio])
maxi <- max(dataset[foto_mes==mes, mextraccion_autoservicio])
dataset[foto_mes==mes, mextraccion_autoservicio := (mextraccion_autoservicio - media)/(maxi-mini)]


dataset[foto_mes==mes, mcheques_depositados := ifelse(is.na(mcheques_depositados), 0, mcheques_depositados)] 
media <- mean(dataset[foto_mes==mes, mcheques_depositados])
mini <- min(dataset[foto_mes==mes, mcheques_depositados])
maxi <- max(dataset[foto_mes==mes, mcheques_depositados])
dataset[foto_mes==mes, mcheques_depositados := (mcheques_depositados - media)/(maxi-mini)]


dataset[foto_mes==mes, mcheques_emitidos := ifelse(is.na(mcheques_emitidos), 0, mcheques_emitidos)] 
media <- mean(dataset[foto_mes==mes, mcheques_emitidos])
mini <- min(dataset[foto_mes==mes, mcheques_emitidos])
maxi <- max(dataset[foto_mes==mes, mcheques_emitidos])
dataset[foto_mes==mes, mcheques_emitidos := (mcheques_emitidos - media)/(maxi-mini)]



dataset[foto_mes==mes, mcheques_depositados_rechazados := ifelse(is.na(mcheques_depositados_rechazados), 0, mcheques_depositados_rechazados)] 
media <- mean(dataset[foto_mes==mes, mcheques_depositados_rechazados])
mini <- min(dataset[foto_mes==mes, mcheques_depositados_rechazados])
maxi <- max(dataset[foto_mes==mes, mcheques_depositados_rechazados])
dataset[foto_mes==mes, mcheques_depositados_rechazados := (mcheques_depositados_rechazados - media)/(maxi-mini)]


dataset[foto_mes==mes, mcheques_emitidos_rechazados := ifelse(is.na(mcheques_emitidos_rechazados), 0, mcheques_emitidos_rechazados)] 
media <- mean(dataset[foto_mes==mes, mcheques_emitidos_rechazados])
mini <- min(dataset[foto_mes==mes, mcheques_emitidos_rechazados])
maxi <- max(dataset[foto_mes==mes, mcheques_emitidos_rechazados])
dataset[foto_mes==mes, mcheques_emitidos_rechazados := (mcheques_emitidos_rechazados - media)/(maxi-mini)]


dataset[foto_mes==mes, matm := ifelse(is.na(matm), 0, matm)] 
media <- mean(dataset[foto_mes==mes, matm])
mini <- min(dataset[foto_mes==mes, matm])
maxi <- max(dataset[foto_mes==mes, matm])
dataset[foto_mes==mes, matm := (matm - media)/(maxi-mini)]


dataset[foto_mes==mes, matm_other := ifelse(is.na(matm_other), 0, matm_other)] 
media <- mean(dataset[foto_mes==mes, matm_other])
mini <- min(dataset[foto_mes==mes, matm_other])
maxi <- max(dataset[foto_mes==mes, matm_other])
dataset[foto_mes==mes, matm_other := (matm_other - media)/(maxi-mini)]


dataset[foto_mes==mes, Master_mfinanciacion_limite := ifelse(is.na(Master_mfinanciacion_limite), 0, Master_mfinanciacion_limite)] 
media <- mean(dataset[foto_mes==mes, Master_mfinanciacion_limite])
mini <- min(dataset[foto_mes==mes, Master_mfinanciacion_limite])
maxi <- max(dataset[foto_mes==mes, Master_mfinanciacion_limite])
dataset[foto_mes==mes, Master_mfinanciacion_limite := (Master_mfinanciacion_limite - media)/(maxi-mini)]

dataset[foto_mes==mes, Master_msaldopesos := ifelse(is.na(Master_msaldopesos), 0, Master_msaldopesos)] 
media <- mean(dataset[foto_mes==mes, Master_msaldopesos])
mini <- min(dataset[foto_mes==mes, Master_msaldopesos])
maxi <- max(dataset[foto_mes==mes, Master_msaldopesos])
dataset[foto_mes==mes, Master_msaldopesos := (Master_msaldopesos - media)/(maxi-mini)]


dataset[foto_mes==mes, Master_msaldodolares := ifelse(is.na(Master_msaldodolares), 0, Master_msaldodolares)] 
media <- mean(dataset[foto_mes==mes, Master_msaldodolares])
mini <- min(dataset[foto_mes==mes, Master_msaldodolares])
maxi <- max(dataset[foto_mes==mes, Master_msaldodolares])
dataset[foto_mes==mes, Master_msaldodolares := (Master_msaldodolares - media)/(maxi-mini)]

dataset[foto_mes==mes, Master_mconsumospesos := ifelse(is.na(Master_mconsumospesos), 0, Master_mconsumospesos)] 
media <- mean(dataset[foto_mes==mes, Master_mconsumospesos])
mini <- min(dataset[foto_mes==mes, Master_mconsumospesos])
maxi <- max(dataset[foto_mes==mes, Master_mconsumospesos])
dataset[foto_mes==mes, Master_mconsumospesos := (Master_mconsumospesos - media)/(maxi-mini)]


dataset[foto_mes==mes, Master_mconsumosdolares := ifelse(is.na(Master_mconsumosdolares), 0, Master_mconsumosdolares)] 
media <- mean(dataset[foto_mes==mes, Master_mconsumosdolares])
mini <- min(dataset[foto_mes==mes, Master_mconsumosdolares])
maxi <- max(dataset[foto_mes==mes, Master_mconsumosdolares])
dataset[foto_mes==mes, Master_mconsumosdolares := (Master_mconsumosdolares - media)/(maxi-mini)]


dataset[foto_mes==mes, Master_mlimitecompra := ifelse(is.na(Master_mlimitecompra), 0, Master_mlimitecompra)] 
media <- mean(dataset[foto_mes==mes, Master_mlimitecompra])
mini <- min(dataset[foto_mes==mes, Master_mlimitecompra])
maxi <- max(dataset[foto_mes==mes, Master_mlimitecompra])
dataset[foto_mes==mes, Master_mlimitecompra := (Master_mlimitecompra - media)/(maxi-mini)]


dataset[foto_mes==mes, Master_madelantopesos := ifelse(is.na(Master_madelantopesos), 0, Master_madelantopesos)] 
media <- mean(dataset[foto_mes==mes, Master_madelantopesos])
mini <- min(dataset[foto_mes==mes, Master_madelantopesos])
maxi <- max(dataset[foto_mes==mes, Master_madelantopesos])
dataset[foto_mes==mes, Master_madelantopesos := (Master_madelantopesos - media)/(maxi-mini)]


dataset[foto_mes==mes, Master_madelantodolares := ifelse(is.na(Master_madelantodolares), 0, Master_madelantodolares)] 
media <- mean(dataset[foto_mes==mes, Master_madelantodolares])
mini <- min(dataset[foto_mes==mes, Master_madelantodolares])
maxi <- max(dataset[foto_mes==mes, Master_madelantodolares])
dataset[foto_mes==mes, Master_madelantodolares := (Master_madelantodolares - media)/(maxi-mini)]


dataset[foto_mes==mes, Master_mpagospesos := ifelse(is.na(Master_mpagospesos), 0, Master_mpagospesos)] 
media <- mean(dataset[foto_mes==mes, Master_mpagospesos])
mini <- min(dataset[foto_mes==mes, Master_mpagospesos])
maxi <- max(dataset[foto_mes==mes, Master_mpagospesos])
dataset[foto_mes==mes, Master_mpagospesos := (Master_mpagospesos - media)/(maxi-mini)]


dataset[foto_mes==mes, Master_mpagosdolares := ifelse(is.na(Master_mpagosdolares), 0, Master_mpagosdolares)] 
media <- mean(dataset[foto_mes==mes, Master_mpagosdolares])
mini <- min(dataset[foto_mes==mes, Master_mpagosdolares])
maxi <- max(dataset[foto_mes==mes, Master_mpagosdolares])
dataset[foto_mes==mes, Master_mpagosdolares := (Master_mpagosdolares - media)/(maxi-mini)]


dataset[foto_mes==mes, Master_mpagominimo := ifelse(is.na(Master_mpagominimo), 0, Master_mpagominimo)] 
media <- mean(dataset[foto_mes==mes, Master_mpagominimo])
mini <- min(dataset[foto_mes==mes, Master_mpagominimo])
maxi <- max(dataset[foto_mes==mes, Master_mpagominimo])
dataset[foto_mes==mes, Master_mpagominimo := (Master_mpagominimo - media)/(maxi-mini)]


dataset[foto_mes==mes, Visa_mfinanciacion_limite := ifelse(is.na(Visa_mfinanciacion_limite), 0, Visa_mfinanciacion_limite)] 
media <- mean(dataset[foto_mes==mes, Visa_mfinanciacion_limite])
mini <- min(dataset[foto_mes==mes, Visa_mfinanciacion_limite])
maxi <- max(dataset[foto_mes==mes, Visa_mfinanciacion_limite])
dataset[foto_mes==mes, Visa_mfinanciacion_limite := (Visa_mfinanciacion_limite - media)/(maxi-mini)]


dataset[foto_mes==mes, Visa_msaldopesos := ifelse(is.na(Visa_msaldopesos), 0, Visa_msaldopesos)] 
media <- mean(dataset[foto_mes==mes, Visa_msaldopesos])
mini <- min(dataset[foto_mes==mes, Visa_msaldopesos])
maxi <- max(dataset[foto_mes==mes, Visa_msaldopesos])
dataset[foto_mes==mes, Visa_msaldopesos := (Visa_msaldopesos - media)/(maxi-mini)]


dataset[foto_mes==mes, Visa_msaldodolares := ifelse(is.na(Visa_msaldodolares), 0, Visa_msaldodolares)] 
media <- mean(dataset[foto_mes==mes, Visa_msaldodolares])
mini <- min(dataset[foto_mes==mes, Visa_msaldodolares])
maxi <- max(dataset[foto_mes==mes, Visa_msaldodolares])
dataset[foto_mes==mes, Visa_msaldodolares := (Visa_msaldodolares - media)/(maxi-mini)]



dataset[foto_mes==mes, Visa_mconsumospesos := ifelse(is.na(Visa_mconsumospesos), 0, Visa_mconsumospesos)] 
media <- mean(dataset[foto_mes==mes, Visa_mconsumospesos])
mini <- min(dataset[foto_mes==mes, Visa_mconsumospesos])
maxi <- max(dataset[foto_mes==mes, Visa_mconsumospesos])
dataset[foto_mes==mes, Visa_mconsumospesos := (Visa_mconsumospesos - media)/(maxi-mini)]



dataset[foto_mes==mes, Visa_mconsumosdolares := ifelse(is.na(Visa_mconsumosdolares), 0, Visa_mconsumosdolares)] 
media <- mean(dataset[foto_mes==mes, Visa_mconsumosdolares])
mini <- min(dataset[foto_mes==mes, Visa_mconsumosdolares])
maxi <- max(dataset[foto_mes==mes, Visa_mconsumosdolares])
dataset[foto_mes==mes, Visa_mconsumosdolares := (Visa_mconsumosdolares - media)/(maxi-mini)]


dataset[foto_mes==mes, Visa_mlimitecompra := ifelse(is.na(Visa_mlimitecompra), 0, Visa_mlimitecompra)] 
media <- mean(dataset[foto_mes==mes, Visa_mlimitecompra])
mini <- min(dataset[foto_mes==mes, Visa_mlimitecompra])
maxi <- max(dataset[foto_mes==mes, Visa_mlimitecompra])
dataset[foto_mes==mes, Visa_mlimitecompra := (Visa_mlimitecompra - media)/(maxi-mini)]


dataset[foto_mes==mes, Visa_madelantopesos := ifelse(is.na(Visa_madelantopesos), 0, Visa_madelantopesos)] 
media <- mean(dataset[foto_mes==mes, Visa_madelantopesos])
mini <- min(dataset[foto_mes==mes, Visa_madelantopesos])
maxi <- max(dataset[foto_mes==mes, Visa_madelantopesos])
dataset[foto_mes==mes, Visa_madelantopesos := (Visa_madelantopesos - media)/(maxi-mini)]


dataset[foto_mes==mes, Visa_madelantodolares := ifelse(is.na(Visa_madelantodolares), 0, Visa_madelantodolares)] 
media <- mean(dataset[foto_mes==mes, Visa_madelantodolares])
mini <- min(dataset[foto_mes==mes, Visa_madelantodolares])
maxi <- max(dataset[foto_mes==mes, Visa_madelantodolares])
dataset[foto_mes==mes, Visa_madelantodolares := (Visa_madelantodolares - media)/(maxi-mini)]


dataset[foto_mes==mes, Visa_mpagospesos := ifelse(is.na(Visa_mpagospesos), 0, Visa_mpagospesos)] 
media <- mean(dataset[foto_mes==mes, Visa_mpagospesos])
mini <- min(dataset[foto_mes==mes, Visa_mpagospesos])
maxi <- max(dataset[foto_mes==mes, Visa_mpagospesos])
dataset[foto_mes==mes, Visa_mpagospesos := (Visa_mpagospesos - media)/(maxi-mini)]


dataset[foto_mes==mes, Visa_mpagosdolares := ifelse(is.na(Visa_mpagosdolares), 0, Visa_mpagosdolares)] 
media <- mean(dataset[foto_mes==mes, Visa_mpagosdolares])
mini <- min(dataset[foto_mes==mes, Visa_mpagosdolares])
maxi <- max(dataset[foto_mes==mes, Visa_mpagosdolares])
dataset[foto_mes==mes, Visa_mpagosdolares := (Visa_mpagosdolares - media)/(maxi-mini)]


dataset[foto_mes==mes, Visa_mpagominimo := ifelse(is.na(Visa_mpagominimo), 0, Visa_mpagominimo)] 
media <- mean(dataset[foto_mes==mes, Visa_mpagominimo])
mini <- min(dataset[foto_mes==mes, Visa_mpagominimo])
maxi <- max(dataset[foto_mes==mes, Visa_mpagominimo])
dataset[foto_mes==mes, Visa_mpagominimo := (Visa_mpagominimo - media)/(maxi-mini)]


## proporcion entre prestamos y haberes. Si haberes cero -> entre prestamos y saldo cuentas
dataset[foto_mes==mes, p_prestamos := ifelse(suma_mpayroll==0, ifelse(suma_msaldo<=0,0,suma_mprestamos/suma_msaldo), suma_mprestamos/suma_mpayroll)] 

## proporcion entre limite compra visa  y haberes. Si cero -> entre limite y saldo cuentas
dataset[foto_mes==mes, p_Visa_mlimitecompra := ifelse(suma_mpayroll==0, ifelse(suma_msaldo<=0,0,Visa_mlimitecompra/suma_msaldo), Visa_mlimitecompra/suma_mpayroll)] 

## proporcion entre limite compra mc  y haberes. Si cero -> entre limite y saldo cuentas
dataset[foto_mes==mes, p_Master_mlimitecompra := ifelse(suma_mpayroll==0, ifelse(suma_msaldo<=0,0,Master_mlimitecompra/suma_msaldo), Master_mlimitecompra/suma_mpayroll)] 


}


#cargo el dataset donde voy a entrenar el modelo
dataset  <- fread("./datasets/competencia2_2022.csv.gz")

FE_campos(202103)
FE_campos(202105)

#Elimino campos



#Guardar el archivo con las nuevas columnas
data.table::fwrite(dataset, file = "./datasets/comp2_FE4_2022.csv.gz")


