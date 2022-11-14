rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("randomForest")
require("ranger")


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#setwd( "~/buckets/b1/" )  #cambiar por la carpeta local
setwd("C:\\Maestria\\dmeyf\\")

#leo el dataset original
# pero podria leer cualquiera que tenga Feature Engineering
#dataset  <- fread( "./datasets/cluster_de_bajas.txt", stringsAsFactors= TRUE)
dataset  <- fread( "./clusters/cluster_de_bajas_FE.txt", stringsAsFactors= TRUE)


dataset[  , .N,  cluster2 ]  #tamaño de los clusters


#ahora a mano veo los centroides de los 7 clusters
#esto hay que hacerlo para cada variable,
#  y ver cuales son las que mas diferencian a los clusters
#esta parte conviene hacerla desde la PC local, sobre  cluster_de_bajas.txt

#dataset[  , mean(ctrx_quarter),  cluster2 ]  #media de la variable  ctrx_quarter
#dataset[  , mean(mtarjeta_visa_consumo),  cluster2 ]
#dataset[  , mean(mcuentas_saldo),  cluster2 ]
#dataset[  , mean(chomebanking_transacciones),  cluster2 ]
#dataset[  , mean(mcaja_ahorro), cluster2]
#dataset[  , mean(cliente_edad), cluster2]
#dataset[  , mean(mrentabilidad_annual), cluster2]
#dataset[  , mean(mprestamos_personales), cluster2]
#dataset[  , mean(ctarjeta_visa_transacciones), cluster2]
#dataset[  , mean(mcuentas_saldo), cluster2]
#dataset[  , mean(mactivos_margen), cluster2]
#dataset[  , mean(mpayroll), cluster2]
#dataset[  , mean(Visa_mpagominimo),cluster2]
#dataset[  , mean(Master_fechaalta),cluster2]
#dataset[  , mean(chomebanking_transacciones),cluster2]
#dataset[  , mean(Visa_msaldopesos),cluster2]
#dataset[  , mean(mrentabilidad),cluster2]
#dataset[  , mean(Visa_msaldototal),cluster2]
#dataset[  , mean(Visa_Fvencimiento),cluster2]
#dataset[  , mean(Master_Fvencimiento),cluster2]
#dataset[  , mean(mcuenta_corriente),cluster2]
#dataset[  , mean(Visa_mpagospesos),cluster2]
#dataset[  , mean(Visa_fechaalta), cluster2]

campos_buenos  <- c( "ctrx_quarter", "ctarjeta_visa_transacciones", "mrentabilidad_annual",
                     "cliente_edad", "chomebanking_transacciones", "Visa_Fvencimiento", "Master_Fvencimiento", 
                     "cliente_antiguedad", "cproductos", "mcomisiones_otras", "thomebanking", "mcuenta_debitos_automaticos",
                     "vm_status01","vm_mfinanciacion_limite","vm_Finiciomora","vm_msaldototal","vm_mlimitecompra" ,
                     "vm_mconsumototal","vm_cconsumos", "vmr_mconsumototal","c_totalprestamos","m_totalprestamos",
                     "c_totalpayroll","m_totalpayroll","m_totalpayroll_sobre_edad","c_totaldescuentos","m_totaldescuentos",
                     "c_totalcomisiones","m_totalcomisiones","m_totalctavista","m_totaldinero","m_totalmargenes","c_totaltransac",
                     "m_totaltransac","c_totalseguros","mr_prestamos","mr_limitecompra","mr_desc_com")



dataset[  , mean(ctrx_quarter), cluster2]
dataset[  , mean(ctarjeta_visa_transacciones), cluster2]
dataset[  , mean(mrentabilidad_annual), cluster2]
dataset[  , mean(cliente_edad), cluster2]
dataset[  , mean(chomebanking_transacciones), cluster2]
dataset[  , mean(Visa_Fvencimiento), cluster2]
dataset[  , mean(Master_Fvencimiento), cluster2]
dataset[  , mean(cliente_antiguedad), cluster2]
dataset[  , mean(cproductos), cluster2]
dataset[  , mean(thomebanking), cluster2]
dataset[  , mean(mcuenta_debitos_automaticos), cluster2]
dataset[  , mean(vm_status01), cluster2]
dataset[  , mean(vm_mfinanciacion_limite), cluster2]
dataset[  , mean(vm_Finiciomora), cluster2]
dataset[  , mean(vm_msaldototal), cluster2]
dataset[  , mean(vm_mlimitecompra), cluster2]
dataset[  , mean(vm_mconsumototal), cluster2]
dataset[  , mean(vm_cconsumos), cluster2]
dataset[  , mean(vmr_mconsumototal), cluster2]
dataset[  , mean(c_totalprestamos), cluster2]
dataset[  , mean(m_totalprestamos), cluster2]
dataset[  , mean(c_totalpayroll), cluster2]
dataset[  , mean(m_totalpayroll), cluster2]
dataset[  , mean(m_totalpayroll_sobre_edad), cluster2]
dataset[  , mean(c_totaldescuentos), cluster2]
dataset[  , mean(m_totaldescuentos), cluster2]
dataset[  , mean(c_totalcomisiones), cluster2]
dataset[  , mean(m_totalcomisiones), cluster2]
dataset[  , mean(m_totalctavista), cluster2]
dataset[  , mean(m_totaldinero), cluster2]
dataset[  , mean(m_totalmargenes), cluster2]
dataset[  , mean(c_totaltransac), cluster2]
dataset[  , mean(m_totaltransac), cluster2]
dataset[  , mean(c_totalseguros), cluster2]
dataset[  , mean(mr_prestamos), cluster2]
dataset[  , mean(mr_limitecompra), cluster2]
dataset[  , mean(mr_desc_com), cluster2]


dataset12  <- fread( "./clusters/cluster_de_bajas_12meses.txt", stringsAsFactors= TRUE)

colnames(dataset12)
dataset12[, .N]

View(dataset12) 

View(dataset) 
dataset[, .N]
