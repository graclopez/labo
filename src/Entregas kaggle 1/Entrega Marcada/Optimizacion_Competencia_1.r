# Este script esta pensado para corren en Google Cloud
# si se lo desea correr en Windows debera
#  * cambiar el setwd()  y las rutas
#  * cuando llame a la funcion mcmapply  poner  mc.cores=1
#  * armarse de mucha paciencia porque va a demorar muchas horas en Windows

#Optimizacion Bayesiana de hiperparametros de  rpart
# Hace  1-Repeated  5-Fold Cross Validation


# NO utiliza Feature Engineering  ( el Fiscal General se enoja ... )


#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rlist")

require("rpart")
require("parallel")

#paquetes necesarios para la Bayesian Optimization
require("DiceKriging")
require("mlrMBO")

#aqui deben ir SUS semillas, se usan para  1-Repeated  (5-Fold Cross Validation)
#ksemilla_azar  <- c(102191)
ksemilla_azar <- c(851159,773567,807797,216617,324757)


#Defino la  Optimizacion Bayesiana

kBO_iter  <- 100   #cantidad de iteraciones de la Optimizacion Bayesiana

hs  <- makeParamSet(
          makeNumericParam("cp"       , lower=  -1.0, upper=    0.1),
          makeNumericParam("minsplit" , lower=   1,   upper= 5000 ),
          makeNumericParam("minbucket", lower=   1,   upper= 1000 ),
          makeIntegerParam("maxdepth" , lower=   3L,  upper=   20L),  #la letra L al final significa ENTERO
          forbidden = quote( minbucket > 0.5*minsplit ) )             # minbuket NO PUEDE ser mayor que la mitad de minsplit


#------------------------------------------------------------------------------
#graba a un archivo los componentes de lista
#para el primer registro, escribe antes los titulos

loguear  <- function( reg, arch=NA, folder="./work/", ext=".txt", verbose=TRUE )
{
  archivo  <- arch
  if( is.na(arch) )  archivo  <- paste0( folder, substitute( reg), ext )

  if( !file.exists( archivo ) )  #Escribo los titulos
  {
    linea  <- paste0( "fecha\t", 
                      paste( list.names(reg), collapse="\t" ), "\n" )

    cat( linea, file=archivo )
  }

  linea  <- paste0( format(Sys.time(), "%Y%m%d %H%M%S"),  "\t",     #la fecha y hora
                    gsub( ", ", "\t", toString( reg ) ),  "\n" )

  cat( linea, file=archivo, append=TRUE )  #grabo al archivo

  if( verbose )  cat( linea )   #imprimo por pantalla
}
#------------------------------------------------------------------------------
# particionar agrega una columna llamada fold a un dataset que consiste en una particion estratificada segun agrupa
# particionar( data=dataset, division=c(70,30), agrupa=clase_ternaria, seed=semilla)   crea una particion 70, 30 
# particionar( data=dataset, division=c(1,1,1,1,1), agrupa=clase_ternaria, seed=semilla)   divide el dataset en 5 particiones

particionar  <- function( data, division, agrupa="", campo="fold", start=1, seed=NA )
{
  if( !is.na( seed)  )   set.seed( seed )

  bloque  <- unlist( mapply(  function(x,y) { rep( y, x ) }, division, seq( from=start, length.out=length(division) )  ) )

  data[ , (campo) :=  sample( rep( bloque, ceiling(.N/length(bloque))) )[1:.N],
           by= agrupa ]
}
#------------------------------------------------------------------------------
#fold_test  tiene el numero de fold que voy a usar para testear, entreno en el resto de los folds
#param tiene los hiperparametros del arbol

ArbolSimple  <- function( fold_test, data, param )
{
  param2 <- param
  #param2$minsplit   <- as.integer( round( 2^param$minsplit ) )
  #param2$minbucket  <- as.integer( round( 2^param$minbucket ) )
  
  #genero el modelo
  modelo  <- rpart("clase_binaria ~ .  -Visa_mpagado -clase_ternaria",
                   data= data[ fold != fold_test, ],  #entreno en todo MENOS el fold_test que uso para testing
                   xval= 0,
                   control= param2 )

  #aplico el modelo a los datos de testing
  prediccion  <- predict( modelo, 
                          data[ fold==fold_test, ],  #aplico el modelo sobre los datos de testing
                          type= "prob")   #quiero que me devuelva probabilidades

  #En el 1er cuatrimestre del Tercer Año de la Maestria se explicaran las siguientes 12 lineas
  dtest <- copy( data[ fold==fold_test , list( clase_ternaria )] )
  dtest[ , pred := prediccion[ ,"SI"] ]
  dtest[ , azar := runif( nrow( dtest ) ) ]
  setorder(  dtest, -pred, azar )

  dtest[ , gan :=  ifelse( clase_ternaria=="BAJA+2", 78000, -2000 ) ]
  dtest[ , gan_acum := cumsum( gan ) ]

  #calculo la ganancia
  dtest2   <- dtest[ (1:100)*100,  ]
  idx_max  <- which.max( dtest2$gan_acum ) 
  ganancia_testing  <- dtest2[ (idx_max-1):(idx_max+1),  mean(gan_acum) ]


  rm( dtest )
  rm( dtest2 )

  return( ganancia_testing )  #esta es la ganancia sobre el fold de testing, NO esta normalizada
}
#------------------------------------------------------------------------------

ArbolesCrossValidation  <- function( semilla, data, param, qfolds, pagrupa )
{
  divi  <- rep( 1, qfolds )  # generalmente  c(1, 1, 1, 1, 1 )  cinco unos

  particionar( data, divi, seed=semilla, agrupa=pagrupa )  #particiono en dataset en folds

  ganancias  <- mcmapply( ArbolSimple, 
                          seq(qfolds), # 1 2 3 4 5
                          MoreArgs= list( data, param), 
                          SIMPLIFY= FALSE,
                          mc.cores= 1 )   #debe ir 1 si es Windows

  data[ , fold := NULL ]

  #devuelvo la primer ganancia y el promedio
  ganancia_promedio  <- mean( unlist( ganancias ) )   #promedio las ganancias
  ganancia_promedio_normalizada  <- ganancia_promedio * qfolds  #aqui normalizo la ganancia

  gc()

  return( ganancia_promedio_normalizada )
}
#------------------------------------------------------------------------------
#esta funcion solo puede recibir los parametros que se estan optimizando
#el resto de los parametros, lamentablemente se pasan como variables globales

EstimarGanancia  <- function( x )
{
   GLOBAL_iteracion  <<-  GLOBAL_iteracion + 1

   xval_folds  <- 5
   vganancias <- mcmapply( ArbolesCrossValidation,
                           ksemilla_azar,
                           MoreArgs= list ( dtrain, param=x, qfolds= xval_folds, pagrupa= "clase_ternaria" ),
                           SIMPLIFY= FALSE,
                           mc.cores = 1 )  #debe ir 1 si es Windows


   ganancia_promedio  <- mean( unlist( vganancias ) )
   #logueo 
   xx  <- x
   xx$xval_folds  <-  xval_folds
   xx$ganancia  <- ganancia_promedio
   xx$iteracion <- GLOBAL_iteracion
   loguear( xx,  arch= archivo_log )

   return( xx$ganancia )
}
#------------------------------------------------------------------------------
#Aqui empieza el programa

#setwd( "~/buckets/b1/" )
setwd("C:\\Maestria\\DMEyF\\")

#cargo el dataset, aqui debe poner  SU RUTA
dataset  <- fread("./datasets/competencia1_2022.csv")   #donde entreno

#creo la clase_binaria  SI= {BAJA+1, BAJA+2}  NO={CONTINUA}
dataset[ foto_mes==202101, clase_binaria :=  ifelse( clase_ternaria=="CONTINUA", "NO", "SI" ) ]

#defino los datos donde entreno
dtrain  <- dataset[ foto_mes==202101, ]

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

saco_variables <- c("cprestamos_personales","cprestamos_prendarios","cprestamos_hipotecarios",
                    "mprestamos_personales","mprestamos_prendarios","mprestamos_hipotecarios",
                    "cpayroll_trx","cpayroll2_trx",
                    "mpayroll","mpayroll2",
                    "ccajeros_propios_descuentos","ctarjeta_visa_descuentos","ctarjeta_master_descuentos",
                    "ccomisiones_mantenimiento","ccomisiones_otras",
                    "mcajeros_propios_descuentos","mtarjeta_visa_descuentos","mtarjeta_master_descuentos",
                    "mcomisiones_mantenimiento","mcomisiones_otras")

dtrain[,c("cprestamos_personales","cprestamos_prendarios","cprestamos_hipotecarios",
                    "mprestamos_personales","mprestamos_prendarios","mprestamos_hipotecarios",
                    "cpayroll_trx","cpayroll2_trx",
                    "mpayroll","mpayroll2",
                    "ccajeros_propios_descuentos","ctarjeta_visa_descuentos","ctarjeta_master_descuentos",
                    "ccomisiones_mantenimiento","ccomisiones_otras",
                    "mcajeros_propios_descuentos","mtarjeta_visa_descuentos","mtarjeta_master_descuentos",
                    "mcomisiones_mantenimiento","mcomisiones_otras"):=NULL]

######################################################################

#creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
#dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( "./exp/HTComp1/", showWarnings = FALSE )
setwd("./exp/HTComp1/")   #Establezco el Working Directory DEL EXPERIMENTO

#defino los archivos donde guardo los resultados de la Bayesian Optimization
archivo_log  <- "HTComp1.txt"
archivo_BO   <- "HTComp1.RDATA"

#leo si ya existe el log, para retomar en caso que se se corte el programa
GLOBAL_iteracion  <- 0

if( file.exists(archivo_log) )
{
 tabla_log  <- fread( archivo_log )
 GLOBAL_iteracion  <- nrow( tabla_log )
}



#Aqui comienza la configuracion de la Bayesian Optimization

funcion_optimizar  <- EstimarGanancia

configureMlr( show.learner.output= FALSE)

#configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
#por favor, no desesperarse por lo complejo
obj.fun  <- makeSingleObjectiveFunction(
              fn=       funcion_optimizar,
              minimize= FALSE,   #estoy Maximizando la ganancia
              noisy=    TRUE,
              par.set=  hs,
              has.simple.signature = FALSE   #espia Tomas Delvechio, dejar este parametro asi
             )

ctrl  <- makeMBOControl( save.on.disk.at.time= 600,  save.file.path= archivo_BO)
ctrl  <- setMBOControlTermination(ctrl, iters= kBO_iter )
ctrl  <- setMBOControlInfill(ctrl, crit= makeMBOInfillCritEI())

surr.km  <- makeLearner("regr.km", predict.type= "se", covtype= "matern3_2", control= list(trace= TRUE))

#inicio la optimizacion bayesiana
if( !file.exists( archivo_BO ) ) {

  run  <- mbo( fun=     obj.fun, 
               learner= surr.km,
               control= ctrl)

} else  run  <- mboContinue( archivo_BO )   #retomo en caso que ya exista





