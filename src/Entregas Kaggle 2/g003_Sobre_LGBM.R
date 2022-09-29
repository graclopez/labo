##
## Sobre LGBM
##
## ---------------------------
## Step 1: Setup
## ---------------------------
##
## Light up the darkness.
## --- Bob Marley

# Bajaremos a detalle como funciona el lightGBM a medida que recorremos sus parámetros

rm(list = ls())
gc(verbose = FALSE)

# Librerías necesarias
require("data.table")
require("rpart")
require("ggplot2")
require("lightgbm")

# Poner la carpeta de la materia de SU computadora local
setwd("C:\\Maestria\\dmeyf\\")
# Poner sus semillas
semillas <- c(851159,773567,807797,216617,324757)


# Cargamos los datasets y nos quedamos solo con 202101 y 202103
#dataset <- fread("./datasets/competencia2_2022.csv.gz")
dataset <- fread("./datasets/comp2_FE3_2022.csv.gz")

marzo <- dataset[foto_mes == 202103]
rm(dataset)

# Clase BAJA+1 y BAJA+2 juntas
clase_binaria <- ifelse(marzo$clase_ternaria == "CONTINUA", 0, 1)
clase_real <- marzo$clase_ternaria
marzo$clase_ternaria <- NULL

saco_variables <- c("mcuenta_corriente_adicional","mcuenta_corriente","mcaja_ahorro","mcaja_ahorro_adicional",
                    "cprestamos_personales","cprestamos_prendarios","cprestamos_hipotecarios",
                    "mprestamos_personales","mprestamos_prendarios","mprestamos_hipotecarios",
                    "cpayroll_trx","cpayroll2_trx",
                    "mpayroll","mpayroll2",
                    "mcuenta_debitos_automaticos","mtarjeta_visa_debitos_automaticos","mttarjeta_master_debitos_automaticos",
                    "mpagodeservicios","mpagomiscuentas",
                    "ccajeros_propios_descuentos","ctarjeta_visa_descuentos","ctarjeta_master_descuentos",
                    "ccomisiones_mantenimiento","ccomisiones_otras",
                    "mcajeros_propios_descuentos","mtarjeta_visa_descuentos","mtarjeta_master_descuentos",
                    "mcomisiones_mantenimiento","mcomisiones_otras",
                    "cforex","mforex_buy","mforex_sell","mtransferencias_recibidas","mtransferencias_emitidas",
                    "mextraccion_autoservicio","mcheques_depositados","mcheques_depositados_rechazados",
                    "matm","matm_other","Master_Fvencimiento","Master_Finiciomora",
                    "Master_msaldopesos","Master_msaldodolares","Master_mconsumospesos","Master_mconsumosdolares",
                    "Master_madelantopesos","Master_madelantodolares","Master_fultimo_cierre",
                    "Master_mpagospesos","Master_mpagosdolares","Master_fechaalta","Master_mconsumototal",
                    "Master_cadelantosefectivo","Master_mpagominimo",
                    "Visa_Fvencimiento","Visa_Finiciomora","Visa_msaldopesos","Visa_msaldodolares",
                    "Visa_mconsumospesos","Visa_mconsumosdolares","Visa_fultimo_cierre","Visa_mpagospesos",
                    "Visa_mpagosdolares","Visa_fechaalta","Visa_mconsumototal","Visa_cadelantosefectivo","Visa_mpagominimo")


#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(marzo), saco_variables )




dtrain  <- lgb.Dataset(data   = data.matrix(marzo,campos_buenos),
                       label  = clase_binaria,
                       # Truco jedi!
                       weight = ifelse(clase_real == "BAJA+2", 1.0000001, 1.0))

# Veremos en detalle esta función un poco más adelante
ganancia_lgbm  <- function(probs, datos) {
    ## Ingresar su estrategia! acá vamos a ir simplemente buscando la máxima gan de la curva.
    gan <- data.table("pred" = probs,
                                                                 # truco para separar las clases
                    "gan" = ifelse(getinfo(datos, "label") == 1 & getinfo(datos, "weight") > 1, 78000, -2000))
    setorder(gan, -pred)
    gan[, gan_acum :=  cumsum(gan)]
    return(list("name" = "ganancia",
                    "value" = gan[, max(gan_acum)] / 0.2,
                    "higher_better" = TRUE))
}

set.seed(semillas[1])

model_lgbm_cv <- lgb.cv(

    # Configuración del CV:
    data = dtrain,
    nfold = 5,
    stratified = TRUE,

    # Función que va a ser evaluada.
    eval = ganancia_lgbm,

    # Veremos algunos, pero hay muchos más https://lightgbm.readthedocs.io/en/v3.3.2/Parameters.html
    param = list(
        seed= semillas[1],
        objective = "binary",
        metric = "custom",
        boosting = "gbdt",
        boost_from_average = TRUE,
        max_bin = 31,
        use_missing = TRUE,
        max_depth = 12, # -1 = No limitar
        min_data_in_leaf = 4000,
        feature_pre_filter = FALSE, #feature_pre_filter: Evita que LightGBM deje de lado variables que considera malas.
        num_leaves = 100,
        feature_fraction = 0.50, # Porcentaje de columnas que van a usarse en un árbol
        bagging_fraction = 1.0, # % de registros a considerar en cada árbol
        extra_tree = FALSE, # Los puntos de corte los elige al azar.
        lambda_l1 = 0.0,
        lambda_l2 = 0.0,
        min_gain_to_split = 0.0,
        learning_rate =  0.01,

        # Cuántos árboles vamos a generar
        num_iterations = 500, # Debe ser un número muy grande, recordar el double descent!!!.
        early_stopping_rounds = 100 # Corta cuando después de tantos árboles no vio una ganancia mejor a la máxima.
    ),
    verbose = -1
)

model_lgbm_cv$best_iter
model_lgbm_cv$best_score

model_lgbm_cv$record_evals

# Armamos el modelo:
model_lgm <- lightgbm(
    data = dtrain,
    params = list(
        seed = semillas[1],
        objective = "binary",
        boost_from_average = TRUE,
        max_bin = 31,
        max_depth = 12,
        min_data_in_leaf = 4000,
        feature_pre_filter = FALSE,
        num_leaves = 100,
        feature_fraction = 0.50,
        learning_rate = 0.01,
        num_iterations = model_lgbm_cv$best_iter
    ),
    verbose = -1
)

lgb.importance(model_lgm)

## ACTIVIDAD para la clase: Armar una Opt. Bayesiana para el LightGBM.
## Empezaran a recibir cada vez más soporte de código, algo que en la vida de
## a deveras no va a pasar. Mis valientes C1 demustren estar preparados para la
## calle haciendo su propia Opt Bay.