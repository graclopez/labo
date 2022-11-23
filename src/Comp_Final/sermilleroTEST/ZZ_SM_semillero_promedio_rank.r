# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")

# Parametros del script
PARAM <- list()
PARAM$experimento <- "FINAL_ZZ_FT_15meses_RF_SinCan"
PARAM$exp_input <- "FINAL_01_SM_FT_15meses_RF_SinCan"
#PARAM$corte <- 11000 # cantidad de envios
PARAM$cortes  <- seq( from=  7000,
                      to=    15000,
                      by=      500)
# FIN Parametros del script

options(error = function() {
  traceback(20)
  options(error = NULL)
  stop("exiting after script error")
})

base_dir <- "~/buckets/b1/"


# creo la carpeta donde va el experimento
dir.create(paste0(base_dir, "exp/", PARAM$experimento, "/"), showWarnings = FALSE)
setwd(paste0(base_dir, "exp/", PARAM$experimento, "/")) # Establezco el Working Directory DEL EXPERIMENTO


path_experimento_semillerio <- paste0(base_dir, "exp/", PARAM$exp_input)
archivos <- list.files(path = path_experimento_semillerio, pattern = "_resultados.csv")

# Esto es MUY dependiente del formato del nombre de los experimentos, se puede romper muy facil
#ksemillas <- strtoi(sapply(strsplit(archivos, "_"), "[", 3))

vez <- 1
for (archivo in archivos) {
  
  ksemilla <- strtoi(sapply(strsplit(archivo, "_"), "[", 8))
  print(ksemilla)

    
  # cols: numero_de_cliente,foto_mes,prob,rank
  tb_prediccion <- fread(paste0(path_experimento_semillerio, '/', archivo))
  # repara bug en z1292, si se fixea ahi, esto no genera problemas
  tb_prediccion[, rank := frank(-prob, ties.method = "random")]
  
  if (vez == 1) {
    # la primera vez genero el dataset con los numeros de cliente para luego ir joineando los rank por semilla
    tb_ranking_semillerio <- tb_prediccion[, list( numero_de_cliente, rank)]
    setnames(tb_ranking_semillerio, "rank", paste0("rank_", ksemilla))
      

    tb_prob_semillerio <- tb_prediccion[, list( numero_de_cliente, prob)]
    setnames(tb_prob_semillerio, "prob", paste0("prob_", ksemilla))  


    vez <- 2
  } else {
  # Agrego el ranking de la semilla al semillerio
    tb_ranking_semillerio <- merge(tb_ranking_semillerio, tb_prediccion, by.x = "numero_de_cliente", by.y = "numero_de_cliente", all.x = FALSE, all.y=FALSE)
    tb_ranking_semillerio[,foto_mes:=NULL]
    tb_ranking_semillerio[,prob:=NULL]
    setnames(tb_ranking_semillerio, "rank", paste0("rank_", ksemilla))
    
    tb_prob_semillerio <- merge(tb_prob_semillerio, tb_prediccion, by.x = "numero_de_cliente", by.y = "numero_de_cliente", all.x = FALSE, all.y=FALSE)
    tb_prob_semillerio[,foto_mes:=NULL]
    tb_prob_semillerio[,rank:=NULL]
    setnames(tb_prob_semillerio, "prob", paste0("prob_", ksemilla))  
  
  }    
}   

fwrite(  tb_ranking_semillerio,
             file= "ranking_semillerio.csv",
             sep= "," )

fwrite(  tb_prob_semillerio,
             file= "prob_semillerio.csv",
             sep= "," )


# Esta es la predicción del semillerio para la semilla i-esima
tb_prediccion_rank_semillerio <- data.table(
    tb_ranking_semillerio[, list(numero_de_cliente)],
    prediccion = rowMeans(tb_ranking_semillerio[, c(-1)]) # excluye el numero_de_cliente del cálculo de la media
  )

fwrite(  tb_prediccion_rank_semillerio,
             file= "prediccion_semillerio.csv",
             sep= "," )

tb_prediccion_prob_semillerio <- data.table(
    tb_prob_semillerio[, list(numero_de_cliente)],
    prediccion = rowMeans(tb_prob_semillerio[, c(-1)]) # excluye el numero_de_cliente del cálculo de la media
  )

fwrite(  tb_prediccion_prob_semillerio,
             file= "prediccion_prob_semillerio.csv",
             sep= "," )


setorder(tb_prediccion_rank_semillerio, prediccion) # Esto es un ranking, entonces de menor a mayor

setorder(tb_prediccion_prob_semillerio, -prediccion) # Esto es una proba, entonces de mayor a menor


for (corte in PARAM$cortes)
{
    # cortes rank
    tb_prediccion_rank_semillerio[, Predicted := 0]
    tb_prediccion_rank_semillerio[1:corte, Predicted := 1L]

    nom_submit  <- paste0( PARAM$experimento, 
                           "_RANK_",
                           sprintf( "%05d", corte ),
                           ".csv" )

    fwrite(  tb_prediccion_rank_semillerio[ , list( numero_de_cliente, Predicted ) ],
             file= nom_submit,
             sep= "," )

    # cortes probabilidad
    tb_prediccion_prob_semillerio[, Predicted := 0]
    tb_prediccion_prob_semillerio[1:corte, Predicted := 1L]

    nom_submit  <- paste0( PARAM$experimento, 
                           "_PROB_",
                           sprintf( "%05d", corte ),
                           ".csv" )

    fwrite(  tb_prediccion_prob_semillerio[ , list( numero_de_cliente, Predicted ) ],
             file= nom_submit,
             sep= "," )
         

}
  
