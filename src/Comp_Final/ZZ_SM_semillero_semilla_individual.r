# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")

# Parametros del script
PARAM <- list()
PARAM$experimento <- "FINAL_01_ZZ_PorSemilla_RF_SinCan"
PARAM$exp_input <- "FINAL_01_SM_FT_15meses_RF_SinCan"

#PARAM$corte <- 11000 # cantidad de envios
PARAM$cortes  <- c(7000,9000,11000,15000)
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
  
  setorder(tb_prediccion, rank) # Esto es un ranking, entonces de menor a mayor

  for (corte in PARAM$cortes)
  {
    tb_prediccion[, Predicted := 0]
    tb_prediccion[1:corte, Predicted := 1L]

    nom_submit  <- paste0( PARAM$experimento, 
                           "_",
                           sprintf( "%05d", ksemilla ),
                           "_",
                           sprintf( "%05d", corte ),
                           ".csv" )

        fwrite(tb_prediccion[, list(numero_de_cliente, Predicted)] ,
             file= nom_submit,
             sep= "," )

  }
  



}   

