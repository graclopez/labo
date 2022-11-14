#tb_cortes  <-  rbind(list( semilla_particion,
#                                  semilla_modelo,
#                                  corte,
#                                  total,
#                                  public,
#                                  private ) )

plot( x = c(0,0.1,0.15,0.2,0.25,0.3,0.4),
       y = c(49.07,47.95,49.65,48.61,49.13,48.72,49.19),
          main=  paste0( "Estrategia Canaritos"),
          xlab= "Ratio",
          ylab= "Ganancia",
          ylim= c(47.5, 50 ),
          col= "black",
          type= "l",
          )

    lines( x= c(0,0.1,0.15,0.2,0.25,0.3,0.4),
           y= c(49.07,47.95,49.65,48.61,49.13,48.72,49.19),
           col= "blue",
           pch= 15 
          )

    lines( x= c(0,0.1,0.15,0.2,0.25,0.3,0.4),
           y= c(49.07,49.07,49.07,49.07,49.07,49.07,49.07),
           col= "red",
           pch= 15 
          )


    legend("topleft", 
           legend= c("ratio canaritos","sin canaritos"),
           col= c( "blue", "red"),
           lty= c(1,1),
           pch= c(20,15), 
          )
  

