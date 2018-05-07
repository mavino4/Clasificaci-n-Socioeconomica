# Mavino 06/05/2018
# V 1.0

# 
# Funcion que predice a que cluster pertenece un elemento 
# dadas las características
# 

# definiendo los centroides que tendrá la funcion por defecto 
# los mismos Se extrayeron del análisis de conglomerados
# realizado sobre la información de la encuesta de hogares 2017
# para los departamentos de La Paz, y Santa Cruz

centroides = read.csv("/home/marco/Documents/techo/indice de riqueza relativa/center_ClusterLpScz.csv")
centroides = t(centroides[,2:6])


# la primera función "predCluster" devuelve el cluster al que pertencen
# el vector x1 con los valores de los siguientes 5 bienes ajustados por
# la raíz cuadrada de las personas del hogar, en el siguiente orden 
# [refriAdj, compuAdj, tvAdj, lavadoraAdj, autoAdj]
# [   1    ,    2    ,   3  ,      4     ,    5  ]

predCluster<-function(x1,centroid=centroides){
  distancias <- c()
  for (n in c(1,2,3)){
    dist_i<-sqrt(sum((x1-centroides[,n])^2))
    distancias <- append(distancias, dist_i)
  }
  clust = which(distancias == min(distancias) ) 
  # los valores que queremos que nos retorne la función en base al cluster que pertenece
  # pueden ser cambiados en los valores entre comillas despues de cada return
  # TENER CUIDADO DE MANTENER EL ORDEN IDENTIFICADO EN EL ANÁLISIS GENERAL
  if (clust ==1) { return("mid")  }
  if (clust ==2){ return("high")  }
  if (clust ==3){ return("low")  }
  
}


#Probando la función por observación

X <- c(1,1,1,3,1)  
Y <- predCluster(X,centroides)
Y

X <- c(3,3,3,3,4)  
Y <- predCluster(X,centroides)
Y

X <- c(1,2,5,3,4)  
Y <- predCluster(X,centroides)
Y

X <- c(2,2,2.54,1.23,1)  
Y <- predCluster(X,centroides)
Y

# la función "predClusterVector" genera un vector se pronosticos 
# del cluster al que pertenecen del mismo largo de las observaciones 
# ingresadas en el df_valores, el cuál debe ser un data frame con n filas,
# siendo n el número de observaciones, y 5 columnas, en el siguiente orden.  
# [refriAdj, compuAdj, tvAdj, lavadoraAdj, autoAdj]
# [   1    ,    2    ,   3  ,      4     ,    5  ]

predClusterVector <- function(df_Valores,centroids=centroides){
  cluster <- c()
  for ( row in 1:dim(df_Valores)[1]){
    cluster <- append(cluster, predCluster(df_Valores[row,]))
  }
  return(cluster)
}


#Probando la función con un vector de variables
Prueba1 <- matrix(runif(50),10,5)

a = predClusterVector(Prueba1)
a

Prueba1 <- as.data.frame(Prueba1)
Prueba1$Cluster <- a 
Prueba1

