require(data.table)

memory.limit(size = 10000)

# Importando la Base de datos. 

equipamiento = read.csv("G:/backup 13-04-12/MaRcO/TECHO/indice de riqueza relativa/CSV/EH2017_Equipamiento.csv", sep =";")
personas = read.csv("G:/backup 13-04-12/MaRcO/TECHO/indice de riqueza relativa/CSV/EH2017_Persona.csv", sep =";")

#seleccionando los items
equipamiento <- equipamiento[which( equipamiento$ITEM != 1),]
equipamiento <- equipamiento[which( equipamiento$ITEM != 2),]
equipamiento <- equipamiento[which( equipamiento$ITEM != 3),]
equipamiento <- equipamiento[which( equipamiento$ITEM != 6),]
equipamiento <- equipamiento[which( equipamiento$ITEM != 7),]
equipamiento <- equipamiento[which( equipamiento$ITEM != 10),]
names(equipamiento) <- c("folio","depto","ITEM", "tiene", "cuantos", "s10c_16", "s10c_17", "upm","estrato","factor")  
equipamiento <- equipamiento[,c("folio", "depto"  ,"ITEM", "tiene", "cuantos","factor")]
head(equipamiento)

#Hallando el número de personas por hogar 
personas <- personas[,c(1,2,4)]
names(personas) <- c("folio","nro","area")
personas <- as.data.table(personas)
personas <- personas[personas[, .I[nro == max(nro)], by=folio]$V1]
personas$sqrtNro <- sqrt(personas$nro)
head(personas)

#Uniendo las bases de datos
general <- personas

refri <- equipamiento[which(equipamiento$ITEM == 4),1:5]
general$refri <- refri$cuantos

compu <- equipamiento[which(equipamiento$ITEM == 5),1:5]
general$compu <- compu$cuantos

tv <- equipamiento[which(equipamiento$ITEM == 4),1:5]
general$tv <- tv$cuantos

lavadora <- equipamiento[which(equipamiento$ITEM == 4),1:5]
general$lavadora <- lavadora$cuantos

auto <- equipamiento[which(equipamiento$ITEM == 4),]
general$auto <- auto$cuantos
general$factor <- auto$factor
general$depto <- auto$depto

rm(auto, compu, lavadora, refri, tv, equipamiento, personas)

head(general)


#Quedandonos solo con las observaciones SCZ y LP
general_LpScz <- general[which( general$depto == 2 | general$depto == 7 ),]
general_LpScz[is.na(general_LpScz)] <- 0

general_OtrosDept <- general[which(general$depto != 2 & general$depto !=7),]
general_OtrosDept[is.na(general_OtrosDept)] <- 0


head(general_LpScz)


bd_LpScz <- general_LpScz[rep(seq_len(nrow(general_LpScz)), general_LpScz$factor),]
bd_OtrosDept <- general_OtrosDept[rep(seq_len(nrow(general_OtrosDept)), general_OtrosDept$factor),]

# Generando variables ajustadas
# Para LP y Santa cruz
bd_LpScz_clust <- bd_LpScz
bd_LpScz_clust$refriAdj <- bd_LpScz$refri/bd_LpScz$sqrtNro
bd_LpScz_clust$compuAdj <- bd_LpScz$compu/bd_LpScz$sqrtNro
bd_LpScz_clust$tvAdj <- bd_LpScz$tv / bd_LpScz$sqrtNro
bd_LpScz_clust$lavadoraAdj <- bd_LpScz$lavadora / bd_LpScz$sqrtNro
bd_LpScz_clust$autoAdj <- bd_LpScz$auto / bd_LpScz$sqrtNro

head(bd_LpScz_clust)
bd_LpScz_clust<- bd_LpScz_clust[,12:16]

#para los demás departamentos

bd_OtrosDept_clust <- bd_OtrosDept
bd_OtrosDept_clust$refriAdj <- bd_OtrosDept$refri/bd_OtrosDept$sqrtNro
bd_OtrosDept_clust$compuAdj <- bd_OtrosDept$compu/bd_OtrosDept$sqrtNro
bd_OtrosDept_clust$tvAdj <- bd_OtrosDept$tv / bd_OtrosDept$sqrtNro
bd_OtrosDept_clust$lavadoraAdj <- bd_OtrosDept$lavadora / bd_OtrosDept$sqrtNro
bd_OtrosDept_clust$autoAdj <- bd_OtrosDept$auto / bd_OtrosDept$sqrtNro
head(bd_OtrosDept_clust)

bd_OtrosDept_clust<- bd_OtrosDept_clust[,12:16]
head(bd_OtrosDept_clust)


# Desarrolando el clustering para LP y santa cruz
#set.seed(12345)
#i_clust.LP <- kproto(bd_LpScz_clust,4)
#i_clust.tableLP <- table(i_clust.LP$cluster)
#round(i_clust.tableLP/nrow(i_input)*100,2)


set.seed(123456)
clusterLpScz <- kmeans(bd_LpScz_clust, 3)
clusterLpScz.table <- table(clusterLpScz$cluster)
round(clusterLpScz.table/nrow(bd_LpScz_clust)*100,2)



#para los demas deparatamentos 
set.seed(123456)
clusterOtros <- kmeans(bd_OtrosDept_clust, 3)
clusterOtros.table <- table(clusterOtros$cluster)
round(clusterOtros.table/nrow(bd_OtrosDept_clust)*100,2)


#realizando la separación por áreas

general$refriAdj <- general$refri/general$sqrtNro
general$compuAdj <- general$compu/general$sqrtNro
general$tvAdj <- general$tv / general$sqrtNro
general$lavadoraAdj <- general$lavadora / general$sqrtNro
general$autoAdj <- general$auto / general$sqrtNro
head(general)


#Ampliando en base al factor de expansión.  
general_factor <- general[rep(seq_len(nrow(general)), general$factor),]


#Separando por áreas
general_Urbano <- general_factor[which( general_factor$area == 1),]
general_Urbano[is.na(general_Urbano)] <- 0
head(general_Urbano)
bd_clusterUrbano <- general_Urbano[, 12:16]
head(bd_clusterUrbano)

general_Rural <- general_factor[which( general_factor$area == 2),]
general_Rural[is.na(general_Rural)] <- 0
head(general_Rural)
bd_clusterRural <- general_Rural[, 12:16]
head(bd_clusterRural)

#Cluster por áreas
set.seed(123456)
clusterUrbano <- kmeans(bd_clusterUrbano, 3)
clusterUrbano.table <- table(clusterUrbano$cluster)
round(clusterUrbano.table/nrow(bd_clusterUrbano)*100,2)

clusterRural <- kmeans(bd_clusterRural, 3)
clusterRural.table <- table(clusterRural$cluster)
round(clusterRural.table/nrow(bd_clusterRural)*100,2)

####################################################
##Adjuntado clusters RURAL al data set expandido ###
####################################################

general_Rural$cluster <- as.factor(clusterRural$cluster) 
levels(general_Rural$cluster) <- c("A", "B", "C")

#Para esta diferenciación el grupo A, nivel medio; B, nivel bajo; C, nivel alto.
levels(general_Rural$cluster) <- c("mid", "low", "high")

#Refri
tapply(general_Rural$refriAdj, general_Rural$cluster, summary)
ggplot(general_Rural, aes(x=cluster, y=refriAdj, fill=cluster)) + 
  geom_boxplot()

round(prop.table(table(general_Rural$refri, general_Rural$cluster), margin =2),2)
ggplot(general_Rural, aes(x=refri, fill=cluster)) + 
  geom_bar(aes(y=..prop.., group = cluster),position=position_dodge())

#TV
tapply(general_Rural$tvAdj, general_Rural$cluster, summary)
ggplot(general_Rural, aes(x=cluster, y=tvAdj, fill=cluster)) + 
  geom_boxplot()

round(prop.table(table(general_Rural$tv, general_Rural$cluster), margin =2),2)
ggplot(general_Rural, aes(x=tv, fill=cluster)) + 
  geom_bar(aes(y=..prop.., group = cluster),position=position_dodge())

#Compu
tapply(general_Rural$compuAdj, general_Rural$cluster, summary)
ggplot(general_Rural, aes(x=cluster, y=compuAdj, fill=cluster)) + 
  geom_boxplot()

round(prop.table(table(general_Rural$compu, general_Rural$cluster), margin =2),2)
ggplot(general_Rural, aes(x=compu, fill=cluster)) + 
  geom_bar(aes(y=..prop.., group = cluster),position=position_dodge())

#Lavadora
tapply(general_Rural$lavadoraAdj, general_Rural$cluster, summary)
ggplot(general_Rural, aes(x=cluster, y=lavadoraAdj, fill=cluster)) + 
  geom_boxplot()

round(prop.table(table(general_Rural$lavadora, general_Rural$cluster), margin =2),2)
ggplot(general_Rural, aes(x=lavadora, fill=cluster)) + 
  geom_bar(aes(y=..prop.., group = cluster),position=position_dodge())

#Auto 
tapply(general_Rural$autoAdj, general_Rural$cluster, summary)
ggplot(general_Rural, aes(x=cluster, y=autoAdj, fill=cluster)) + 
  geom_boxplot()

round(prop.table(table(general_Rural$auto, general_Rural$cluster), margin =2),2)
ggplot(general_Rural, aes(x=auto, fill=cluster)) + 
  geom_bar(aes(y=..prop.., group = cluster),position=position_dodge())






###########################################################
##Adjuntado clusters Urbano al data set expandido ######### 
###########################################################
general_Urbano$cluster <- as.factor(clusterUrbano$cluster) 
levels(general_Urbano$cluster) <- c("A", "B", "C")

#Para esta diferenciación el grupo A, nivel bajo; B, nivel medio; C, nivel alto.
levels(general_Urbano$cluster) <- c("low", "mid", "high")

#Refri
tapply(general_Urbano$refriAdj, general_Urbano$cluster, summary)
ggplot(general_Urbano, aes(x=cluster, y=refriAdj, fill=cluster)) + 
  geom_boxplot()

round(prop.table(table(general_Urbano$refri, general_Urbano$cluster), margin =2),2)
ggplot(general_Urbano, aes(x=refri, fill=cluster)) + 
  geom_bar(aes(y=..prop.., group = cluster),position=position_dodge())

#TV
tapply(general_Urbano$tvAdj, general_Urbano$cluster, summary)
ggplot(general_Urbano, aes(x=cluster, y=tvAdj, fill=cluster)) + 
  geom_boxplot()

round(prop.table(table(general_Urbano$tv, general_Urbano$cluster), margin =2),2)
ggplot(general_Urbano, aes(x=tv, fill=cluster)) + 
  geom_bar(aes(y=..prop.., group = cluster),position=position_dodge())

#Compu
tapply(general_Urbano$compuAdj, general_Urbano$cluster, summary)
ggplot(general_Urbano, aes(x=cluster, y=compuAdj, fill=cluster)) + 
  geom_boxplot()

round(prop.table(table(general_Urbano$compu, general_Urbano$cluster), margin =2),2)
ggplot(general_Urbano, aes(x=compu, fill=cluster)) + 
  geom_bar(aes(y=..prop.., group = cluster),position=position_dodge())

#Lavadora
tapply(general_Urbano$lavadoraAdj, general_Urbano$cluster, summary)
ggplot(general_Urbano, aes(x=cluster, y=lavadoraAdj, fill=cluster)) + 
  geom_boxplot()

round(prop.table(table(general_Urbano$lavadora, general_Urbano$cluster), margin =2),2)
ggplot(general_Urbano, aes(x=lavadora, fill=cluster)) + 
  geom_bar(aes(y=..prop.., group = cluster),position=position_dodge())

#Auto 
tapply(general_Urbano$autoAdj, general_Urbano$cluster, summary)
ggplot(general_Urbano, aes(x=cluster, y=autoAdj, fill=cluster)) + 
  geom_boxplot()

round(prop.table(table(general_Urbano$auto, general_Urbano$cluster), margin =2),2)
ggplot(general_Urbano, aes(x=auto, fill=cluster)) + 
  geom_bar(aes(y=..prop.., group = cluster),position=position_dodge())






###########################################################
##Adjuntado clusters LP SCZ al data set expandido ######### 
###########################################################
bd_LpScz = cbind(bd_LpScz, bd_LpScz_clust)
bd_LpScz$cluster <- as.factor(clusterLpScz$cluster) 
levels(bd_LpScz$cluster) <- c("A", "B", "C")

#Para esta diferenciación el grupo A, nivel bajo; B, nivel medio; C, nivel alto.
levels(bd_LpScz$cluster) <- c("mid", "high", "low")

#Refri
tapply(bd_LpScz$refriAdj, bd_LpScz$cluster, summary)
ggplot(bd_LpScz, aes(x=cluster, y=refriAdj, fill=cluster)) + 
  geom_boxplot()

round(prop.table(table(bd_LpScz$refri, bd_LpScz$cluster), margin =2),2)
ggplot(bd_LpScz, aes(x=refri, fill=cluster)) + 
  geom_bar(aes(y=..prop.., group = cluster),position=position_dodge())

#TV
tapply(bd_LpScz$tvAdj, bd_LpScz$cluster, summary)
ggplot(bd_LpScz, aes(x=cluster, y=tvAdj, fill=cluster)) + 
  geom_boxplot()

round(prop.table(table(bd_LpScz$tv, bd_LpScz$cluster), margin =2),2)
ggplot(bd_LpScz, aes(x=tv, fill=cluster)) + 
  geom_bar(aes(y=..prop.., group = cluster),position=position_dodge())

#Compu
tapply(bd_LpScz$compuAdj, bd_LpScz$cluster, summary)
ggplot(bd_LpScz, aes(x=cluster, y=compuAdj, fill=cluster)) + 
  geom_boxplot()

round(prop.table(table(bd_LpScz$compu, bd_LpScz$cluster), margin =2),2)
ggplot(bd_LpScz, aes(x=compu, fill=cluster)) + 
  geom_bar(aes(y=..prop.., group = cluster),position=position_dodge())

#Lavadora
tapply(bd_LpScz$lavadoraAdj, bd_LpScz$cluster, summary)
ggplot(bd_LpScz, aes(x=cluster, y=lavadoraAdj, fill=cluster)) + 
  geom_boxplot()

round(prop.table(table(bd_LpScz$lavadora, bd_LpScz$cluster), margin =2),2)
ggplot(bd_LpScz, aes(x=lavadora, fill=cluster)) + 
  geom_bar(aes(y=..prop.., group = cluster),position=position_dodge())

#Auto 
tapply(bd_LpScz$autoAdj, bd_LpScz$cluster, summary)
ggplot(bd_LpScz, aes(x=cluster, y=autoAdj, fill=cluster)) + 
  geom_boxplot()

round(prop.table(table(bd_LpScz$auto, bd_LpScz$cluster), margin =2),2)
ggplot(bd_LpScz, aes(x=auto, fill=cluster)) + 
  geom_bar(aes(y=..prop.., group = cluster),position=position_dodge())






###########################################################
##Adjuntado clusters otros deptos al data set expandido ######### 
###########################################################
bd_OtrosDept = cbind(bd_OtrosDept, bd_OtrosDept_clust)
bd_OtrosDept$cluster <- as.factor(clusterOtros$cluster) 
levels(bd_OtrosDept$cluster) <- c("low", "hig", "mid")

#Para esta diferenciación el grupo A, nivel bajo; B, nivel medio; C, nivel alto.
#levels(bd_OtrosDept$cluster) <- c("mid", "high", "low")

#Refri
tapply(bd_OtrosDept$refriAdj, bd_OtrosDept$cluster, summary)
ggplot(bd_OtrosDept, aes(x=cluster, y=refriAdj, fill=cluster)) + 
  geom_boxplot()

round(prop.table(table(bd_OtrosDept$refri, bd_OtrosDept$cluster), margin =2),2)
ggplot(bd_OtrosDept, aes(x=refri, fill=cluster)) + 
  geom_bar(aes(y=..prop.., group = cluster),position=position_dodge())

#TV
tapply(bd_OtrosDept$tvAdj, bd_OtrosDept$cluster, summary)
ggplot(bd_OtrosDept, aes(x=cluster, y=tvAdj, fill=cluster)) + 
  geom_boxplot()

round(prop.table(table(bd_OtrosDept$tv, bd_OtrosDept$cluster), margin =2),2)
ggplot(bd_OtrosDept, aes(x=tv, fill=cluster)) + 
  geom_bar(aes(y=..prop.., group = cluster),position=position_dodge())

#Compu
tapply(bd_OtrosDept$compuAdj, bd_OtrosDept$cluster, summary)
ggplot(bd_OtrosDept, aes(x=cluster, y=compuAdj, fill=cluster)) + 
  geom_boxplot()

round(prop.table(table(bd_OtrosDept$compu, bd_OtrosDept$cluster), margin =2),2)
ggplot(bd_OtrosDept, aes(x=compu, fill=cluster)) + 
  geom_bar(aes(y=..prop.., group = cluster),position=position_dodge())

#Lavadora
tapply(bd_OtrosDept$lavadoraAdj, bd_OtrosDept$cluster, summary)
ggplot(bd_OtrosDept, aes(x=cluster, y=lavadoraAdj, fill=cluster)) + 
  geom_boxplot()

round(prop.table(table(bd_OtrosDept$lavadora, bd_OtrosDept$cluster), margin =2),2)
ggplot(bd_OtrosDept, aes(x=lavadora, fill=cluster)) + 
  geom_bar(aes(y=..prop.., group = cluster),position=position_dodge())

#Auto 
tapply(bd_OtrosDept$autoAdj, bd_OtrosDept$cluster, summary)
ggplot(bd_OtrosDept, aes(x=cluster, y=autoAdj, fill=cluster)) + 
  geom_boxplot()

round(prop.table(table(bd_OtrosDept$auto, bd_OtrosDept$cluster), margin =2),2)
ggplot(bd_OtrosDept, aes(x=auto, fill=cluster)) + 
  geom_bar(aes(y=..prop.., group = cluster),position=position_dodge())

