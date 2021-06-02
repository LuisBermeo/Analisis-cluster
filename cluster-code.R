###################################################################################
#                         Bermeo Fernández Luis Antonio                           #
#                             Econometría Aplicada                                #
#                                     Tarea 3                                     #
#                                   Clustering                                    #
###################################################################################

#cargamos la base del modelo 


setwd("D:/Luis S. Gott/ENOE/cluster")

install.packages("haven")
library(haven)

install.packages("readxl")
library(readxl)

install.packages("xlsx")
library(xlsx)

modelo<-read_excel("modelo.xlsx", col_names=TRUE)

dindiceP<-modelo$ind20-modelo$ind19


x<-modelo[,2:3]
x<-cbind(x,dindiceP)
row.names(x)<-modelo$entidad

#verificar que x sea data.frame

df<-scale(x) #estandariza la base de datos (quita la media y divide entre el rango)

#instalamos la paqueteria que nos servira para realizar los cluster 

install.packages("factoextra")

library(factoextra)

#encontramos el numero de cluster optimos mediante cross-validation 
fviz_nbclust(df,kmeans, method = "wss")

#encontramos que el minimo está en 8 clusters

fviz_nbclust(df,kmeans, method = "wss")+
  geom_vline(xintercept = 8, linetype=2)

#realizamos cluster usando k=8
#usamos kmeans y después damos un resumen del proceso con print

set.seed(123)
km.1=kmeans(df,8, nstart = 20)
print(km.1)

#calculamos la media de cada variable del cluster 

aggregate(x, by=list(cluster=km.1$cluster), mean)

#pegamos el cluster

dd=cbind(x,cluster=km.1$cluster)

#calculamos centros y tamaños 

km.1$size
head(km.1$cluster)
km.1$centers

#visualización de datos, recordad que este enfoque es mediante PCA 
#si no quieres usar PCA lo que púedes hacer es realizar cluster por separado de 2 en 2

fviz_cluster(km.1, data = df, ellipse.type = "euclid", star.plot=TRUE, repel = TRUE, ggtheme = theme_minimal())

#Realizamos el mismo proceso pero ahora solo usamos 4 clusters 


set.seed(123)
km.2=kmeans(df,4, nstart = 20)
print(km.2)

aggregate(x, by=list(cluster=km.2$cluster), mean)

dd2

km.2$size
head(km.2$cluster)
km.2$centers

fviz_cluster(km.2, data = df, ellipse.type = "euclid", star.plot=TRUE, repel = TRUE, ggtheme = theme_minimal())

res.k2<-prcomp(df, scale=TRUE)

fviz_contrib(res.k2, choice = "var", axes= 2)

########################################################################

#Realizar cluster sin PCA 
#empleo salario

km.3=kmeans(df[,1:2],4, nstart = 20)

p<-fviz_cluster(km.3, data = df[,1:2], ellipse.type = "euclid", star.plot=TRUE, repel = TRUE, ggtheme = theme_minimal())
p<-p+xlab("cambio del empleo")+ylab("cambio en el salario")
p

#empleo indice de pobreza 

km.4=kmeans(df[,c(1,3)],4, nstart = 20)

p<-fviz_cluster(km.4, data = df[,c(1,3)], ellipse.type = "euclid", star.plot=TRUE, repel = TRUE, ggtheme = theme_minimal())
p<-p+xlab("cambio del empleo")+ylab("cambio en el índice de pobreza laboral")
p

#salario indice de pobreza 

km.5=kmeans(df[,2:3],4, nstart = 20)

p<-fviz_cluster(km.5, data = df[,2:3], ellipse.type = "euclid", star.plot=TRUE, repel = TRUE, ggtheme = theme_minimal())
p<-p+xlab("cambio del salario")+ylab("cambio en índice de pobreza laboral")
p

