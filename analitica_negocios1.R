library('dplyr')
library('ggplot2')
library('readxl')
library('class')
library('ggdendro')
library('tidyverse')
library('rpart')
library('rpart.plot')
library('caret')
library('skimr')
library(readxl)
datos <- read_excel("Base_Score.xlsx")
if (!require('cluster')) install.packages('cluster')
library(dplyr)
library(cluster)
str(datos)
datosi<-datos[datos$SEXO==1 & datos$COMPORTAMIENTO=="BUENO",c("RENTA","EDAD")]
fit_i<-kmeans(datosi,5)
##
plot(datosi, col=factor(fit_i$cluster) )

##
wcss <- vector()
for(i in 1:10){ 
  wcss[i] <- sum(kmeans(datosi, i)$withinss) 
}

# Graficando los resultados M?todo del Codo

ggplot() + 
  geom_point(aes(x = 1:10, y = wcss), color = 'blue') + 
  geom_line(aes(x = 1:10, y = wcss), color = 'blue') + 
  ggtitle("Metodo del Codo") + 
  xlab('Cantidad de Centroides k') + 
  ylab('WCSS')

table(fit_i$cluster)
prop.table(table(fit_i$cluster)) 

datosi$cluster<-fit_i$cluster
datosi %>% group_by(cluster) %>% summarise(media=mean(RENTA),mediana=median(RENTA),Maximo=max(RENTA),Minimo=min(RENTA) )

##

datosi$cluster<-fit_i$cluster
datosi %>% group_by(cluster) %>% summarise(media=mean(EDAD),mediana=median(EDAD),Maximo=max(EDAD),Minimo=min(EDAD) )


##
par(mfrow=c(2,3))
plot(datosi[datosi$cluster==1,c(1,2)], main="Grupo 1: Edad vs Renta")
plot(datosi[datosi$cluster==2,c(1,2)], main="Grupo 2: Edad vs Renta")
plot(datosi[datosi$cluster==3,c(1,2)], main="Grupo 3: Edad vs Renta")
plot(datosi[datosi$cluster==4,c(1,2)], main="Grupo 4: Edad vs Renta")
plot(datosi[datosi$cluster==5,c(1,2)], main="Grupo 5: Edad vs Renta")

par(mfrow=c(1,1))


##

m<-c( cor(datosi[datosi$cluster==1,1],datosi[datosi$cluster==1,2]),
cor(datosi[datosi$cluster==2,1],datosi[datosi$cluster==2,2]),
cor(datosi[datosi$cluster==3,1],datosi[datosi$cluster==3,2]),
cor(datosi[datosi$cluster==4,1],datosi[datosi$cluster==4,2]),
cor(datosi[datosi$cluster==5,1],datosi[datosi$cluster==5,2]) )

names(m)<-c("grupo 1","grupo 2","grupo 3","grupo 4","grupo 5")
m



##
#Grupo1
summary(datosi[datosi$cluster==1,c(1,2)])
skim(datosi[datosi$cluster==1,c(1,2)])
# Edad 
hist(datosi$EDAD[datosi$cluster==1])
plot(density(datosi$EDAD[datosi$cluster==1]))
# Renta
hist(datosi$RENTA[datosi$cluster==1])
plot(density(datosi$RENTA[datosi$cluster==1]))



##
#Grupo1
summary(datosi[datosi$cluster==2,c(1,2)])
skim(datosi[datosi$cluster==2,c(1,2)])
# Edad 
hist(datosi$EDAD[datosi$cluster==2])
plot(density(datosi$EDAD[datosi$cluster==2]))
# Renta
hist(datosi$RENTA[datosi$cluster==2])
plot(density(datosi$RENTA[datosi$cluster==2]))



##
#Grupo3
summary(datosi[datosi$cluster==3,c(1,2)])
skim(datosi[datosi$cluster==3,c(1,2)])
# Edad 
hist(datosi$EDAD[datosi$cluster==3])
plot(density(datosi$EDAD[datosi$cluster==3]))
# Renta
hist(datosi$RENTA[datosi$cluster==3])
plot(density(datosi$RENTA[datosi$cluster==3]))

##
#Grupo4
summary(datosi[datosi$cluster==4,c(1,2)])
skim(datosi[datosi$cluster==4,c(1,2)])
# Edad 
hist(datosi$EDAD[datosi$cluster==4])
plot(density(datosi$EDAD[datosi$cluster==4]))
# Renta
hist(datosi$RENTA[datosi$cluster==4])
plot(density(datosi$RENTA[datosi$cluster==4]))

##
#Grupo3
summary(datosi[datosi$cluster==5,c(1,2)])
skim(datosi[datosi$cluster==5,c(1,2)])
# Edad 
hist(datosi$EDAD[datosi$cluster==5])
plot(density(datosi$EDAD[datosi$cluster==5]))
# Renta
hist(datosi$RENTA[datosi$cluster==5])
plot(density(datosi$RENTA[datosi$cluster==5]))

agrupa <- hclust(dist(datosi, method = 'euclidean'), method = 'ward.D')
clusterk <- cutree(agrupa, k = 5)
datosi$clusterk <- clusterk

##
plot(datosi[,c(1,2)], col=factor(datosi$clusterk) )

## 
datos_reg <- datos %>% 
  filter(SEXO == 1) %>% 
  filter(COMPORTAMIENTO == "MALO") %>% 
  filter(TIPO_NAC == "C") %>% 
  select(RENTA,
         EDAD)


modelo <- lm(RENTA ~ EDAD, data = datos_reg)
summary(modelo)
round(modelo$coefficients,5)


##

datos_cart <- datos %>% 
  filter(SEXO == 1) %>% 
  filter(COMPORTAMIENTO == "MALO") %>% 
  filter(TIPO_NAC == "C") %>% 
  select(-(`ID CLIENTE`))



# Identifica NA -----------------------------------------------------------

datos_cart %>% map_dbl(.f = function(x){sum(is.na(x))})


# Data train --------------------------------------------------------------

set.seed(1000) 					#.- Cualquier semilla
train <- sample_frac(datos_cart, .8) 	# Se extrae el 80% de la muestra
valida <- setdiff(datos_cart, train)

train %>% 
  count(GSE)

valida %>% 
  count(GSE)

train <- train %>% 
  filter(GSE != 'NA')

valida <- valida %>% 
  filter(GSE != 'NA')




## --------------------------------------------------------------------------------------------------
# Entrenando arbol -------------------------------------------------------

arbol <- rpart(formula = factor(train$GSE) ~ . , data = train)
summary(arbol)

## --------------------------------------------------------------------------------------------------
# Evaluando el modelo -----------------------------------------------------

rpart.plot(arbol)


## --------------------------------------------------------------------------------------------------


pred <- predict(arbol, newdata = valida, type = "class")


## 

confusionMatrix(pred, factor(valida$GSE))

