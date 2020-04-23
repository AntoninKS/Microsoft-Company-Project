library(gsheet)
library(cluster)
library(fpc)
library(tidyverse)
library(factoextra)
library(gridExtra)
library(factoextra)
library(ggplot2)
library(dplyr)
library(recommenderlab)
library(lsa)
library(normalr)

mydata <- read.csv("AceSales_Database  - Sheet1.csv", header = TRUE, sep = ",")

mydata_train <- mydata[1:448,]
mydata_test <-mydata[449:500,]

##########################################################################
#Plot each category

mydata_train[1:2] <- NULL
head(mydata_train)

ggplot(mydata_train, aes(x = mydata_train$Gin_Beefeature.Usual, y = mydata_train$Gin_Beefeature24.Premium, color = mydata_train$Type)) + geom_point(position = "jitter", alpha  = 0.6)

ggplot(mydata_train, aes(x = mydata_train$Whisky_Campbell.Usual, y = mydata_train$Whisky_Shivas.12y...Premium, color = mydata_train$Type)) + geom_point(position = "jitter", alpha  = 0.6)

ggplot(mydata_train, aes(x = mydata_train$Vodka_Absolut.Usual, y = mydata_train$Vodka_AbsolutElyx.Premium, color = mydata_train$Type)) + geom_point(position = "jitter", alpha  = 0.6)

##########################################################################

# Clustering with Dendogram
dd <- dist(mydata_train, method = "euclidean")
hc <- hclust(dd, method = "ward.D2")

plot(hc, labels = mydata_train$Type)

##########################################################################

#Clustering with K-Means
mydata_cluster <- mydata_train
mydata_cluster[1:2] <- NULL

mydata_scale <- mydata_cluster[2:10]
mydata_type <- mydata_cluster[1]
mydata_scale <- scale(mydata_scale)

mydata_final <-cbind(mydata_type, mydata_scale)
                            
set.seed(123)
k.max <- 15
wss <- sapply(1:k.max, 
              function(k){kmeans(mydata_final[2:10], k, nstart=50,iter.max = 15 )$tot.withinss})
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

set.seed(123)
kmeans2 <- kmeans(mydata_final[2:10], 4, nstart = 25)
print(kmeans2)

fviz_cluster(kmeans2, data = mydata_final[2:10], geom = "point", main = "Companies Cluster")

##########################################################################

#RECOMMENDER SYSTEM

############################### TEST 1 ###################################
# mydata[is.na(mydata)] <- 0
# mydata_test[1:2] <- NULL
# mydata[1:2] <- NULL
# mydata[2] <- NULL
# 
# affinity.matrix<- as(mydata,"realRatingMatrix")
# 
# rec <- Recommender(affinity.matrix, method = "IBCF")
# rec
# getModel(rec)
# 
# pre <- predict(rec, 449:500 , data = affinity.matrix, type="ratings")
# pre2 <- predict(rec, 449:500 , data = affinity.matrix, n= 1)
# 
# pre
# pre2
# as(pre2, "list")
# as(pre2, "matrix")[,1:1]

############################### TEST 2 ###################################

mydata2 <- read.csv("AceSales_Database  - Sheet1.csv", header = TRUE, sep = ",")
mydata2[is.na(mydata2)] <- 0
mydata2[1:2] <- NULL

item_sim = cosine(as.matrix(mydata2[4:12]))

mydata3 <- mydata2
mydata3 <- mydata3[1:448,]
item_sim2 = cosine(as.matrix(mydata3[4:12]))
reg_lin2 <- lm(mydata3$Champange.Perrier.Jouêt ~ mydata3$Gin_Beefeature24.Premium, data = mydata3)
plot(Champange.Perrier.Jouêt ~ Gin_Beefeature24.Premium, data = mydata3)
abline(reg_lin, col = "red")
reg_mul3 <- lm(mydata3$Champange.Perrier.Jouêt ~ mydata3$Whisky_Campbell.Usual + mydata3$Whisky_Shivas.12y...Premium + mydata3$Vodka_Absolut.Usual + mydata3$Vodka_AbsolutElyx.Premium + mydata3$Gin_Beefeature.Usual + mydata3$Gin_Beefeature24.Premium, data = mydata3)
abline(reg_mul3, col ="blue")
print(reg_lin2)

#Simple linear regression
reg_lin <- lm(mydata2$Champange.Perrier.Jouêt ~ mydata2$Gin_Beefeature24.Premium, data = mydata2)
plot(Champange.Perrier.Jouêt ~ jitter(Gin_Beefeature24.Premium, 1), data = mydata2, xlab = "Premium GIN", ylab = "Champagne")
abline(reg_lin, col = "red")

#Multiple linear regression
reg_mul <- lm(mydata2$Champange.Perrier.Jouêt ~ mydata2$Whisky_Campbell.Usual + mydata2$Whisky_Shivas.12y...Premium + mydata2$Vodka_Absolut.Usual + mydata2$Vodka_AbsolutElyx.Premium + mydata2$Gin_Beefeature.Usual + mydata2$Gin_Beefeature24.Premium, data = mydata2)
abline(reg_mul, col ="blue")

summary(reg_lin)
summary(reg_mul)

str(reg_lin)
str(reg_mul)

print(reg_lin)
print(reg_mul)