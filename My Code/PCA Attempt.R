library(devtools)
install_github("vqv/ggbiplot")

library(dplyr)
getwd()
shannonall <- read.csv("Shannon Wiener Merged All Data.csv")

shannonall <- shannonall[,c(1,2,3,4,6,10,23,24)]
View(shannonall)

idcol1 <- character(681)
for (i in 1:681) {
  idcol1[i] <- paste(shannonall$Dock[i],shannonall$Mat[i],shannonall$Shell[i])
}
idcol1

shannonall <- cbind(idcol1, shannonall)
View(shannonall)
str(shannonall)
shannonall$Date <- as.character(shannonall$Date)
shannonall$Date<- mdy(shannonall$Date)
str(shannonall)

shannonall <- shannonall[,-c(2,3,4)]
View(shannonall)

#Now let's do a PCA for shannonall

shannonall_pca <- prcomp(shannonall[,c(2,3,4)], center = TRUE,scale. = TRUE)
summary(shannonall_pca)
str(shannonall_pca)

library(ggbiplot)
ggbiplot(shannonall_pca)
ggbiplot(shannonall_pca, labels = shannonall$Season)

#Now let's do a PCA for the CompleteWaterandPercents data

read.csv("CompleteWaterandPercents.csv")
View(CompleteWaterandPercents)
CompleteWaterandPercents <- na.omit(CompleteWaterandPercents)
CompleteWaterandPercents_pca <- prcomp(CompleteWaterandPercents[,-c(1,2,3,21,22,23,24)], center = TRUE,scale. = TRUE)
summary(CompleteWaterandPercents_pca)
str(CompleteWaterandPercents_pca)

library(ggbiplot)
ggbiplot(CompleteWaterandPercents_pca)
ggbiplot(CompleteWaterandPercents_pca, labels = CompleteWaterandPercents$Season)
