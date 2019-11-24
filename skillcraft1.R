library("MASS")
library("caret")
library("dplyr")
library("FactoMineR")
library("cluster")
library(rgl)
library("RColorBrewer")
library("ggplot2")
library("gridExtra")
library("grid")
library(neuralnet)
library(class)

# clear all previous variables and clear console
rm(list = ls())
cat("\014")

#2

scDf = read.csv(
  "C:/Users/Didou/Documents/A2-S2-LORMEAU/Analyse de données/skillcraft1/SkillCraft1_dataset.csv"
)

scDf[scDf == "?"] <- NA
scDf <- scDf[, -1]

#2.1

# summary(scDf)
#
# print(scDf[rowSums(is.na(scDf)) > 0,],max = 100000)


# for (col in colnames(scDf)) {
#   print(paste(col , " is of type : " , class(scDf[[col]])))
# }

str(scDf)

for (col in colnames(scDf)) {
  if (class(scDf[[col]]) == "factor") {
    scDf[[col]] <- as.numeric(levels(scDf[[col]]))[scDf[[col]]]
  }
  else if (class(scDf[[col]]) == "integer"  &&
           col != "LeagueIndex") {
    scDf[[col]] <- as.numeric(scDf[[col]])
  }
}

scDf$LeagueIndex <- factor(scDf$LeagueIndex)

#2.2

scDf[which(scDf$LeagueIndex == 8), 2] <-
  median(scDf$Age[scDf$LeagueIndex == 7])
scDf[which(scDf$LeagueIndex == 8), 3] <-
  mean(scDf$HoursPerWeek[scDf$LeagueIndex == 7])
scDf[which(scDf$LeagueIndex == 8), 4] <-
  mean(scDf$TotalHours[scDf$LeagueIndex == 7])

scDf$HoursPerWeek[which(is.na(scDf$HoursPerWeek))] <-
  median(scDf$HoursPerWeek[scDf$LeagueIndex == 5], na.rm = TRUE)
scDf$TotalHours[which(is.na(scDf$TotalHours))] <-
  median(scDf$TotalHours[scDf$LeagueIndex == 5], na.rm = TRUE)


#2.3

# summary(scDf)
# for (col in colnames(scDf)) {
#   if(col!="LeagueIndex"){
#     boxplot(scDf[[col]], xlab = paste("boxplot of :", col))
#   }
# }
# boxplot(scale(scDf[-1794,-1]))


# On constate la présence de valeurs aberrantes dans le nombre d'heures par semaine et le nombre total d'heures, aussi bien trop basses que trop hautes

lowIndex = round(0.005 * 3395)
topIndex = round(0.995 * 3395)

for (col in colnames(scDf)) {
  if (col  != "Age") {
    scDf <- scDf[order(scDf[[col]]), ]
    scDf[1:lowIndex, col] <- scDf[lowIndex, col]
    scDf[topIndex:3395, col] <- scDf[topIndex, col]
  }
}

# boxplot(scale(scDf[,-1]))


# for (i in 2:19) {
#   bronze <- scDf[which(scDf$LeagueIndex == 1),]
#   p1 <-
#     ggplot(bronze, aes(bronze[,i])) + geom_histogram()
#   silver <- scDf[which(scDf$LeagueIndex == 2),]
#   p2 <-
#     ggplot(silver, aes(silver[, i],)) + geom_histogram()
#   gold <- scDf[which(scDf$LeagueIndex == 3),]
#   p3 <-
#     ggplot(gold, aes(gold[, i])) + geom_histogram()
#   platinum <- scDf[which(scDf$LeagueIndex == 4),]
#   p4 <-
#     ggplot(platinum, aes(platinum[, i])) + geom_histogram()
#   diamond <- scDf[which(scDf$LeagueIndex == 5),]
#   p5 <-
#     ggplot(diamond, aes(diamond[,i])) + geom_histogram()
#   master <- scDf[which(scDf$LeagueIndex == 6),]
#   p6 <-
#     ggplot(master, aes(master[,i])) + geom_histogram()
#   grandMaster <- scDf[which(scDf$LeagueIndex == 7),]
#   p7 <-
#     ggplot(grandMaster, aes(grandMaster[,i])) + geom_histogram()
#   professionnal <- scDf[which(scDf$LeagueIndex == 8),]
#   p8 <-
#     ggplot(professionnal, aes(professionnal[,i])) + geom_histogram()
#   ag <- arrangeGrob(p1, p2, p3, p4, p5, p6, p7, p8, ncol = 4,top=textGrob(paste(colnames(scDf)[i]),gp=gpar(fontsize=20,font=3)))
#   ggsave(file=paste(colnames(scDf)[i],".png",sep = ""), ag)
# }

#3

# 3.1 Univariée

# for (col in colnames(scDf)) {
#   if (class(scDf[[col]]) == "numeric") {
#     barplot(table(scDf[[col]]), xlab = col)
#   }
#   else{
#     plot(table(scDf[[col]]), xlab = col)
#   }
# }

#  3.2 Bivariée

# corMatrix <- matrix(nrow = 18, ncol = 18)
#
# for (i in 2:19) {
#   for (j in 2:19) {
#     if (i > j) {
#       tempCor = cor.test(scDf[, i], scDf[, j], conf.level = 0.99)
#       if ((tempCor$estimate > 0.5 ||
#            tempCor$estimate < -0.5) && tempCor$p.value < 0.01) {
#         corMatrix[i - 1, j - 1] <- tempCor$estimate
#       }
#     }
#   }
# }
#
# rownames(corMatrix) <- colnames(scDf[, -1])
# colnames(corMatrix) <- colnames(scDf[, -1])
# corMatrix <-
#   corMatrix[rowSums(is.na(corMatrix)) != ncol(corMatrix), colSums(is.na(corMatrix)) != ncol(corMatrix)]
# print(corMatrix)
# pvalues <- cor.mtest(corMatrix)
# corrplot(corMatrix$correlations, p.mat = corMatrix$tests, sig.level = 0.01)


trainingSet <- data.frame()
for (i in 1:8) {
  sampleNumber = 200
  replaceBool = table(scDf$LeagueIndex)[i] < sampleNumber
  trainingSet <-
    rbind(trainingSet,
          sample_n(scDf[which(scDf$LeagueIndex == i),], sampleNumber, replace = replaceBool))
}
trainingSet
validationSet <- setdiff(scDf, trainingSet)

# trainRows <-
#   createDataPartition(scDf$LeagueIndex, p = 0.7, list = FALSE)
# trainingSet <- scDf[trainRows,]
# validationSet <- scDf[-trainRows,]
#
# accuracyFinalTable <- c()
#
# ordinalLogisticRegression <- polr(LeagueIndex~Age , scDf=trainingSet)
# summary(ordinalLogisticRegression)
# predictedLeague <- predict(ordinalLogisticRegression, validationSet)
# table(validationSet$LeagueIndex, predictedLeague)
# sum(validationSet$LeagueIndex == predictedLeague, na.rm = TRUE)/nrow(validationSet)
# accuracyFinalTable[1] <- sum(validationSet$LeagueIndex == predictedLeague, na.rm = TRUE)/nrow(validationSet)
#
#
# ordinalLogisticRegression <- polr(LeagueIndex~HoursPerWeek , scDf=trainingSet)
# summary(ordinalLogisticRegression)
# predictedLeague <- predict(ordinalLogisticRegression, validationSet)
# table(validationSet$LeagueIndex, predictedLeague)
# sum(validationSet$LeagueIndex == predictedLeague, na.rm = TRUE)/nrow(validationSet)
# accuracyFinalTable[2] <- sum(validationSet$LeagueIndex == predictedLeague, na.rm = TRUE)/nrow(validationSet)
#
# ordinalLogisticRegression <- polr(LeagueIndex~TotalHours , scDf=trainingSet)
# summary(ordinalLogisticRegression)
# predictedLeague <- predict(ordinalLogisticRegression, validationSet)
# table(validationSet$LeagueIndex, predictedLeague)
# sum(validationSet$LeagueIndex == predictedLeague, na.rm = TRUE)/nrow(validationSet)
# accuracyFinalTable[3] <- sum(validationSet$LeagueIndex == predictedLeague, na.rm = TRUE)/nrow(validationSet)
#
# ordinalLogisticRegression <- polr(LeagueIndex~APM , scDf=trainingSet)
# summary(ordinalLogisticRegression)
# predictedLeague <- predict(ordinalLogisticRegression, validationSet)
# table(validationSet$LeagueIndex, predictedLeague)
# sum(validationSet$LeagueIndex == predictedLeague, na.rm = TRUE)/nrow(validationSet)
# accuracyFinalTable[4] <- sum(validationSet$LeagueIndex == predictedLeague, na.rm = TRUE)/nrow(validationSet)
#
# ordinalLogisticRegression <- polr(LeagueIndex~SelectByHotkeys , scDf=trainingSet)
# summary(ordinalLogisticRegression)
# predictedLeague <- predict(ordinalLogisticRegression, validationSet)
# table(validationSet$LeagueIndex, predictedLeague)
# sum(validationSet$LeagueIndex == predictedLeague, na.rm = TRUE)/nrow(validationSet)
# accuracyFinalTable[5] <- sum(validationSet$LeagueIndex == predictedLeague, na.rm = TRUE)/nrow(validationSet)
#
# ordinalLogisticRegression <- polr(LeagueIndex~AssignToHotkeys , scDf=trainingSet)
# summary(ordinalLogisticRegression)
# predictedLeague <- predict(ordinalLogisticRegression, validationSet)
# table(validationSet$LeagueIndex, predictedLeague)
# sum(validationSet$LeagueIndex == predictedLeague, na.rm = TRUE)/nrow(validationSet)
# accuracyFinalTable[6] <- sum(validationSet$LeagueIndex == predictedLeague, na.rm = TRUE)/nrow(validationSet)
#
# ordinalLogisticRegression <- polr(LeagueIndex~UniqueHotkeys , scDf=trainingSet)
# summary(ordinalLogisticRegression)
# predictedLeague <- predict(ordinalLogisticRegression, validationSet)
# table(validationSet$LeagueIndex, predictedLeague)
# sum(validationSet$LeagueIndex == predictedLeague, na.rm = TRUE)/nrow(validationSet)
# accuracyFinalTable[7] <- sum(validationSet$LeagueIndex == predictedLeague, na.rm = TRUE)/nrow(validationSet)
#
# ordinalLogisticRegression <- polr(LeagueIndex~MinimapAttacks , scDf=trainingSet)
# summary(ordinalLogisticRegression)
# predictedLeague <- predict(ordinalLogisticRegression, validationSet)
# table(validationSet$LeagueIndex, predictedLeague)
# sum(validationSet$LeagueIndex == predictedLeague, na.rm = TRUE)/nrow(validationSet)
# accuracyFinalTable[8] <- sum(validationSet$LeagueIndex == predictedLeague, na.rm = TRUE)/nrow(validationSet)
#
# ordinalLogisticRegression <- polr(LeagueIndex~MinimapRightClicks , scDf=trainingSet)
# summary(ordinalLogisticRegression)
# predictedLeague <- predict(ordinalLogisticRegression, validationSet)
# table(validationSet$LeagueIndex, predictedLeague)
# sum(validationSet$LeagueIndex == predictedLeague, na.rm = TRUE)/nrow(validationSet)
# accuracyFinalTable[9] <- sum(validationSet$LeagueIndex == predictedLeague, na.rm = TRUE)/nrow(validationSet)
#
# ordinalLogisticRegression <- polr(LeagueIndex~NumberOfPACs , scDf=trainingSet)
# summary(ordinalLogisticRegression)
# predictedLeague <- predict(ordinalLogisticRegression, validationSet)
# table(validationSet$LeagueIndex, predictedLeague)
# sum(validationSet$LeagueIndex == predictedLeague, na.rm = TRUE)/nrow(validationSet)
# accuracyFinalTable[10] <- sum(validationSet$LeagueIndex == predictedLeague, na.rm = TRUE)/nrow(validationSet)
#
# ordinalLogisticRegression <- polr(LeagueIndex~GapBetweenPACs , scDf=trainingSet)
# summary(ordinalLogisticRegression)
# predictedLeague <- predict(ordinalLogisticRegression, validationSet)
# table(validationSet$LeagueIndex, predictedLeague)
# sum(validationSet$LeagueIndex == predictedLeague, na.rm = TRUE)/nrow(validationSet)
# accuracyFinalTable[11] <- sum(validationSet$LeagueIndex == predictedLeague, na.rm = TRUE)/nrow(validationSet)
#
# ordinalLogisticRegression <- polr(LeagueIndex~ActionLatency , scDf=trainingSet)
# summary(ordinalLogisticRegression)
# predictedLeague <- predict(ordinalLogisticRegression, validationSet)
# table(validationSet$LeagueIndex, predictedLeague)
# sum(validationSet$LeagueIndex == predictedLeague, na.rm = TRUE)/nrow(validationSet)
# accuracyFinalTable[12] <- sum(validationSet$LeagueIndex == predictedLeague, na.rm = TRUE)/nrow(validationSet)
#
# ordinalLogisticRegression <- polr(LeagueIndex~ActionsInPAC , scDf=trainingSet)
# summary(ordinalLogisticRegression)
# predictedLeague <- predict(ordinalLogisticRegression, validationSet)
# table(validationSet$LeagueIndex, predictedLeague)
# sum(validationSet$LeagueIndex == predictedLeague, na.rm = TRUE)/nrow(validationSet)
# accuracyFinalTable[13] <- sum(validationSet$LeagueIndex == predictedLeague, na.rm = TRUE)/nrow(validationSet)
#
# ordinalLogisticRegression <- polr(LeagueIndex~TotalMapExplored , scDf=trainingSet)
# summary(ordinalLogisticRegression)
# predictedLeague <- predict(ordinalLogisticRegression, validationSet)
# table(validationSet$LeagueIndex, predictedLeague)
# sum(validationSet$LeagueIndex == predictedLeague, na.rm = TRUE)/nrow(validationSet)
# accuracyFinalTable[14] <- sum(validationSet$LeagueIndex == predictedLeague, na.rm = TRUE)/nrow(validationSet)
#
# ordinalLogisticRegression <- polr(LeagueIndex~WorkersMade , scDf=trainingSet)
# summary(ordinalLogisticRegression)
# predictedLeague <- predict(ordinalLogisticRegression, validationSet)
# table(validationSet$LeagueIndex, predictedLeague)
# sum(validationSet$LeagueIndex == predictedLeague, na.rm = TRUE)/nrow(validationSet)
# accuracyFinalTable[15] <- sum(validationSet$LeagueIndex == predictedLeague, na.rm = TRUE)/nrow(validationSet)
#
# ordinalLogisticRegression <- polr(LeagueIndex~UniqueUnitsMade , scDf=trainingSet)
# summary(ordinalLogisticRegression)
# predictedLeague <- predict(ordinalLogisticRegression, validationSet)
# table(validationSet$LeagueIndex, predictedLeague)
# sum(validationSet$LeagueIndex == predictedLeague, na.rm = TRUE)/nrow(validationSet)
# accuracyFinalTable[16] <- sum(validationSet$LeagueIndex == predictedLeague, na.rm = TRUE)/nrow(validationSet)
#
# ordinalLogisticRegression <- polr(LeagueIndex~ComplexUnitsMade , scDf=trainingSet)
# summary(ordinalLogisticRegression)
# predictedLeague <- predict(ordinalLogisticRegression, validationSet)
# table(validationSet$LeagueIndex, predictedLeague)
# sum(validationSet$LeagueIndex == predictedLeague, na.rm = TRUE)/nrow(validationSet)
# accuracyFinalTable[17] <- sum(validationSet$LeagueIndex == predictedLeague, na.rm = TRUE)/nrow(validationSet)
#
# ordinalLogisticRegression <- polr(LeagueIndex~ComplexAbilitiesUsed , scDf=trainingSet)
# summary(ordinalLogisticRegression)
# predictedLeague <- predict(ordinalLogisticRegression, validationSet)
# table(validationSet$LeagueIndex, predictedLeague)
# sum(validationSet$LeagueIndex == predictedLeague, na.rm = TRUE)/nrow(validationSet)
# accuracyFinalTable[18] <- sum(validationSet$LeagueIndex == predictedLeague, na.rm = TRUE)/nrow(validationSet)
#
# names(accuracyFinalTable) <- colnames(scDf)[-1]
#
# accuracyFinalTable <- as.data.frame(accuracyFinalTable)
# accuracyFinalTable
#
# ordinalLogisticRegression <- polr(LeagueIndex~NumberOfPACs+ActionLatency+APM+AssignToHotkeys+GapBetweenPACs , scDf=trainingSet)
# summary(ordinalLogisticRegression)
# predictedLeague <- predict(ordinalLogisticRegression, validationSet)
# table(validationSet$LeagueIndex, predictedLeague)
# sum(validationSet$LeagueIndex == predictedLeague, na.rm = TRUE)/nrow(validationSet)

#multivariate analysis

#on commence par standardisé nos données car cela est nécessaire pour obtenir la meilleure performance des algorithmes lorsqu'on a plusieurs variables prédictives
scaledDf <- scDf
scaledDf <- as.data.frame(scale((scaledDf[, -1])))
scaledDf <- cbind(scDf$LeagueIndex, scaledDf)
colnames(scaledDf)[1] <- "LeagueIndex"

trainingSetScaled <- trainingSet
trainingSetScaled <- as.data.frame(scale((trainingSetScaled[, -1])))
trainingSetScaled <-
  cbind(trainingSet$LeagueIndex, trainingSetScaled)
colnames(trainingSetScaled)[1] <- "LeagueIndex"

validationSetScaled <- validationSet
validationSetScaled <-
  as.data.frame(scale((validationSetScaled[, -1])))
validationSetScaled <-
  cbind(validationSet$LeagueIndex, validationSetScaled)
colnames(validationSetScaled)[1] <- "LeagueIndex"

# pca = princomp(scaledDf[,-1], cor = T)
# pc.comp = pca$scores
# pc.comp1 = pc.comp[, 1]
# pc.comp2 = pc.comp[, 2]
#
# pcapercentages <- as.data.frame(pca$sdev ^ 2 / sum(pca$sdev ^ 2))
# ggplot(scaledDf, aes(pc.comp1, pc.comp2, color = LeagueIndex)) +
#   geom_point(size = 2) +
#   scale_color_manual(values = brewer.pal(n = 8, name = "Accent"))
# # text(pc.comp1, pc.comp2, labels = 1:8, pos = 4)
# readline(prompt = "Press [enter] to continue")
#
# plot3d(pca$scores[, 1:3], col = scDf$LeagueIndex,)
# readline(prompt = "Press [enter] to continue")

# d <- dist(scaledDf) # euclidean distances between the rows
# fit <- cmdscale(d, eig = TRUE, k = 3) # k is the number of dim
# fit # view results
#
# # plot solution
# x <- fit$points[, 1]
# y <- fit$points[, 2]
# z <- fit$points[, 3]
#
# plot3d(fit$points[, 1:3], col = scDf$LeagueIndex)

# text(x, y, labels = row.names(scaledDf[-3143, ]), cex = .7)

# variables selection with bidirectional selection

ordinalLogisticRegressionModel <-
  polr(
    LeagueIndex ~ TotalHours + SelectByHotkeys + AssignToHotkeys + ActionLatency +
      UniqueHotkeys + MinimapAttacks + NumberOfPACs + GapBetweenPACs + TotalMapExplored + WorkersMade ,
    data = trainingSetScaled
  )

# optimizedModel = step(ordinalLogisticRegressionModel, direction = "both")

optimizedModel = ordinalLogisticRegressionModel
summary(optimizedModel)
predictedLeague <- predict(optimizedModel, validationSetScaled)
table(validationSetScaled$LeagueIndex, predictedLeague)
sum(validationSetScaled$LeagueIndex == predictedLeague, na.rm = TRUE) /
  nrow(validationSetScaled)


# outputMeanProbabilities <- matrix(0,nrow=2,ncol=20)
# outputMeanProbabilities[1,] <- 1:20
# 
# for (i in 1:20) {
#   totalSum <- 0
#   
#   for (j in 1:50) {
#     
#     # create a new trainingSet and validationSet
#     trainRows <-createDataPartition(scDf$LeagueIndex, p = 0.7, list = FALSE)
#     trainingSet <- scDf[trainRows,]
#     validationSet <- scDf[-trainRows,]
#     
#     # scale the new sets
#     trainingSetScaled <- trainingSet
#     trainingSetScaled <- as.data.frame(scale((trainingSetScaled[, -1])))
#     trainingSetScaled <-cbind(trainingSet$LeagueIndex, trainingSetScaled)
#     colnames(trainingSetScaled)[1] <- "LeagueIndex"
#     validationSetScaled <- validationSet
#     validationSetScaled <- as.data.frame(scale((validationSetScaled[, -1])))
#     validationSetScaled <- cbind(validationSet$LeagueIndex, validationSetScaled)
#     colnames(validationSetScaled)[1] <- "LeagueIndex"
#     
#     # run the model
#     knnModel <-knn(train = trainingSetScaled,test = validationSetScaled,cl = trainingSetScaled$LeagueIndex,k = i,prob = TRUE)
#     
#     # append accuracy rate of current i-NN to totalSum
#     totalSum <- totalSum + sum(validationSetScaled$LeagueIndex == knnModel, na.rm = TRUE) / nrow(validationSetScaled)
#   }
#   outputMeanProbabilities[2,i] <- totalSum/j 
# }
# 
# outputMeanProbabilities

# run the model
knnModel <-knn(train = trainingSetScaled,test = validationSetScaled,cl = trainingSetScaled$LeagueIndex,k = 13,prob = TRUE)
table(validationSetScaled$LeagueIndex, knnModel)
sum(validationSetScaled$LeagueIndex == knnModel, na.rm = TRUE) / nrow(validationSetScaled)
table(validationSetScaled$LeagueIndex)
diag(as.matrix(table(validationSetScaled$LeagueIndex, knnModel)))/table(validationSetScaled$LeagueIndex)