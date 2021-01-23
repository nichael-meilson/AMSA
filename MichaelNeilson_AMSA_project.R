library(mvoutlier)
library(cluster)
library(mclust)
library(heplots)
library(MASS)
library(plotrix)

data <- read.csv('full_data.csv')
dim(data)
summary(data)
attach(data)

par(mfrow=c(1,1))
par(mar=c(10, 4.1, 2.1, 2.1))
boxplot(data[, 4:12], las=2)
pairs(data[,4:12])
out <- chisq.plot(data[,4:12])
par(mfrow=c(3,3))
for (i in (4:12)) {hist(data[,i], main=names(data)[i], xlab="Countries")}

# PCA
pca.matrix <- data[,5:12]
names(pca.matrix)
pca.data <- princomp(pca.matrix, cor=T)
names(pca.data)
attributes(pca.data)
summary(pca.data)
par(mfrow=c(1,1))
screeplot(pca.data, type="line", main="Scree Plot")

# Biplot
scalefactor<-.2
scores <- pca.data$scores*scalefactor
par(mfrow=c(1,1))
par(srt=0)
plot(scores[,2]~ scores[,1], xlim=c(-1.5,1.5), ylim=c(-1.5,1.5), type="n",main='Biplot',xlab='PC1',ylab='PC2')
text(scores[,1], scores[,2], col=as.numeric(data[,1]))
for (i in seq(1,nrow(pca.data$loadings))){
  arrows(0,0,pca.data$loadings[i,1],pca.data$loadings[i,2],lwd=1, col="red")
  text(pca.data$loadings[,1],pca.data$loadings[,2],as.character(dimnames(data)[[2]]),cex=0.5,font=2, col="blue")}
draw.circle(0,0,1,border='red')

# K-means
data.kmc <- kmeans(pca.data$scores,5,20) # Diseases
plot(pca.data$scores[,1:2], col = data.kmc$cluster)
points(data.kmc$centers[,c(1,3)], col = "blue", pch ="+", cex=2)
clusplot(data, data.kmc$cluster, stand=TRUE)
classError(data.kmc$cluster, data[,1])

# Tree
data$sex <- as.factor(data$sex)
data$country <- as.factor(data$country)
data$disease <- as.factor(data$disease)

set.seed(1)
par(mfrow=c(2,3))
par(mar=c(5.1, 4.1, 4.1, 2.1))
for (i in 1:length(unique(data$sex))) {
  disease.data <- data[data$sex == unique(data$sex)[i],]
  data.tree <- tree(disease~ncd_deaths+pollution+underweight+overweight+water+raised_cholesterol+fasting_blood+raised_bp+phys_activity, data=disease.data)
  par(srt=270)
  plot(data.tree)
  text(data.tree, cex=1, adj=c(-0.1,0))
  title(unique(data$sex)[i])
  cv.datatree <- cv.tree(data.tree, FUN=prune.tree, K=10)
  plot(cv.datatree, type="b")
  prune.datatree <- prune.tree(data.tree, best=6)
  plot(prune.datatree)
  text(prune.datatree,cex=1, adj=c(-0.1,0))
  title("CV Pruned")
  print(summary(data.tree))
  print(summary(prune.datatree))
}
