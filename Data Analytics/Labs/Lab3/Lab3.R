# Exercise 1

# set seed for random numbers in matrix
set.seed(12345)
help(par)

par(mar = rep(0.2, 4))
data_matrix <- matrix(rnorm(400), nrow = 40)
image(1:10, 1:40, t(data_matrix)[,nrow(data_matrix):1])

# Heatmap
par(mar = rep(0.2, 4))
heatmap(data_matrix)

# Doing random coin flip to add pattern to data 
help('rbinom')
set.seed(678910)
for(i in 1:40) {
  # Flipping coin and getting data
  coin_Flip <- rbinom(1, size = 1, prob = 0.5)
  if(coin_Flip) {
    # if coin is true then add common pattern to row
    data_matrix[i, ] <- data_matrix[i, ] + rep(c(0, 3), each = 5)
  }
}

par(mar = rep(0.2, 4))
image(1:10, 1:40, t(data_matrix)[,nrow(data_matrix):1])

par(mar = rep(0.2, 4))
heatmap(data_matrix)

# Closer look at patterns in rows and columns
hh <- hclust(dist(data_matrix))
data_matrix_ordered <- data_matrix[hh$order,]
par(mfrow = c(1, 3))
image(t(data_matrix_ordered)[,nrow(data_matrix_ordered):1])
plot(rowMeans(data_matrix_ordered), 40:1, , xlab = "The Row Mean", ylab = "Row", pch = 19)
plot(colMeans(data_matrix_ordered), xlab = "Column", ylab = "Column Mean", pch = 19)

# Exercise 2

library(class)
abalone_data <- read.csv("C:/Users/leem21/STM32CubeIDE/DataAnalytics2024_MICHAEL_LEE/Data\ Analytics/Labs/Lab3/abalone.csv")
View(abalone_data)

colnames(abalone_data) <- c("sex", "length", 'diameter', 'height', 'whole_weight', 'shucked_wieght', 'viscera_wieght', 'shell_weight', 'rings')
summary(abalone_data)
str(abalone_data)
summary(abalone_data$rings)

abalone_data$rings <- as.numeric(abalone_data$rings)
abalone_data$rings <- cut(abalone_data$rings, br=c(-1,8,11,35), labels = c("young", 'adult', 'old'))
abalone_data$rings <- as.factor(abalone_data$rings)
summary(abalone_data$rings)

aba <- abalone_data
aba$sex <- NULL

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
aba[1:7] <- as.data.frame(lapply(aba[1:7], normalize))
summary(aba$shucked_wieght)
ind <- sample(2, nrow(aba), replace=TRUE, prob=c(0.7, 0.3))
KNNtrain <- aba[ind==1,]
KNNtest <- aba[ind==2,]
sqrt(2918) # round to 55
KNNpred <- knn(train = KNNtrain[1:7], test = KNNtest[1:7], cl = KNNtrain$rings, k = 55)
KNNpred
table(KNNpred)

# Exercise 3

# iris dataset is from UCI ML repository.
library(ggplot2) # we will use ggplot2 to visualize the data.
head(iris)
str(iris)
summary(iris)
sapply(iris[,-5], var)
summary(iris)
# plot Sepal.Length Vs Sepal.Width using ggplot
ggplot(iris,aes(x = Sepal.Length, y = Sepal.Width, col= Species)) + geom_point()
# plot Petal.Length Vs Sepal.Width using ggplot
ggplot(iris,aes(x = Petal.Length, y = Petal.Width, col= Species)) + geom_point()
# kmeans clustering
set.seed(300)
k.max <- 12
wss<- sapply(1:k.max,function(k){kmeans(iris[,3:4],k,nstart = 20,iter.max = 20)$tot.withinss})
wss # within sum of squares.
plot(1:k.max,wss, type= "b", xlab = "Number of clusters(k)", ylab = "Within cluster sum of squares")
icluster <- kmeans(iris[,3:4],3,nstart = 20)
table(icluster$cluster,iris$Species)
# In the table we can see that most of the observations have been clustered correctly
# however, two of the versicolor have been put in the cluster with all the virginica
# and four of the verginica have been put in cluster 3 which mostly has versicolor