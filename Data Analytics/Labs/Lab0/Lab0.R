# MASS Library
install.packages("MASS")
library(MASS) 
attach(Boston) 
?Boston 
head(Boston)
dim(Boston) 
names(Boston)
str(Boston) 
nrow(Boston)
ncol(Boston) 
summary(Boston) 
summary(Boston$crim)

# ISLR Library
install.packages("ISLR")
library(ISLR)
?Auto
data(Auto)
head(Auto)
names(Auto)
summary(Auto)
summary(Auto$mpg)
fivenum(Auto$mpg)
boxplot(Auto$mpg)
hist(Auto$mpg)
summary(Auto$horsepower)
summary(Auto$weight)
fivenum(Auto$weight)
boxplot(Auto$weight)
mean(Auto$weight)
median(Auto$weight)

# Lab0
data <- read.csv(file.choose(), header = TRUE)
data
summary(data)
boxplot(data$EPI)
fivenum(data$EPI)
hist(data$EPI)