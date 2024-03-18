library(ISLR)
library(dplyr)

head(Hitters)
dim(Hitters)
is.na(Hitters)
HittersData <- na.omit(Hitters)
head(HittersData)
dim(HittersData)

SalaryPredictModel1 <- lm(Salary ~., data = HittersData)
summary(SalaryPredictModel1)

cooksD <- cooks.distance(SalaryPredictModel1)
influential <- cooksD[(cooksD > (3 * mean(cooksD, na.rm = TRUE)))]
influential

names_of_influential <- names(influential)
names_of_influential

outliers <- HittersData[names_of_influential,]
HittersWithoutOutliers <- HittersData %>% anti_join(outliers)

SalaryPredictModel2 <- lm(Salary ~., data = HittersWithoutOutliers)
summary(SalaryPredictModel2)

set.seed(10)
data1 <- rnorm(50)

set.seed(30)
data2 <- rnorm(50)

shapiro.test(data1)
hist(data1, col = 'green')

shapiro.test(data2)
hist(data2, col = 'steelblue')

set.seed(0)
data <- rnorm(100)

shapiro.test(data)

set.seed(0)
data_ <- rpois(n = 100, lambda = 3)

shapiro.test(data_)
hist(data_, col = 'yellow')

library(nortest)

set.seed(1)
x <- rnorm(100, 0, 1)
ad.test(x)

y <- runif(100, 0, 1)
ad.test(y)