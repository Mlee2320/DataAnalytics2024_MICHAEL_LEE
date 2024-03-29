nyt18 = read.csv("C:/Users/leem21/STM32CubeIDE/DataAnalytics2024_MICHAEL_LEE/Data\ Analytics/Assignments/Assignment\ 3/nyt18.csv")
nyt19 = read.csv("C:/Users/leem21/STM32CubeIDE/DataAnalytics2024_MICHAEL_LEE/Data\ Analytics/Assignments/Assignment\ 3/nyt19.csv")
nyt24 = read.csv("C:/Users/leem21/STM32CubeIDE/DataAnalytics2024_MICHAEL_LEE/Data\ Analytics/Assignments/Assignment\ 3/nyt24.csv")
nyt25 = read.csv("C:/Users/leem21/STM32CubeIDE/DataAnalytics2024_MICHAEL_LEE/Data\ Analytics/Assignments/Assignment\ 3/nyt25.csv")
nyt26 = read.csv("C:/Users/leem21/STM32CubeIDE/DataAnalytics2024_MICHAEL_LEE/Data\ Analytics/Assignments/Assignment\ 3/nyt26.csv")
nyt27 = read.csv("C:/Users/leem21/STM32CubeIDE/DataAnalytics2024_MICHAEL_LEE/Data\ Analytics/Assignments/Assignment\ 3/nyt27.csv")
nyt30 = read.csv("C:/Users/leem21/STM32CubeIDE/DataAnalytics2024_MICHAEL_LEE/Data\ Analytics/Assignments/Assignment\ 3/nyt30.csv")

View(nyt18)
View(nyt19)
View(nyt24)
View(nyt25)
View(nyt26)
View(nyt27)
View(nyt30)

boxplot(nyt18$Age, nyt19$Age, nyt24$Age,
        nyt25$Age, nyt26$Age, nyt27$Age, 
        nyt30$Age, 
        names = c("18", "19", "24", "25", "26", "27", "30"),
        main = "Age", xlab = "Dataset", ylab = "Age")

boxplot(nyt18$Impressions, nyt19$Impressions, 
        nyt24$Impressions, nyt25$Impressions, 
        nyt26$Impressions, nyt27$Impressions, 
        nyt30$Impressions, 
        names = c("18", "19", "24", "25", "26", "27", "30"),
        main = "Impressions", xlab = "Dataset", ylab = "Impressions")

# Anderson Darling Normality Test
install.packages("nortest")
library(nortest)
# > 0.05 accept null hypothesis follows normal distribution
print(ad.test(nyt18$Age)$p.value > 0.05)
print((ad.test(nyt18[nyt18$Age > 0,]$Age))$p.value > 0.05)

print(ad.test(nyt19$Age)$p.value > 0.05)
print((ad.test(nyt19[nyt19$Age > 0,]$Age))$p.value > 0.05)

print(ad.test(nyt24$Age)$p.value > 0.05)
print((ad.test(nyt24[nyt24$Age > 0,]$Age))$p.value > 0.05)

print(ad.test(nyt25$Age)$p.value > 0.05)
print((ad.test(nyt25[nyt25$Age > 0,]$Age))$p.value > 0.05)

print(ad.test(nyt26$Age)$p.value > 0.05)
print((ad.test(nyt26[nyt26$Age > 0,]$Age))$p.value > 0.05)

print(ad.test(nyt27$Age)$p.value > 0.05)
print((ad.test(nyt27[nyt27$Age > 0,]$Age))$p.value > 0.05)

print(ad.test(nyt30$Age)$p.value > 0.05)
print((ad.test(nyt30[nyt30$Age > 0,]$Age))$p.value > 0.05)


print(ad.test(nyt18$Impressions)$p.value > 0.05)
print(ad.test(nyt19$Impressions)$p.value > 0.05)
print(ad.test(nyt24$Impressions)$p.value > 0.05)
print(ad.test(nyt25$Impressions)$p.value > 0.05)
print(ad.test(nyt26$Impressions)$p.value > 0.05)
print(ad.test(nyt27$Impressions)$p.value > 0.05)
print(ad.test(nyt30$Impressions)$p.value > 0.05)
# ALL are not normally distributed
install.packages("ggplot2")
library(ggplot2)
install.packages("ggpubr")
library(ggpubr)

hist18 <- ggplot(data=nyt18, aes(x=Age)) + 
  geom_histogram(bins=10) + 
  ggtitle("Dataset 18") 
hist19 <- ggplot(data=nyt19, aes(x=Age)) + 
  geom_histogram(bins=10) + 
  ggtitle("Dataset 19") 
hist24 <- ggplot(data=nyt24, aes(x=Age)) + 
  geom_histogram(bins=10) + 
  ggtitle("Dataset 24") 
hist25 <- ggplot(data=nyt25, aes(x=Age)) + 
  geom_histogram(bins=10) + 
  ggtitle("Dataset 25") 
hist26 <- ggplot(data=nyt26, aes(x=Age)) + 
  geom_histogram(bins=10) + 
  ggtitle("Dataset 26")
hist27 <- ggplot(data=nyt27, aes(x=Age)) + 
  geom_histogram(bins=10) + 
  ggtitle("Dataset 27") 
hist30 <- ggplot(data=nyt30, aes(x=Age)) + 
  geom_histogram(bins=10) + 
  ggtitle("Dataset 30")

ages <- ggarrange(hist18, hist19, hist24,
                  hist25, hist26, hist27, hist30)
annotate_figure(ages, top = text_grob("Age Histograms",
                                      color = "blue", face = "bold", size = 10))
# Ages without zeros
hist18 <- ggplot(data=nyt18[nyt18$Age > 0,], aes(x=Age)) + 
  geom_histogram(bins=15) + 
  ggtitle("Dataset 18") 
hist19 <- ggplot(data=nyt19[nyt19$Age > 0,], aes(x=Age)) + 
  geom_histogram(bins=15) + 
  ggtitle("Dataset 19")
hist24 <- ggplot(data=nyt24[nyt24$Age > 0,], aes(x=Age)) + 
  geom_histogram(bins=15) + 
  ggtitle("Dataset 24") 
hist25 <- ggplot(data=nyt25[nyt25$Age > 0,], aes(x=Age)) + 
  geom_histogram(bins=15) + 
  ggtitle("Dataset 25") 
hist26 <- ggplot(data=nyt26[nyt26$Age > 0,], aes(x=Age)) + 
  geom_histogram(bins=15) + 
  ggtitle("Dataset 26") 
hist27 <- ggplot(data=nyt27[nyt27$Age > 0,], aes(x=Age)) + 
  geom_histogram(bins=15) + 
  ggtitle("Dataset 27") 
hist30 <- ggplot(data=nyt30[nyt30$Age > 0,], aes(x=Age)) + 
  geom_histogram(bins=15) + 
  ggtitle("Dataset 30")

age_no_zero <- ggarrange(hist18, hist19, hist24,
                         hist25, hist26, hist27, hist30)
annotate_figure(age_no_zero, top = text_grob("Age Histograms (No Zero)",
                                             color = "blue", face = "bold", size = 10))
# Impression Histograms
hist18 <- ggplot(data=nyt18, aes(x=Impressions)) + 
  geom_histogram(bins=10) + 
  ggtitle("Dataset 18") 
hist19 <- ggplot(data=nyt19, aes(x=Impressions)) + 
  geom_histogram(bins=10) + 
  ggtitle("Dataset 19") 
hist24 <- ggplot(data=nyt24, aes(x=Impressions)) + 
  geom_histogram(bins=10) + 
  ggtitle("Dataset 24") 
hist25 <- ggplot(data=nyt25, aes(x=Impressions)) + 
  geom_histogram(bins=10) + 
  ggtitle("Dataset 25") 
hist26 <- ggplot(data=nyt26, aes(x=Impressions)) + 
  geom_histogram(bins=10) + 
  ggtitle("Dataset 26")
hist27 <- ggplot(data=nyt27, aes(x=Impressions)) + 
  geom_histogram(bins=10) + 
  ggtitle("Dataset 27") 
hist30 <- ggplot(data=nyt30, aes(x=Impressions)) + 
  geom_histogram(bins=10) + 
  ggtitle("Dataset 30")

impressions <- ggarrange(hist18, hist19, hist24,
                         hist25, hist26, hist27, hist30)
annotate_figure(impressions, top = text_grob("Impressions Histograms",
                                             color = "green", face = "bold", size = 10))

# Empirical Cumulative Distribution Function for Age
ecdf18 <- ggplot(data=nyt18[nyt18$Age > 0,], aes(x=Age)) + 
  stat_ecdf(geom = "point") + ggtitle("Dataset 18")
ecdf19 <- ggplot(data=nyt19[nyt19$Age > 0,], aes(x=Age)) + 
  stat_ecdf(geom = "point") + ggtitle("Dataset 19")
ecdf24 <- ggplot(data=nyt24[nyt24$Age > 0,], aes(x=Age)) + 
  stat_ecdf(geom = "point") + ggtitle("Dataset 24")
ecdf25 <- ggplot(data=nyt25[nyt25$Age > 0,], aes(x=Age)) + 
  stat_ecdf(geom = "point") + ggtitle("Dataset 25")
ecdf26 <- ggplot(data=nyt26[nyt26$Age > 0,], aes(x=Age)) + 
  stat_ecdf(geom = "point") + ggtitle("Dataset 26")
ecdf27 <- ggplot(data=nyt27[nyt27$Age > 0,], aes(x=Age)) + 
  stat_ecdf(geom = "point") + ggtitle("Dataset 27")
ecdf30 <- ggplot(data=nyt30[nyt30$Age > 0,], aes(x=Age)) + 
  stat_ecdf(geom = "point") + ggtitle("Dataset 30")

Age_ecdf <- ggarrange(ecdf18, ecdf19, ecdf24,
                      ecdf25, ecdf26, ecdf27, ecdf30)
annotate_figure(Age_ecdf, top = text_grob("Age ECDF",
                                          color = "blue", face = "bold", size = 10))
# ECDF for Impressions
ecdf18 <- ggplot(data=nyt18, aes(x=Impressions)) + 
  stat_ecdf(geom = "point") + ggtitle("Dataset 18")
ecdf19 <- ggplot(data=nyt19, aes(x=Impressions)) + 
  stat_ecdf(geom = "point") + ggtitle("Dataset 19")
ecdf24 <- ggplot(data=nyt24, aes(x=Impressions)) + 
  stat_ecdf(geom = "point") + ggtitle("Dataset 24")
ecdf25 <- ggplot(data=nyt25, aes(x=Impressions)) + 
  stat_ecdf(geom = "point") + ggtitle("Dataset 25")
ecdf26 <- ggplot(data=nyt26, aes(x=Impressions)) + 
  stat_ecdf(geom = "point") + ggtitle("Dataset 26")
ecdf27 <- ggplot(data=nyt27, aes(x=Impressions)) + 
  stat_ecdf(geom = "point") + ggtitle("Dataset 27")
ecdf30 <- ggplot(data=nyt30, aes(x=Impressions)) + 
  stat_ecdf(geom = "point") + ggtitle("Dataset 30")

impressions_ecdf <- ggarrange(ecdf18, ecdf19, ecdf24,
                         ecdf25, ecdf26, ecdf27, ecdf30)
annotate_figure(impressions_ecdf, top = text_grob("Impressions ECDF",
                                             color = "blue", face = "bold", size = 10))

# QQ Plot for Age
qq18 <- ggplot(data=nyt18[nyt18$Age > 0,], aes(sample=Age)) + stat_qq() + ggtitle("Dataset 18")
qq19 <- ggplot(data=nyt19[nyt19$Age > 0,], aes(sample=Age)) + stat_qq() + ggtitle("Dataset 19")
qq24 <- ggplot(data=nyt24[nyt24$Age > 0,], aes(sample=Age)) + stat_qq() + ggtitle("Dataset 24")
qq25 <- ggplot(data=nyt25[nyt25$Age > 0,], aes(sample=Age)) + stat_qq() + ggtitle("Dataset 25")
qq26 <- ggplot(data=nyt26[nyt26$Age > 0,], aes(sample=Age)) + stat_qq() + ggtitle("Dataset 26")
qq27 <- ggplot(data=nyt27[nyt27$Age > 0,], aes(sample=Age)) + stat_qq() + ggtitle("Dataset 27")
qq30 <- ggplot(data=nyt30[nyt30$Age > 0,], aes(sample=Age)) + stat_qq() + ggtitle("Dataset 30")

age_qq <- ggarrange(qq18, qq19, qq24, qq25, qq26, qq27, qq30)
annotate_figure(age_qq, top = text_grob("Age QQ-Plots",
                                        color = "purple", face = "bold", size = 10))

# QQ Plot for Impressions
qq18 <- ggplot(data=nyt18, aes(sample=Impressions)) + stat_qq() + ggtitle("Dataset 18")
qq19 <- ggplot(data=nyt19, aes(sample=Impressions)) + stat_qq() + ggtitle("Dataset 19")
qq24 <- ggplot(data=nyt24, aes(sample=Impressions)) + stat_qq() + ggtitle("Dataset 24")
qq25 <- ggplot(data=nyt25, aes(sample=Impressions)) + stat_qq() + ggtitle("Dataset 25")
qq26 <- ggplot(data=nyt26, aes(sample=Impressions)) + stat_qq() + ggtitle("Dataset 26")
qq27 <- ggplot(data=nyt27, aes(sample=Impressions)) + stat_qq() + ggtitle("Dataset 27")
qq30 <- ggplot(data=nyt30, aes(sample=Impressions)) + stat_qq() + ggtitle("Dataset 30")

impressions_qq <- ggarrange(qq18, qq19, qq24, qq25, qq26, qq27, qq30)
annotate_figure(impressions_qq, top = text_grob("Impressions QQ-Plots",
                                           color = "red", face = "bold", size = 10))

# Significance test

lin_model <- lm(nyt18$Age[nyt18$Age > 0] ~ nyt18$Impressions[nyt18$Age > 0])
lin_model
plot(nyt18$Age[nyt18$Age > 0]~nyt18$Impressions[nyt18$Age > 0], xlab="Impressions", ylab="Age", main="Age vs. Impressions")
abline(lin_model, col="blue", lwd=2)
