library(ggplot2)
library(ggpubr)
library(dplyr)
library(rpart)
library(rpart.plot)
library(randomForest)

crime <- read.csv("C:/Users/leem21/STM32CubeIDE/DataAnalytics2024_MICHAEL_LEE/Data\ Analytics/Assignments/Assignment\ 6/Crime_Data_from_2020_to_Present.csv")
View(crime)

# EDA
summary(crime)
summary(crime$Crm.Cd.Desc)
par("usr")
categories <- table(crime$Crm.Cd.Desc)[table(crime$Crm.Cd.Desc) > 50000]
names(categories)
labels = names(categories)
plot(as.table(table(crime$Crm.Cd.Desc)[table(crime$Crm.Cd.Desc) > 50000]), xaxt = "n", yaxt = "n", ylab = "Count", main = "Top 7 Crimes Committed")
axis(side = 1, labels = FALSE)
axis(side = 2, las = 2, mgp = c(3, 0.75, 0))
text(x = 1:length(crime),
     y = par("usr")[3] - 5000,
     labels = names(categories),
     xpd = NA,
     srt = 40,
     adj = 0.55,
     cex = 0.52)

summary(crime$Vict.Age)
summary(crime$Vict.Sex)
summary(crime$Vict.Descent)
vict_age_plot <- ggplot(data = crime, aes(x = Vict.Age)) + geom_bar()
vict_age_plot
vict_sex_plot <- ggplot(data = crime, aes(x = Vict.Sex)) + geom_bar()
vict_sex_plot
vict_desc_plot <- ggplot(data = crime, aes(x = Vict.Descent)) + geom_bar()
vict_desc_plot

summary(crime$AREA.NAME)
crime_area <- ggplot(data = crime, aes(x = AREA.NAME)) + geom_bar()
crime_area + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Logistic Regression
filtered_crime <- crime %>%
  filter(Vict.Sex %in% c("M", "F"))
filtered_crime$Vict.Sex <- ifelse(filtered_crime$Vict.Sex == "M", 1,
  ifelse(filtered_crime$Vict.Sex == "F", 0, filtered_crime$Vict.Sex))
filtered_crime$Vict.Sex <- as.numeric(filtered_crime$Vict.Sex)
top_crimes <- names(sort(table(filtered_crime$Crm.Cd.Desc), decreasing = TRUE))[1:7]
top_crimes
filtered_crime <- filtered_crime[filtered_crime$Crm.Cd.Desc %in% top_crimes, ]
View(filtered_crime)

logit <- glm(filtered_crime$Vict.Sex ~ filtered_crime$Crm.Cd.Desc,
         data = filtered_crime, family = "binomial")
summary(logit)

# Decision Tree

# Crime Committed vs Victim Age
sample_crime1 <- sample(1:nrow(filtered_crime), 100)
train_crime1 <- filtered_crime[sample_crime1, ]
test_crime1 <- filtered_crime[-sample_crime1, ]
dt1 <- rpart(Crm.Cd.Desc ~ Vict.Age, data = train_crime1)
dt1
rpart.plot(dt1)

# Crime Committed vs Victim Descent
sample_crime2 <- sample(1:nrow(filtered_crime), 100)
train_crime2 <- filtered_crime[sample_crime2, ]
test_crime2 <- filtered_crime[-sample_crime2, ]
dt2 <- rpart(Crm.Cd.Desc ~ Vict.Descent, data = train_crime2)
dt2
rpart.plot(dt2)

#Crime Committed vs Victime Age + Victim Descent
sample_crime3 <- sample(1:nrow(filtered_crime), 50)
train_crime3 <- filtered_crime[sample_crime3, ]
test_crime3 <- filtered_crime[-sample_crime3, ]
dt3 <- rpart(Crm.Cd.Desc ~ Vict.Descent + Vict.Age, data = train_crime3)
dt3
rpart.plot(dt3)
