EPI_data <- read.csv("C:/Users/leem21/STM32CubeIDE/DataAnalytics2024_MICHAEL_LEE/Data\ Analytics/Labs/Lab1/2010EPI_data.csv")
#or
#EPI_data <- read.xlsx(”<path>/2010EPI_data.xlsx")
# Note: replace default data frame name – cannot start with numbers!
names(EPI_data) <- as.matrix(EPI_data[1, ])
EPI_data <- EPI_data[-1, ]
EPI_data[] <- lapply(EPI_data, function(x)
type.convert(as.character(x)))
View(EPI_data)
#
attach(EPI_data) 	# sets the ‘default’ object
fix(EPI_data) 	# launches a simple data editor

# EPI
EPI 			# prints out values EPI_data$EPI
tf_EPI <- is.na(EPI) # records True values if the value is NA
E_EPI <- EPI[!tf_EPI] # filters out NA values, new array
summary(EPI)
fivenum(EPI, na.rm = T)
stem(EPI)
hist(EPI)
hist(EPI, seq(30., 95., 1.0), prob = TRUE)
lines(density(EPI, na.rm = TRUE, bw = 1.))
rug(EPI)
plot(ecdf(EPI), do.points = FALSE, verticals = TRUE)
par(pty="s")
qqnorm(EPI); qqline(EPI)
x_EPI <- seq(30, 95, 1)
qqplot(qt(ppoints(250), df = 5), x_EPI, xlab = "Q-Q plot for tdsn")
qqline(x_EPI)

# DALY
DALY
tf_DALY <- is.na(DALY)
E_DALY <- DALY[!tf_DALY]
summary(DALY)
fivenum(DALY, na.rm = T)
stem(DALY)
hist(DALY)
hist(DALY, seq(0., 99, 1.0), prob = TRUE)
lines(density(DALY, na.rm = TRUE, bw = 1.))
rug(DALY)
plot(ecdf(DALY), do.points = FALSE, verticals = TRUE)
par(pty = "s")
qqnorm(DALY); qqline(DALY)
x_DALY <- seq(0, 99, 1)
qqplot(qt(ppoints(250), df = 5), x_DALY,xlab = "Q-Q plot for tdsn")
qqline(x_DALY)

# WATER_H
WATER_H
tf_WATER_H <- is.na(WATER_H)
E_WATER_H <- WATER_H[!tf_WATER_H]
summary(WATER_H)
fivenum(WATER_H, na.rm = T)
stem(WATER_H)
hist(WATER_H)
hist(WATER_H, seq(0., 100, 1.0), prob = TRUE)
lines(density(WATER_H, na.rm = TRUE, bw = 1.))
rug(WATER_H)
plot(ecdf(WATER_H), do.points = FALSE, verticals = TRUE)
par(pty = "s")
qqnorm(WATER_H); qqline(WATER_H)
x_WATER_H <- seq(0, 100, 1)
qqplot(qt(ppoints(250), df = 5), x_DALY, xlab = "Q-Q plot for tdsn")
qqline(x_WATER_H)

# Intercompare EPI, ENVHEALTH, ECOSYSTEM, DALY, AIR_H, WATER_H, AIR_E, WATER_E, BIODIVERSITY


boxplot(EPI, ENVHEALTH, ECOSYSTEM, DALY, AIR_H, WATER_H, AIR_E, WATER_E, BIODIVERSITY)
qqplot(EPI, ENVHEALTH)
qqplot(EPI, ECOSYSTEM)
qqplot(EPI, DALY)
qqplot(EPI, AIR_H)
qqplot(EPI, WATER_H)
qqplot(EPI, AIR_E)
qqplot(EPI, WATER_E)
qqplot(EPI, BIODIVERSITY)

# Exercise 2: filtering (populations)

# EPILand
EPILand <- EPI[!Landlock]
ELand <- EPILand[!is.na(EPILand)]
summary(ELand)
fivenum(ELand, na.rm = TRUE)
stem(ELand)
hist(ELand)
hist(ELand, seq(30., 95., 1.0), prob = TRUE)
lines(density(ELand, na.rm = TRUE, bw = 1.))
rug(ELand)

# No_surface_water
NoSurfaceWater <- EPI[!No_surface_water]
NSW <- NoSurfaceWater[!is.na(NoSurfaceWater)]
summary(NSW)
fivenum(NSW, na.rm = TRUE)
stem(NSW)
hist(NSW)
hist(NSW, seq(30., 95., 1.0), prob = TRUE)
lines(density(NSW, na.rm = TRUE, bw = 1.))
rug(NSW)

# Desert
Desert <- EPI[!Desert]
Des <- Desert[!is.na(Desert)]
summary(Des)
fivenum(Des, na.rm = TRUE)
stem(Des)
hist(Des)
hist(Des, seq(30., 95., 1.0), prob = TRUE)
lines(density(Des, na.rm = TRUE, bw = 1.))
rug(Des)

# High_Population_Density
High_Population_Density <- EPI[!High_Population_Density]
HPD <- High_Population_Density[!is.na(High_Population_Density)]
summary(HPD)
fivenum(HPD, na.rm = TRUE)
stem(HPD)
hist(HPD)
hist(HPD, seq(30., 95., 1.0), prob = TRUE)
lines(density(HPD, na.rm = TRUE, bw = 1.))
rug(HPD)

# Exercise: filter on EPI_regions or GEO_subregion
EPI_South_Asia <- EPI[EPI_data$GEO_subregion == "South Asia"]
SouthAsia <- EPI_South_Asia[!is.na(EPI_South_Asia)]
hist(SouthAsia)

# GRUMP
GRUMP <- read.csv("C:/Users/leem21/STM32CubeIDE/DataAnalytics2024_MICHAEL_LEE/Data\ Analytics/Labs/Lab1/GPW3_GRUMP_SummaryInformation_2010.csv")
View(GRUMP)

# Largest.Point..pop.
LargestPointPop <- GRUMP$Largest.Point..pop.
LPP <- as.numeric(gsub(",", "", LargestPointPop))
LPP <- LargestPointPop[!is.na(LargestPointPop)]
View(LPP)
summary(LPP)
fivenum(LPP, na.rm = TRUE)
stem(LPP)
hist(LPP)
lines(density(LPP, na.rm = TRUE, bw = 1.))
rug(LPP)
plot(ecdf(LPP), do.points = FALSE, verticals = TRUE)
par(pty="s")
qqnorm(LPP); qqline(LPP)