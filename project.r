# https://www.kaggle.com/datasets/fedesoriano/heart-failure-prediction
data.orig <- read.csv("data/heart_data.csv")

head(data.orig)

# we get the shape of the dataframe
nrows <- dim(data.orig)[1]
ncols <- dim(data.orig)[2]

# we check if there are missing values
anyNA(data.orig) # apparently no

summary(data.orig)

data <- data.frame(data.orig)

# categorical values
# Sex: 0 -> male, 1 -> female
data$Sex[data$Sex == "M"] <- 0
data$Sex[data$Sex == "F"] <- 1
data$Sex <- as.numeric(data$Sex)

# ChestPainType: 1 -> TA, 2 -> ATA, 3 -> NAP, 4 -> ASY
data$ChestPainType[data$ChestPainType == "TA"] <- 1
data$ChestPainType[data$ChestPainType == "ATA"] <- 2
data$ChestPainType[data$ChestPainType == "NAP"] <- 3
data$ChestPainType[data$ChestPainType == "ASY"] <- 4
data$ChestPainType <- as.numeric(data$ChestPainType)

# RestingECG: 0 -> "Normal", 1 -> "ST", 2 -> "LVH"
data$RestingECG[data$RestingECG == "Normal"] <- 0
data$RestingECG[data$RestingECG == "ST"] <- 1
data$RestingECG[data$RestingECG == "LVH"] <- 2
data$RestingECG <- as.numeric(data$RestingECG)

# ExerciseAngina: 0 -> "N", 1 -> "Y"
data$ExerciseAngina[data$ExerciseAngina == "N"] <- 0
data$ExerciseAngina[data$ExerciseAngina == "Y"] <- 1
data$ExerciseAngina <- as.numeric(data$ExerciseAngina)

# ST_slope: 1 -> "Up", 2 -> "Flat", 3 -> "Down"
data$ST_Slope[data$ST_Slope == "Up"] <- 1
data$ST_Slope[data$ST_Slope == "Flat"] <- 2
data$ST_Slope[data$ST_Slope == "Down"] <- 3
data$ST_Slope <- as.numeric(data$ST_Slope)

attach(data)

# data balance check
prop.table(table(HeartDisease))

# visualizing the data
barplot(table(data.orig$ChestPainType))

counts <- table(Sex, HeartDisease)
barplot(counts, beside=T, names.arg=c("Normal", "Heart disease"),
        legend.text = c("M", "F"))

counts <- table(data.orig$ChestPainType, HeartDisease)
barplot(counts, beside=T, names.arg=c("Normal", "Heart disease"),
        legend.text = T)

# TODO: implement better legend and better graphics

boxplot(Age ~ HeartDisease)
hist(Age)

boxplot(RestingBP ~ HeartDisease)
hist(RestingBP)

boxplot(Cholesterol ~ HeartDisease)
hist(Cholesterol)

# TODO
# - plot values with histogram and boxplot
# - correlations, pairplot