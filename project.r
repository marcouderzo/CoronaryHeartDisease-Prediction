# https://www.kaggle.com/datasets/fedesoriano/heart-failure-prediction
install.packages("ggplot2")
install.packages("corrplot")
install.packages("tidymodels")

library(ggplot2)
library(corrplot)
library(tidymodels)

data.orig <- read.csv("data/heart_data.csv", stringsAsFactors = T)

head(data.orig)

# we get the shape of the dataframe
nrows <- dim(data.orig)[1]
ncols <- dim(data.orig)[2]

# we check if there are missing values
anyNA(data.orig)

# copy of dataset in case I need to change it
data <- data.frame(data.orig)

attach(data)

# data balance check
prop.table(table(HeartDisease))

# visualizing the data
# TODO: implement better legend and better graphics
barplot(table(ChestPainType))

counts <- table(Sex, HeartDisease)
barplot(counts, beside=T, names.arg=c("Normal", "Heart disease"),
        legend.text = c("M", "F"))

counts <- table(ChestPainType, HeartDisease)
barplot(counts, beside=T, names.arg=c("Normal", "Heart disease"),
        legend.text = T)

boxplot(Age ~ HeartDisease)
hist(Age)

boxplot(RestingBP ~ HeartDisease)
hist(RestingBP)

boxplot(Cholesterol ~ HeartDisease)
ggplot(data, aes(x = Cholesterol, fill=HeartDisease)) + 
  geom_density(alpha = 0.4) +
  facet_grid(~HeartDisease)

counts <- table(FastingBS, HeartDisease)
barplot(counts, beside=T, names.arg=c("Normal", "Heart disease"),
        legend.text = T)

counts <- table(RestingECG, HeartDisease)
barplot(counts, beside=T, names.arg=c("Normal", "Heart disease"),
        legend.text = T)

boxplot(MaxHR ~ HeartDisease)
hist(MaxHR)

counts <- table(ExerciseAngina, HeartDisease)
barplot(counts, beside=T, names.arg=c("Normal", "Heart disease"),
        legend.text = T)

boxplot(Oldpeak ~ HeartDisease)
hist(Oldpeak)

counts <- table(ST_Slope, HeartDisease)
barplot(counts, beside=T, names.arg=c("Normal", "Heart disease"),
        legend.text = T)

# correlation matrix
nums <- unlist(lapply(data, is.numeric), use.names = FALSE)
cor.data <- cor(data[, nums])
corrplot(cor.data,
         method="color",
         diag=F,
         tl.cex=0.4,
         number.cex=0.5,
         tl.col="black",
         addCoef.col="grey50",
         cl.pos="n")

# Train-Test split
set.seed(42)
split <- initial_split(data, prop=0.80)

train <- training(split)
test <- testing(split)

y.train <- train$HeartDisease
X.train <- train[, !names(train) %in% c("HeartDisease")]

y.test <- test$HeartDisease
X.test <- test[, !names(test) %in% c("HeartDisease")]


# Evaluation
# Simple Logistic Regression
# For Marco: this is just a test, do as you wish
glm.compl <- glm(data=train, HeartDisease~., family="binomial")
s <- summary(glm.compl)
r2 <- 1 - (s$deviance/s$null.deviance)
1/(1-r2)

pred.glm.compl <- predict(glm.compl, test, type="response")
pred.glm.compl.05 <- ifelse(pred.glm.compl > 0.6, 1, 0)

table(test$HeartDisease, pred.glm.compl.05)
mean(pred.glm.compl.05 != test$HeartDisease)

# TODO
# - plot values with histogram and boxplot
# - correlations, pairplot