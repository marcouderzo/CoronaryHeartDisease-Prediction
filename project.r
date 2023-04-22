# https://www.kaggle.com/datasets/fedesoriano/heart-failure-prediction
install.packages("ggplot2")
install.packages("corrplot")
install.packages("tidymodels")
install.packages("naivebayes")

library(ggplot2)
library(corrplot)
library(tidymodels)
library(naivebayes)

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

glm.model <- glm(data=train, HeartDisease~., family="binomial")
glm_summary <- summary(glm.model)

#calculate odds of success given R-squared value
r2 <- 1 - (glm_summary$deviance/glm_summary$null.deviance) # null.deviance: deviance of model with only intercept term.
1/(1-r2) # odds of success for a particular observation in logistic regression model: probability of success / probability of failure

# prediction and conversion to binary
prediction.glm.model <- predict(glm.model, newdata=test, type="response")
prediction.glm.model.binary <- ifelse(prediction.glm.model > 0.6, 1, 0)

conf_matrix <- table(test$HeartDisease, prediction.glm.model.binary) #tried also with correlationMatrix from carel package but having problems with levels
mean(prediction.glm.model.binary != test$HeartDisease) #how many are wrong

accuracy <- mean(diag(conf_matrix)) # proportion of correct predictions
precision <- conf_matrix[2,2] / sum(conf_matrix[,2]) # true positive rate
recall <- conf_matrix[2,2] / sum(conf_matrix[2,]) # sensitivity

cat("Accuracy:", round(accuracy, 3), "\n")
cat("Precision:", round(precision, 3), "\n")
cat("Recall:", round(recall, 3), "\n")
cat("Confusion Matrix:\n")
print(conf_matrix)



### Naive Bayes Classifier

train$HeartDisease <- as.factor(train$HeartDisease)

naivebayes.model <- naive_bayes(HeartDisease~., data=train)
prediction <- predict(naivebayes.model, train)
head(cbind(prediction, train))


#Error in table(prediction, test$HeartDisease) : 
#tutti gli argomenti devono avere la medesima lunghezza
#BUT WHY WOULD THAT BE

naivebayes.conf_matrix <- table(prediction, test$HeartDisease)


# TODO
# - plot values with histogram and boxplot
# - correlations, pairplot

