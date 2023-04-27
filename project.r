# https://www.kaggle.com/datasets/fedesoriano/heart-failure-prediction
install.packages("pROC")
install.packages("ggplot2")
install.packages("corrplot")
install.packages("tidymodels")
install.packages("naivebayes")
install.packages("ROCR")
install.packages("glmnet")

library(MASS)
library(pROC)
library(class)
library(ggplot2)
library(corrplot)
library(tidymodels)
library(naivebayes)
library(ROCR)
library(glmnet)

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


# dealing with missing values
RestingBP[RestingBP == 0] <- median(RestingBP)


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

accuracy <- sum(diag(conf_matrix))/sum(conf_matrix) # proportion of correct predictions
precision <- conf_matrix[2,2] / sum(conf_matrix[,2]) # true positive rate
recall <- conf_matrix[2,2] / sum(conf_matrix[2,]) # sensitivity

cat("Accuracy:", round(accuracy, 3), "\n") # Accuracy 0.875
cat("Precision:", round(precision, 3), "\n") # Precision: 0.9
cat("Recall:", round(recall, 3), "\n") # Recall: 0.908
cat("Confusion Matrix:\n")
print(conf_matrix)


# Variable Selection using p-value


# Removing Age
glm.model <- glm(data=train, HeartDisease~. - Age, family="binomial")
glm_summary <- summary(glm.model)
summary(glm.model)

#calculate odds of success given R-squared value
r2 <- 1 - (glm_summary$deviance/glm_summary$null.deviance)
1/(1-r2)

# prediction and conversion to binary
prediction.glm.model <- predict(glm.model, newdata=test, type="response")
prediction.glm.model.binary <- ifelse(prediction.glm.model > 0.6, 1, 0)

conf_matrix <- table(test$HeartDisease, prediction.glm.model.binary)
mean(prediction.glm.model.binary != test$HeartDisease)
accuracy <- sum(diag(conf_matrix))/sum(conf_matrix) # proportion of correct predictions
precision <- conf_matrix[2,2] / sum(conf_matrix[,2]) # true positive rate
recall <- conf_matrix[2,2] / sum(conf_matrix[2,]) # sensitivity

cat("Accuracy:", round(accuracy, 3), "\n") # Accuracy: 0.88
cat("Precision:", round(precision, 3), "\n") # Precision: 0.901
cat("Recall:", round(recall, 3), "\n") # Recall: 0.916
cat("Confusion Matrix:\n")
print(conf_matrix)


# Removing RestingECG
glm.model <- glm(data=train, HeartDisease~. - Age - RestingECG, family="binomial")
glm_summary <- summary(glm.model)
summary(glm.model)


#calculate odds of success given R-squared value
r2 <- 1 - (glm_summary$deviance/glm_summary$null.deviance)
1/(1-r2)

# prediction and conversion to binary
prediction.glm.model <- predict(glm.model, newdata=test, type="response")
prediction.glm.model.binary <- ifelse(prediction.glm.model > 0.6, 1, 0)

conf_matrix <- table(test$HeartDisease, prediction.glm.model.binary)
mean(prediction.glm.model.binary != test$HeartDisease)
accuracy <- sum(diag(conf_matrix))/sum(conf_matrix) # proportion of correct predictions
precision <- conf_matrix[2,2] / sum(conf_matrix[,2]) # true positive rate
recall <- conf_matrix[2,2] / sum(conf_matrix[2,]) # sensitivity

cat("Accuracy:", round(accuracy, 3), "\n") # Accuracy: 0.875
cat("Precision:", round(precision, 3), "\n") # Precision: 0.914
cat("Recall:", round(recall, 3), "\n") # Recall: 0.916 -> 0.891
cat("Confusion Matrix:\n")
print(conf_matrix)

# Removing MaxHR
glm.model <- glm(data=train, HeartDisease~. - Age - RestingECG - MaxHR, family="binomial")
glm_summary <- summary(glm.model)
summary(glm.model)


#calculate odds of success given R-squared value
r2 <- 1 - (glm_summary$deviance/glm_summary$null.deviance)
1/(1-r2)

# prediction and conversion to binary
prediction.glm.model <- predict(glm.model, newdata=test, type="response")
prediction.glm.model.binary <- ifelse(prediction.glm.model > 0.4, 1, 0)

conf_matrix <- table(test$HeartDisease, prediction.glm.model.binary)
mean(prediction.glm.model.binary != test$HeartDisease)
accuracy <- sum(diag(conf_matrix))/sum(conf_matrix) # proportion of correct predictions
precision <- conf_matrix[2,2] / sum(conf_matrix[,2]) # true positive rate
recall <- conf_matrix[2,2] / sum(conf_matrix[2,]) # sensitivity

cat("Accuracy:", round(accuracy, 3), "\n") # Accuracy: 0.897
cat("Precision:", round(precision, 3), "\n") # Precision: 0.917
cat("Recall:", round(recall, 3), "\n") # Recall: 0.916 -> 0.891 -> 0.924
cat("Confusion Matrix:\n")
print(conf_matrix)

# threshold > 3: accuracy 0.902, precision 0.874, recall 0.992
# threshold > 4: accuracy 0.913, precision 0.899, recall 0.975 <-
# threshold > 5: accuracy 0.897, precision 0.897, recall 0.95  <-
# threshold > 6: accuracy 0.886, precision 0.908, recall 0.916

# to do: use update function and separate each model with different names.


pred <- prediction(prediction.glm.model.binary, test$HeartDisease)

perf <- performance(pred, measure = "tpr", x.measure = "fpr")

# Plot the ROC curve
plot(perf, main="ROC Curve", colorize=T)


# Lasso Regression

X <- model.matrix(prediction.glm.model) #design matrix
y <- prediction.glm.model$HeartDisease #response vector

lasso.model <- cv.glmnet(X, y, family = "binomial", type.measure = "class")


lasso.coef <- coef(lasso.model, s = "lambda.min") # Extract the coefficients and non-zero variables
lasso.vars <- rownames(lasso.coef)[-1][lasso.coef[-1,] != 0]

cat("Selected variables:", paste(lasso.vars, collapse = ", "))




### Naive Bayes Classifier

train$HeartDisease <- as.factor(train$HeartDisease)

naivebayes.model <- naive_bayes(HeartDisease~., data=train)
prediction <- predict(naivebayes.model, train)
head(cbind(prediction, train))


#Error in table(prediction, test$HeartDisease) : 
#tutti gli argomenti devono avere la medesima lunghezza
#BUT WHY WOULD THAT BE

naivebayes.conf_matrix <- table(prediction, test$HeartDisease)

# For Marco

# - variable selection with p-value
# - logistic regression with Ridge and Lasso regularization
# - Naive Bayes
# - KNN


# LDA
lda.fit <- lda(HeartDisease~., data=train)
plot(lda.fit, type="density")

# TODO: show which variables have greater impact

lda.pred <- predict(lda.fit, test, type="response")
lda.res <- lda.pred$posterior

lda.pred.t3 <- as.factor(ifelse(lda.res[,2] > 0.3, 1, 0))
lda.pred.t4 <- as.factor(ifelse(lda.res[,2] > 0.4, 1, 0))
lda.pred.t5 <- lda.pred$class

# the best result is when t = 4 if we want to minimize False Positives
conf.mat <- table(test$HeartDisease, lda.pred.t4)
conf.mat

# error rate
mean(lda.pred.t4!=test$HeartDisease)

acc <- sum(diag(conf.mat))/sum(conf.mat)
prec <- conf.mat[2,2] / sum(conf.mat[,2])
rec <- conf.mat[2,2] / sum(conf.mat[2,])
f1.score <- 2*prec*rec/(prec+rec)

roc.out <- roc(controls=test$HeartDisease, cases=lda.pred$posterior[,2],
               direction=">")
plot(roc.out, print.auc=TRUE, legacy.axes=TRUE,
     xlab="False positive rate", ylab="True positive rate")
auc(roc.out)

ldahist(lda.pred$x[,1], g=lda.pred$class, col=2)

# TODO: how do I choose the best threshold?
# TODO: how do I upgrade the model?


# QDA
qda.fit <- qda(HeartDisease~., data=train)

qda.pred <- predict(qda.fit, test)

qda.pred.t3<- as.factor(ifelse(qda.pred$posterior[,2] > 0.3, 1, 0))
qda.pred.t4<- as.factor(ifelse(qda.pred$posterior[,2] > 0.4, 1, 0))
qda.pred.t5<- as.factor(ifelse(qda.pred$posterior[,2] > 0.5, 1, 0))
qda.pred.t6<- as.factor(ifelse(qda.pred$posterior[,2] > 0.6, 1, 0))

conf.mat <- table(qda.pred.t5, test$HeartDisease)

acc <- sum(diag(conf.mat))/sum(conf.mat)
prec <- conf.mat[2,2] / sum(conf.mat[,2])
rec <- conf.mat[2,2] / sum(conf.mat[2,])
f1.score <- 2*prec*rec/(prec+rec)

roc.out <- roc(controls=test$HeartDisease, cases=qda.pred$posterior[,2],
               direction=">")
plot(roc.out, print.auc=TRUE, legacy.axes=TRUE,
     xlab="False positive rate", ylab="True positive rate")
auc(roc.out)






# KNN
knn.pred <- knn(X.train[, -c(2, 3, 7, 9, 11)], X.test[, -c(2, 3, 7, 9, 11)],
                y.train, k=5)
table(knn.pred, y.test)


# TODO
# - plot values with histogram and boxplot
# - correlations, pairplot


# TODO: for each model write down accuracy, precision, recall
