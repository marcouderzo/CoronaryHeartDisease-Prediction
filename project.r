# https://www.kaggle.com/datasets/fedesoriano/heart-failure-prediction
install.packages("pROC")
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("corrplot")
install.packages("ggm")
install.packages("tidymodels")
install.packages("naivebayes")
install.packages("ROCR")
install.packages("glmnet")

library(MASS)
library(pROC)
library(class)
library(ggplot2)
library(corrplot)
library(ggm)
library(tidymodels)
library(naivebayes)
library(ROCR)
library(glmnet)

data.orig <- read.csv("data/heart_data.csv", stringsAsFactors = T)

head(data.orig)

# we check if there are missing values
anyNA(data.orig)

# copy of dataset in case I need to change it
data <- data.frame(data.orig)

attach(data)

# data balance check
prop.table(table(HeartDisease))

# continuous variables
cont.idx <- c(1,4,5,8,10)

# visualizing the data
# continuous variables
colours <- c("#F8766D", "#00BFC4")

# Age
age.plot <- ggplot(data, aes(x=Age, group=HeartDisease,
                             fill=factor(HeartDisease))) +
  geom_density(alpha=0.4) + 
  ggtitle("Age - Density Plot") + xlab("Age") +
  guides(fill = guide_legend(title="Heart disease"))

# RestingBP
restingBP.plot <- ggplot(data, aes(x=RestingBP, group=HeartDisease,
                               fill=factor(HeartDisease))) +
  geom_density(alpha=0.4) + 
  ggtitle("RestingBP - Density Plot") + xlab("RestingBP") +
  guides(fill = guide_legend(title="Heart disease"))

# Cholesterol
chol.plot <- ggplot(data, aes(x=Cholesterol, group=HeartDisease,
                                            fill=factor(HeartDisease))) +
  geom_density(alpha=0.4) + 
  ggtitle("Cholesterol - Density Plot") + xlab("Cholesterol") +
  guides(fill = guide_legend(title="Heart disease"))

# MaxHR
maxHR.plot <- ggplot(data, aes(x=MaxHR, group=HeartDisease,
                               fill=factor(HeartDisease))) +
  geom_density(alpha=0.4) + 
  ggtitle("MaxHR - Density Plot") + xlab("MaxHR") +
  guides(fill = guide_legend(title="Heart disease"))

# Oldpeak
oldpeak.plot <- ggplot(data, aes(x=Oldpeak, group=HeartDisease,
                               fill=factor(HeartDisease))) +
  geom_density(alpha=0.4) + 
  ggtitle("Oldpeak - Density Plot") + xlab("Oldpeak") +
  guides(fill = guide_legend(title="Heart disease"))

# FastingBS
fastingBS.plot <- ggplot(data, aes(x=FastingBS, group=HeartDisease,
                                   fill=factor(HeartDisease))) +
  geom_bar(alpha=0.5, position="dodge") +
  guides(fill = guide_legend(title="Heart disease"))


# Boxplots
age.box <- boxplot(Age ~ HeartDisease, col=colours)
restingBP.box <- boxplot(RestingBP ~ HeartDisease, col=colours)
chol.box <- boxplot(Cholesterol ~ HeartDisease, col=colours)
maxHR.box <- boxplot(MaxHR ~ HeartDisease, col=colours)
oldpeak.box <- boxplot(Oldpeak ~ HeartDisease, col=colours)


# categorical variables
# Sex
sex.plot <- ggplot(data, aes(x=Sex, group=HeartDisease,
                                   fill=factor(HeartDisease))) +
  geom_bar(alpha=0.5, position="dodge") +
  guides(fill = guide_legend(title="Heart disease"))

# ChestPainType
cpt.plot <- ggplot(data, aes(x=ChestPainType, group=HeartDisease,
                                   fill=factor(HeartDisease))) +
  geom_bar(alpha=0.5, position="dodge") +
  guides(fill = guide_legend(title="Heart disease"))

# RestingECG
restingECG.plot <- ggplot(data, aes(x=RestingECG, group=HeartDisease,
                                   fill=factor(HeartDisease))) +
  geom_bar(alpha=0.5, position="dodge") +
  guides(fill = guide_legend(title="Heart disease"))

# ExerciseAngina
exAn.plot <- ggplot(data, aes(x=ExerciseAngina, group=HeartDisease,
                                   fill=factor(HeartDisease))) +
  geom_bar(alpha=0.5, position="dodge") +
  guides(fill = guide_legend(title="Heart disease"))

# ST_Slope
st.plot <- ggplot(data, aes(x=ST_Slope, group=HeartDisease,
                                   fill=factor(HeartDisease))) +
  geom_bar(alpha=0.5, position="dodge") +
  guides(fill = guide_legend(title="Heart disease"))


# missing values
# NOTE: changing missing values doesn't change accuracy of predictions
# NOTE: can it be measure of both HDL and LDL?
# TODO: why most values with 0 Chol have 1 HD?
head(data[Cholesterol == 0,])

ch <- Cholesterol[Cholesterol != 0]
hd <- data[Cholesterol != 0, "HeartDisease"]
aov(hd ~ ch)

# point-biserial correlation
cor.test(HeartDisease, Cholesterol) # negative correlation
cor.test(hd, ch) # positive correlation


data$Cholesterol[data$Cholesterol == 0] <- median(data$Cholesterol)

data$RestingBP[data$RestingBP == 0] <- median(data$RestingBP)

attach(data)

# outliers
# Age -> no outliers
# RestingBP -> the values even if they are high they are plausible
# Cholesterol -> many missing values, too high outliers, no correlation
# serum mean total chol not useful information

# MaxHR -> no outliers
# Oldpeak -> values follow the range, there are many values with 0


# correlations

# chi-squared test
sex.t <- table(HeartDisease, Sex)
chisq.test(sex.t)

cpt.t <- table(HeartDisease, ChestPainType)
chisq.test(cpt.t)

fbs.t <- table(HeartDisease, FastingBS)
chisq.test(fbs.t)

recg.t <- table(HeartDisease, RestingECG)
chisq.test(recg.t)

exan.t <- table(HeartDisease, ExerciseAngina)
chisq.test(exan.t)

st.t <- table(HeartDisease, ST_Slope)
chisq.test(st.t)

cont.idx <- c(1,4,8,10)

# correlation matrix
cor.data <- cor(data[,cont.idx])
corrplot(cor.data,
         method="color",
         diag=F,
         tl.cex=0.4,
         number.cex=0.5,
         tl.col="black",
         addCoef.col="grey50",
         cl.pos="n")


# igraph
S <- var(data[,cont.idx])
R <- -cov2cor(solve(S))
G <- abs(R)>0.1
diag(G) <- 0
G
# Gi <- as(G, "igraph")
# tkplot(Gi, vertex.color="white")


# Train-Test split
set.seed(123)
split <- initial_split(data[,-c(5)], prop=0.75) # removed Cholesterol

train <- training(split)
test <- testing(split)


calculate.metrics <- function(conf.mat) {
  acc <- sum(diag(conf.mat))/sum(conf.mat)
  prec <- conf.mat[2,2] / sum(conf.mat[,2])
  rec <- conf.mat[2,2] / sum(conf.mat[2,])
  f1.score <- 2*prec*rec/(prec+rec)
  out <- list(acc, prec, rec, f1.score)
  return(out)
}

model.plot.roc <- function(predm, labl) {
  pred <- prediction(predm, labl)
  perf <- performance(pred, measure="tpr", x.measure="fpr")
  plot(perf, main="ROC")
  abline(a=0, b= 1)
  auc.perf <- performance(pred, measure = "auc")
  return(auc.perf@y.values)
}




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
#mean(prediction.glm.model.binary != test$HeartDisease) #how many are wrong

glm.model.metrics <- calculate.metrics(conf_matrix)





# Variable Selection using p-value

glm.model.1 <- update(glm.model, ~. - Age)
glm.model.2 <- update(glm.model.1, ~. - RestingECG)
glm.model.3 <- update(glm.model.2, ~. - MaxHR)

glm_summary <- summary(glm.model.3)
summary(glm.model.3)


#calculate odds of success given R-squared value
r2 <- 1 - (glm_summary$deviance/glm_summary$null.deviance)
1/(1-r2)


# prediction and conversion to binary
prediction.glm.model.3 <- predict(glm.model.3, newdata=test, type="response")
prediction.glm.model.3.binary <- ifelse(prediction.glm.model > 0.4, 1, 0)

conf_matrix <- table(test$HeartDisease, prediction.glm.model.3.binary)
conf_matrix
glm.model.3.metrics <- calculate.metrics(conf_matrix)
glm.model.3.metrics


# threshold > 3: accuracy 0.902, precision 0.874, recall 0.992
# threshold > 4: accuracy 0.913, precision 0.899, recall 0.975 <-
# threshold > 5: accuracy 0.897, precision 0.897, recall 0.95  <-
# threshold > 6: accuracy 0.886, precision 0.908, recall 0.916


# Plot ROC Curve


model.plot.roc(prediction.glm.model.3, test$HeartDisease)


# Lasso Regression


X <- model.matrix(glm.model.3)
y <- train$HeartDisease

lasso.model <- cv.glmnet(X, y, family = "binomial", type.measure = "class")

lasso.coef <- coef(lasso.model, s = "lambda.min")
lasso.vars <- rownames(lasso.coef)[-1][lasso.coef[-1,] != 0]

cat("Selected variables with Lasso Regression:", paste(lasso.vars, collapse = ", "))

# Selected variables: SexM, ChestPainTypeATA, ChestPainTypeNAP, ChestPainTypeTA, RestingBP, Cholesterol, FastingBS, ExerciseAnginaY, Oldpeak, ST_SlopeFlat, ST_SlopeUp



# Ridge Regression



X <- model.matrix(glm.model.3)
y <- train$HeartDisease

fit <- cv.glmnet(X, y, family = "binomial", alpha = 0, type.measure = "deviance") # alpha=0 is ridge regression. deviance to be minimized

coef(fit, s = "lambda.min") # extract the coefficients from the optimal model. optimal value of regularization parameter with s="lambda.min"

#The coefficients represent the effect of each predictor variable on the log odds of 
#the response variable, after adjusting for the other predictor variables and the regularization penalty.

#Ridge regression is a model tuning method that is used to analyse any data that suffers from multicollinearity. 
#This method performs L2 regularization. When the issue of multicollinearity occurs, least-squares are unbiased, 
#and variances are large, this results in predicted values being far away from the actual values.


### Naive Bayes Classifier

train$HeartDisease <- as.factor(train$HeartDisease)
test$HeartDisease <- as.factor(test$HeartDisease)

naivebayes.model <- naive_bayes(HeartDisease~., data=train)
naivebayes.prediction <- predict(naivebayes.model, test, type= "prob")
head(cbind(naivebayes.prediction, test$HeartDisease))

naivebayes.conf_matrix <- table(naivebayes.prediction, test$HeartDisease)
naivebayes.conf_matrix

naivebayes.metrics <- calculate.metrics(naivebayes.conf_matrix)
naivebayes.metrics

naivebayes.probabilities <- attr(naivebayes.prediction, "probabilities")[, "Yes"]



model.plot.roc(naivebayes.probabilities, test$HeartDisease)



### LDA
lda.fit <- lda(HeartDisease~., data=train)
plot(lda.fit, type="density")

# TODO: show which variables have greater impact

lda.pred <- predict(lda.fit, test, type="response")
lda.res <- lda.pred$posterior

lda.pred.best <- as.factor(ifelse(lda.res[,2] > 0.6, 1, 0))

# the best result is when t = 0.6 if we want to minimize False Positives
lda.conf.mat <- table(test$HeartDisease, lda.pred.best)
lda.conf.mat

# error rate
mean(lda.pred.best!=test$HeartDisease)

# accuracy, precision, recall, f1 score
lda.metrics <- calculate.metrics(lda.conf.mat)
lda.metrics

# ROC
lda.auc <- model.plot.roc(lda.res[,2], test$HeartDisease)
lda.auc

ldahist(lda.pred$x[,1], g=lda.pred$class, col=2)



### QDA
qda.fit <- qda(HeartDisease~., data=train)

qda.pred <- predict(qda.fit, test)
qda.pred.best <- as.factor(ifelse(lda.res[,2] > 0.5, 1, 0))

qda.conf.mat <- table(qda.pred.best, test$HeartDisease)
qda.conf.mat

qda.metrics <- calculate.metrics(qda.conf.mat)
qda.metrics

qda.auc <- model.plot.roc(qda.pred$posterior[,2], test$HeartDisease)
qda.auc
