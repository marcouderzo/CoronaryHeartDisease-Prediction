## ----setup, include=FALSE---------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(warning = FALSE, message = FALSE)


## ---- include=FALSE---------------------------------------------------------------------------------------------------------
library(MASS)
library(pROC)
library(class)
library(ggplot2)
library(ggcorrplot)
library(gridExtra)
library(corrplot)
library(correlation)
library(ggm)
library(igraph)
library(tidymodels)
library(naivebayes)
library(ROCR)
library(glmnet)
library(stats)

data <- read.csv("data/heart_data.csv", stringsAsFactors = T)

attach(data)


## ---- include=TRUE----------------------------------------------------------------------------------------------------------
prop.table(table(HeartDisease))


## ---- include=FALSE---------------------------------------------------------------------------------------------------------
# continuous variables
cont.idx <- c(1,4,5,8,10)

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



## ---- include=TRUE----------------------------------------------------------------------------------------------------------
chol.plot
oldpeak.plot
fastingBS.plot
restingBP.plot


## ---- include=TRUE----------------------------------------------------------------------------------------------------------
age.box <- boxplot(Age ~ HeartDisease, col=colours)
maxHR.box <- boxplot(MaxHR ~ HeartDisease, col=colours)


## ---- include=FALSE---------------------------------------------------------------------------------------------------------
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



## ---- include=TRUE----------------------------------------------------------------------------------------------------------
cpt.plot
restingECG.plot
exAn.plot
st.plot


## ---- include=TRUE----------------------------------------------------------------------------------------------------------
head(data[Cholesterol == 0, c("RestingBP", "MaxHR", "RestingECG")])


## ---- include=TRUE----------------------------------------------------------------------------------------------------------
cont.idx <- c(1,4,8,10)

cor.data <- cor(data[,cont.idx])
corrplot(cor.data,
         method="color",
         diag=F,
         tl.cex=0.4,
         number.cex=0.5,
         tl.col="black",
         addCoef.col="grey50",
         cl.pos="n")


## ---- include=TRUE----------------------------------------------------------------------------------------------------------
S <- var(data[,cont.idx])
R <- -cov2cor(solve(S))
G <- abs(R)>0.1
diag(G) <- 0


## ---- include=TRUE----------------------------------------------------------------------------------------------------------
set.seed(123)
split <- initial_split(data[,-c(5)], prop=0.75)

train <- training(split)
test <- testing(split)


## ---- include=FALSE---------------------------------------------------------------------------------------------------------
calculate.metrics <- function(conf.mat) {
  acc <- sum(diag(conf.mat))/sum(conf.mat)
  prec <- conf.mat[2,2] / sum(conf.mat[,2])
  rec <- conf.mat[2,2] / sum(conf.mat[2,])
  f1.score <- 2*prec*rec/(prec+rec)
  fn.rate <- conf.mat[1,2]/sum(conf.mat[,2])
  out <- list(acc, prec, rec, f1.score, fn.rate)
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



## ---- include=TRUE----------------------------------------------------------------------------------------------------------
lda.fit <- lda(HeartDisease~., data=train)
lda.fit
lda.pred <- predict(lda.fit, test, type="response")
lda.res <- lda.pred$posterior

# if we want to minimize the false positives the best result is when t = 0.6
lda.pred.best <- as.factor(ifelse(lda.res[,2] > 0.6, 1, 0))

lda.conf.mat <- table(test$HeartDisease, lda.pred.best)
lda.conf.mat

# accuracy, precision, recall, f1 score
lda.metrics <- calculate.metrics(lda.conf.mat)
lda.metrics


## ---- include=TRUE----------------------------------------------------------------------------------------------------------
# ROC
lda.auc <- model.plot.roc(lda.res[,2], test$HeartDisease)
lda.auc

# ldahist(lda.pred$x[,1], g=lda.pred$class, col=2)


## ---- include=TRUE----------------------------------------------------------------------------------------------------------
qda.fit <- qda(HeartDisease~., data=train)

qda.pred <- predict(qda.fit, test)
qda.pred.best <- as.factor(ifelse(lda.res[,2] > 0.5, 1, 0))

qda.conf.mat <- table(qda.pred.best, test$HeartDisease)
qda.conf.mat

qda.metrics <- calculate.metrics(qda.conf.mat)
qda.metrics

qda.auc <- model.plot.roc(qda.pred$posterior[,2], test$HeartDisease)
qda.auc



## ---- include=TRUE----------------------------------------------------------------------------------------------------------
glm.model <- glm(data=train, HeartDisease~., family="binomial")
glm_summary <- summary(glm.model)
glm_summary


## ---- include=TRUE----------------------------------------------------------------------------------------------------------

r2 <- 1 - (glm_summary$deviance/glm_summary$null.deviance)
r2


## ---- include=TRUE----------------------------------------------------------------------------------------------------------
prediction.glm.model <- predict(glm.model, newdata=test, type="response")
prediction.glm.model.binary <- ifelse(prediction.glm.model > 0.6, 1, 0)

conf_matrix <- table(test$HeartDisease, prediction.glm.model.binary)
glm.model.metrics <- calculate.metrics(conf_matrix)
glm.model.metrics

glm.model.auc <- model.plot.roc(prediction.glm.model, test$HeartDisease)
glm.model.auc


## ---- include=TRUE----------------------------------------------------------------------------------------------------------
glm.model.1 <- update(glm.model, ~. - Age)
glm.model.2 <- update(glm.model.1, ~. - RestingECG)
glm.model.3 <- update(glm.model.2, ~. - MaxHR)
glm.model.4 <- update(glm.model.3, ~. - RestingBP)

summary(glm.model.4)



## ---- include=TRUE----------------------------------------------------------------------------------------------------------


glm.models = list(glm.model, glm.model.1, glm.model.2, glm.model.3, glm.model.4)

i=0
for (mdl in glm.models){
  r2 <- 1 - (mdl$deviance/mdl$null.deviance)
  vif <- 1/(1-r2)
  cat("GLM Model ", i, " - ", "VIF: ", vif, "\n")
  i=i+1
}






## ---- include=TRUE----------------------------------------------------------------------------------------------------------
glm.models = list(glm.model, glm.model.1, glm.model.2, glm.model.3, glm.model.4)

i=0
for (mdl in glm.models){
  glm.bic <- BIC(mdl)
  cat("GLM Model ", i, " - ","BIC: ", glm.bic, "\n")
  i=i+1
}



## ---- include=TRUE----------------------------------------------------------------------------------------------------------
# Full model

glm.model.full <- glm(data=train, HeartDisease~., family="binomial")

deviance(glm.model.full)


pi.hat <- predict(glm.model.full, type="response")
DF <- -2*sum(train$HeartDisease*log(pi.hat)+(1-train$HeartDisease)*log(1-pi.hat))


df.F <- glm.model.full$df.residual



glm.model.reduced <- glm.model.4


deviance(glm.model.reduced)

pi.hat <- predict(glm.model.reduced, type="response")
DR <- -2*sum(train$HeartDisease*log(pi.hat)+(1-train$HeartDisease)*log(1-pi.hat))

df.R <- glm.model.reduced$df.residual


# Deviance difference test

dev.test <- DR-DF

df <- df.R- df.F


pvalue <- 1-pchisq(dev.test, df)
pvalue

# deviance difference using the anova() function

anova(glm.model.reduced, glm.model.full, test="Chisq")



## ---- include=TRUE----------------------------------------------------------------------------------------------------------

thresholds = c(0.3, 0.4, 0.5, 0.6)

for (thr in thresholds) {

  prediction.glm.model.4 <- predict(glm.model.4, newdata=test, type="response")
  prediction.glm.model.4.binary <- ifelse(prediction.glm.model.4 > thr, 1, 0)
  
  conf_matrix <- table(test$HeartDisease, prediction.glm.model.4.binary)
  conf_matrix
  glm.model.4.metrics <- calculate.metrics(conf_matrix)
  
  cat("Threshold: ", thr, "\n")
  cat("Accuracy, Precision, Rec, F1-Score", paste(glm.model.4.metrics, collapse = ", "), "\n")
  
  
  
}


prediction.glm.model.4 <- predict(glm.model.4, newdata=test, type="response")
prediction.glm.model.4.binary <- ifelse(prediction.glm.model.4 > 0.4, 1, 0)

conf_matrix <- table(test$HeartDisease, prediction.glm.model.4.binary)
conf_matrix
glm.model.4.metrics <- calculate.metrics(conf_matrix)

cat("Accuracy, Precision, Rec, F1-Score", paste(glm.model.4.metrics, collapse = ", "), "\n")


## ---- include=TRUE----------------------------------------------------------------------------------------------------------
glm.model.4.auc <- model.plot.roc(prediction.glm.model.4, test$HeartDisease)
glm.model.4.auc



## ---- include=TRUE----------------------------------------------------------------------------------------------------------
X <- model.matrix(glm.model)
y <- train$HeartDisease

lasso.model <- cv.glmnet(X, y, family = "binomial", type.measure = "class") #cross-validation handled by glmnet

lasso.coef <- coef(lasso.model, s = "lambda.min")
lasso.vars <- rownames(lasso.coef)[-1][lasso.coef[-1,] != 0]

cat("Selected variables with Lasso Regression:\n\n", paste(lasso.vars, collapse = "\n"))


lasso.X_test <- model.matrix(glm.model, data = test)


lasso.y_pred <- predict(lasso.model, newx = lasso.X_test, s = "lambda.min", type = "response")


lasso.y_pred_class <- ifelse(lasso.y_pred > 0.6, 1, 0)  # Convert probabilities to classes


lasso.model
lasso.coef




## ---- include=TRUE----------------------------------------------------------------------------------------------------------

lasso.conf.mat <- table(lasso.y_pred_class, test$HeartDisease)
lasso.conf.mat



## ---- include=TRUE----------------------------------------------------------------------------------------------------------

lasso.metrics <- calculate.metrics(lasso.conf.mat)
lasso.metrics

lasso.auc <- model.plot.roc(lasso.y_pred, test$HeartDisease)
lasso.auc




## ---- include=TRUE----------------------------------------------------------------------------------------------------------
X <- model.matrix(glm.model)
y <- train$HeartDisease

ridge.fit <- cv.glmnet(X, y, family = "binomial", alpha = 0, type.measure = "deviance") #cross-validation handled by glmnet

coef(ridge.fit, s = "lambda.min")

ridge.X_test <- model.matrix(glm.model, data = test)

ridge.y_pred <- predict(ridge.fit, newx = ridge.X_test, s = "lambda.min", type = "response")

ridge.y_pred_class <- ifelse(ridge.y_pred > 0.6, 1, 0)  # Convert probabilities to classes

lambda_min_value <- ridge.fit$lambda.min
lambda_min_value


## ---- include=TRUE----------------------------------------------------------------------------------------------------------
ridge.conf.mat <- table(ridge.y_pred_class, test$HeartDisease)
ridge.conf.mat


## ---- include=TRUE----------------------------------------------------------------------------------------------------------
ridge.metrics <- calculate.metrics(ridge.conf.mat)
ridge.metrics


ridge.auc <- model.plot.roc(ridge.y_pred, test$HeartDisease)
ridge.auc


## ---- include=TRUE----------------------------------------------------------------------------------------------------------
train$HeartDisease <- as.factor(train$HeartDisease)
test$HeartDisease <- as.factor(test$HeartDisease)


naivebayes.model <- naive_bayes(HeartDisease~., data=test)
naivebayes.prediction <- predict(naivebayes.model, newx = test, type = "prob")
naivebayes.y_pred_class <- ifelse(naivebayes.prediction > 0.6, 1, 0)



naivebayes.conf.mat <- table(naivebayes.y_pred_class[,2], test$HeartDisease)


naivebayes.metrics <- calculate.metrics(naivebayes.conf.mat)
naivebayes.metrics

naivebayes.auc <- model.plot.roc(naivebayes.prediction[,2], test$HeartDisease)
naivebayes.auc



## ---- include=TRUE----------------------------------------------------------------------------------------------------------

# acc, prec, rec, f1.score

lda.acc <- lda.metrics[1][[1]]
lda.prec <- lda.metrics[2][[1]]
lda.rec <- lda.metrics[3][[1]]
lda.f1 <- lda.metrics[4][[1]]
lda.fn <- lda.metrics[5][[1]]
lda.auc <- lda.auc[[1]]

qda.acc <- qda.metrics[1][[1]]
qda.prec <- qda.metrics[2][[1]]
qda.rec <- qda.metrics[3][[1]]
qda.f1 <- qda.metrics[4][[1]]
qda.fn <- qda.metrics[5][[1]]
qda.auc <- qda.auc[[1]]

glm.4.acc <- glm.model.4.metrics[[1]][[1]]
glm.4.prec <- glm.model.4.metrics[2][[1]]
glm.4.rec <- glm.model.4.metrics[3][[1]]
glm.4.f1 <- glm.model.4.metrics[4][[1]]
glm.4.fn <- glm.model.4.metrics[5][[1]]
glm.4.auc <- glm.model.4.auc[[1]]

lasso.acc <- lasso.metrics[1][[1]]
lasso.prec <- lasso.metrics[2][[1]]
lasso.rec <- lasso.metrics[3][[1]]
lasso.f1 <- lasso.metrics[4][[1]]
lasso.fn <- lasso.metrics[5][[1]]
lasso.auc <- lasso.auc[[1]]

ridge.acc <- ridge.metrics[1][[1]]
ridge.prec <- ridge.metrics[2][[1]]
ridge.rec <- ridge.metrics[3][[1]]
ridge.f1 <- ridge.metrics[4][[1]]
ridge.fn <- ridge.metrics[5][[1]]
ridge.auc <- ridge.auc[[1]]

naivebayes.acc <- naivebayes.metrics[1][[1]]
naivebayes.prec <- naivebayes.metrics[2][[1]]
naivebayes.rec <- naivebayes.metrics[3][[1]]
naivebayes.f1 <- naivebayes.metrics[4][[1]]
naivebayes.fn <- naivebayes.metrics[5][[1]]
naivebayes.auc <- naivebayes.auc[[1]]




acc <- c(lda.acc, qda.acc, glm.4.acc, lasso.acc, ridge.acc, naivebayes.acc)
prec <- c(lda.prec, qda.prec, glm.4.prec, lasso.prec, ridge.prec, naivebayes.prec)
rec <- c(lda.rec, qda.rec, glm.4.rec, lasso.rec, ridge.rec, naivebayes.rec)
f1.score <- c(lda.f1, qda.f1, glm.4.f1, lasso.f1, ridge.f1, naivebayes.f1)
fn.rate <- c(lda.fn, qda.fn, glm.4.fn, lasso.fn, ridge.fn, naivebayes.fn)
auc <- c(lda.auc, qda.auc, glm.4.auc, lasso.auc, ridge.auc, naivebayes.auc)


model_performance <- matrix(c(acc, prec, rec, f1.score, fn.rate, auc), nrow = 6, ncol = 6, byrow = FALSE)

model_names <- c("LDA", "QDA", "Logistic Regression", "Lasso Regression", "Ridge Regression", "Naive Bayes")

performance_summary <- data.frame(Model = model_names, model_performance)


colnames(performance_summary) <- c("Model", "Accuracy", "Precision", "Recall", "F1 Score", "FN Rate", "AUC")

print(performance_summary)



## ---- include=TRUE----------------------------------------------------------------------------------------------------------

roc.lda <- roc(test$HeartDisease, lda.res[,2])
roc.qda <- roc(test$HeartDisease, qda.pred$posterior[,2])
roc.glm <- roc(test$HeartDisease, prediction.glm.model.4)
roc.lasso <- roc(test$HeartDisease, lasso.y_pred)
roc.ridge <- roc(test$HeartDisease, ridge.y_pred)
roc.naivebayes <- roc(test$HeartDisease, naivebayes.prediction[,2])


plot(roc.lda, col = "blue", print.auc = FALSE, print.auc.y = 0, lwd=1.5)

lines(roc.qda, col = "red", print.auc = FALSE, print.auc.y = 0, lwd=1.5)
lines(roc.glm, col = "green", print.auc = FALSE, print.auc.y = 0, lwd=1.5)
lines(roc.lasso, col = "orange", print.auc = FALSE, print.auc.y = 0, lwd=1.5)
lines(roc.ridge, col = "pink", print.auc = FALSE, print.auc.y = 0, lwd=1.5)
lines(roc.naivebayes, col = "cyan", print.auc = FALSE, print.auc.y = 0,lwd=1.5)



legend("bottomright", legend = c("LDA", "QDA", "GLM","Lasso", "Ridge", "Naive Bayes"), col = c("blue", "red", "green", "orange", "pink", "cyan"), lty = 1)




