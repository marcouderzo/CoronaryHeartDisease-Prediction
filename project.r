# https://www.kaggle.com/datasets/fedesoriano/heart-failure-prediction
install.packages("pROC")
install.packages("ggplot2")
install.packages("ggcorrplot")
install.packages("gridExtra")
install.packages("corrplot")
install.packages("correlation")
install.packages("ggm")
install.packages("tidymodels")
install.packages("naivebayes")
install.packages("ROCR")
install.packages("glmnet")

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

# continuous variables
cont.val <- c("Age", "RestingBP", "Cholesterol", "FastingBS", "MaxHR",
              "Oldpeak")

# categorical variables
cat.val <- c("Sex", "ChestPainType", "RestingECG", "ExerciseAngina", "ST_Slope")

# visualizing the data
# continuous variables
colours <- c("#F8766D", "#00BFC4")

# Age
age.plot.1 <- ggplot(data, aes(x=Age, group=HeartDisease,
                             fill=factor(HeartDisease))) +
  geom_density(alpha=0.4) + 
  ggtitle("Age - Density Plot") + xlab("Age") +
  guides(fill = guide_legend(title="Heart disease"))

age.plot.2 <- ggplot(data, aes(x=Age, group=Sex,
                               fill=factor(Sex))) +
  geom_density(alpha=0.4) + 
  ggtitle("Age - Density Plot") + xlab("Age") +
  guides(fill = guide_legend(title="Gender"))

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

grid.arrange(age.plot.1, age.plot.2, restingBP.plot, chol.plot, maxHR.plot,
             oldpeak.plot, fastingBS.plot, nrow = 2)

# Boxplots
# TODO: should we deal with outliers?
age.box.1 <- boxplot(Age ~ HeartDisease, col=colours)
age.box.2 <- boxplot(Age ~ Sex, col=colours)
restingBP.box <- boxplot(RestingBP ~ HeartDisease, col=colours) # TODO: check outliers?
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


# QQ plots
# TODO: what if data doesn't follow normal distribution?
qqnorm(Age, pch = 1, frame = FALSE)
qqline(Age, col="steelblue")

qqnorm(RestingBP, pch = 1, frame = FALSE)
qqline(RestingBP, col="steelblue")

qqnorm(Cholesterol, pch = 1, frame = FALSE)
qqline(Cholesterol, col="steelblue")

qqnorm(MaxHR, pch = 1, frame = FALSE)
qqline(MaxHR, col="steelblue")

qqnorm(Oldpeak, pch = 1, frame = FALSE)
qqline(Oldpeak, col="steelblue")


# dealing with missing values
data$RestingBP[data$RestingBP == 0] <- median(data$RestingBP)
data$Cholesterol[data$Cholesterol == 0] <- round(rnorm(length(data$Cholesterol[data$Cholesterol == 0]),
                                                 mean(Cholesterol), sd(Cholesterol)),0)


# ANOVA
# TODO: study meaning of this
# TODO: chi-square test?
sex.aov <- aov(HeartDisease ~ Sex)
summary(sex.aov)

sex.aov.2 <- aov(Age ~ Sex)
summary(sex.aov) # TODO: why high p-value?

cpt.aov <- aov(HeartDisease ~ ChestPainType)
summary(cpt.aov)

restingECG.aov <- aov(HeartDisease ~ RestingECG)
summary(restingECG.aov)

exAn.aov <- aov(HeartDisease ~ ExerciseAngina)
summary(exAn.aov)

st.aov <- aov(HeartDisease ~ ST_Slope)
summary(sex.aov)

# correlations
# TODO: what if I find correlations between parameters?
correlation(data, partial=T)

cor(Age, HeartDisease)
cor(RestingBP, HeartDisease)
cor(Cholesterol, HeartDisease)
cor(FastingBS, HeartDisease)
cor(MaxHR, HeartDisease)
cor(Oldpeak, HeartDisease)

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

model.matrix(~0+., data=data) %>%
  cor(use="pairwise.complete.obs") %>%
  ggcorrplot(show.diag=FALSE, lab=T, lab_size=1.5, tl.cex=5)

# removed Oldpeak matrix no more singular
# TODO: ask the professor for the graph
S <- var(cor.data[, -c(6)])
R <- -cov2cor(solve(S))
G <- abs(R)>0.1
diag(G) <- 0
Gi <- as(G, "igraph")
tkplot(Gi, vertex.color="white")


# scaled dataset
data.scaled <- data.frame(data)
for (i in c(1,4,5,8,10)) {
  v <- data.scaled[,i]
  data.scaled[,i] <- (v-min(v))/(max(v)-min(v))
}


# Train-Test split
set.seed(123)
split <- initial_split(data, prop=0.75)
split.scaled <- initial_split(data.scaled, prop=0.75)

train <- training(split)
test <- testing(split)

y.train <- train$HeartDisease
X.train <- train[, !names(train) %in% c("HeartDisease")]

y.test <- test$HeartDisease
X.test <- test[, !names(test) %in% c("HeartDisease")]

train.scaled <- training(split.scaled)
test.scaled <- testing(split.scaled)

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


model.plot.roc(prediction.glm.model.3.binary, test$HeartDisease)


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


### Naive Bayes Classifier

train$HeartDisease <- as.factor(train$HeartDisease)
test$HeartDisease <- as.factor(test$HeartDisease)

naivebayes.model <- naive_bayes(HeartDisease~., data=train)
naivebayes.prediction <- predict(naivebayes.model, test)
head(cbind(naivebayes.prediction, test$HeartDisease))

naivebayes.conf_matrix <- table(naivebayes.prediction, test$HeartDisease)
naivebayes.conf_matrix

naivebayes.metrics <- calculate.metrics(naivebayes.conf_matrix)
naivebayes.metrics

model.plot.roc(naivebayes.prediction, test$HeartDisease)



### LDA
# NOTE: scaling dataset gets worse results
lda.fit <- lda(HeartDisease~., data=train)
plot(lda.fit, type="density")

# TODO: show which variables have greater impact

lda.pred <- predict(lda.fit, test, type="response")
lda.res <- lda.pred$posterior

lda.pred.t3 <- as.factor(ifelse(lda.res[,2] > 0.3, 1, 0))
lda.pred.t4 <- as.factor(ifelse(lda.res[,2] > 0.4, 1, 0))
lda.pred.t5 <- lda.pred$class

lda.pred.best <- as.factor(ifelse(lda.res[,2] > 0.6, 1, 0))

# the best result is when t = 0.6 if we want to minimize False Positives
conf.mat <- table(test$HeartDisease, lda.pred.best)
conf.mat

# error rate
mean(lda.pred.best!=test$HeartDisease)

# accuracy, precision, recall, f1 score
metrics <- calculate.metrics(conf.mat)

# ROC
lda.auc <- model.plot.roc(lda.res[,2], test$HeartDisease)


# TODO: wrong ROC?
roc.out <- roc(controls=test$HeartDisease, cases=lda.pred$posterior[,2],
               direction=">")
plot(roc.out, print.auc=TRUE, legacy.axes=TRUE,
     xlab="False positive rate", ylab="True positive rate")
auc(roc.out)

ldahist(lda.pred$x[,1], g=lda.pred$class, col=2)

# TODO: how do I choose the best threshold?
# TODO: how do I upgrade the model?



### QDA
qda.fit <- qda(HeartDisease~., data=train)

qda.pred <- predict(qda.fit, test)
qda.pred.best <- as.factor(ifelse(lda.res[,2] > 0.5, 1, 0))

conf.mat <- table(qda.pred.best, test$HeartDisease)

metrics <- calculate.metrics(conf.mat)

qda.auc <- model.plot.roc(qda.pred$posterior[,2], test$HeartDisease)

roc.out <- roc(controls=test$HeartDisease, cases=qda.pred$posterior[,2],
               direction=">")
plot(roc.out, print.auc=TRUE, legacy.axes=TRUE,
     xlab="False positive rate", ylab="True positive rate")
auc(roc.out)
