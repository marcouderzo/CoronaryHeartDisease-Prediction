# https://www.kaggle.com/datasets/fedesoriano/heart-failure-prediction
install.packages("pROC")
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("corrplot")
install.packages("tidymodels")
install.packages("naivebayes")

library(MASS)
library(pROC)
library(class)
library(ggplot2)
library(gridExtra)
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

grid.arrange(age.plot.1, age.plot.2, restingBP.plot, chol.plot, maxHR.plot,
             oldpeak.plot, nrow = 2)

# FastingBS
fastingBS.plot <- ggplot(data, aes(x=FastingBS, group=HeartDisease,
                                   fill=factor(HeartDisease))) +
  geom_bar(alpha=0.5, position="dodge") +
  guides(fill = guide_legend(title="Heart disease"))

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
qqnorm(Cholesterol, pch = 1, frame = FALSE)
qqline(Cholesterol, col="steelblue")


# dealing with missing values
RestingBP[RestingBP == 0] <- median(RestingBP)


# ANOVA
# TODO: study meaning of this
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

# Train-Test split
set.seed(123)
split <- initial_split(data, prop=0.75)

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


### LDA
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
# TODO: strange AUC curve


### QDA
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
# TODO: use also categorical variables
knn.pred <- knn(X.train[, -c(2, 3, 7, 9, 11)], X.test[, -c(2, 3, 7, 9, 11)],
                y.train, k=5)
table(knn.pred, y.test)


# TODO
# - for each model write down accuracy, precision, recall
