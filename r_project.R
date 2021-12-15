library(pROC)
library(MASS)
library(class)
library(leaps)
library(glmnet)
library(klaR)
library(readr)
library(openintro)

creditcard <- read.csv("~/Desktop/Università/Statistical Learning/Statistical Learning B/Project/creditcard.csv")

View(creditcard)

attach(creditcard)

dim(creditcard)

reg = lm(Class~., data = creditcard)

summary(reg)

creditcard <- subset (creditcard, select = -Time)

boxplot(creditcard)

for (column in colnames(creditcard)) {
       column <- (column - mean(column))/sd(column)
}

scale_data <- scale(creditcard)
scale_data <- as.data.frame(scale_data)
scale_data$Class <- Class
scale_data$Time <- Time

attach(scale_data)

set.seed(1)
index <- sample(1:284807, 284807)
train <- scale_data[index[1:floor(284807*0.66)],]
test <- scale_data[index[(floor(284807*0.66) + 1): 284807],]

################################################################################
# LOGISTIC REGRESSION

# Binomial
genreg <- glm(Class~., data = train, family = binomial)
summary(genreg)

predicted.prob2 <- predict(genreg, test, type="response")

roc.out <- roc(test$Class, predicted.prob2)
plot(roc.out, print.auc=TRUE, legacy.axes=TRUE, xlab="False positive rate", ylab="True positive rate")

th <- coords(roc.out, "best", ret = "threshold")


predicted.values2 <- predicted.prob2 > th$threshold
table(predicted.values2, test$Class)

# Gaussian (13 - 6003)
genreg <- glm(Class~., data = train, family = gaussian)
summary(genreg)

predicted.prob2 <- predict(genreg, test, type="response")

roc.out <- roc(test$Class, predicted.prob2)
plot(roc.out, print.auc=TRUE, legacy.axes=TRUE, xlab="False positive rate", ylab="True positive rate")

th <- coords(roc.out, "best", ret = "threshold")


predicted.values2 <- predicted.prob2 > th$threshold
table(predicted.values2, test$Class)

# Poisson (32 - 3255)
genreg <- glm(Class~., data = train, family = poisson)
summary(genreg)

predicted.prob2 <- predict(genreg, test, type="response")

roc.out <- roc(test$Class, predicted.prob2)
plot(roc.out, print.auc=TRUE, legacy.axes=TRUE, xlab="False positive rate", ylab="True positive rate")

th <- coords(roc.out, "best", ret = "threshold")


predicted.values2 <- predicted.prob2 > th$threshold
table(predicted.values2, test$Class)
################################################################################
# DROP VARIABLES DEPENDING ON p-VALUE
#-V25   19 - 2945
genreg <- glm(Class~.-V25, data = train, family = binomial)

predicted.prob2 <- predict(genreg, test, type="response")

roc.out <- roc(test$Class, predicted.prob2)
th <- coords(roc.out, "best", ret = "threshold")
predicted.values2 <- predicted.prob2 > th$threshold
table(predicted.values2, test$Class)

summary(genreg)

# -V26 19 - 2980
genreg <- glm(Class~.-V25-V26, data = train, family = binomial)

predicted.prob2 <- predict(genreg, test, type="response")

roc.out <- roc(test$Class, predicted.prob2)
th <- coords(roc.out, "best", ret = "threshold")
predicted.values2 <- predicted.prob2 > th$threshold
table(predicted.values2, test$Class)

summary(genreg)

#-V16 19-2946
genreg <- glm(Class~.-V25-V26-V16, data = train, family = binomial)

predicted.prob2 <- predict(genreg, test, type="response")

roc.out <- roc(test$Class, predicted.prob2)
th <- coords(roc.out, "best", ret = "threshold")
predicted.values2 <- predicted.prob2 > th$threshold
table(predicted.values2, test$Class)

summary(genreg)

#-V24 19-3013
genreg <- glm(Class~.-V25-V26-V16-V24, data = train, family = binomial)

predicted.prob2 <- predict(genreg, test, type="response")

roc.out <- roc(test$Class, predicted.prob2)
th <- coords(roc.out, "best", ret = "threshold")
predicted.values2 <- predicted.prob2 > th$threshold
table(predicted.values2, test$Class)

summary(genreg)

#-V7 19-2905
genreg <- glm(Class~.-V25-V26-V16-V24-V7, data = train, family = binomial)

predicted.prob2 <- predict(genreg, test, type="response")

roc.out <- roc(test$Class, predicted.prob2)
th <- coords(roc.out, "best", ret = "threshold")
predicted.values2 <- predicted.prob2 > th$threshold
table(predicted.values2, test$Class)

summary(genreg)

#-V15  19 - 2794
genreg <- glm(Class~.-V25-V26-V16-V24-V7-V15, data = train, family = binomial)

predicted.prob2 <- predict(genreg, test, type="response")

roc.out <- roc(test$Class, predicted.prob2)
th <- coords(roc.out, "best", ret = "threshold")
predicted.values2 <- predicted.prob2 > th$threshold
table(predicted.values2, test$Class)

summary(genreg)

#-V3 18 - 3602
genreg <- glm(Class~.-V25-V26-V16-V24-V7-V15-V3, data = train, family = binomial)

predicted.prob2 <- predict(genreg, test, type="response")

roc.out <- roc(test$Class, predicted.prob2)
th <- coords(roc.out, "best", ret = "threshold")
predicted.values2 <- predicted.prob2 > th$threshold
table(predicted.values2, test$Class)

summary(genreg)

#-V11 18 - 3424
genreg <- glm(Class~.-V25-V26-V16-V24-V7-V15-V3-V11, data = train, family = binomial)

predicted.prob2 <- predict(genreg, test, type="response")

roc.out <- roc(test$Class, predicted.prob2)
th <- coords(roc.out, "best", ret = "threshold")
predicted.values2 <- predicted.prob2 > th$threshold
table(predicted.values2, test$Class)

summary(genreg)

#-V6 23 - 886
genreg <- glm(Class~.-V25-V26-V16-V24-V7-V15-V3-V11-V6, data = train, family = binomial)

predicted.prob2 <- predict(genreg, test, type="response")

roc.out <- roc(test$Class, predicted.prob2)
th <- coords(roc.out, "best", ret = "threshold")
predicted.values2 <- predicted.prob2 > th$threshold
table(predicted.values2, test$Class)

summary(genreg)

#-V2 19 - 3195
genreg <- glm(Class~.-V25-V26-V16-V24-V7-V15-V3-V11-V6-V2, data = train, family = binomial)

predicted.prob2 <- predict(genreg, test, type="response")

roc.out <- roc(test$Class, predicted.prob2)
th <- coords(roc.out, "best", ret = "threshold")
predicted.values2 <- predicted.prob2 > th$threshold
table(predicted.values2, test$Class)

summary(genreg)
################################################################################
# STEP FORWARD WITH 10 VARIABLES
biggest <- formula(glm(Class~., data = train, family = binomial))

step(glm(Class~+1, data = train, family = binomial), steps = 10, scope = biggest, direction = 'forward')

genreg_step <- glm(formula = Class ~ V14 + V10 + V4 + V3 + V16 + V8 + V13 + V21 + V22 + V23, family = binomial, data = train)

predicted.prob2 <- predict(genreg_step, test, type="response")

roc.out <- roc(test$Class, predicted.prob2)
plot(roc.out, print.auc=TRUE, legacy.axes=TRUE, xlab="False positive rate", ylab="True positive rate")

th <- coords(roc.out, "best", ret = "threshold")

predicted.values2 <- predicted.prob2 > th$threshold
table(predicted.values2, test$Class)

train_balanced <- rbind(train[train$Class == 0,][1:150,], train[train$Class == 1,])
################################################################################
# STEP BACKWARD DROPPING 10 VARIABLES
step.mod <- step(genreg, steps=10, trace=1, direction="backward")

###############################################################################
# K - MEANS

valid_t <- train[train$Class == 0,]
km.out <- kmeans(valid_t, centers = 200, iter.max = 200, algorithm="MacQueen")
summary(km.out)
center <- km.out$center
center <- as.data.frame(center)

train_fraud <- train[train$Class == 1,]

new_data <- rbind(center,train[train$Class == 0,][100000:187972,], train_fraud)
### LOG REG and K-MEANS
genreg <- glm(Class~., data = new_data, family = binomial)
summary(genreg)

predicted.prob <- predict(genreg, test, type="response")
predicted.values <- predicted.prob > 0.2 
table(predicted.values, test$Class)

roc.out <- roc(test$Class, predicted.prob)
plot(roc.out, print.auc=TRUE, legacy.axes=TRUE, xlab="False positive rate", ylab="True positive rate")

th <- coords(roc.out, "best", ret = "threshold")

predicted.prob2 <- predict(genreg, test, type="response")
predicted.values2 <- predicted.prob2 > th$threshold
table(predicted.values2, test$Class)

### LDA and K-MEANS
lda.fit <- lda(Class~., data = new_data)
lda.pred <- predict(lda.fit, test)
table(lda.pred$class, test$Class)

roc.lda <- roc(test$Class, lda.pred$x)
plot(roc.lda, print.auc=TRUE, legacy.axes=TRUE, xlab="False positive rate", ylab="True positive rate")
###############################################################################
# VARIABLE SELECTION

step.mod <- step(genreg, steps=20, trace=1, direction="backward")

step.mod <- step(simplest, steps=10, trace=0, scope=list(lower=formula(simplest), upper=formula(genreg)), direction="forward")


regfit.full <- regsubsets(Class~., nvmax = 29, data = train)
reg.summary <- summary(regfit.full)

## 15 VARIABLES
coef(regfit.full, 15)
genreg15 <- glm(Class~V1+V2+V3+V4+V5+V6+V7+V9+V10+V11+V12+V14+V16+V17+V18, data = train, family = binomial)

predicted.prob15 <- predict(genreg15 , test, type="response")
predicted.values15  <- predicted.prob15  > 0.2 
table(predicted.values15 , test$Class)

roc.out15  <- roc(test$Class, predicted.prob15 )
plot(roc.out15 , print.auc=TRUE, legacy.axes=TRUE, xlab="False positive rate", ylab="True positive rate")

th15  <- coords(roc.out15 , "best", ret = "threshold")

predicted.prob15  <- predict(genreg15 , test, type="response")
predicted.values15  <- predicted.prob15  > th15 $threshold
table(predicted.values15 , test$Class)

## 20 VARIABLES
coef(regfit.full, 20)
genreg20 <- glm(Class~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V14+V16+V17+V18+V19+V21+V27+Amount, data = train, family = binomial)

predicted.prob20 <- predict(genreg20, test, type="response")

roc.out20 <- roc(test$Class, predicted.prob2)
plot(roc.out20, print.auc=TRUE, legacy.axes=TRUE, xlab="False positive rate", ylab="True positive rate")

th20 <- coords(roc.out20, "best", ret = "threshold")

predicted.values20 <- predicted.prob20 > th20$threshold
table(predicted.values20, test$Class)

#predicted.values20     0     1
#             FALSE 93151    17
#             TRUE   3520   147

###############################################################################
# REGULARIZATION
grid <- 1:100/100

ridge.mod <- glmnet(subset(train, select = -Class), train$Class, alpha=0, lambda = grid)
predicted.prob.ridge <- predict(ridge.mod, as.matrix(subset(test, select = -Class)), type="response")


roc.ridge <- roc(test$Class, predicted.prob.ridge)
plot(roc.ridge, print.auc=TRUE, legacy.axes=TRUE, xlab="False positive rate", ylab="True positive rate")

th.ridge <- coords(roc.ridge, "best", ret = "threshold")

predicted.values.ridge <- predicted.prob.ridge > th.ridge$threshold
table(predicted.values.ridge, test$Class)

#REGULARIZATION AND 20 VARIABLES

ridge.mod <- glmnet(subset(train, select = c(V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V14,V16,V17,V18,V19,V21,V27,Amount)), train$Class, alpha=0, lambda = 1)
predicted.prob.ridge <- predict(ridge.mod, as.matrix(subset(test, select = c(V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V14,V16,V17,V18,V19,V21,V27,Amount))), type="response")


roc.ridge <- roc(test$Class, predicted.prob.ridge)
plot(roc.ridge, print.auc=TRUE, legacy.axes=TRUE, xlab="False positive rate", ylab="True positive rate")

th.ridge <- coords(roc.ridge, "best", ret = "threshold")

predicted.values.ridge <- predicted.prob.ridge > th.ridge$threshold
table(predicted.values.ridge, test$Class)

#LASSO REGRESSION
ridge.mod <- glmnet(subset(train, select = -Class), train$Class, alpha=1, lambda = 1)
predicted.prob.ridge <- predict(ridge.mod, as.matrix(subset(test, select = -Class)), type="response")


roc.ridge <- roc(test$Class, predicted.prob.ridge)
plot(roc.ridge, print.auc=TRUE, legacy.axes=TRUE, xlab="False positive rate", ylab="True positive rate")

th.ridge <- coords(roc.ridge, "best", ret = "threshold")

predicted.values.ridge <- predicted.prob.ridge > th.ridge$threshold
table(predicted.values.ridge, test$Class)

##########################
# Linear Discriminant Analysis (LDA)
###########################
lda.fit <- lda(Class~., data = train)

summary(lda.fit)
lda.pred <- predict(lda.fit, test)
table(lda.pred$class, test$Class)

roc.lda <- roc(test$Class, lda.pred$x)
plot(roc.lda, print.auc=TRUE, legacy.axes=TRUE, xlab="False positive rate", ylab="True positive rate")

##########################
# Quadratic Discriminant Analysis (QDA)
###########################
qda.fit <- qda(Class~., data = train)
qda.pred <- predict(qda.fit, test)
table(qda.pred$class, test$Class)

##########################
# Regularized Discriminant Analysis (RDA)
###########################
rda.fit <- rda(Class~., data = train, alpha = 0.5)
rda.pred <- predict(rda.fit, test)
table(rda.pred$class, test$Class)

roc.lda <- roc(test$Class, lda.pred$x)
plot(roc.lda, print.auc=TRUE, legacy.axes=TRUE, xlab="False positive rate", ylab="True positive rate")

##########################
# K-Nearest Neighbors (KNN)
###########################

library(class)
train.X <-  subset (train, select = -Class)
test.X  <- subset (test, select = -Class)
y.train <- train$Class
set.seed(1)
knn.pred <- knn(train.X, test.X, y.train, k=1)
table(knn.pred,Direction.2005)
(83+43)/252


library(leaps)
regfit.full <- regsubsets(Class~., nvmax = 29, data = train)
#summary(regfit.full)

reg.summary <- summary(regfit.full)

# elements of reg.summary
names(reg.summary)


par(mfrow=c(2,2))

# panel 1
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")

# panel 2
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
which.max(reg.summary$adjr2)
points(20,reg.summary$adjr2[20], col="red",cex=2,pch=20)

# panel 3
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
which.min(reg.summary$cp)
points(6,reg.summary$cp[6],col="red",cex=2,pch=20)

# panel 4
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
which.min(reg.summary$bic)
points(4,reg.summary$bic[4],col="red",cex=2,pch=20)

par(mfrow=c(1,1))


coef(regfit.full, 14)

predict.regsubsets =function (object ,newdata ,id ,...){
  form=as.formula(object$call [[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object ,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}

predicted.prob <- predict.regsubsets(regfit.full, test, 14)

roc.red <- roc(test$Class, predicted.prob)

predicted.values <- (predicted.prob > 0.2)#coords(roc.red, "best", ret = "threshold")$threshold)

#predicted.values <- (predicted.prob > coords(roc.red, "best", ret = "sensitivity")$threshold)

table(predicted.values, test$Class)

plot(roc.red, print.auc=TRUE, legacy.axes=TRUE, xlab="False positive rate", ylab="True positive rate")

aucs <- c()
for (num.var in c(1:29)) {
  predicted.prob <- predict.regsubsets(regfit.full, test, num.var)
  roc.red <- roc(test$Class, predicted.prob)
  aucs <- c(aucs, roc.red$auc)
  
}

plot(1:29, aucs)


#
# second group of plots 
#


plot(regfit.full,scale="r2")
plot(regfit.full,scale="adjr2")
plot(regfit.full,scale="Cp")
plot(regfit.full,scale="bic")

# Cp best
coef(regfit.full,6)

# BIC best
coef(regfit.full,4)

# check the coding of Direction
contrasts(Direction)

# confusion matrix

glm.pred <- rep("Down",1250)
glm.pred[glm.probs>.5] <- "Up"
table(glm.pred,Direction)

### ROC curve in R
library(pROC)
# levels = controls (0’s) as first element and cases (1’s) as second > roc.out <- roc(default, logistic.prob, levels=c("No", "Yes"))
roc.out <- roc(Direction, glm.probs, levels=c("Down", "Up"))
plot(roc.out, print.auc=TRUE, legacy.axes=TRUE, xlab="False positive rate", ylab="True positive rate")

# train/test set selection

train <- (Year<2005)
Smarket.2005 <- Smarket[!train,]
dim(Smarket.2005)
Direction.2005 <- Direction[!train]

red.mod1 <- update(full.mod, ~.-horsepower)
summary(red.mod1)

abline(mod.out, lwd=2)

par(mfrow=c(1,2))
plot(TV, sales)
abline(mod.out, col="blue", lwd=2)
plot(fitted(mod.out), residuals(mod.out), col="gray40", xlab="fitted values", ylab="residuals")
lines(loess.smooth(fitted(mod.out), residuals(mod.out)), col="blue", lwd=2)
abline(h=0, lty=2)
par(mfrow=c(1,1))

par(mfrow=c(2,2))
plot(mod.out)
par(mfrow=c(1,1))

mod.out.poly <- lm(mpg~poly(horsepower, 5, raw=TRUE))

mod.out3 <- lm(log(mpg)  ~ horsepower)


# check for heteroscedasticity 

mod.out <- lm(sales~TV)
summary(mod.out)

e <- residuals(mod.out)
stud.e <- rstandard(mod.out)

par(mfrow=c(1,2))
plot(TV, e, pch=16)
plot(TV, stud.e, pch=16)
par(mfrow=c(1,1))

is.factor(diet)
contrasts(diet)

is.factor(origin)

origin.f <- as.factor(origin)

is.factor(origin.f)

mod.out <- lm(mpg ~ origin.f)
summary(mod.out)
contrasts(origin.f)
