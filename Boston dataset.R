library(MASS) #loads the package MASS into memory
summary(Boston)
fit.zn <- lm(crim ~ zn) #linear regression analysis between dependent variable crim and model zm
summary(fit.zn) #complete statistical summary of the model
layout(matrix(1:4,2,2))
plot(fit.zn)
Boston$chas <- factor(Boston$chas, labels = c("N","Y"))
summary(Boston$chas)
layout(matrix(1:4,2,2)) #fits all the model evaluations in a single window
plot(Boston$chas)
lm.indus = lm(crim~indus)
summary(lm.indus) # yes
layout(matrix(1:4,2,2))
plot(lm.indus) #visually examine the residuals
chas <- as.factor(chas)
fit.chas <- lm(crim ~ chas)
summary(fit.chas) # no
layout(matrix(1:4,2,2))
plot(fit.chas)
lm.nox = lm(crim~nox)
summary(lm.nox) # yes
layout(matrix(1:4,2,2))
plot(lm.nox)
lm.rm = lm(crim~rm)
summary(lm.rm) # yes
layout(matrix(1:4,2,2))
plot(lm.rm)
lm.age = lm(crim~age)
summary(lm.age) # yes
layout(matrix(1:4,2,2))
plot(lm.age)
lm.dis = lm(crim~dis)
summary(lm.dis) # yes
layout(matrix(1:4,2,2))
plot(lm.dis)
lm.rad = lm(crim~rad)
summary(lm.rad) # yes
layout(matrix(1:4,2,2))
plot(lm.rad)
lm.tax = lm(crim~tax)
summary(lm.tax) # yes
layout(matrix(1:4,2,2))
plot(lm.tax)
lm.ptratio = lm(crim~ptratio)
summary(lm.ptratio) # yes
layout(matrix(1:4,2,2))
plot(lm.ptratio)
lm.black = lm(crim~black)
summary(lm.black) # yes
layout(matrix(1:4,2,2))
plot(lm.black)
lm.lstat = lm(crim~lstat)
summary(lm.lstat) # yes
layout(matrix(1:4,2,2))
plot(lm.lstat)
lm.medv = lm(crim~medv)
summary(lm.medv) # yes
layout(matrix(1:4,2,2))
plot(lm.medv)




library(MASS)
summary(Boston)
fit.zn<-lm(crim~zn,Boston)
pass_predictors<-function(predictor){
        lm.predictor=lm(Boston$crim~predictor)
        print(summary(lm.predictor))
        layout(matrix(1:4,2,2))
        plot(lm.predictor)
}
names<-colnames(Boston)
for (n in 2:14){
       # print(names[n])
       pass_predictors(Boston[,n])
}

par(mfrow=c(2,3)) #Create 4 X 4 plot grid

names<-colnames(Boston)
for (n in 2:14){
        plot(Boston[,n],Boston$crim,xlab=names[n],ylab="crime")
        abline(lm(Boston[,n]~Boston$crim))
}