library(MASS)
attach(Boston)
crim01 <- rep(0, length(crim))
crim01[crim > median(crim)] <- 1
Boston <- data.frame(Boston, crim01)

train <- 1:(length(crim) / 2)
test <- (length(crim) / 2 + 1):length(crim)
Boston.train <- Boston[train, ] #obtaining training data
Boston.test <- Boston[test, ] #obtaining testing data
crim01.test <- crim01[test]

fit.glm <- glm(crim01 ~ . - crim01 - crim, data = Boston, family = binomial, subset = train)
summary(fit.glm)
probs <- predict(fit.glm, Boston.test, type = "response") # obtains predictions from glm object for Y given X 
pred.glm <- rep(0, length(probs)) #replicate values of vector
pred.glm[probs > 0.5] <- 1
table(pred.glm, crim01.test) #results in Confusion Matrix

mean(pred.glm != crim01.test) #finds the mean value

#glm function is used to find logistic regression
fit.glm <- glm(crim01 ~ . - crim01 - crim - chas - nox, data = Boston, family = binomial, subset = train)
layout(matrix(1:4,2,2))
plot(fit.glm)

probs <- predict(fit.glm, Boston.test, type = "response")
pred.glm <- rep(0, length(probs))
pred.glm[probs > 0.5] <- 1
table(pred.glm, crim01.test)

mean(pred.glm != crim01.test)


