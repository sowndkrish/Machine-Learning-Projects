install.packages("truncnorm")
install.packages("optimbase")
library(optimbase)
library(truncnorm)
require(MASS)
x=runif(1000,-500,+500)
epsilon=rtruncnorm(1,-1,1,0,1)
y=2+3*x+4*x^2+5*x^3+epsilon
summary(y)
lm(y ~ x + I(x^2) + I(x^3))
fit1 <- lm( y~poly(x,1))
fit2 <- lm( y~poly(x,2))
fit3 <- lm( y~poly(x,3))
fit4 <- lm( y~poly(x,4))
fit5 <- lm( y~poly(x,5))
anova(fit1,fit2,fit3,fit4,fit5)
summary(fit1)
df <- x
fractionTraining   <- 0.50
fractionValidation <- 0.25
fractionTest       <- 0.25
sampleSizeTraining   <- floor(fractionTraining   * NROW(df))
sampleSizeValidation <- floor(fractionValidation * NROW(df))
sampleSizeTest       <- floor(fractionTest       * NROW(df))
indicesTraining    <- sort(sample(seq_len(NROW(df)), size=sampleSizeTraining))
indicesNotTraining <- setdiff(seq_len(NROW(df)), indicesTraining)
indicesValidation  <- sort(sample(indicesNotTraining, size=sampleSizeValidation))
indicesTest        <- setdiff(indicesNotTraining, indicesValidation)
training_y=2+3*indicesTraining+4*indicesTraining^2+5*indicesTraining^3+epsilon
#degree 1
training_x_value=data.matrix(cbind(indicesTraining,1))
training_x_trans_value=transpose(training_x_value)
x_tand_x=training_x_trans_value %*% training_x_value 
inverse_x_matrix=ginv(x_tand_x)
X_t_and_y=training_x_trans_value %*% training_y
coeffi=inverse_x_matrix %*% X_t_and_y
print(coeffi)
lm(training_y~poly(indicesTraining,1))

#degree 2
x1=indicesTraining
x2=indicesTraining^2
training_x2_value=data.matrix(cbind(x2,x1,1))
training_x2_trans_value=transpose(training_x2_value)
x_tand_x2=training_x2_trans_value %*% training_x2_value 
inverse_x2_matrix=ginv(x_tand_x2)
X_t2_and_y=training_x2_trans_value %*% training_y
coeffi2=inverse_x2_matrix %*% X_t2_and_y
print(coeffi2)
lm(training_y~poly(indicesTraining,2))

#degree 3
x1=indicesTraining
x2=indicesTraining^2
x3=indicesTraining^3
training_x3_value=data.matrix(cbind(x3,x2,x1,1))
training_x3_trans_value=transpose(training_x3_value)
x_tand_x3=training_x3_trans_value %*% training_x3_value 
inverse_x3_matrix=ginv(x_tand_x3)
X_t3_and_y=training_x3_trans_value %*% training_y
coeffi3=inverse_x3_matrix %*% X_t3_and_y
print(coeffi3)
lm(training_y~poly(indicesTraining,3))

#degree 4
x1=indicesTraining
x2=indicesTraining^2
x3=indicesTraining^3
x4=indicesTraining^4
training_x4_value=data.matrix(cbind(x4,x3,x2,x1,1))
training_x4_trans_value=transpose(training_x4_value)
x_tand_x4=training_x4_trans_value %*% training_x4_value 
inverse_x4_matrix=ginv(x_tand_x4)
X_t4_and_y=training_x4_trans_value %*% training_y
coeffi4=inverse_x4_matrix %*% X_t4_and_y
print(coeffi4)
lm(training_y~poly(indicesTraining,4))

#degree 5
x1=indicesTraining
x2=indicesTraining^2
x3=indicesTraining^3
x4=indicesTraining^4
x5=indicesTraining^5
training_x5_value=data.matrix(cbind(x5,x4,x3,x2,x1,1))
training_x5_trans_value=transpose(training_x5_value)
x_tand_x5=training_x5_trans_value %*% training_x5_value 
inverse_x5_matrix=ginv(x_tand_x5)
X_t5_and_y=training_x5_trans_value %*% training_y
coeffi5=inverse_x5_matrix %*% X_t5_and_y
print(coeffi5)
lm(training_y~poly(indicesTraining,5))

y1= 4435246 -973913960*x
(y1=1.271e+09+ 2.856e+10*x)
y2= 6655.835 -2002707.035*x -8045.059*x^2
y3= 4.999994e+00 + 4.008954e+00*x + 1.199174e-02*x^2 + 2.982041e-05*x^3
y4= -6.912679e-06 + 5.010689e+00*x + 1.341302e-02*x^2 + 2.820339e-05*x^3 +5.626321e-08*x^4
y5= 6.238618e-06 + 6.930621e-09*x+ 7.777006e-12*x^2+8.834758e-15*x^3+ 1.019281e-17*x^4+1.199791e-20*x^5

#degree 1
v_x1=indicesValidation
v_y1= 4435246 -973913960*v_x1+epsilon
( v_y1= 1.271e+09+ 2.856e+10*v_x1+epsilon )
v_x_value=data.matrix(cbind(v_x1,1))
v_x_trans_value=transpose(v_x_value)
v_x_tand_x=v_x_trans_value %*% v_x_value 
inverse_v_x_matrix=ginv(v_x_tand_x)
v_X_t_and_y=v_x_trans_value %*% v_y1
v_coeffi=inverse_v_x_matrix %*% v_X_t_and_y
print(v_coeffi)
lm(v_y1~poly(indicesValidation,1))

v_y1=  -973913960 + 4435245*indicesValidation+epsilon
v_y1= -4.972e+11 + -4.447e+12*x
( v_y1= 1.458e+13+ 1.304e+14*indicesValidation+epsilon )

(1/248)^0.5 * sum(training_y-v_y1)

(-2.11417e+18)

-606061951
1.582679e+13

#degree 2
v_x1=indicesValidation
v_x2=indicesValidation^2
v_y2= 6655.835 -2002707.035*indicesValidation -8045.059*indicesValidation^2+epsilon
v_x2_value=data.matrix(cbind(v_x2,v_x1,1))
v_x2_trans_value=transpose(v_x2_value)
v_x2_tand_x=v_x2_trans_value %*% v_x2_value 
inverse_v_x2_matrix=ginv(v_x2_tand_x)
v_X2_t_and_y=v_x2_trans_value %*% v_y2
v_coeffi2=inverse_v_x2_matrix %*% v_X2_t_and_y
print(v_coeffi2)
lm(v_y2~poly(indicesValidation,2))

v_y2=-8045.106 -2002649.637*indicesValidation -7898.104*indicesValidation^2+epsilon

((1/248) * sum(training_y-v_y2)^2)^0.5
159101345751


#degree 3
v_x1=indicesValidation
v_x2=indicesValidation^2
v_x3=indicesValidation^3
v_y3= 4.999994e+00 + 4.008954e+00*indicesValidation + 1.199174e-02*indicesValidation^2 + 2.982041e-05*indicesValidation^3+epsilon
v_x3_value=data.matrix(cbind(v_x3,v_x2,v_x1,1))
v_x3_trans_value=transpose(v_x3_value)
v_x3_tand_x=v_x3_trans_value %*% v_x3_value 
inverse_v_x3_matrix=ginv(v_x3_tand_x)
v_X3_t_and_y=v_x3_trans_value %*% v_y3
v_coeffi3=inverse_v_x3_matrix %*% v_X3_t_and_y
print(v_coeffi3)
lm(v_y3~poly(indicesValidation,3))

v_y2=2.133544e-05 +2.410810e-02*indicesValidation+ 7.267689e-05*indicesValidation^2+1.826527e-07*indicesValidation^3+epsilon

(1/248)^0.5 * sum(training_y-v_y3)
 40364387644


#degree 3
v_x1=indicesValidation
v_x2=indicesValidation^2
v_x3=indicesValidation^3
v_y3= 4.999994e+00 + 4.008954e+00*indicesValidation + 1.199174e-02*indicesValidation^2 + 2.982041e-05*indicesValidation^3+epsilon
v_x3_value=data.matrix(cbind(v_x3,v_x2,v_x1,1))
v_x3_trans_value=transpose(v_x3_value)
v_x3_tand_x=v_x3_trans_value %*% v_x3_value 
inverse_v_x3_matrix=ginv(v_x3_tand_x)
v_X3_t_and_y=v_x3_trans_value %*% v_y3
v_coeffi3=inverse_v_x3_matrix %*% v_X3_t_and_y
print(v_coeffi3)
lm(v_y3~poly(indicesValidation,3))

v_y2=2.133544e-05 +2.410810e-02*indicesValidation+ 7.267689e-05*indicesValidation^2+1.826527e-07*indicesValidation^3+epsilon

(1/248)^0.5 * sum(training_y-v_y3)
 40364387644

print(y)
summary(fit1)
