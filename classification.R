getclass<-function(intercept,slope,x,y) {
pred_y=slope*x+intercept
ifelse(y>pred_y,-1,1)
}

error=0

#get subset of the list
sublistx <- function(list,start,end){
        if(start < 1 | end > 5000 | start > 5000 | end < 1)
                NULL
        else
                list[start:end,]
}

sublisty <- function(list,start,end){
        if(start < 1 | end > 5000 | start > 5000 | end < 1)
                NULL
        else
                list[start:end]
}

distance_from_plane = function(z,w,b) { 
sum(z*w) + b 
}

classify = function(x,w,b) {
distances = apply(x, 1, distance_from_plane, w, b)
return(ifelse(distances < 0, -1, +1))
}

euclidean_norm = function(x) {sqrt(sum(x * x))}


perceptron = function(x, class, learning_rate=1) 
{
w=c(runif(1,-10,10),runif(1,-10,10))  #start with a randomized w
bias = runif(1,1,5)      #start with a randomized bias
max_euclidean_dist = max(apply(x, 1, euclidean_norm))
iteration=0
error = TRUE       #to enter the while loop
while (error && iteration < 1000) 
{
error=FALSE       #start errors with False
predicted_y <- classify(x,w,bias)
for (i in 1:length(x[,1])) 
 {
 if (class[i] != predicted_y[i]) 
  {
  w = w + learning_rate * class[i]*x[i,]
  bias = bias + learning_rate * class[i]*max_euclidean_dist^2
  error=TRUE
  }
 } 
iteration=iteration+1
}
euclid=euclidean_norm(w)
return(list(w=w/euclid,bias=bias/euclid,iter=iteration))
}


x1=runif(2500,-10,-1)
x2=runif(2500,-10,-1)
x3=runif(2500,1,10)
x4=runif(2500,1,10)
x1=append(x1,x3)
x2=append(x2,x4)
x=cbind(x1,x2)
class=rep(-1,5000)

for(i in 1:5000){ 
if(x2[i]>15*x1[i]+1) # x2 > 1+15*x1 is the equation
{
class[i]=1} 
} 
index=sample(1:5000,4500)#split the data into 80% training = 4000 and 20% testing = 1000

train_x=x[index,]
train_class=class[index]
test_x=x[-index,]
test_class=class[-index]

plot(train_x,col=ifelse(train_class>0,"green","blue"),xlim=c(-10,10),ylim=c(-10,10),cex=0.5)

perceptron_line=perceptron(train_x,train_class,10)
intercept <- - perceptron_line$bias / perceptron_line$w[[2]]
slope <- - perceptron_line$w[[1]] /perceptron_line$w[[2]]

if(perceptron_line$iter==1000) #breaking condition for the loop
cat("The loop breaks\n")
cat("Intercept is ",intercept,"and slope is ",slope,"\n")
abline(intercept,slope,col="red",lwd=5,lty="dotted")

               		for (i in 1:NROW(test_x)) 
 			{
 			if (test_class[i] != getclass(intercept,slope,test_x[[i,1]],test_x[[i,2]])) 
  			{
			error=error+1;
  			}
}
cat("The error obtained using all training data 90%is",error)




#Performing cross validation 
err<-matrix(ncol=1) #matrix to hold the errors.

for(fold in 1:5){
        #generate train and test data set
        testx<-x[((fold-1)*1000+1):((fold)*1000),]
      #testx<-matrix(testx,ncol=2,byrow=TRUE)  
	testy<-class[((fold-1)*1000+1):((fold)*1000)]
        trainx<-c(sublistx(x,1,(fold-1)*1000),sublistx(x,fold*1000+1,5000))
      trainx<-matrix(trainx,ncol=2,byrow=TRUE)  
	trainy<-c(sublisty(class,1,(fold-1)*1000),sublisty(class,fold*1000+1,5000))
	error=0
                #Use training set to obtain model
                perceptron_line=perceptron(trainx,trainy,10)
                intercept <- - perceptron_line$bias / perceptron_line$w[[2]]
                slope <- - perceptron_line$w[[1]] /perceptron_line$w[[2]]
			for (i in 1:NROW(testx)) 
 			{
 				if (testy[i] != getclass(intercept,slope,testx[[i,1]],testx[[i,2]])) 
  				{
					error=error+1;

  				}
			}
                #calculate the mse for this degree
                err[fold]<-error
	if(fold==1){
		min_error=err[fold]+1
	}
	if(err[fold]<min_error){
		min_error=err[fold]
		best_slope=slope
		best_intercept=intercept
	}
		
}
cat("The errors obtained from 5 fold cross validation",err)
cat("The minimum error from the perceptron line among the folds",min_error)
cat("The intercept obtained using minimum error perceptron line",best_intercept)
cat("The slope obtained using minimum error perceptron line",best_slope)
plot(x,col=ifelse(class>0,"green","blue"),xlim=c(-10,10),ylim=c(-10,10),cex=0.5)
abline(best_intercept,best_slope,col="blue",lwd=5,lty="dotted")

               	for (i in 1:NROW(test_x)) 
 			{
 			if (test_class[i] != getclass(best_intercept,best_slope,test_x[[i,1]],test_x[[i,2]])) 
  			{
			error=error+1;
  			}
}
cat("The test error obtained on the perceptron model with least error",error)


