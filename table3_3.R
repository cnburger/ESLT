#NOTE:--------------
#The answers will not correspond 100% with that in the textbook
#we are making use of cross-validation which splits the data up
#randomly. Our answers are close but not exact due to the seed
#that is different. Each time that the functions is ran the
#answers will change but not drastically, within a small margin
#the answers are should still becorrect

library('ElemStatLearn')
dataset <- prostate #from ElemStatLearn package

#TEST DATA
test_set <- prostate[prostate[,10] != TRUE,1:9]
head(test_set)
test_response <- test_set[,9]
test_set <- as.data.frame(scale(test_set[,-9]))
test_scaled <- cbind(test_response,test_set)

#TRAIN DATA
train_set <- prostate[prostate[,10] == TRUE,1:9]
#Extracting the response
response <- train_set$lpsa
#removing the response & scale the data
train_set <- as.data.frame(scale(train_set[,-9]))

#Merging the scaled data and the response:
train_scaled <- cbind(response,train_set)
#To see for unit variance between predictors
var(train_scaled)


######
######
######
######-----------------------LS--------------------------------
######
######
######


linear.model <- lm(response ~., data =train_scaled) #creating a linear model
summary(linear.model) #see coefficients & errors


#Test Error
predictedLS <- predict(linear.model,test_scaled)
mean((predictedLS -test_scaled$test_response)^2)


#Std error: variance/sqrt(n - p)  #final model has 2 parameters
var(predictedLS)/sqrt(length(predictedLS)-length(coef(linear.model)))

###### 01
######
######
######-----------------------BEST SUBSET--------------------------------
######
######
######

#Libraries
#install.packages('leaps') #for subset selection
library(leaps)

#Fitting the best subset
regfit.full = regsubsets(response~., data = train_scaled)
#Extracting the coefficients
summary(regfit.full)
reg.sum <- summary(regfit.full)

#Set 2 is the same as in the book
coef(regfit.full,2)

predict(regfit.full)
?predict

#Names of the coefficients
names(reg.sum)

#Creating plots of criterions
criterion.plots <- function(){
dev.new() #Create a new plotting window
par(mfrow =c(2,2)) #2x2 plotting window

#RSS--------------------------------------------------------------------
plot(reg.sum$rss, xlab = "Nymber of Variables", ylab = "RSS", type = "l")


#####AJUSTED R-Squared--------------------------------------------------
plot(reg.sum$adjr2, xlab ="Number of Variables", ylab = "Adjusted RSq", type = "l")
which.max(reg.sum$adjr2)
#Answer: 7
points(7, reg.sum$adjr2[7], col = "red", cex = 2, pch =20)#adding point to plot


#####Mallow Cp----------------------------------------------------------
plot(reg.sum$cp, xlab = "Number of Variables", ylab = "Cp", type = 'l')
which.min(reg.sum$cp)
#Answer = 7
points(7,reg.sum$cp[7], col = "red", cex = 2, pch = 20)#adding point to plot



####BIC ---------------------------------------------------------------
plot(reg.sum$bic, xlab = "Number of variables", ylab = "BIC", type = 'l')
which.min(reg.sum$bic)
points(2,reg.sum$bic[2], col = "red", cex = 2, pch = 20) #adding point to plot

dev.new() #new plotting window
par(mfrow =c(2,2)) 
#Plotting the individual criterions
plot(regfit.full, scale = "r2")
plot(regfit.full, scale = "adjr2")
plot(regfit.full, scale = "Cp")
plot(regfit.full, scale = "bic")

}
criterion.plots()

#Final coefficients based on BIC
coef(regfit.full,2)

#BEST FIT = 2 

#TEST ERROR
best.fit = regsubsets(response~., data = train_scaled[,c('lcavol','lweight')])
best.sub.beta0 <- as.numeric(coef(regfit.full,2)[1])
best.sub.beta1 <- as.numeric(coef(regfit.full,2)[2])
best.sub.beta2 <- as.numeric(coef(regfit.full,2)[3])

best.sub.predictedY <- best.sub.beta0 + best.sub.beta1*test_scaled$lcavol + best.sub.beta2*test_scaled$lweight

best.sub.TESTERROR <- mean((best.sub.predictedY-test_scaled$test_response)^2)
best.sub.TESTERROR
#Std erro: variance/sqrt(n - p)  #final model has 2 parameters + Intercept
var(best.sub.predictedY)/sqrt(length(best.sub.predictedY)-3)

######
######
######
######-------------------------------RIDGE REGRESSION--------------------------
######
######
######


#install.packages("glmnet")
library(glmnet)



#####-----------------------Data preperation for RIDGE & LASSO----------

#Extracting training list from dataset
train <- prostate[,10]
#Extracting the test list
test = !train

#Scaling the data (without the training data)
data_scaled <- as.data.frame(scale(prostate[,-10])) #important to scale data
#Data matrix
x = model.matrix(lpsa ~.,data_scaled )[,-1] #removing the intercept

#Extrating the response
y = prostate$lpsa 
#Extracting the test response
y.test = y[test]



#Grid of values from lambda 10e10 to 10e-2
grid = 10^seq(10,-2,length = 1000) 

#Creating a Ridge model on the training data
ridge.mod = glmnet(x[train,],y[train],alpha = 0, lambda = grid, thresh = 1e-12)

#Highest possible error
mean((mean(y[train])-y.test)^2)

#Check whether there is any benifit to performing ridge regression with other values
dev.off()#Clear plot

#Cross validation of Ridge
cv.Ridge.out = cv.glmnet(x[train,], y[train], alpha = 0)
#plotting cross validation errors MSE
plot(cv.Ridge.out)
#Extracting the best lambda value
bestlam.ridge = cv.Ridge.out$lambda.min
bestlam.ridge
#0.0878804


ridge.pred = predict(ridge.mod, s = bestlam.ridge, newx = x[test,])
#Test Error
mean((ridge.pred -y.test)^2)
#0.4944481

#Extracting the paramater values
ridge.out = glmnet(x,y,alpha = 0)
ridge.parameters <- predict(ridge.out, type = "coefficients", s = bestlam.ridge, newx = x[test,])
ridge.parameters

#Std erro: variance/sqrt(n - p - 1) #the extra -1 is for the lambda in ridge
var(ridge.pred)/sqrt(length(ridge.pred)-length(ridge.parameters)-1)
#0.1168568

#####
#####
#####
#####-----------------------------LASSO -----------------------------------
#####
#####
#####

#Creating the lasso model
x.lasso <- x[,-c(3,6,7,8)]
lasso.mod <- glmnet(x.lasso[train,], y[train], alpha = 1, lambda=grid)
plot(lasso.mod)

#Cross validation:
cv.lasso.out <- cv.glmnet(x.lasso[train,], y[train], alpha = 1)
#plotting the different intervals of MSE
plot(cv.lasso.out)

#Extracting the best lambda
bestlam.lasso <- cv.lasso.out$lambda.min
bestlam.lasso
#0.01010499

lasso.pred <- predict(lasso.mod, s = bestlam.lasso, newx = x.lasso[test,])
#Test ERROR LASSO:
mean((lasso.pred - y.test)^2)
#0.45307

#Extracting the coefficients
out.lasso <- glmnet(x.lasso,y,alpha =1, lambda = grid)
lasso.coef = predict(out.lasso, type = "coefficients", s = bestlam.lasso)
lasso.coef

var(lasso.pred)/sqrt(length(lasso.pred)-length(lasso.coef)-1)
#0.1150187

