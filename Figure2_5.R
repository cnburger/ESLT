install.packages('ElemStatLearn')
library(ElemStatLearn)
#Using the elements of statistical learning package provided in the book
str(mixture.example)

#Extracting the probabilities
probs<-mixture.example$prob 

#Extracting probability of x1 & x2 values
px1 <- mixture.example$px1 
px2 <- mixture.example$px2

#Extractig x1 & x2 values
x <- mixture.example$x


#crating a matrix of probabilities
probabilites.inmat <- matrix(probs, length(px1), length(px2)) 

#Adding the decision boundary
contour(px1, px2, probabilites.inmat, 
        levels=0.5, labels="", xlab="x1", ylab="x2", main="Bayes boundary") 
points(x, col=ifelse(mixture.example$y==1, "red", "blue"))

