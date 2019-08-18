func_fig_2.2 <- function(k.in)
{
  #k.in (int): Integer value of k in KNN
  library(class) #to use KNN
  dataset <- GaussianMixture() #function already written

  #Extracting the X & Y Values
  x <- (dataset[,1:2])
  y <- dataset[,3]
  
  
  nnn <- 0.08 #Increment variables for the dots
  xx1_range <- round(range(x[, 1]), 1) #range of 1 values
  xx2_range <- round(range(x[, 2]), 1) #range of x2 values
  
  px1 <- seq(xx1_range[1], xx1_range[2], by = nnn) # vector with x extent
  px2 <- seq(xx2_range[1], xx2_range[2], by = nnn) # vector with y extent
  xnew <- as.matrix(expand.grid(px1, px2)) #creating a matrix of the sequences for the plot 
  
  
  k <- k.in #k input variable
  
  knn.model <- knn(x, xnew, y, k=k, prob=TRUE) #KNN model function
  prob <- attr(knn.model, "prob") #getting object attribute from knn model
  prob <- ifelse(knn.model=="1", prob, 1-prob) #if == 1, retrun probability else return 1-prob (opposite)
  prob15 <- matrix(prob, length(px1), length(px2)) #Creating a matrix of probabilities

  #Adding the decision boundary
  contour(px1, px2, prob15, levels=0.5, labels="", xlab="", ylab="", main=
            "15-nearest neighbour", axes=FALSE)
  points(x, col=ifelse(y==1, "red", "blue"))  #addig points & colour by corresponding y value class
  grid <- expand.grid(x=px1, y=px2) #creating a grid of the sequences
  points(grid, pch=".", cex=1.2, col=ifelse(prob15>0.5, "red", "blue")) #plotting the classification array
  box()  #adding a border
}
  
func_fig_2.2(k =15)


