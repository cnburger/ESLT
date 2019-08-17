

GaussianMixture <- function(N = 10,mu1 = c(1,0),mu2 = c(0,1),sigma1 = matrix(c(1,0,0,1), ncol = 2),
                            sigma2 = matrix(c(1,0,0,1), ncol = 2)/5, amt_rows = 100 )
{
  library(MASS) #to use the functions import this library
  
  #PARAMATERS:
  #N (int): The number of elements to sample from 
  #mu1 (float): Mean of Gaussian 1
  #mu2 (float): Mean of Gaussian 2
  #sigma1: Covariance Matrix, in this example it is shared, identity matrix(2x2)
  #sigma2: Covariance new data generations values that will be sampled fro the original gaussians above
  #amt_rows: The amount of rows of the generated dataseet
  #Generate N means
  means_1_vec <- mvrnorm(N, mu =mu1, Sigma = sigma)
  means_2_vec <- mvrnorm(N, mu =mu2, Sigma = sigma)
  

  
  #For each class generate 100 observations
  for (i in 1:amt_rows) {
    #Sample at random with probability 0.1, each observation is equal as likely to be chosen
    sample_val_1 <- sample(1:10,1) 
    sample_val_2 <- sample(1:10,1)
    
    #Extracting x1 x2 data values from the gaussians at random each iteration
    mu_temp1 <-means_1_vec[sample_val_1,]
    mu_temp2 <-means_2_vec[sample_val_2,]
    
    if(i == 1) #for the first iteration to set values to fill the list
    { #Generating a new mixture
      new_obs1 <- mvrnorm(1,mu = mu_temp1,Sigma = sigma2)
      new_obs2 <- mvrnorm(1,mu = mu_temp2,Sigma = sigma2)
    }else{
      #Creating a long list of the different distriutions
      new_obs1 <- rbind(new_obs1, mvrnorm(1,mu = mu_temp1,Sigma = sigma2))
      new_obs2 <- rbind(new_obs2, mvrnorm(1,mu = mu_temp2,Sigma = sigma2))
    }
  }
  
  #Ressetting/Declaring data values
  data1 = NULL
  data2 = NULL
  dataset = NULL
  
  #Merging data in one data matrix & adding data labels (Y-values)
  data1 = cbind(new_obs1, 0)
  data2 = cbind(new_obs2, 1)
  #One big dataset
  dataset <- rbind(as.numeric(data1),as.numeric(data2)) #numeric data
  dataset <- matrix(c(dataset), ncol = 3)
  colnames(dataset) <- c('X1','X2','Y') #new headers
  
  return(dataset)
}
GaussianMixture()
