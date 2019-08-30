softThreas <- function(t, lambda)
  {
    #Getting the signs og the values
    sign_val = sign(t)
    #Calculating the positive part
    x_plus <- (abs(t)-lambda)
    
    #array to store answer in
    ans = rep(0, length(t))
  
    for( i in (1:length(t)))
    {
      #Extracting the positive part
      if(x_plus[i] > 0)
        ans[i] = (sign_val[i]*x_plus[i])
      else
        ans[i] = 0 #aswer is zero
    }
    #return answer
    return(ans)
  }
  
#Create an input sequence
x <- seq(-30,30,0.5)
  
lambda = 10
#Calculating y 
y <- softThreas(x,lambda)
y

#Plotting result
plot(x,y,ty = "l", cex = 0.5, main = "Plotting the soft-threasholding operator",
     xlab =expression(paste(x, " , blue lines indicates  ",lambda)))
#plotting lambda lines
abline( v=lambda, lty = "dashed", col = "blue")
abline( v=-lambda, lty = "dashed", col = "blue")



