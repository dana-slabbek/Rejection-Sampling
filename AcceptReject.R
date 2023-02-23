# Estimate using rejection sampling

#step 1: find g(x) such that it's a function with a density over f(x)
  # f(x) <- exp((-abs(x)^3)/3)
  # g(x) <- exp((-abs(x)^2)/2)

#step 2: find f(x)/g(x) to give r
  # r = exp((x^2/2)-(abs(x)^3/3)

#step 3: find maximum of r by taking r'
  # r' = exp((x^2/2)-abs(x)^3/3)*(x-(x^3/abs(x)))

#step 4: find values where r' = 0
  # r' = 0 if x=-1 & x=1

#step 5: plug in max zero value to r to get K
  # K = exp(1/6)


#plot of f(x)
x<-seq(0,2,length=20)
plot(x,exp((-abs(x)^3)/3))

rejection_gen <- function(n){
# simulates from Gamma pdf with alpha=2 
# using the acceptance rejection algorithm
  K <- exp(1/6)     # equation with max plugged in
  f <- function(x) exp((-abs(x)^3)/3)   # original function
  g <- function(x) exp((-abs(x)^2)/2)  
   
   x <- vector("numeric",length=n)
   for (i in 1:n){
     keep_x <- F
     while (!keep_x){
     u <- rexp(n=1,rate=1)  
       v <- runif(n=1,min=0, max=K*g(u)) 
       
     if (v < f(u)) {     #generate as many Us as necessary until we find one to keep
         keep_x <- T   
         x[i] <- u
       }
     }
   }
   return(x)
 }


