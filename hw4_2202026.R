#Consider a random variable W with the following probability density function
#f(w)=2-2w; 0<w<1 and f(w)=0 for all other values.


###TASK1 VERIFY THAT f(w)=2-2w IS INDEED A PROBABILITY FUNCTION

##Analytical Derivations : 2 conditions must be satisfied for a function to be a PDF

#1st condition : The function must satisfy f(w)>=0 for 0 < w < 1
#w = 0, f(0)=2 / w = 1, f(1)=0 -> we can conclude that all values are in the interval of [0,2]
#1st condition is satisfied because f(w) is always bigger or euqual to zero. 

#2nd condition : the integral of f(w) over the entire domain(0,1) must be 1. 
#When we take the integral of f(w) according to the interval of (0,1) it is equal to 1.  
#2nd condition is satisfied too. 
#Now we can say that f(w) is theoretically a Probability Density Function. 

##Corresponding R code:

f <- function(w) {
  if (w>0 && w<1) {
    cat("1st condition is satisfied. The result is:" ,2 - 2*w , "\n") }
    else {
      print("1st condition failed. The result is negative") }
}
f(0.5)
f(5)

integral_result <- integrate(function(w) 2- 2*w, lower=0, upper=1)
print(integral_result$value)

if (integral_result$value == 1) {
  print("2nd condition is satisfied. f(w) is a Probability Density Function")
} else {
  print("2nd condition is failed. f(w) is NOT a Probability Density Function")
}

##Graphs and visualizations:

f <- function(w)
  ifelse(w > 0 & w < 1, 2- 2*w, 0)
w_values <- seq(0, 1, 0.01)
f_values <- f(w_values)
plot(w_values, f_values, type="l", main= "f(w)= 2- 2*w", xlab = "w", ylab="f(w)")


###TASK2 FIND THE CORRESPONDING CUMULATIVE FUNCTION F(w)

##Analytical Derivations : To find the cumulative distribution function F(w);

#we integrate f(w) again. but this time the lower limit of the integral is 0 and upper one is w. 
#the integral of f(w) is [2t- t^2 +C] when we put the limits (0 and w) 
#the result of the integration becomes 2w- w^2
#So we can say that ; 
#if w=<0       F(w) = 0 
#if 0 < w < 1  F(w) = 2w - w^2 
#if w >= 1     F(w) = 1  

##Corresponding R code:

F <- function(w) {
ifelse(w <= 0, 0,
       ifelse( w>= 1, 1,
               2*w - w^2))
}

w_values <- seq(-0.5, 1.5, 0.01)

F_values <- F(w_values)

##Graphs and visualizations:

plot(w_values, F_values, type = "l", lwd = 2, col = "purple",
     main = "Cumulative Distribution Func F(w)",
     xlab = "w", ylab = "F(w)")


###TASK3 FIND THE EXPECTATION OF W

##Analytical Derivation: To find the expectation of w, we multiply w with f(w) and take the integral. with the limits (0,1)
#∫(w * (2-2w)) = w^2 - (2/3)w^3 put the limiits 0 and 1 -> 1/3
#so the expected value of W E[W] is 1/3. 


##Corresponding R code

expectation_integral <- function(w) {
  w * (2 - 2*w)
}

expectation_result <- integrate(expectation_integral, lower = 0, upper = 1)

cat("Expected value (E[w]) is:", expectation_result$value, "\n")


###TASK4 FIND THE STANDARD DEVIATION OF w

##Analytical Derivation: standard deviation is the square root of variance. 
#And Variance = E[w^2] - (E[w])^2     -> so the square root of this equation equals standard deviation.

#we calculated that E[w] = 1/3 just above so (E[w])^2 = 1/9
#to calculate E[w^2] we will do the integration part again but this time we will do it with w^2 instead of w
#and this calculation will be 1/6
#so the Variance(w) = 1/6 - 1/9 = 1/18
#we were looking for the standard deviation SD(w) = √(1/18) ≈ 0.2357

##Corresponding R code

E_W2_integral <- function(w) {
  w^2 * (2 - 2*w)
}

E_W2_result <- integrate(E_W2_integral, lower = 0, upper = 1)$value

E_W <- 1/3

variance <- E_W2_result - E_W^2

std_dev <- sqrt(variance)

cat("E[W^2] =", E_W2_result, "\n")
cat("Variance =", variance, "\n")
cat("Standard Deviation =", std_dev, "\n")


###TASK5 FIND THE MEDIAN OF THIS DISTRIBUTION

##Analytical Derivation: F(m) must be equal to 0.5 at the median value. 
#F(m)= 2m - m^2 = 0.5      ->  m= (-2+√2)/-2 = 1 - √2/2 ≈ 0.2929
#Median(W) ≈ 0.2929


##Corresponding R value:

F <- function(w) {
  ifelse(w <= 0, 0,
         ifelse(w >= 1, 1,
                2*w - w^2))
}

cdf_median_minus_half <- function(w) {
  F(w) - 0.5                                #that is what we trying to solve. 
}

median_result <- uniroot(cdf_median_minus_half, c(0, 1))  #using uniroot() to solve F(w) = 0.5 in the interval of (0, 1)

cat("The median of W is ≈", median_result$root, "\n")
