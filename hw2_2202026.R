##TASK1 PLOTTING THE PROBABILITY DISTRIBUTION FUNCTIONS (i.e., PMFs) of both 𝑋and𝑌using R
# X ~ Binomial(n = 50, p = 0.10)
# Y ~ Binomial(n = 20, p = 0.25)

n_X <- 50
prob_X <- 0.10
n_Y <- 20
prob_Y <- 0.25

#pmf for X
values_X <- 0:n_X                                   #pmf is defined for the values of X (0, 1, 2, ..., 50)
pmf_X <- dbinom(values_X, size=n_X, prob = prob_X )   #dbinom() for P(X=3) =exact 3 
pmf_X

#pmf for Y
values_Y <- 0:n_Y
pmf_Y <- dbinom(values_Y, size=n_Y, prob= prob_Y)
pmf_Y
#plotting X 
plot(values_X, pmf_X, type="h", col="blue",
     main="pmf of X",
     xlab = "x", ylab="P(X=x)", lwd=7)
#plotting Y
plot(values_Y, pmf_Y, type="h", col="red",
     main="pmf of Y", 
     xlab="y", ylab="P(Y=y)", lwd=7)


##TASK 2 COMPUTING THE MEAN AND STANDARD DEVIATION FOR EACH DISTRIBUTION

# μ = n⋅p               mean for the X-Bin(n, p)
# σ = (n⋅p⋅(1−p))^0.5   standard deviation 

#the mean and standard deviation for X
mean_X <- n_X * prob_X
sd_X <- sqrt(n_X * prob_X * (1- prob_X) )
cat("the μ of X:", mean_X, "\n")
cat("the σ of X:", sd_X, "\n")

#the mean and standard deviation for Y
mean_Y <- n_Y * prob_Y
sd_Y <- sqrt(n_Y * prob_Y * (1- prob_Y))
cat("the μ of Y:", mean_Y, "\n")
cat("the σ of Y:", sd_Y, "\n")


##TASK 3 DETERMINE: WHICH DISTRIBUTION HAS THE HIGHER MEAN AND STANDARD DEVIATION
#Comparing the means  -> they are equal for both X and Y
#the μ of X: 5 , the μ of Y: 5
#Comparing the standard deviations -> σX > σY
#the σ of X: 2.12132 , the σ of Y: 1.936492


##TASK 4 INTERPRETING THE RESULTS IN MY OWN WORDS
#Both X and Y models have the same expected value(mean = 5), but their spreading is differently. 
#X's standard deviation(2.12) is larger than the Y's(1.93). This shows us that X's values are spreading in a larger interval around the mean.
#This makes sense because X has more trials(n=50) with a smaller probability(p=0.1). This could also show us that X is more spreadable than Y.
#On the other hand, Y has smaller trials(n=20) and larger probability (p=0.25). So Y's values are more concentrated around the mean. 
#Also when we look at their histogram plots above, we can see these conclusions are true. 
#In summary, we can say that they both have the same center but different variability. 

