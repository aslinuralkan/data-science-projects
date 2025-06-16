##TASK1 THEORETICAL DERIVATION 

#"Recall the properties of linear combinations of independent normal variables."
#When you take two independent normal variables and combine them using addition or multiplication with constants, the result is also normally distributed.
#This is really helpful because it means the properties of the normal distribution stay with us even after transformations.

#"Show that a linear combination of independent normal variables is also normally distributed."
#if X and Y are independent and normally distributed then any linear combination of them would be normally distributed.(Z=aX+bY)
#X ~ Binomial(n=50, p=0.1)
#Y ~ Binomial(n=20, p=0.25)

#"Compute the mean and variance of 𝑍=3𝑋+2𝑌using: 
#(μZ = 3*μX + 2*μY)  and (σZ^2 = {(3^2 * σX^2) + (2^2 * σY^2)]"
μ_X = 50*0.1          # 5
μ_Y = 20*0.25         # 5
μ_Z = (3*5)+(2*5)     # 25 

σ2_X = 50 * 0.1 * 0.9          # 4.5
σ2_Y = 20 * 0.25 * 0.75        # 3.75
σ2_Z = (9 * 4.5) + (4 * 3.75)  # 55.5

cat("Theoretical mean of Z:", μ_Z , "\n")
cat("Theoretical variance of Z:", σ2_Z , "\n")

#"Clearly state the resulting distribution Z ~ N(μZ, σZ^2)"
#Z ~ N(25, 55.5)


#TASK 2 SIMULATION IN R: Perform a Monte Carlo simulation to empirically validate the distribution of 𝑍.
#Monte Carlo simulation is used  for the affects of random variables to system. 

set.seed(42)     #for randomization

n <- 10000       #number of simulation 

X <- rbinom(n, size = 50, prob=0.1)    #Generate   10,000 samples form each binomial distribution. 
Y <- rbinom(n, size = 20, prob=0.25)
Z <- 3*X + 2*Y                         #Computing the Z = 3X +2Y

mu_Z_empirical <- mean(Z)
var_Z_empirical <- var(Z)

cat("Empirical mean of Z:", mu_Z_empirical, "\n")
cat("Empirical variance of Z:", var_Z_empirical, "\n")

#creating a histogram to see if we can empirically validate the distribution of Z. 
hist(Z,
     main = "Histogram of Z = 3X + 2Y",
     xlab = "Z values",
     col = "blue",
     prob = TRUE)                      #makes the density instead of frequency
lines(density(Z), col="red", lwd=2)


#histogram looks symmetric and similar to the normal distribution. 
#meaning, the empirical mean and variance will be close to the expected theoretical μ and σ^2


##TASK 3 COMPARISON AND INTERPRETATION
#"Compare the empirical mean and variance with the theoretical values."

cat("Theoretical mean of Z:", μ_Z , "\n")                #theoretical mean of Z is 25 
cat("Emprical mean of Z:", mu_Z_empirical, "\n")         #empirical mean of Z is 24.9342 

cat("Theoretical variance of Z:", σ2_Z , "\n")           #theoretical variance of Z is 55.5 
cat("Empirical variance of Z:", var_Z_empirical, "\n")   #empirical variance of Z is 56.42511

#theoretical mean is bigger than the empirical but also close to each other
#empirical variance is a little bigger than the theoretical but also close 

#"Comment on the shape of the histogram. Does it resemble a normal distribution?"
#yes the shape of the histogram resembles normal distribution. we can also say that by the comparison above. they are close values so we can interpret the normality of the distribution there. 

#"Discuss the role of the coefficients 3 and 2 in shaping the distribution of 𝑍."
#The larger the coefficient, the more that variable shapes the distribution of Z. 
#Since X is multiplied by 3, it has a bigger impact on Z compared to Y, which is only multiplied by 2. So basically, X pulls Z more than Y does , both in terms of average and how spread out the values are.

#"Briefly explain how this relates to real-world applications, such as regression models or portfolio risk analysis"

#In regression models, impact of each input to the output shown with its coefficients. 
#in our equation *Z = 3X + 2Y*, it shows us that X has more impact on the output compared to Y. 
#For example we can understand which feature is more effective in predicting the output. 
#In a house pricing model it could answer a question like: "which matters more, location or the number of rooms?

# Just like in regression, portfolio risk analysis also combines variables using simple math.
# The coefficients basically show how much each investment affects the total return and risk.

