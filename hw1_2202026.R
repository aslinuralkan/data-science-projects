##TASK 1 DESCRIPTION OF THE PROBLEM & ASSUMPTIONS

#description of the problem using my own words
#we need to determine the appropriate sample size to avoid unplanned mistakes.
#to do that we have given values which are in the question. 
#we need to find the sample size to have at least 92% probability to find at least 1 defected item in the sample. 
#we know that in 100,000 items 100 of them are defected. which equals a defect rate of 0.1% 
#our goal is to calculate the smallest "n" value that gives at least a 92% probability of detecting at least one defected item in the sample.

#assumptions
#sampling without replacement: when we take an item to check whether it is defected or not
#                              we don't put it back to population. meaning, with each selection
#                              the probability of selecting a defective item changes slightly
#                              this is why we use hypergeometric instead of binomial distribution. 
#random sampling: as stated in the problem, it is a random sampling, assumed to be selected randomly. 
#                 meaning, every item has an equal chance of being selected. 


##TASK 2 MATHEMATICAL MODELING

#as stated above, we use the hypergeometric because we are doing sampling without replacement. 
#we could use binomial distribution if the population were very large and the sampling with replacement were acceptable. 

N <- 100000   #total number of items
D <- 100      #number of defected items
              #n = minimum sample size which we will find the real with the next tasks 
              #X = number of defected items 'in the sample'
U <- N - D    #number of undefected items

#p_zero <- dhyper(0, D, U, n)  #we don't know 'n' yet that's why I used '#' 
                               #the probability of selecting 0 defected items. (dhyper(x, m, n, k, log = FALSE))
                               #P(X=0) = (N-D, n)/(N, n)

#p_least_one <- 1- p_zero      #to find the prob of detecting at least one defected item (complement rule)
                               #P(at least one defected) = 1- P(X=0)
                               #our goal:  1-P(X=0) >= 0.92  -> * P(X=0) =< 0.08 *

#cat("Probability of having 0 defected item is:", p_zero, "\n")
#cat("Probability of having at least one defected item is:", p_least_one)


##TASK 3 ANALYTICAL DERIVATION

#our goal is to find ->  1-P(X=0) >= 0.92 -> P(X = 0) =< 0.08

for (n in 1:5000) {
  p_zero <- dhyper(0, D, U, n)
  if (p_zero <= 0.08) 
    break
  }

cat("Minimum sample size n is:", n, "\n")
cat("Probability of having 0 defected item:", p_zero, "\n")
cat("Probability of at least one defected item:", 1 - p_zero)


##TASK 4 R-BASED SIMULATION

set.seed(42)

trials <- 1000                            #to increase our code's accuracy rate

sim_prob <- function(n, trials=1000) {
  samples <- rhyper(trials, D, U, n)      #number of defected items in each simulation
  mean(samples >= 1)                      #?
}
sample_sizes <- 0:5000                              #every n value between 0000:5000 
probabilities <- sapply(sample_sizes, sim_prob)     #success probability of each n between 0000:5000. probability of having at least one defected

library(ggplot2)

df <- data.frame(                      #creating a data frame because we need it to use ggplot
  sample_size = sample_sizes,
  probability = probabilities
)

min_n_sim <- df$sample_size[which(df$probability >= 0.92)[1]]      #whcih()[1] gives first true n value. which is minimum true n value

ggplot(df, aes(x=sample_size, y=probability)) +
  geom_line(color="red") +
  geom_hline(yintercept = 0.92, color = "blue") +
  geom_vline(xintercept = min_n_sim, color = "green", linetype = "dashed") +
  labs(
    title = "estimated probability vs sample size",
    x= "sample size(n)",
    y="probability of at least one defected item"
  )

cat("the smallest sample size that consistently achieves or exceeds 92% probability:", min_n_sim)   #identifying


##TASK 5 INTERPRETATION & DISCUSSION

##"Interpreting our findings from both analytical and simulation based methods"
#In task 3 we calculated the mathematical derivation with hypergeometric distribution. 
#with this way we obtained **n = 2493**. This answer is theoretically true and obtained directly from the formula. 
#and in task 4 we applied a simulation based method , which by using random sampling with hypergeometric(rhyper) to estimate probabilities. 
#This simulation based approach resulted in a smaller sample size different from the previous task. we iterate the random sampling 1000 times with rhyper function
#we get the minimum sample size **n = 2243**. 
#the two answers are close to each other but not the same ,the reason for that is:
#analytical method assumes a theoretical world. it is precise but rigid
#simulation method displays a better result according to the real-world randomness


##“Comment on sensitivity to parameter changes (e.g., increasing defect count or batch size)”
#what would happen if defected item(D) number increases?
#if D was 200 instead of 100 -> the rate of having defected item would be more than 0.1 -> n(minimum sample size) would be smaller 
#what would happen if total item size increases?
#if N was 200,000 instead of 100,000 ->the rate of having defected item would be smaller -> n(minimum sample size) wowuld be larger
#what would happen if the probability of detecting at least one defective item in the sample was 42% instead of 92%
#92% requires high confidence -> requires larger sample size (n). 42% doesn't need that much confidence -> smaller sample size(n) would be enough. 


##"Discuss practical implications for quality control processes in industrial settings."
#it is impossible to check every item one by one in the manufacturing line. It is inefficient with both time and money. choosing the right sample size is the key. 
#thats why with the statistics, determining n sample size, -> with the early detection of the defected items ->dissatisfactions and mistakes would be prevented.