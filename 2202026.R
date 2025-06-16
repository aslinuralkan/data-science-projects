#TASK1 

library(MASS)
data("Boston")
head(Boston)

model <- lm(medv ~ rm, data = Boston)
summary(model)

##Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  -34.671      2.650  -13.08   <2e-16 ***
## rm             9.102      0.419   21.72   <2e-16 ***
## Multiple R-squared:  0.4835, Adjusted R-squared:  0.48

#rm and medv has a strong relationship because p-val < 0.05



#TASK2

model2 <- lm(medv ~ rm + lstat + crim, data = Boston)
summary(model2)

## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -2.56225    3.16602  -0.809  0.41873    
## rm           5.21695    0.44203  11.802  < 2e-16 ***
## lstat       -0.57849    0.04767 -12.135  < 2e-16 ***
## crim        -0.10294    0.03202  -3.215  0.00139 ** 
##Multiple R-squared:  0.6459, Adjusted R-squared:  0.6437 

#rm is positively related to medv. howeever the other variables are negatively related to medv.
#all predictors are significantly related to medv, except the intercept
#r squared values are equal to roughly %65 we can say that is a mid-good percantage
#it explains ~64.6% of the variation in medv. 



#TASK3

model3 <- lm( medv ~ poly(rm, 2), data = Boston )
summary(model3)

## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   22.5328     0.2753  81.849   <2e-16 ***
## poly(rm, 2)1 143.7164     6.1927  23.208   <2e-16 ***
## poly(rm, 2)2  52.6526     6.1927   8.502   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 6.193 on 503 degrees of freedom
## Multiple R-squared:  0.5484, Adjusted R-squared:  0.5466 

#both of the model's p-values are less than 0.05. 
#but model3's r-squared is ~54.8% model1's is ~48%
#which means that adding a quadratic term improves 
#predictions better than simple lineer regression in task1



#TASK4

library(car)
vif(model2)
#we used model2 because model2 is a multiple lineer regression model

##       rm    lstat     crim 
## 1.616468 1.941883 1.271372

#all vif values are less than 5 , this means multicollinearity isnt a concern
#in this model, predictors can stay.



#TASK5

library(glmnet)

independent_var <- as.matrix(Boston[, c("rm", "lstat", "crim", "age", "dis", "tax")])
dependent_var <- Boston$medv
model5 <- cv.glmnet(independent_var, dependent_var, alpha = 1)
plot(model5)
coef(model5, s = "lambda.min")

## 7 x 1 sparse Matrix of class "dgCMatrix"
##                       s1
## (Intercept)  6.594545627
## rm           5.044579221
## lstat       -0.581959786
## crim        -0.069940487
## age         -0.021780555
## dis         -0.895185901
## tax         -0.007980414

#none of the variables have non-zero coefficients after Lasso regularization
#rm and dis are the most prominent features , we can say they are the most important
#predictors of the model
#none of the variables were shrunked to zero but 
#tax and age have very small coefficients and may be less important



#TASK6

median_medv <- median(Boston$medv)
Boston$high_medv <- ifelse(Boston$medv > median_medv, 1, 0)
Boston$high_medv <- as.factor(Boston$high_medv)

model6 <- glm(high_medv ~ rm + lstat, data = Boston, family = binomial)
#the goal is here to see if whether medv is above or below the median value using rm and lstat
summary(model6)

## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -3.73825    2.05386  -1.820   0.0687 .  
## rm           1.25167    0.31212   4.010 6.06e-05 ***
## lstat       -0.34614    0.03563  -9.714  < 2e-16 ***

#both rm and lsat is significant in predicting whether medv is "above" the median
#because their p-values are less then 0.05
#rm has a positive effect, while lstat has a negative
#intercept isnt significant but doesnt affect the overall model inetrpretation



#TASK7

model <- lm(medv ~ rm, data = Boston)
par(mfrow = c(2, 2))
plot(model)

#residuals are radomly scattered around 0 (y axis is residuals) 
#there is a slight curve which suggests a small degree of non-linearity
#no there is no funnel shape
#yes there are few outliers such as 366, 369, 373


