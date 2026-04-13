####Simple Linear Regression & Correlation####

###packages###
#if any are not installed, install them
library(package = "psych")
library(package = "lsr") # will likely need to install
library(package = "ggplot2")
library(package = "tidyverse")
library(package = "jtools") #will likely need to install
library(package = "car")
library(package = "sciplot") #will likely need to install
library(package = "performance") #will likely need to install
library(package = "lmtest") #will likely need to install

####import and name dataset####
therapy <- 

#check data types of the variables
str(therapy)

####Let's determine--are these variables correlated?####
## Assumption of Normality
describe(therapy$sessions)
describe(therapy$symptoms)

shapiro.test(therapy$sessions)
shapiro.test(therapy$symptoms)

## Assumptions of Homoscedasticity and Linearity

# For linearity, we expect graph to be approximately linear
plot(therapy$symptoms, therapy$sessions, main='Scatter Plot for Linearity Check', xlab='Symptoms', ylab='Session')
abline(model)

# For homoscedasticity, we expect the variability of data points to be similar as we move away from the y-axis; in other words, we expect the variability of data points to have a similar (homo) variance or scatter (scedasticity) along the regression line (x-axis).
# For linearity, we also expect data points to be randomly dispersed around the regression line (x-axis); data points should not show nonlinear patterns. 
# in this case, we plot the residuals
# residual = sample datapoint - predicted datapoint

model <- lm(sessions ~ symptoms, data = therapy)
residuals <- resid(model)
fitted_values <- fitted(model)

plot(fitted_values, residuals, xlab='Fitted Values', ylab='Residuals', main='Residual Plot')
abline(h=0, col='red')

# Checking for outliers 

boxplot(therapy$symptoms, main='Boxplot for Outlier Detection')
boxplot(therapy$sessions, main='Boxplot for Outlier Detection')

# Conduct Pearson correlation

cor.test(therapy$sessions,therapy$symptoms,method=c("pearson"),
         alternative=c("two.sided"), conf.level=.95)

# What is effect size (coefficient of determination)? 

# Now, let's conduct a simple linear regression

####evaluate assumptions####

#####linearity#####
#####homoscedasticity#####
#####normality of errors#####
#####independence of errors#####

#review descriptive statistics for each variable
describe(therapy) #get an idea of M and SD for each variable

#graph correlation to see if it seems roughly linear
#plot(df$IV, df$DV)
plot(therapy$session, therapy$symptoms)

####Compute Linear Regression Model####
reg.model <- lm(formula = DV ~ IV, 
                data = df)

reg.model <- lm(symptoms ~ sessions, 
                data = therapy)
summary(reg.model)

#coefficients section - estimate column gives the y intercept and slop of the regression line
#residual standard error = measure of the accuracy of predictions made using the regression

####check assumptions####
#for regression we check the assumptions after we run the model because we need to create the model object to test the assumptions

#visualize assumption tests
check_model(reg.model)

#view graph labeled linearity - should fall along line

## other ways to test linearity--qqplots and scatter plots
qqnorm(therapy$sessions)
qqline(therapy$sessions)

qqnorm(therapy$symptoms)
qqline(therapy$symptoms)

# For linearity, we expect graph to be approximately linear
plot(therapy$sessions, therapy$symptoms, main='Scatter Plot for Linearity Check', xlab='Sessions', ylab='Symptoms')
abline(reg.model)

#normality of residuals (errors)
#review graph titled "homogeneity of residuals - should fall along line
#Shapiro-Wilk test on residuals
check_normality(reg.model)

#homoscedasticity
check_heteroscedasticity(reg.model)

#independence of errors
#Durbin-Watson test
dwtest(formula = reg.model)

####visualize data####

reg.plot <- ggplot(therapy, aes(x = sessions, y = symptoms)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_apa() + 
  labs(title = "Linear Regression of Symptoms of Sessions",
       x = "Sessions",
       y = "Symptoms")
reg.plot
