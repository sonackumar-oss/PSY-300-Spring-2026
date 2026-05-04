# Titanic Dataset: Interpreting Binary Predictors
library("psych")

# View Dataset

View(titanic_train)

# What do the variables mean? 
?titanic_train

# What are binary outcomes in this dataset? 

str(titanic_train)

# What are our descriptives? 

describe(titanic_train)

# What variables predicted the cost of a ticket (Fare) on the Titanic? 

# Class example: examining whether survival status predicts ticket cost

titanic.model2 <- lm(Fare ~ Survived, data = titanic_train)
summary(titanic.model2)

survived <- subset(titanic_train, Survived == "1")
no.survived <- subset(titanic_train, Survived == "0")
describe(survived$Fare)
describe(no.survived$Fare)

# Let's add other predictors
# Conduct regression with Gender (Sex), Age, and Survival (Survived) as predictors of Fare

# Make Gender binary (0s and 1s)

titanic_train$Gender <- ifelse(titanic_train$Sex == "female", 1, 0)

# Then, run model

titanic.model <- lm(Fare ~ Gender + Age + Survived, data = titanic_train)
summary(titanic.model)

# How do we interpret the output? 

