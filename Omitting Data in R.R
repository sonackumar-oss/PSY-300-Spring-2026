install.packages("psych")
library("psych")

# Read in csv
pew_missing <- read.csv("~/Downloads/pew_t.csv") 

# First, change missing values from code 99 (or . or blank) to NA

pew_missing[pew_missing == "99"] <- NA 

# Then do na.omit which removes any row with missing values (NA)
pew_cleaned <- na.omit(pew_missing)

# Calculating descriptives
# We can tell R to omit missing values using na.rm = TRUE
# This way, it does calculations without including NA values

mean(pew_missing$GROUP_TRUST_b, na.rm = TRUE)

# It can't calculate mean when na.rm = FALSE
mean(pew_missing$GROUP_TRUST_b, na.rm = FALSE)

# Note that there is a difference in means when we run the mean of pew_cleaned
# This is because in this case, R omitted any rows with NA
# So we have fewer observations thatn in the case of pew_missing

mean(pew_cleaned$GROUP_TRUST_b)


