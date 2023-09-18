library(tibble)
library(dplyr)
library(stringr)
setwd("C:/Users/Chioroglo/Desktop/CreditScoringStudying")
data <- read.csv("resources/train.csv", sep = ',')

# get names of all columns within a dataset
names(data)

# get type of data in certain column
class(data$Credit_Score)


# Because initial data is "character", not "factor", mutate this column to be able to see all categories as FACTOR
# Here, I transform all categorical data to R-ish fct column type
data$Credit_Score <- as.factor(data$Credit_Score)
data$Payment_Behaviour <- as.factor(data$Payment_Behaviour)


levels_of_credit_score <- levels(data$Credit_Score)
levels_of_payment_behaviour <- levels(data$Payment_Behaviour)
print(levels_of_credit_score)
print(levels_of_payment_behaviour)

# I have spotted some garbage data within a 'levels_of_payment_behaviour' column.
# I want to know how largely this garbage data appears in this column
# I want to replace it with concise name
result <- data %>%  count(Payment_Behaviour) %>%
  arrange(desc(n))

data <- data %>% mutate(
        Payment_Behaviour = case_when(Payment_Behaviour == "!@9#%8" ~  "Unknown", TRUE ~ Payment_Behaviour)
    )

result <- data %>%  count(Payment_Behaviour) %>%
  arrange(desc(n))


# Now, I am looking forward to analyse other columns

# I've spotted that in "Age" column exist garbage data.
# Kinds of garbage data within this column:
# Unreal age (age > 100, but we'll see)
# Dirty string, that should be purged
# To purify strings, I would parse a numbers using regex "\d+"


# After this mutation, I've noticed, that all ages that are above 56 are presented
# in less than 3 rows each. For the sake of simplicity, omit these ones, but firstly
data <- data %>% mutate(Age = as.numeric(str_extract(Age, "\\d+"))) 

# I want to calculate how much rows would be lost.
data_with_invalid_age <- filter(data, Age >= 56)
glimpse(data_with_invalid_age)
# Amount: 3143. I will replace that values with mean of Age

average_age <- mean(data$Age)
print(average_age)


data <- filter(data, Age <= 56)

glimpse(data)


# result <- data %>%  count(Age) %>%
#   arrange(desc(Age))

# print(result)
 

# glimpse(data)