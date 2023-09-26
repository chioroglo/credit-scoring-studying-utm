library(tibble)
library(dplyr)
library(stringr)
library(ggplot2)
setwd("../CreditScoringStudying")
data <- read.csv("resources/train.csv", sep = ',')

# get names of all columns within a dataset
names(data)

# get type of data in certain column
class(data$Credit_Score)

glimpse(data)

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
data_with_invalid_age <- data %>% filter(Age > 56)
glimpse(data_with_invalid_age %>% arrange(Age))
# Amount: 2781. I want replace that values with mean of Age
# TODO proof that removal or replac with mean is necessary. proof via plotting and hypothesis



# I've spotted, that ages highet that 56 have only from 1 to 3 occurences
# so i consider them as outliers and eliminate'em for now
data <- data %>% filter(Age <= 56)

# Lets show the correlation between salary and age of my dataset
scatter_plot_income_vs_age <- ggplot(data,aes(x=data$Age,y=data$Monthly_Inhand_Salary)) +
  geom_point() +  
  labs(title="Scatter plot of Income vs. Age", x="AGE", y="Salary")
ggsave(filename="../creditscoringstudying/media/Scatter_Plot_Income_vs_Age.png", scatter_plot_income_vs_age)
# This information gives me basically nothing, only see, that there is practically
# no high salary before the age of 20-21, approximately 


# I want to see mean salary for each age, presented in the dataset
# to see whats going on for the mean values
age_mean_salary_number_of_clients_data <- data %>%
  filter(!is.na(Monthly_Inhand_Salary)) %>%
  group_by(Age) %>%
  summarize(
    Mean_Salary = mean(Monthly_Inhand_Salary, na.rm = TRUE),
    Total_Clients = n()
  )

mean_salary_and_age_plot <- ggplot(age_mean_salary_number_of_clients_data, aes(x = Age, y = Mean_Salary)) +
geom_point(shape = 19, size = 3, color = "red", alpha = 0.7) +
labs(
  title = "Mean Inhand Salary by Age",
  x = "Age",
  y = "Mean Inhand Salary"
) +
theme_minimal()
ggsave('../creditscoringstudying/media/Mean_Inhand_Salary_By_Age.png',mean_salary_and_age_plot)
#After plotting this table, I observe three clusters of data
# 1. [AGE < 20], [SALARY < 3500]
# 2. [20 <= AGE <= 45], [3750 < SALARY < 4500]
# 3. [AGE >= 45], [4500 <= SALARY <= 5500]