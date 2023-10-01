# link to the dataset: https://www.kaggle.com/datasets/parisrohan/credit-score-classification?resource=download
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


glimpse(data)


# After certain researches, I've decided to do a general analysis of an entire dataset.
# Generally, each row represents a financial credit condition of a customer
# in a certain month. We may view it as a credit scoring reports for a customers of a single bank
# Explanation of columns for this dataset:

# ID - identifier for a record. Ex: 0x1602, 0x1603

# Customer_ID - identifier for a customer. Ex: CUS_0xd40, CUS_0x4157

# Month - month in the year, when the record was made

# Name - name of this customer

# Age - age at the moment of reporting

# SSN - social security number

# Occupation - occupation of customer at the moment of reporting

# Annual_Income

# Monthly_Inhand_Salary

# Num_Bank_Accounts - number of bank accounts

# Num_Credit_Card - number of active credit cards

# Interest_Rate - interest rate in percents, established for this client

# Number of loans - number of active loans at the moment

# Type_Of_Loan - string, that is representing type of each loan, that customer has at the moment
# Ex. "Auto Loan, Credit-Builder Loan, Personal Loan, and Home Equity Loan", while customer has 4 active loans

# Delay_from_due_date - represents the average number of days delayed from the payment date

# Num_Of_delayed_payment - number of payments that were paid after due date and were penalized

# Num_Of_Credit_Inquiries - amount of times when client has tried to receive his credit score and apply for a credit

# Credit_Mix - category specific for this dataset, indicating if types of credits that customer
# has is good for the company or bad. Label category


# Outstanding_Debt - is amount of debt that customer owes this company at the moment

# Credit Utilization Ratio - coefficient, that is being calculated by formula CUR = (amount_in_hand/credit_limit) * 100
# this tells us how much money from that credit is being used by the customer

# Total_EMI_per_month - monthly payment, required from the customer to cover his payment for the previous month.

# Payment_Of_Min_Amount - flag, indicating  whether customer has covered his minimal payments for this month

# Payment_Behavior - label, characteristic of a customers payment behaviour, his nominal spending and value payments

# Monthly_balance - total amount of money on the account of this customer.