# link to the dataset: https://www.kaggle.com/datasets/parisrohan/credit-score-classification?resource=download

source("utils.r")
initialize_environment()
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
theme(panel.background = element_rect(fill= 'white'))

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




# I want to analyze if there is correlation between number of credit inquiries for a client
# and his credit score. I will group by credit_score and see what's average number and mode of credit inquiries for this label

credit_inquiries_summary <- data %>% filter(!is.na(Num_Credit_Inquiries)) %>% group_by(Credit_Score) %>%
  summarize(
    median_num_of_credit_inquiries = median(Num_Credit_Inquiries),
    mode_num_of_credit_inquiries = Mode(Num_Credit_Inquiries),
    average_num_of_credit_inquiries = mean(Num_Credit_Inquiries)
  )

glimpse(credit_inquiries_summary)


# i want to do a scatter plot to see whether there is a certain correlation
median_num_of_Credit_inquiries_vs_credit_score <- ggplot(data, aes(x = Credit_Score, y = Num_Credit_Inquiries)) +
  geom_bar(stat = "summary", fun = "median") +
  labs(x = "Credit Score", y = "Median Number of Credit Inquiries") +
  ggtitle("Bar Plot of Credit Score vs. Median Number of Credit Inquiries") +
  theme(panel.background = element_rect(fill= 'white'))

ggsave('../creditscoringstudying/media/BarPlot_Credit_Score_Median_Number_Credit_Inquiries.png',median_num_of_Credit_inquiries_vs_credit_score)

# Number of
occupations_count <- data %>%
  distinct(Occupation,Customer_ID) %>%
  group_by(Occupation) %>%
  summarise(count = n()) %>%
  ungroup()

print(occupations_count)


salary_occupation_count_of_occupation <- data %>%
  group_by(Customer_ID) %>%
  mutate(Monthly_Inhand_Salary_Mean = mean(Monthly_Inhand_Salary, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(Occupation) %>%
  mutate(Count_of_this_occupation = n()) %>%
  ungroup() %>%
  mutate(Occupation = as.factor(Occupation)) %>%
  select(Customer_ID, Occupation, Monthly_Inhand_Salary_Mean, Count_of_this_occupation) %>%
  distinct(Customer_ID, .keep_all = TRUE)

print(salary_occupation_count_of_occupation)


salary_occupation_preprocessed_for_graph <- salary_occupation_count_of_occupation %>%
  group_by(Occupation) %>%
  mutate(Salary_Median_Per_Occupation = median(Monthly_Inhand_Salary_Mean, na.rm = TRUE)) %>%
  ungroup() %>%
  select(Occupation, Salary_Median_Per_Occupation, Count_of_this_occupation)

print(salary_occupation_preprocessed_for_graph)

graph_salary_occupation <- ggplot(data = salary_occupation_preprocessed_for_graph, aes(x = Occupation,y = Salary_Median_Per_Occupation)) +
geom_col(aes(fill = Count_of_this_occupation)) +
labs(x = "Occupation", y = "Monthly Salary Median Per Occupation") +
scale_fill_gradient2(low = "red", high = "green", midpoint = median(salary_occupation_preprocessed_for_graph$Count_of_this_occupation)) +
coord_flip()

ggsave("../creditscoringstudying/media/Bar_Plot_Salary_Median_Per_Occupation.png",graph_salary_occupation)


# Part 2 of cleansing and arranging the dataset 29.10.2023

# Columns, that hold information about personal data of the lead are:
# "SSN" - social security number - does not affect credit score - https://en.wikipedia.org/wiki/Social_Security_number
# "Name" - does not affect credit score, for identification purposes , I use Customer_ID


# Type_Of_Loan - indicates stringified version of all types of credit of that lead, but we have a column
# named "Credit_Mix", which practically gives us only conclusions that
# could be made from this column without calculations, so I decide to drop it.


# Month - as we are looking for more summarized data, recognise
# each record regardless of order of month, so I may drop it as
# irrelevant.

# Remove them from the dataset

data <- data %>% select(-SSN, -Name, -Type_of_Loan, -Month)
glimpse(data)

# I want to take a look over a data within columns "Monthly_Inhand_Salary" and "Annual Income".
# According to the information, came from the documentation:
# Annual_Income - Represents the annual income of the person
# Monthly_Inhand_Salary - Represents the monthly base salary of a person


data$Annual_Income <- sapply(data$Annual_Income, parse_decimal_or_na)

data <- data %>%
  group_by(Customer_ID) %>%
  mutate(Monthly_Inhand_Salary = ifelse(Monthly_Inhand_Salary %in% c(NA, "NA"),
    mean(Monthly_Inhand_Salary, na.rm=TRUE),
    Monthly_Inhand_Salary)
  ) %>%
  ungroup()

# treat the outlines replacing'em with mean over a customer_ID
# if value is less than 1st quantile and greater than 6th quantile

q1 <- quantile(data$Annual_Income, 0.1, na.rm = TRUE)
q6 <- quantile(data$Annual_Income, 0.6, na.rm = TRUE)

# [Annual_Income] replace values with mean over a group when income is out of range of
# Q1 and Q6 quartiles
data$Annual_Income <- as.numeric(data$Annual_Income)

data <- data %>% group_by(Customer_ID) %>%
  mutate(Annual_Income = ifelse(Annual_Income <= q1 | Annual_Income >= q6,
  mean(Annual_Income,na.rm = TRUE),
  Annual_Income)) %>%
  ungroup()

# [Num_Bank_Accounts] replace values over q6 with mode over Customer_ID
q6 <- quantile(data$Num_Bank_Accounts, 0.6, na.rm = TRUE)

data <- data %>%
group_by(Customer_ID) %>%
  mutate(Num_Bank_Accounts = ifelse(Num_Bank_Accounts > q6,
  Mode(data$Num_Bank_Accounts),
  Num_Bank_Accounts)
) %>%
ungroup()


# [Num_Credit_Card] replace values over q6 with mode over Customer_ID
q6 <- quantile(data$Num_Credit_Card, 0.6, na.rm = TRUE)

data <- data %>%
group_by(Customer_ID) %>%
  mutate(Num_Credit_Card = ifelse(Num_Credit_Card > q6,
  Mode(data$Num_Credit_Card),
  Num_Credit_Card)
) %>%
ungroup()

# frequency graph over interest rate
filtered_outlines_interest_rate <- data %>% filter(Interest_Rate < 50)
histogram_interest_rate_frequency <- hist(filtered_outlines_interest_rate$Interest_Rate)


png(filename="../creditscoringstudying/media/Interest_Rate_Frequency_Rate.png")
###

plot(histogram_interest_rate_frequency,main="Interest rate frequency histogram",
xlab="Interest Rate",
ylab="Frequency",
col="lightblue",
border="black")

dev.off()

glimpse(data)
### I've considered to manually replace outliers within Interest Rate column
data <- data %>%
group_by(Customer_ID) %>%
mutate(Interest_Rate = ifelse(Interest_Rate >= 50, Mode(Interest_Rate),Interest_Rate)
) %>%
ungroup()

# [Num_of_Loan] replace values over q6 with mode over Customer_ID

data$Num_of_Loan <- sapply(data$Num_of_Loan, parse_decimal_or_na)

q6 <- quantile(data$Num_of_Loan, 0.6, na.rm = TRUE)
data <- data %>%
group_by(Customer_ID) %>%
  mutate(Num_of_Loan = ifelse(is.na(Num_of_Loan) | Num_of_Loan > q6 | Num_of_Loan < 0,
  Mode(Num_of_Loan),
  Num_of_Loan)
) %>%
ungroup()

# [Num_of_Delayed_Payment] replace values over q6 with mode over Customer_ID and if no mode can be calculated, then just pust 0
data$Num_of_Delayed_Payment <- sapply(data$Num_of_Delayed_Payment, parse_decimal_or_na)
q6 <- quantile(data$Num_of_Delayed_Payment, 0.6, na.rm = TRUE)

data <- data %>%
group_by(Customer_ID) %>%
  mutate(Num_of_Delayed_Payment = ifelse(is.na(Num_of_Delayed_Payment) | Num_of_Delayed_Payment > q6 | Num_of_Delayed_Payment < 0,
  ifelse(is.na(Mode(Num_of_Delayed_Payment)),0,Mode(Num_of_Delayed_Payment)),
  Num_of_Delayed_Payment)
) %>%
ungroup()

# [Changed_Credit_Limit] replace values with 0 if it isn't parsable decimal number, because 0 won't affect result at all.
data$Changed_Credit_Limit <- sapply(data$Changed_Credit_Limit, parse_decimal_or_na())
data <- data %>%
 mutate(Changed_Credit_Limit = as.numeric(Changed_Credit_Limit)) %>%
 mutate(Changed_Credit_Limit = ifelse(is.na(Changed_Credit_Limit),0,Changed_Credit_Limit))

# print(data %>% select(Changed_Credit_Limit) %>% distinct() %>% arrange(Changed_Credit_Limit),n=4375)


# [Num_Credit_Inquiries] replace values over q6 with mode over customer_ID

data$Num_Credit_Inquiries <- sapply(data$Num_Credit_Inquiries, parse_decimal_or_na)

q6 <- quantile(data$Num_Credit_Inquiries, 0.6, na.rm = TRUE)

data <- data %>%
group_by(Customer_ID) %>%
  mutate(Num_Credit_Inquiries = ifelse(is.na(Num_Credit_Inquiries) | Num_Credit_Inquiries > q6 | Num_Credit_Inquiries < 0,
  Mode(Num_Credit_Inquiries),
  Num_Credit_Inquiries))  %>%
  mutate(Num_Credit_Inquiries = ifelse(is.na(Num_Credit_Inquiries),0,Num_Credit_Inquiries)) %>%
ungroup()


# [Credit_Mix]
print(data %>% select(Credit_Mix) %>% distinct())
# available values:
#  Credit_Mix
#  <chr>
#1 _
#2 Good
#3 Standard
#4 Bad
# try to eliminate missing values with Mode over a Customer_ID group, if they still remain -> "Bad" would serve as a fallback value

available_labels_credit_mix = c("Good", "Standard", "Bad")

data <- data %>%
  group_by(Customer_ID) %>%
  mutate(Credit_Mix = ifelse(Credit_Mix %in% available_labels_credit_mix, Credit_Mix, Mode(Credit_Mix))) %>%
  mutate(Credit_Mix = ifelse(Credit_Mix %in% available_labels_credit_mix, Credit_Mix, "Bad"))


# [Outstanding_Debt] there is no obvious outliers, so just parse it as decimal
data$Outstanding_Debt <- sapply(data$Outstanding_Debt, parse_decimal_or_na)


# Occupation
data <- data %>%
group_by(Customer_ID) %>%
  mutate(Occupation = ifelse(is.na(Occupation) | Occupation == "_______",
  Mode(Occupation),
  Occupation))  %>%
ungroup()



data <- data %>%
mutate(Credit_History_Age = tolower(Credit_History_Age)) %>%
mutate(Credit_History_Age = parse_credit_history_age_column_to_amounth_of_months(Credit_History_Age))


# TODO remove before committing
#data <- read.csv("resources/output_after_preprocessing.csv", sep = ',')

# Filter out NA credit history age.
data <- filter(data,!is.na(data$Credit_History_Age))


# amount invested monthly

data <- data %>% mutate(Amount_invested_monthly = ifelse(Amount_invested_monthly == "","__10000__",Amount_invested_monthly))

data <- data %>%
  mutate(Amount_invested_monthly = ifelse(is_convertible_to_numeric(Amount_invested_monthly),Amount_invested_monthly,0)) %>%
  mutate(Amount_invested_monthly = as.numeric(Amount_invested_monthly)) %>%
  group_by(Customer_ID) %>%
  mutate(Amount_invested_monthly = ifelse(Amount_invested_monthly == 0,mean(Amount_invested_monthly),Amount_invested_monthly)) %>%
  ungroup()

data <- data %>% mutate(Monthly_Balance = ifelse(Monthly_Balance == "",0,Monthly_Balance)) %>%
  mutate(Monthly_Balance = as.numeric(Monthly_Balance)) %>%
  group_by(Customer_ID) %>%
  mutate(Monthly_Balance = ifelse(Monthly_Balance == 0,mean(Monthly_Balance),Monthly_Balance)) %>%
  ungroup()

# max
max(data$Monthly_Balance)
# Now, to remove outliers in more general way, I want to execute a function that would
data$Monthly_Balance <- replace_outliers_with_mean_numeric(data$Monthly_Balance)
data$Annual_Income <- replace_outliers_with_mean_numeric(data$Annual_Income)
data$Monthly_Inhand_Salary <- replace_outliers_with_mean_numeric(data$Monthly_Inhand_Salary)
data$Outstanding_Debt <- replace_outliers_with_mean_numeric(data$Outstanding_Debt)

ggplot(data, aes(x = data$Monthly_Balance)) +
geom_boxplot(fill = "lightblue", color = "black") +
labs(title="Boxplot", y = "Values")

write.csv(data,
file = "resources/output_after_preprocessing.csv",
row.names = FALSE)

# cleaning finished, start complex analysis

source("utils.r")
initialize_environment()
data <- read.csv("resources/output_after_preprocessing.csv", sep = ',')
glimpse(data)

# I want to compare credit score label and salary, portray as boxplots
credit_score_salary_graph <- ggplot(data, aes(x = Credit_Score, y = Monthly_Inhand_Salary, fill = Credit_Score)) +
geom_boxplot() +
labs(title = "Boxplot") +
scale_fill_manual(values = c("red","blue","green"))

ggsave(filename="../creditscoringstudying/media/Boxplot_Score_And_Monthly_salary.png", credit_score_salary_graph)

# I want to compare credit score label and salary, portray as boxplots
credit_score_monthly_balance_graph <- ggplot(data, aes(x = Credit_Score, y = Monthly_Balance, fill = Credit_Score)) +
geom_boxplot() +
labs(title = "Boxplot") +
scale_fill_manual(values = c("red", "blue", "green"))

ggsave(filename="../creditscoringstudying/media/Boxplot_Score_And_Monthly_Balance.png", credit_score_monthly_balance_graph)

score_num_bank_accounts <- ggplot(data, aes(x = Credit_Score, y = Num_Bank_Accounts, fill = Credit_Score)) +
geom_boxplot() +
labs("Boxplot") +
scale_fill_manual(values = c("green","maroon","gold"))

ggsave(filename="../creditscoringstudying/media/Boxplot_Score_Num_Bank_Accounts.png", score_num_bank_accounts)


score_num_credit_cards <- ggplot(data, aes(x = Credit_Score, y = Num_Credit_Card, fill = Credit_Score)) +
geom_boxplot() +
labs("Boxplot") +
scale_fill_manual(values = c("green","maroon","gold"))

ggsave(filename="../creditscoringstudying/media/Boxplot_Score_Num_Credit_Cards.png", score_num_credit_cards)

score_interest_rate <- ggplot(data, aes(x = Credit_Score, y = Interest_Rate, fill = Credit_Score)) +
geom_boxplot() +
labs("Boxplot") +
scale_fill_manual(values = c("green","maroon","gold"))

ggsave(filename="../creditscoringstudying/media/Boxplot_Score_Interest_Rate.png", score_interest_rate)


score_num_of_loans <- ggplot(data, aes(x = Credit_Score, y = Num_of_Loan, fill = Credit_Score)) +
geom_boxplot() +
labs("Boxplot") +
scale_fill_manual(values = c("green","maroon","gold"))

ggsave(filename="../creditscoringstudying/media/Boxplot_Score_Num_Loans.png", score_num_of_loans)

score_due_date <- ggplot(data, aes(x = Credit_Score, y = Delay_from_due_date, fill = Credit_Score)) +
geom_boxplot() +
labs("Boxplot") +
scale_fill_manual(values = c("green","maroon","gold"))

ggsave(filename="../creditscoringstudying/media/Boxplot_Score_Max_DPD.png", score_due_date)


score_annual_income <- ggplot(data, aes(x = Credit_Score, y = Annual_Income, fill = Credit_Score)) +
geom_boxplot() +
labs("Boxplot") +
scale_fill_manual(values = c("green","maroon","gold"))

ggsave(filename="../creditscoringstudying/media/Boxplot_Score_Annual_Income.png",score_annual_income)


score_number_of_delayed_payments <- ggplot(data, aes(x = Credit_Score, y = Num_of_Delayed_Payment, fill = Credit_Score)) +
geom_boxplot() +
labs("Boxplot") +
scale_fill_manual(values = c("green","maroon","gold"))

ggsave(filename="../creditscoringstudying/media/Boxplot_Score_Number_Of_delayed_payments.png",score_number_of_delayed_payments)

score_number_outstanding_debt <- ggplot(data, aes(x = Credit_Score, y = Outstanding_Debt, fill = Credit_Score)) +
geom_boxplot() +
labs("Boxplot") +
scale_fill_manual(values = c("green","maroon","gold"))

ggsave(filename="../creditscoringstudying/media/Boxplot_Score_Number_Outstanding_Debt.png",score_number_outstanding_debt)


score_number_credit_utilization_ratio <- ggplot(data, aes(x = Credit_Score, y = Credit_Utilization_Ratio, fill = Credit_Score)) +
geom_boxplot() +
labs("Boxplot") +
scale_fill_manual(values = c("green","maroon","gold"))

ggsave(filename="../creditscoringstudying/media/Boxplot_Score_Credit_Utilization_Ratio.png",score_number_credit_utilization_ratio)

score_credit_history_age_months <- ggplot(data, aes(x = Credit_Score, y = Credit_History_Age, fill = Credit_Score)) +
geom_boxplot() +
labs("Boxplot") +
scale_fill_manual(values = c("green","maroon","gold"))

ggsave(filename="../creditscoringstudying/media/Boxplot_Score_Credit_History_Age.png",score_credit_history_age_months)

score_credit_emi_payment_load <- ggplot(data, aes(x = Credit_Score, y = Total_EMI_per_month, fill = Credit_Score)) +
geom_boxplot() +
labs("Boxplot") +
scale_fill_manual(values = c("green","maroon","gold"))

ggsave(filename="../creditscoringstudying/media/Boxplot_Score_EMI_payment_load.png",score_credit_emi_payment_load)

score_credit_amount_invested_monthly <- ggplot(data, aes(x = Credit_Score, y = Amount_invested_monthly, fill = Credit_Score)) +
geom_boxplot() +
labs("Boxplot") +
scale_fill_manual(values = c("green","maroon","gold"))

ggsave(filename="../creditscoringstudying/media/Boxplot_Score_Monthly_Investitions.png",score_credit_amount_invested_monthly)


payment_of_min_amount_graph <- ggplot(data, aes(x = Payment_of_Min_Amount)) +
geom_bar() +
labs(title = "Count plot by payment of min amount", x = "Payment Of Min Amount", y = "Count")

ggsave("../creditscoringstudying/media/BarPlot_Payment_of_min_amount.png",payment_of_min_amount_graph)


source("utils.r")
initialize_environment()
data <- read.csv("resources/output_after_preprocessing.csv", sep = ',')
glimpse(data)

print(
  unique(data %>% group_by(Payment_Behaviour) %>% summarise(count_of_label = n()))
  )

# begin EDA over this variable

contigency_table_payment_behavior_credit_score <- table(data$Credit_Score, data$Payment_Behaviour)
print(contigency_table_payment_behavior_credit_score)

credit_score_distribution_payment_behaviour_graph <- ggplot(data, aes(x = Payment_Behaviour, fill = Credit_Score)) +
  geom_bar(position = "stack") +
  labs(title = "Credit Score Distribution by Payment Behaviour", fill = "Credit Score")

ggsave("../creditscoringstudying/media/MosaicPlot_Payment_Behaviour_Credit_Score.png",credit_score_distribution_payment_behaviour_graph)


# I want to analyse the correlation between this categorical column and final result
# I want to perform a Chi Square testing

chi_square_result_pb_cs <- chisq.test(contigency_table_payment_behavior_credit_score)
print(chi_square_result_pb_cs)

# The chi-squared statistic (1238.1 in this case) measures the extent
# of the difference between the observed and expected frequencies
# in the contingency table. A larger chi-squared value suggests
# a greater difference.


# Degrees of Freedom (df):
# The degrees of freedom (12 in this case) depend on the number
# of categories in both variables. For a contingency table,
# df = (number of rows - 1) * (number of columns - 1).
# In this case, if we have three levels for "Credit_Score"
# and seven levels for "Payment_Behaviour," we would have
# (3 - 1) * (7 - 1) = 12 degrees of freedom.

# The p-value is extremely small (< 2.2e-16), indicating strong
# evidence against the null hypothesis. In the context of the chi
# squared test for independence, the null hypothesis is that the
# two categorical variables (in this case, "Credit_Score" and "Payment_Behaviour")
# are independent. The small p-value suggests that there is a significant association
# between these two variables.

conditional_probability_pb_cs <- prop.table(contigency_table_payment_behavior_credit_score, margin = 2)
print(conditional_probability_pb_cs)