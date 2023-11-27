source("utils.r")
initialize_environment()

train_data <- read.csv("resources/output_after_preprocessing.csv")
test_data <- read.csv("resources/test_preprocessed.csv")

# Drop identifier columns
train_data <- select(train_data, -Customer_ID, -ID)
test_data <- select(test_data, -Customer_ID, -ID)


model <- multinom(Credit_Score ~ .,data = train_data, family="multinomial")

predictions <- predict(model , newdata = test_data, type="class")

test_data$Predicted_Credit_Score <- predictions

# Save the combined data frame with predictions to a new CSV file
write.csv(test_data, "resources/predicted_results_test.csv", row.names = FALSE)



test_data_labeled <- read.csv("resources/predicted_results_test.csv")

ggplot(test_data_labeled, aes(x = Predicted_Credit_Score)) +
geom_bar() + 
labs(title = "Bar Plot Of Credit Scores", x = "Credit Score", y = "Count")