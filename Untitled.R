# Load necessary libraries
library(dplyr) # for data manipulation

# Set seed for reproducibility
set.seed(100)

# Create a synthetic dataset
data <- tibble(
  Age = sample(18:70, 1000, replace = TRUE), # Age of the customer
  Income = sample(15000:100000, 1000, replace = TRUE), # Annual income in USD
  Education = sample(c('Highschool', 'Bachelor', 'Master'), 1000, replace = TRUE), # Highest level of education
  Occupation = sample(c('Employed', 'Unemployed'), 1000, replace = TRUE), # Current employment status
  Subscribed = sample(c(0, 1), 1000, replace = TRUE) # Subscription status (1 = subscribed, 0 = not subscribed)
)

# Introduce some NA values randomly
for(col in 1:4) {
  data[sample(1:nrow(data), size = 50), col] <- NA
}

# Write the raw dataset to a CSV file
write.csv(data, file = "customer_data.csv", row.names = FALSE)

# Print the first few rows of the dataset
head(data)
summary(data)
df <- read.csv("customer_data.csv")
head(df)
View(df)
summary(df)
age_m <- median(df$Age, na.rm = TRUE)
income_m <- median(df$Income, na.rm = TRUE)

get_mode <- function(v){
  unq <- unique(v)
  unq[which.max(tabulate(match(v,unq)))]
}

education_mode <- get_mode(df$Education[!is.na(df$Education)])
occupation_mode <- get_mode(df$Occupation[!is.na(df$Occupation)])

df$Age[is.na(df$Age)] <- age_m
df$Income[is.na(df$Income)] <- income_m
df$Education[is.na(df$Education)] <- education_mode
df$Occupation[is.na(df$Occupation)] <- occupation_mode

head(df)
summary(df)

library(randomForest)
library(caret)
library(ggplot2)

set.seed(100)
df$Education <- as.factor(df$Education)
df$Occupation <- as.factor(df$Occupation)

training_indices <- createDataPartition(df$Subscribed, p = 0.75, list = FALSE)
training_data = df[training_indices, ]
testing_data = df[-training_indices, ]
rf_model = randomForest(Subscribed ~ ., data = training_data, ntree = 100)
plot(rf_model, main = "Error")
importance(rf_model)
varImpPlot(rf_model)
predictions <- predict(rf_model, testing_data)
cm <- table(predictions, testing_data$Subscribed)
cm
predictions
summary(rf_model)
