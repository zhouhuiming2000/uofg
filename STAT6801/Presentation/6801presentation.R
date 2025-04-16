install.packages("randomForest")
install.packages("ggplot2")
install.packages("rpart")
install.packages("rpart.plot")

library(rpart)
library(rpart.plot)

library(randomForest)
library(ggplot2)

data = read.csv('/Users/vanris/Documents/UG-STA6801/presentation/OnlineNewsPopularity.csv')
head(data,10)
logshares = log(data$shares)
data$shares = logshares
data = data[ , !(names(data) %in% c("url"))]

ggplot(data, aes(x = shares, y = kw_avg_avg)) +
  geom_point(alpha = 0.5, color = "blue") +
  labs(title = "Scatter Plot of shares vs. kw_avg_avg",
       x = "shares",
       y = "kw_avg_avg") +
  theme_minimal()


set.seed(666)
n = nrow(data)
train_indices = sample(seq_len(n), size = 0.5 * n)
train_data = data[train_indices, ]
test_data = data[-train_indices, ]


#start by figure out the hyperparameter numvars and iterations
# start by trying iteration from 10 to 100 increased by 5

ntree_values <- seq(10, 100, by = 5)
oob_errors = numeric(length = length(ntree_values))
validation_errors = numeric(length = length(ntree_values))


for (i in seq_along(ntree_values)){
  ntree_value = ntree_values[i]
  
  rf_model <- randomForest(shares ~ ., data = train_data, ntree = ntree_value, mtry = 1, importance = TRUE)
  
  # Get the OOB error estimate (Mean Squared Error for regression)
  oob_error <- rf_model$mse[ntree_value]  # Access MSE for OOB
  
  # Make predictions on the test set
  predictions <- predict(rf_model, newdata = test_data)
  
  # Calculate MSE on the test set (validation error)
  validation_error <- mean((predictions - test_data$shares)^2)
  
  # Store the errors
  oob_errors[i] <- oob_error
  validation_errors[i] <- validation_error
  
}

validation_errors = sqrt(validation_errors)

plot(ntree_values, oob_errors, type = "b", col = "blue", pch = 19, 
     xlab = "Number of Trees (ntree)", ylab = "Error (RMSE)",
     main = "OOB and Validation Errors vs. Number of Trees",
     ylim = c(min(c(oob_errors, validation_errors)), max(c(oob_errors, validation_errors))))
lines(ntree_values, validation_errors, type = "b", col = "red", pch = 17)

# Add a legend
legend("topright", legend = c("OOB Error", "Validation Error"),
       col = c("blue", "red"), pch = c(19, 17), lty = 1)


# i will take iteration 100 as the article does and now determine the numvars(0)
numvars = seq(1, ncol(data)-1, by = 1)
oob_errors = numeric(length = length(numvars))
validation_errors = numeric(length = length(numvars))

for (i in numvars){
  mtry = numvars[i]
  
  rf_model <- randomForest(shares ~ ., data = train_data, ntree = 100, mtry = mtry, importance = TRUE)
  
  # Get the OOB error estimate (Mean Squared Error for regression)
  oob_error <- rf_model$mse[ntree_value]  # Access MSE for OOB
  
  # Make predictions on the test set
  predictions <- predict(rf_model, newdata = test_data)
  
  # Calculate MSE on the test set (validation error)
  validation_error <- mean((predictions - test_data$shares)^2)
  
  # Store the errors
  oob_errors[i] <- oob_error
  validation_errors[i] <- validation_error
  
}
# transform to rmse instead of mse

validation_errors = sqrt(validation_errors)
plot(numvars, oob_errors, type = "b", col = "blue", pch = 19, 
     xlab = "Number of Variables (numvars)", ylab = "Error (RMSE)",
     main = "OOB and Validation Errors vs. Number of Variables",
     ylim = c(min(c(oob_errors, validation_errors)), max(c(oob_errors, validation_errors))))

lines(numvars, validation_errors, type = "b", col = "red", pch = 17)

# Add a legend
legend("topright", legend = c("OOB Error", "Validation Error"),
       col = c("blue", "red"), pch = c(19, 17), lty = 1)



min_validation_error_index = which.min(validation_errors)
best_numvar = numvars[min_validation_error_index]
oob_errors[min_validation_error_index]
validation_errors[min_validation_error_index]


# The final model has numvars = 7 and iteration = 100
# oob = 0.7408 , validation rmse = 0.8545

final_rf_model = randomForest(shares ~ ., data = train_data, ntree = 100, mtry = 7, importance = TRUE)
importances = importance(final_rf_model,type = 1)
var_imp_df = data.frame(Variable = rownames(importances), Importance = importances[, 1])

threshold = 0.4 * max(var_imp_df$Importance)
important_vars_df = var_imp_df[var_imp_df$Importance > threshold, ]

ggplot(important_vars_df, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_bar(stat = 'identity', fill = 'lightblue') +
  labs(title = "Variable Importance (> 10%)", x = "Importance", y = "Variable") +
  theme_minimal()



# linear regression
lm1 = lm(shares ~ ., data = train_data)

test_data_without_shares = test_data[, !names(test_data) %in% 'shares']

predictions = predict(lm1, newdata = test_data_without_shares)

rmse = sqrt(mean((predictions - test_data$shares)^2))
rmse

# got rmse = 1.935756 which is way higher than random forest






data = read.csv('/Users/vanris/Documents/UG-STA6801/presentation/default of credit card clients.csv')
head(data)
data$MARRIAGE <- as.factor(data$MARRIAGE)
onehot <- model.matrix(~MARRIAGE - 1, data)
data_onehot <- cbind(data[, !names(data) %in% "MARRIAGE"], onehot)
data_onehot$default.payment.next.month <- as.factor(data_onehot$default.payment.next.month)

head(data_onehot)
set.seed(666)
n = nrow(data_onehot)
train_indices = sample(seq_len(n), size = 0.5 * n)
train_data = data_onehot[train_indices, ]
test_data = data_onehot[-train_indices, ]

#start by figure out the hyperparameter numvars and iterations
# start by trying iteration from 10 to 100 increased by 5

ntree_values <- seq(10, 500, by = 25)
oob_errors = numeric(length = length(ntree_values))
validation_errors = numeric(length = length(ntree_values))


for (i in seq_along(ntree_values)){
  ntree_value = ntree_values[i]
  
  rf_model <- randomForest(default.payment.next.month ~ ., data = train_data, 
                           ntree = ntree_value, mtry = 1, importance = TRUE)
  
  # Get the OOB error estimate (OOB Error for regression)
  oob_error <- rf_model$err.rate[ntree_value,"OOB"]  # Access MSE for OOB
  
  # Make predictions on the test set
  predictions <- predict(rf_model, newdata = test_data)
  
  # Calculate Accuracy on the test set (validation error)
  validation_error <- mean(predictions != test_data$default.payment.next.month)
  
  # Store the errors
  oob_errors[i] <- oob_error
  validation_errors[i] <- validation_error
  
  print(i)
  
}



plot(ntree_values, oob_errors, type = "b", col = "blue", pch = 19, 
     xlab = "Number of Trees (ntree)", ylab = "Error (Inaccuracy)",
     main = "OOB and Validation Errors vs. Number of Trees",
     ylim = c(min(c(oob_errors, validation_errors)), max(c(oob_errors, validation_errors))))
lines(ntree_values, validation_errors, type = "b", col = "red", pch = 17)

# Add a legend
legend("topright", legend = c("OOB Error", "Validation Error"),
       col = c("blue", "red"), pch = c(19, 17), lty = 1)



# i will take iteration 500 as the article does and now determine the numvars(0)
numvars = seq(1, ncol(data)-1, by = 1)
oob_errors = numeric(length = length(numvars))
validation_errors = numeric(length = length(numvars))

for (i in numvars){
  mtry = numvars[i]
  
  rf_model <- randomForest(default.payment.next.month ~ ., data = train_data, 
                           ntree = 500, mtry = mtry, importance = TRUE)
  
  # Get the OOB error estimate (OOB Error for regression)
  oob_error <- rf_model$err.rate[500,"OOB"]  # Access MSE for OOB
  
  # Make predictions on the test set
  predictions <- predict(rf_model, newdata = test_data)
  
  # Calculate Accuracy on the test set (validation error)
  validation_error <- mean(predictions != test_data$default.payment.next.month)
  
  # Store the errors
  oob_errors[i] <- oob_error
  validation_errors[i] <- validation_error
  print(mtry)
  
}


plot(numvars, oob_errors, type = "b", col = "blue", pch = 19, 
     xlab = "Number of Variables (numvars)", ylab = "Error (Inaccuracy)",
     main = "OOB and Validation Errors vs. Number of Variables",
     ylim = c(min(c(oob_errors, validation_errors)), max(c(oob_errors, validation_errors))))

lines(numvars, validation_errors, type = "b", col = "red", pch = 17)

# Add a legend
legend("topright", legend = c("OOB Error", "Validation Error"),
       col = c("blue", "red"), pch = c(19, 17), lty = 1)



min_validation_error_index = which.min(validation_errors)
best_numvar = numvars[min_validation_error_index]
oob_errors[min_validation_error_index]
validation_errors[min_validation_error_index]


# The final model has numvars = 5 and iteration = 500
# oob = 0.1842667 , validation  = 0.1805

final_rf_model = randomForest(default.payment.next.month ~ ., data = train_data, ntree = 500, mtry = 15, importance = TRUE)
importances = importance(final_rf_model,type = 1)
var_imp_df = data.frame(Variable = rownames(importances), Importance = importances[, 1])

threshold = 0.2 * max(var_imp_df$Importance)
important_vars_df = var_imp_df[var_imp_df$Importance > threshold, ]

ggplot(important_vars_df, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_bar(stat = 'identity', fill = 'lightblue') +
  labs(title = "Variable Importance (> 10%)", x = "Importance", y = "Variable") +
  theme_minimal()

data_onehot$default.payment.next.month <- factor(data_onehot$default.payment.next.month, levels = c(0, 1))
# 使用ggplot2绘制直方图
ggplot(data_onehot, aes(x = PAY_0, fill = default.payment.next.month)) +
  geom_histogram(data = subset(data_onehot, default.payment.next.month == '0'), 
                 binwidth = 1, 
                 position = "dodge", 
                 fill = "gray", 
                 color = "black",  # 黑色边线
                 alpha = 0.5) +  # 透明效果
  geom_histogram(data = subset(data_onehot, default.payment.next.month == '1'), 
                 binwidth = 1, 
                 position = "dodge", 
                 fill = "darkgray", 
                 color = NA,  # 黑色边线
                 alpha = 0.5) +  # 实心灰色柱子
  labs(title = "Histogram of PAY_0 Grouped by Default Payment",
       x = "PAY_0",
       y = "Frequency") +
  scale_fill_manual(values = c("0" = "gray", "1" = "darkgray"), 
                    name = "Default Payment",
                    labels = c("No", "Yes")) +
  guides(fill = guide_legend(title = "Default Payment Status", 
                             labels = c("black = '0'", "grey = '1'"))) +  # 手动设置图例标签
  theme_minimal()+
  theme(legend.position.inside = c(1, 1),  # 设置图例位置为右上角
        legend.justification = c(1, 1))  # 设置图例对齐位置









tree_model <- rpart(default.payment.next.month ~ ., data = train_data, method = "class")

predictions <- predict(tree_model, newdata = test_data, type = "class")

accuracy <- mean(predictions == test_data$default.payment.next.month)
print(paste("Inaccuracy:", 1 - accuracy))
# inaccuracy: 0.1811
