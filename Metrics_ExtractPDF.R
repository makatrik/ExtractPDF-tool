##################### Metrics calculation and their replication####
###These metrics show the robustness of the tool to extract information and its reliability

# Predicted 1, Actual 1: Correctly extracted information (true positive).
# Predicted 1, Actual 0: Incorrectly extracted information (false positive).
# Predicted 0, Actual 1: Information that should have been extracted but wasn’t (false negative).
# Predicted 0, Actual 0: Correctly not extracted information (true negative).

# Install and load the caret package 
if (!require(caret)) {
  install.packages("caret")
  library(caret)
}
###################
# Create predicted and actual class labels for the 1st 10 papers
##############################
predicted_1st <- c(1, 1, 1, 0, 1, 1, 1, 1,
                   0, 1, 1, 0, 1, 1, 1, 1,
                   1, 1, 1, 0, 1, 1, 1, 1,
                   1, 0, 1, 0, 1, 0, 0, 0,
                   1, 1, 1, 0, 1, 1, 1, 0,
                   0, 1, 0, 0, 0, 1, 1, 1,
                   1, 1, 1, 1, 1, 1, 1, 1,
                   1, 1, 1, 1, 1, 0, 1, 1,
                   1, 1, 0, 0, 0, 1, 1, 1,
                   1, 1, 1, 1, 1, 1, 1, 1)

print(predicted)
print(actual)

actual_1st <- c(1, 1, 1, 0, 1, 1, 1, 1,
                1, 1, 1, 0, 1, 1, 1, 1,
                1, 1, 1, 0, 1, 1, 1, 0,
                1, 0, 1, 0, 1, 0, 1, 1,
                1, 1, 1, 0, 1, 1, 1, 1,
                1, 1, 1, 0, 1, 1, 1, 1,
                1, 1, 1, 0, 1, 1, 1, 1,
                1, 1, 1, 0, 1, 1, 1, 0,
                1, 0, 1, 0, 1, 0, 1, 1,
                1, 1, 1, 0, 1, 1, 1, 1)

# Assuming thershold to be 0.5 
table(ACTUAL=actual_1st, PREDICTED=predicted_1st)

# Calculate precision
true_positives = 54
false_positives = 7
precision_1st <- true_positives / (true_positives + false_positives)

# Calculate recall (since actual class is always correct, recall is the same as precision)
#total_actual = true Positive and False Negative.
false_negatives = 10
total_actual = true_positives + false_negatives
recall_1st <- true_positives / total_actual

# Calculate accuracy
accuracy <- (TP+TN)/TotalSample
accuracy_1st <- 63 / 80

# Calculate F1-score
f1_score_1st <- 2 * (precision_1st * recall_1st) / (precision_1st + recall_1st)

# Calculate metrics
accuracy <- (TP + TN) / sum(confusion_matrix)
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
f1_score <- 2 * (precision * recall) / (precision + recall)


# Create predicted and actual class labels for the 2nd 10 papers
predicted_2nd <- c(1, 1, 1, 1, 0, 0, 1, 1,
                   1, 1, 1, 0, 0, 1, 1, 1, 
                   1, 1, 1, 0, 1, 0, 1, 1,
                   1, 1, 1, 1, 0, 0, 1, 1,
                   1, 1, 1, 0, 1, 1, 1, 1,
                   1, 1, 1, 1, 1, 0, 1, 1,
                   1, 1, 1, 0, 1, 0, 0, 1,
                   1, 1, 1, 0, 1, 1, 1, 1,
                   1, 1, 1, 0, 1, 0, 1, 0,
                   1, 1, 1, 1, 1, 1, 1, 1)

actual_2nd <- c(1, 1, 1, 1, 0, 0, 1, 1,
                1, 1, 1, 0, 0, 1, 1, 1,
                1, 1, 1, 0, 1, 0, 1, 1,
                1, 1, 1, 0, 1, 0, 1, 0,
                1, 1, 1, 0, 1, 1, 1, 1,
                1, 0, 1, 1, 1, 0, 1, 0,
                1, 1, 1, 0, 1, 0, 1, 1,
                1, 1, 1, 0, 1, 1, 1, 1,
                1, 1, 1, 0, 1, 0, 1, 0,
                1, 1, 1, 1, 1, 1, 1, 1) 

# Assuming thershold to be 0.5 
table(ACTUAL=actual_2nd, PREDICTED=predicted_2nd)

# Calculate precision
true_positives = 59
false_positives = 4
precision_2nd <- true_positives / (true_positives + false_positives)

# Calculate recall 
false_negatives = 2
total_actual = true_positives + false_negatives
recall_2nd <- 59 / 61

# Calculate accuracy
accuracy <- (TP+TN)/TotalSample
accuracy_2nd<- (59+15)/ 80

# Calculate F1-score
f1_score_2nd <- 2 * (precision_2nd * recall_2nd) / (precision_2nd + recall_2nd)


####
# Create predicted and actual class labels for the 3rd 10 papers
predicted_3rd <- c(1, 1, 1, 0, 1, 0, 1, 1,
                   1, 1, 1, 0, 0, 0, 1, 1, 
                   1, 1, 1, 1, 0, 0, 1, 1,
                   1, 1, 1, 1, 1, 0, 1, 1,
                   0, 1, 1, 0, 1, 0, 1, 1,
                   1, 1, 1, 0, 1, 0, 1, 1,
                   1, 1, 1, 0, 1, 1, 1, 1,
                   1, 1, 1, 0, 1, 1, 1, 1,
                   1, 0, 1, 1, 1, 1, 1, 1,
                   1, 1, 1, 0, 0, 0, 1, 1)

actual_3rd <- c(1, 1, 0, 0, 1, 0, 0, 1,
                1, 1, 0, 0, 0, 0, 1, 1,
                1, 1, 1, 0, 0, 0, 1, 1,
                1, 1, 1, 1, 1, 0, 1, 1,
                1, 1, 0, 0, 1, 0, 1, 1,
                1, 1, 1, 0, 1, 0, 1, 0,
                1, 0, 1, 0, 1, 1, 1, 1,
                1, 1, 1, 0, 1, 1, 1, 1,
                1, 0, 1, 1, 1, 1, 1, 1,
                1, 1, 1, 0, 1, 0, 1, 1)


# Confusion matrix
table(ACTUAL=actual_3rd, PREDICTED=predicted_3rd)
# Calculate precision
true_positives_3rd = 54
false_positives_3rd = 7
precision_3rd <- true_positives_3rd / (true_positives_3rd + false_positives_3rd)


# Calculate recall 
false_negatives_3rd = 2
total_actual_3rd = true_positives_3rd + false_negatives_3rd
recall_3rd <- true_positives_3rd / total_actual_3rd

# Calculate accuracy
accuracy <- (TP+TN)/TotalSample
accuracy_3rd<- (54+17)/ 80

# Calculate F1-score
f1_score_3rd <- 2 * (precision_3rd * recall_3rd) / (precision_3rd + recall_3rd)


# Create a confusion matrix
conf_matrix <- confusionMatrix(as.factor(predicted), as.factor(actual))

# Extract the metrics
precision <- conf_matrix$byClass["Pos Pred Value"]
recall <- conf_matrix$byClass["Sensitivity"]
accuracy <- conf_matrix$overall["Accuracy"]
f1_score <- 2 * (precision * recall) / (precision + recall)

# Print the metrics
print(paste("Precision:", precision))
print(paste("Recall:", recall))
print(paste("Accuracy:", accuracy))
print(paste("F1-score:", f1_score))


######################################
library(caret)
library(Metrics)
###########################

###Multiple bar plots##########
Metrics <- c("precision", "recall", "accuracy", "F1-score")
Precision <- c(1, 1, 1, 1)
Recall <- c(2,2,2,2)
Accuracy<- c(3,3,3,3)
F1score <- c(5, 5, 5,3)

# Load the ggplot2 package
library(ggplot2)

# Sample data frame
data <- data.frame(
  replication = rep(c("replication1", "replication2", "replication3"), each = 4),
  Metrics = rep(c("precision", "recall", "accuracy", "F1-score"), times = 3),
  Value = c(0.88, 0.84, 0.78, 0.86, 0.93, 0.96, 0.92, 0.95, 0.88, 0.96, 0.88, 0.92)
)
View(data)

# Create the bar plots

plot1 <- ggplot(data, aes(x = replication, y = Value, fill = Metrics)) +
  geom_bar(stat = "identity", position = "dodge") +
  # facet_wrap(~ Metric, scales = "free_y") +
  labs(title = "Performance Metrics",
       x = "Replications of performance")+
  #y = "Value") +
  theme_minimal()+
  theme(text = element_text(size = 15),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 14),
        axis.title.y = element_text(size = 20),
        plot.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 20)) 
plot1
