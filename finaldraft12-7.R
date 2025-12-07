
options(repos = c(CRAN = "https://cran.rstudio.com/"))

install.packages("ggfortify")
install.packages("mvnormtest")
install.packages("datarium")
install.packages("ggplot2")
install.packages("caret")
install.packages("mvtnorm")
install.packages("pROC")
install.packages("tinytex")
install.packages("scales")
install.packages("janitor")
install.packages("ada")
install.packages("ggplotify")
install.packages("ggrepel")
install.packages("randomForest")
install.packages("rpart.plot") 
install.packages("glmnet")
install.packages("gbm")
install.packages("kernlab") 
install.packages("KDA")
install.packages("xgboost")

library(MASS) 
library(datarium)
library(ggplot2)
library(broom) 
library(ggfortify)
library(tidyverse)
library(mvnormtest)
library(data.table)
library(gridExtra)
library(dplyr)
library(tinytex)
library(tidyr)
library(scales)

## data cleaning 

data1 <- read.csv("C:/Users/harip/Downloads/predict+students+dropout+and+academic+success/data.csv", sep=";")

colnames(data1)
names(data1) <- c("Marital status",
                 "Application mode", 
                 "Application order",
                 "Course",
                 "Daytime/evening attendance",
                 "Previous qualification",
                 "Previous qualification grade",
                 "Nationality",
                 "Mother's qualification",
                 "Father's qualification",
                 "Mother's occupation",
                 "Father's occupation",
                 "Admission grade",
                 "Displaced",
                 "Educational special needs",
                 "Debtor",
                 "Tuition fees up to date",
                 "Gender",
                 "Scholarship holder",
                 "Age at enrollment",
                 "International",
                 "Semester 1 credited units",
                 "Semester 1 enrolled units",
                 "Semester 1 evaluations",
                 "Semester 1 approved units",
                 "Semester 1 grade",
                 "Semester 1 units without evaluations",
                 "Semester 2 credited units",
                 "Semester 2 enrolled units",
                 "Semester 2 evaluations",
                 "Semester 2 approved units",
                 "Semester 2 grade",
                 "Semester 2 units without evaluations",
                 "Unemployment rate",
                 "Inflation rate",
                 "GDP",
                 "Target")

data1 <- select(data1, "Marital status",
                  "Application mode", 
                  "Application order",
                  "Daytime/evening attendance",
                  "Previous qualification",
                  "Previous qualification grade",
                  "Mother's qualification",
                  "Father's qualification",
                  "Admission grade",
                  "Displaced",
                  "Educational special needs",
                  "Debtor",
                  "Gender",
                  "Scholarship holder",
                  "Age at enrollment",
                  "International",
                  "Semester 1 enrolled units",
                  "Semester 1 approved units",
                  "Semester 1 grade",
                  "Semester 2 enrolled units",
                  "Semester 2 approved units",
                  "Semester 2 grade",
                  "Unemployment rate",
                  "Inflation rate",
                  "GDP",
                  "Target")

data1 <- data1 |>
  mutate(
    `Unemployment rate` = as.numeric(`Unemployment rate`)/100,
    `Inflation rate` = as.numeric(`Inflation rate`)/100,
    `GDP` = as.numeric(`GDP`)/100
         ) 

#assumption: students do not vary over time, no intrinsic distribution change 


data1$"Marital status" <- ifelse(data1$"Marital status" == 1, "single", 
                          ifelse(data1$"Marital status" == 2, "married",
                          ifelse(data1$"Marital status" == 3, "widower",
                          ifelse(data1$"Marital status" == 4, "divorced",
                          ifelse(data1$"Marital status" == 5, "facto union",
                          ifelse(data1$"Marital status" == 6, "legally separated", NA)))))) 

data1$"Application mode" <- ifelse(data1$"Application mode" == 1, "1st phase - general contingent", 
                            ifelse(data1$"Application mode" == 2, "Ordinance No. 612/93",
                            ifelse(data1$"Application mode" == 5, "1st phase - special contingent (Azores Island)",
                            ifelse(data1$"Application mode" == 7, "Holders of other higher courses",
                            ifelse(data1$"Application mode" == 10, "Ordinance No. 854-B/99",
                            ifelse(data1$"Application mode" == 15, "International student (bachelor)", 
                            ifelse(data1$"Application mode" == 16, "1st phase - special contingent (Madeira Island)",
                            ifelse(data1$"Application mode" == 17, "2nd phase - general contingent",
                            ifelse(data1$"Application mode" == 18, "3rd phase - general contingent",
                            ifelse(data1$"Application mode" == 26, "Ordinance No. 533-A/99, item b2) (Different Plan)",
                            ifelse(data1$"Application mode" == 27, "Ordinance No. 533-A/99, item b3 (Other Institution)",
                            ifelse(data1$"Application mode" == 39, "Over 23 years old",
                            ifelse(data1$"Application mode" == 42, "Transfer",
                            ifelse(data1$"Application mode" == 43, "Change of course",
                            ifelse(data1$"Application mode" == 44, "Technological specialization diploma holders",
                            ifelse(data1$"Application mode" == 51, "Change of institution/course",
                            ifelse(data1$"Application mode" == 53, "Short cycle diploma holders",
                            ifelse(data1$"Application mode" == 57, "Change of institution/course (International)", NA)))))))))))))))))) 

data1$"Application order" <- ifelse(data1$"Application order" == 0, "1st choice",
                             ifelse(data1$"Application order" == 1, "2nd choice",
                             ifelse(data1$"Application order" == 2, "3rd choice",
                             ifelse(data1$"Application order" == 3, "4th choice",
                             ifelse(data1$"Application order" == 4, "5th choice",
                             ifelse(data1$"Application order" == 5, "6th choice",
                             ifelse(data1$"Application order" == 6, "7th choice",
                             ifelse(data1$"Application order" == 7, "8th choice",
                             ifelse(data1$"Application order" == 8, "9th choice choice",
                             ifelse(data1$"Application order" == 9, "Last choice", NA)))))))))) 

data1$"Daytime/evening attendance" <- ifelse(data1$"Daytime/evening attendance" == 1, "daytime", "evening") 

data1$"Previous qualification" <- ifelse(data1$"Previous qualification" == 1, "Secondary education",
                                  ifelse(data1$"Previous qualification" == 2, "Higher education - bachelors degree",
                                  ifelse(data1$"Previous qualification" == 3, "Higher education - degree",
                                  ifelse(data1$"Previous qualification" == 4, "Higher education - masters",
                                  ifelse(data1$"Previous qualification" == 5, "Higher education - doctorate",
                                  ifelse(data1$"Previous qualification" == 6, "Frequency of higher education",
                                  ifelse(data1$"Previous qualification" == 9, "12th year of schooling - not completed",
                                  ifelse(data1$"Previous qualification" == 10, "11th year of schooling - not completed",
                                  ifelse(data1$"Previous qualification" == 12, "Other - 11th year of schooling",
                                  ifelse(data1$"Previous qualification" == 14, "10th year of schooling",
                                  ifelse(data1$"Previous qualification" == 15, "10th year of schooling - not completed",
                                  ifelse(data1$"Previous qualification" == 19, "Basic education 3rd cycle (9th/10th/11th year) or equiv.",
                                  ifelse(data1$"Previous qualification" == 38, "Basic education 2nd cycle (6th/7th/8th year) or equiv.",
                                  ifelse(data1$"Previous qualification" == 39, "Technological specialization course",
                                  ifelse(data1$"Previous qualification" == 40, "Higher education - degree (1st cycle)",
                                  ifelse(data1$"Previous qualification" == 42, "Professional higher technical course",
                                  ifelse(data1$"Previous qualification" == 43, "Higher education - master (2nd cycle)", NA))))))))))))))))) 

data1$"Mother's qualification" <- ifelse(data1$"Mother's qualification" == 1, "Secondary Education - 12th Year of Schooling or Eq.",
                                  ifelse(data1$"Mother's qualification" == 2, "Higher Education - Bachelor's Degree",
                                  ifelse(data1$"Mother's qualification" == 3, "Higher Education - Degree",
                                  ifelse(data1$"Mother's qualification" == 4, "Higher Education - Master's",
                                  ifelse(data1$"Mother's qualification" == 5, "Higher Education - Doctorate",
                                  ifelse(data1$"Mother's qualification" == 6, "Frequency of Higher Education",
                                  ifelse(data1$"Mother's qualification" == 9, "12th Year of Schooling - Not Completed",
                                  ifelse(data1$"Mother's qualification" == 10, "11th Year of Schooling - Not Completed",
                                  ifelse(data1$"Mother's qualification" == 11, "7th Year (Old)",
                                  ifelse(data1$"Mother's qualification" == 12, "Other - 11th Year of Schooling",
                                  ifelse(data1$"Mother's qualification" == 14, "10th Year of Schooling",
                                  ifelse(data1$"Mother's qualification" == 18, "General commerce course",
                                  ifelse(data1$"Mother's qualification" == 19, "Basic Education 3rd Cycle (9th/10th/11th Year) or Equiv.",
                                  ifelse(data1$"Mother's qualification" == 22, "Technical-professional course",
                                  ifelse(data1$"Mother's qualification" == 26, "7th year of schooling",
                                  ifelse(data1$"Mother's qualification" == 27, "2nd cycle of the general high school course",
                                  ifelse(data1$"Mother's qualification" == 29, "9th Year of Schooling - Not Completed",
                                  ifelse(data1$"Mother's qualification" == 30, "8th year of schooling",
                                  ifelse(data1$"Mother's qualification" == 34, "Unknown",
                                  ifelse(data1$"Mother's qualification" == 35, "Can't read or write",
                                  ifelse(data1$"Mother's qualification" == 36, "Can read without having a 4th year of schooling",
                                  ifelse(data1$"Mother's qualification" == 37, "Basic education 1st cycle (4th/5th year) or equiv.",
                                  ifelse(data1$"Mother's qualification" == 38, "Basic Education 2nd Cycle (6th/7th/8th Year) or Equiv.",
                                  ifelse(data1$"Mother's qualification" == 39, "Technological specialization course",
                                  ifelse(data1$"Mother's qualification" == 40, "Higher education - degree (1st cycle)",
                                  ifelse(data1$"Mother's qualification" == 41, "Specialized higher studies course",
                                  ifelse(data1$"Mother's qualification" == 42, "Professional higher technical course",
                                  ifelse(data1$"Mother's qualification" == 43, "Higher Education - Master (2nd cycle)",
                                  ifelse(data1$"Mother's qualification" == 44, "Higher Education - Doctorate (3rd cycle)", NA))))))))))))))))))))))))))))) 

data1$"Father's qualification" <- ifelse(data1$"Father's qualification" == 1, "Secondary Education - 12th Year of Schooling or Eq.",
                                  ifelse(data1$"Father's qualification" == 2, "Higher Education - Bachelor's Degree",
                                  ifelse(data1$"Father's qualification" == 3, "Higher Education - Degree",
                                  ifelse(data1$"Father's qualification" == 4, "Higher Education - Master's",
                                  ifelse(data1$"Father's qualification" == 5, "Higher Education - Doctorate",
                                  ifelse(data1$"Father's qualification" == 6, "Frequency of Higher Education",
                                  ifelse(data1$"Father's qualification" == 9, "12th Year of Schooling - Not Completed",
                                  ifelse(data1$"Father's qualification" == 10, "11th Year of Schooling - Not Completed",
                                  ifelse(data1$"Father's qualification" == 11, "7th Year (Old)",
                                  ifelse(data1$"Father's qualification" == 12, "Other - 11th Year of Schooling",
                                  ifelse(data1$"Father's qualification" == 13, "2nd year complementary high school course", 
                                  ifelse(data1$"Father's qualification" == 14, "10th Year of Schooling",
                                  ifelse(data1$"Father's qualification" == 18, "General commerce course",
                                  ifelse(data1$"Father's qualification" == 19, "Basic Education 3rd Cycle (9th/10th/11th Year) or Equiv.",
                                  ifelse(data1$"Father's qualification" == 20, "Complementary High School Course", 
                                  ifelse(data1$"Father's qualification" == 22, "Technical-professional course",
                                  ifelse(data1$"Father's qualification" == 25, "Complementary High School Course - not concluded", 
                                  ifelse(data1$"Father's qualification" == 26, "7th year of schooling",
                                  ifelse(data1$"Father's qualification" == 27, "2nd cycle of the general high school course",
                                  ifelse(data1$"Father's qualification" == 29, "9th Year of Schooling - Not Completed",
                                  ifelse(data1$"Father's qualification" == 30, "8th year of schooling",
                                  ifelse(data1$"Father's qualification" == 31, "General Course of Administration and Commerce", 
                                  ifelse(data1$"Father's qualification" == 33, "Supplementary Accounting and Administration", 
                                  ifelse(data1$"Father's qualification" == 34, "Unknown",
                                  ifelse(data1$"Father's qualification" == 35, "Can't read or write",
                                  ifelse(data1$"Father's qualification" == 36, "Can read without having a 4th year of schooling",
                                  ifelse(data1$"Father's qualification" == 37, "Basic education 1st cycle (4th/5th year) or equiv.",
                                  ifelse(data1$"Father's qualification" == 38, "Basic Education 2nd Cycle (6th/7th/8th Year) or Equiv.",
                                  ifelse(data1$"Father's qualification" == 39, "Technological specialization course",
                                  ifelse(data1$"Father's qualification" == 40, "Higher education - degree (1st cycle)",
                                  ifelse(data1$"Father's qualification" == 41, "Specialized higher studies course",
                                  ifelse(data1$"Father's qualification" == 42, "Professional higher technical course",
                                  ifelse(data1$"Father's qualification" == 43, "Higher Education - Master (2nd cycle)",
                                  ifelse(data1$"Mother's qualification" == 44, "Higher Education - Doctorate (3rd cycle)", "NA")))))))))))))))))))))))))))))))))) 

data1$"Displaced" <- ifelse(data1$"Displaced" == 1, "yes", "no") 

data1$"Educational special needs" <- ifelse(data1$"Educational special needs" == 1, "yes", "no") 

data1$"Debtor" <- ifelse(data1$"Debtor" == 1, "yes", "no") 

data1$"Gender" <- ifelse(data1$"Gender" == 1, "Male", "Female") 

data1$"Scholarship holder" <- ifelse(data1$"Scholarship holder" == 1, "yes", "no") 

data1$"International" <- ifelse(data1$"International" == 1, "yes", "no") 

categorical_cols <- c("Marital status",
                      "Application mode", 
                      "Application order",
                      "Daytime/evening attendance",
                      "Previous qualification",
                      "Mother's qualification",
                      "Father's qualification",
                      "Displaced",
                      "Educational special needs",
                      "Debtor",
                      "Gender",
                      "Scholarship holder",
                      "International")


data1$`Marital status` <- ifelse(
  data1$`Marital status` == "single", 
  "single", 
  "not single"
)

data1$`Application order` <- ifelse(
  data1$`Application order` == "2nd choice", 
  "2nd choice", 
  "not 2nd choice"
)

data1$`Previous qualification` <- ifelse(
  data1$`Previous qualification` == "Other - 11th year of schooling" | 
    data1$`Previous qualification` == "10th year of schooling"| 
    data1$`Previous qualification` == "10th year of schooling - not completed" | 
    data1$`Previous qualification` == "11th year of schooling - not completed" |
    data1$`Previous qualification` == "12th year of schooling - not completed" |
    data1$`Previous qualification` == "Basic education 2nd cycle (6th/7th/8th year) or equiv." |
    data1$`Previous qualification` == "Basic education 3rd cycle (9th/10th/11th year) or equiv.", 
  "underqualified", 
  ifelse(
    data1$`Previous qualification` == "Secondary education",
    "qualified", 
    "overqualified"
  )
)

data1$`Application mode` <- ifelse(
  data1$`Application mode` == "3rd phase - general contingent" | 
    data1$`Application mode` == "2nd phase - general contingent" | 
    data1$`Application mode` == "1st phase - general contingent", 
  "General contingent", 
  ifelse(
    data1$`Application mode` == "Over 23 years old",
    "Over 23 years old", 
    "Other"
  )
)

data1$`Mother's qualification` <- ifelse(
  data1$`Mother's qualification` == "Unknown" | 
  data1$`Mother's qualification` == "Can read without having a 4th year of schooling" | 
  data1$`Mother's qualification` == "Can't read or write" |
  data1$`Mother's qualification` == "9th Year of Schooling - Not Completed" |
  data1$`Mother's qualification` == "10th Year of Schooling" |
  data1$`Mother's qualification` == "11th Year of Schooling - Not Completed" |
  data1$`Mother's qualification` == "12th Year of Schooling - Not Completed" |
  data1$`Mother's qualification` == "2nd cycle of the general high school course" |
  data1$`Mother's qualification` == "7th Year (Old)" |
  data1$`Mother's qualification` == "7th year of schooling" |
  data1$`Mother's qualification` == "8th year of schooling",
    "Other", 
  ifelse(
    data1$`Mother's qualification` == "Basic education 1st cycle (4th/5th year) or equiv." |
    data1$`Mother's qualification` == "Basic Education 2nd Cycle (6th/7th/8th Year) or Equiv." |
    data1$`Mother's qualification` == "Basic Education 3rd Cycle (9th/10th/11th Year) or Equiv." |
    data1$`Mother's qualification` == "Other - 11th Year of Schooling",
    "Basic education", 
    "Secondary education and above"
  )
)

data1$`Father's qualification` <- ifelse(
  data1$`Father's qualification` == "NA" | 
  data1$`Father's qualification` == "Unknown" | 
  data1$`Father's qualification` == "Can read without having a 4th year of schooling" | 
  data1$`Father's qualification` == "Can't read or write" | 
  data1$`Father's qualification` == "10th Year of Schooling" | 
  data1$`Father's qualification` == "11th Year of Schooling - Not Completed" | 
  data1$`Father's qualification` == "12th Year of Schooling - Not Completed" | 
  data1$`Father's qualification` == "2nd cycle of the general high school course" | 
  data1$`Father's qualification` == "2nd year complementary high school course" | 
  data1$`Father's qualification` == "7th Year (Old)" | 
  data1$`Father's qualification` == "7th year of schooling" | 
  data1$`Father's qualification` == "8th year of schooling" | 
  data1$`Father's qualification` == "9th Year of Schooling - Not Completed",
  "Other", 
  ifelse(
    data1$`Father's qualification` == "Basic Education 3rd Cycle (9th/10th/11th Year) or Equiv." |
    data1$`Father's qualification` == "Basic Education 2nd Cycle (6th/7th/8th Year) or Equiv." | 
    data1$`Father's qualification` == "Basic education 1st cycle (4th/5th year) or equiv." | 
    data1$`Father's qualification` == "Other - 11th Year of Schooling" | 
    data1$`Father's qualification` == "Complementary High School Course - not concluded",
    "Basic education", 
    "Secondary education and above"
  )
)

write.csv(data1,"C:/Users/harip/Downloads/data1.csv", row.names = FALSE)

ggplot(data1, aes(x = Target, fill = Target)) +
  geom_bar() +
  labs(title = "Frequency of Target",
       x = "Target",
       y = "Count")

library(ggplot2)
library(scales)
library(gridExtra)


plot_list <- list() 

for (i in categorical_cols) {
  plot <- ggplot(data1, aes(x = .data[[i]], fill = Target)) +
    geom_bar(position = "dodge") +
    labs(
      title = paste(i, "by Target"),
      x = i,
      y = "Count"
    ) +
    theme_minimal() + 
    scale_x_discrete(
      labels = label_wrap(30) 
    ) + 
    coord_flip() + 
    theme(
      axis.text = element_text(size = 5), 
      plot.title = element_text(size = 8), 
      axis.title.x = element_text(size = 8), 
      axis.title.y = element_text(size = 8), 
      legend.position = "none"
    )
  plot_list[[i]] <- plot
}

grid.arrange(grobs = plot_list, ncol = 3) 


ggplot(data1, aes(x = `Marital status`, fill = Target)) +
  geom_bar(position = "dodge", alpha = 0.7) +
  facet_wrap(~Target, scales = "free_y") +
  labs(y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

numeric_cols <- names(data1)[sapply(data1, is.numeric)]

plot_list <- list() 

for (i in numeric_cols) {
  plot <- ggplot(data1, aes(x = .data[[i]], fill = Target, color = Target)) +
    geom_density(alpha = 0.6) +
    labs(title = paste(i, "by Target"), x = i, y = "Density") +
    theme_minimal() + 
    theme(
      axis.text = element_text(size = 5), 
      plot.title = element_text(size = 8), 
      axis.title.x = element_text(size = 8), 
      axis.title.y = element_text(size = 8), 
      legend.position = "none"
    )
  plot_list[[i]] <- plot
}

grid.arrange(grobs = plot_list, ncol = 4) 


for (col_name in categorical_cols) {
  a <- data.frame(x1 = data1[[col_name]], x2 = data1[["Target"]])
  table(a) 
  b <- table(a) 
  c <- b/rowSums(b) #% 
  print(c)
}

#summary statistics 
library(dplyr)
library(tidyr)

summary_stats <- data1 %>%
  summarise(across(numeric_cols,
                   .fns = list(Mean = mean,
                               Median = median,
                               SD = sd),
                   na.rm = TRUE)) 

formatted_stats <- summary_stats %>%
  pivot_longer(everything(),
               names_to = c("Variable", ".value"),
               names_sep = "_")

print(formatted_stats)

#define helper for decision boundary visualization 
decisionplot <- function(model, data, class = NULL, predict_type = "class",
                         resolution = 200, ...) {
  require(data.table)
  require(ggplot2)
  
  if (!is.data.table(data)) data <- as.data.table(data)
  
  if (!is.null(class)) {
    cl <- data[[class]]
  } else {
    stop("You must provide the class column name using `class =`")
  }
  
  data_xy <- data[, 1:2, with = FALSE]
  k <- length(unique(cl))
  
  # Build grid
  r <- sapply(data_xy, range, na.rm = TRUE)
  grid_x1 <- seq(r[1, 1], r[2, 1], length.out = resolution)
  grid_x2 <- seq(r[1, 2], r[2, 2], length.out = resolution)
  grid <- as.data.table(expand.grid(x1 = grid_x1, x2 = grid_x2))
  
  # Predict over grid
  p <- predict(model, newdata = grid, type = predict_type)
  if (is.list(p)) p <- p$class
  grid[, yhat := as.factor(p)]
  
  # Return ggplot object
  plt <- ggplot() +
    geom_point(data = grid, aes(x1, x2, color = yhat), alpha = 0.05, shape = 15) +
    geom_point(data = data, aes(x1, x2, color = get(class)), shape = 1) +
    labs(title = "Decision Boundary", color = "Class") +
    theme_minimal()
  
  print(plt)
  invisible(plt)
}

# using model.matrix to factor the categorical data 

library(tidyverse)
library(caret)
library(nnet)
library(pROC)
library(dplyr)
library(janitor) 

data <- data1 
head(data)
data <- data |>
  rename(y = Target)
head(data)

X <- model.matrix(y ~ ., data = data)[, -1]
head(X) 
y <- factor(data$y)
data <- data.frame(y = data$y, X)

# kernel transform 

library(kernlab)

model_kfa <- kfa(X, kernel="rbfdot", kpar=list(sigma=0.1), features=29)
kfa_features <- predict(model_kfa, X)
kfa_data <- as.data.frame(kfa_features)
colnames(kfa_data) <- names(data[, -which(names(data) == "y")])
kfa_data$y <- y
head(kfa_data)

# remove constant variables 
kfa_data <- kfa_data[, -c(3,4,6,7,8,11,12,18,19,20,21,22,23,24,25,26,27,28,29)]
kfa_data <- kfa_data[, -c(2,  3,  5,  6,  7,  8,  9, 10)]

# splitting for test and train 

set.seed(100)
idx <- sample(nrow(kfa_data), floor(0.8*nrow(kfa_data)))
kfa.dat.train <- kfa_data[idx,]
kfa.dat.test <- kfa_data[-idx,]

# LDA 

lda.fit <- lda(y ~ ., data = kfa.dat.train)
pred.lda <- predict(lda.fit, newdata = kfa.dat.test)
table(Predicted = pred.lda$class, Actual = kfa.dat.test$y)
mean(pred.lda$class == kfa.dat.test$y)

# AI help for metrics 
library(caret)
library(pROC)

conf_matrix <- table(LDA_Predicted = pred.lda$class, True = kfa.dat.test$y)
print(conf_matrix)
metrics <- confusionMatrix(data = pred.lda$class, reference = as.factor(kfa.dat.test$y), mode = "everything")

overall_accuracy <- metrics$overall["Accuracy"]
cat("\nOverall Accuracy: ", overall_accuracy, "\n\n")

precision_per_class <- metrics$byClass[, "Pos Pred Value"]
cat("Precision (PPV) per class:\n")
print(precision_per_class)
cat("\n")

recall_per_class <- metrics$byClass[, "Sensitivity"]
cat("Recall (Sensitivity) per class:\n")
print(recall_per_class)
cat("\n")

predicted_probabilities <- as.data.frame(pred.lda$posterior)
multiclass_auc <- multiclass.roc(response = kfa.dat.test$y, predictor = predicted_probabilities)
overall_multiclass_auc <- multiclass_auc$auc
cat("Overall Multi-class AUC (Hand and Till): ", overall_multiclass_auc, "\n")

## PC 

train_pca <- prcomp(kfa.dat.train[, -which(names(kfa.dat.train) == "y")], scale. = TRUE)
test_pca <- predict(train_pca, newdata = kfa.dat.test[, -which(names(kfa.dat.test) == "y")])
data_pca <- data.frame(PC1 = pca$x[,1], PC2 = pca$x[,2], y = kfa_data$y, label=idx)
#error 

ggplot(test_pca, aes(x = PC1, y = PC2, color = pred.lda$class, shape = y, fill=label)) +
  geom_point(size = 2) +
  labs(title = "LDA Prediction on First Two Principal Components") +
  theme_minimal()

#AI help for the plot 
train_pca_scores <- as.data.frame(train_pca$x[, 1:2]) %>%
  mutate(y = kfa.dat.train$y, label = "Train")
test_pca_scores <- as.data.frame(test_pca[, 1:2]) %>%
  mutate(y = kfa.dat.test$y, label = "Test")
data_pca_combined <- bind_rows(train_pca_scores, test_pca_scores)
#data_pca_combined$predicted_class <- pred.lda$class 
ggplot(data_pca_combined, aes(x = PC1, y = PC2, color = y, shape = label)) +
  geom_point(size = 2, alpha = 0.7) +
  labs(
    title = "PCA Plot of Training and Testing Data",
    color = "Class",
    shape = "Dataset Status"
  ) +
  theme_minimal()
# I do not understand how to add the pred.lda$class, need help here 

## QDA 
qda.fit <- qda(y ~ ., data = kfa.dat.train)
pred.qda <- predict(qda.fit, newdata = kfa.dat.test)
table(Predicted = pred.qda$class, Actual = kfa.dat.test$y)
mean(pred.qda$class == kfa.dat.test$y)

# AI help for metrics 
library(caret)
library(pROC)

conf_matrix <- table(QDA_Predicted = pred.qda$class, True = kfa.dat.test$y)
print(conf_matrix)
metrics <- confusionMatrix(data = pred.qda$class, reference = as.factor(kfa.dat.test$y), mode = "everything")

overall_accuracy <- metrics$overall["Accuracy"]
cat("\nOverall Accuracy: ", overall_accuracy, "\n\n")

precision_per_class <- metrics$byClass[, "Pos Pred Value"]
cat("Precision (PPV) per class:\n")
print(precision_per_class)
cat("\n")

recall_per_class <- metrics$byClass[, "Sensitivity"]
cat("Recall (Sensitivity) per class:\n")
print(recall_per_class)
cat("\n")

predicted_probabilities <- as.data.frame(pred.lda$posterior)
multiclass_auc <- multiclass.roc(response = kfa.dat.test$y, predictor = predicted_probabilities)
overall_multiclass_auc <- multiclass_auc$auc
cat("Overall Multi-class AUC (Hand and Till): ", overall_multiclass_auc, "\n")


library(caret)
fit <- train(y ~ ., data = dat.train, method = "lda")
imp <- varImp(fit)
# error here 
top2_vars <- rownames(imp$importance)[order(imp$importance$Overall, decreasing = TRUE)][1:2]

# below is the previous attempt. Keeping because it showed top 2 vars 

# attempt with lab code 

svdx = svd(X)
svdx$d
svdx$v

par(mar=c(1,1,1,1))
layout(matrix(1:81,9,9))
mycols = rainbow(length(Y))
orY = order(Y)
for(i in 1:9)
{
  for(j in 1:9)
  {
    plot(svdx$u[,i],svdx$u[,j],type="p",pch=16,col=mycols[orY])
  }
}

varex = 0; cumvarex = 0;
for(i in 1:9)
{
  varex[i] = svdx$d[i]^2/sum(svdx$d^2)
  cumvarex[i] = sum(varex)
}
par(mfrow=c(1,2))
par(mar=c(5,4,4,2))
barplot(varex,ylab="Amount of Var Explained",xlab="PCs")
barplot(cumvarex,ylab="Cummulative Var Explained",xlab="PCs")

# attempt with AI 

loadings_matrix <- svdx$v
num_vars <- nrow(loadings_matrix)
pc1_loadings <- loadings_matrix[, 1]
most_important_var_pc1 <- which.max(abs(pc1_loadings))
cat("Variable most important for PC1:", names(X)[most_important_var_pc1], 
    "with absolute loading of", abs(pc1_loadings[most_important_var_pc1]), "\n")
pc2_loadings <- loadings_matrix[, 2]
most_important_var_pc2 <- which.max(abs(pc2_loadings))
cat("Variable most important for PC2:", names(X)[most_important_var_pc2], 
    "with absolute loading of", abs(pc2_loadings[most_important_var_pc2]), "\n")

ggplot(data, aes(x = previous_qualification_grade, y = admission_grade, color = Y)) +
  geom_point(size = 2, alpha = 0.7) +
  scale_shape_manual(values = c(16, 17, 18, 19, 20, 21)) +
  ggtitle("") +
  labs(color = "True Class", shape = "Predicted Class") +
  theme(legend.position = "bottom")

## LDA 

lda_model <- lda(Y ~ previous_qualification_grade + admission_grade, data = data)
lda_pred <- predict(lda_model, data)
table("LDA" = lda_pred$class, "True" = Y)

# AI help for metrics below 
conf_matrix <- table("LDA Predicted" = lda_pred$class, "True" = Y)
print(conf_matrix)
metrics <- confusionMatrix(data = lda_pred$class, reference = Y, mode = "everything")
overall_accuracy <- metrics$overall['Accuracy']
cat("\nOverall Accuracy:", overall_accuracy, "\n\n")
precision_per_class <- metrics$byClass[, 'Pos Pred Value']
cat("Precision (PPV) per class:\n")
print(precision_per_class)
cat("\n")
recall_per_class <- metrics$byClass[, 'Sensitivity']
cat("Recall (Sensitivity) per class:\n")
print(recall_per_class)
cat("\n")
predicted_probabilities <- as.data.frame(lda_pred$posterior)
multiclass_auc <- multiclass.roc(response = Y, predictor = predicted_probabilities)
overall_multiclass_auc <- multiclass_auc$auc
cat("Overall Multi-class AUC (Hand and Till):", overall_multiclass_auc, "\n")

decisionplot(lda_model, data, class=Y)
# stopped here: recursive indexing error 

## QDA 

qda_model <- qda(Y ~ previous_qualification_grade + admission_grade, data = data)
qda_pred <- predict(qda_model, data)
table("QDA" = qda_pred$class, "True" = Y)


# again, AI help for metrics below 
conf_matrix <- table("QDA Predicted" = qda_pred$class, "True" = Y)
print(conf_matrix)
metrics <- confusionMatrix(data = qda_pred$class, reference = Y, mode = "everything")
overall_accuracy <- metrics$overall['Accuracy']
cat("\nOverall Accuracy:", overall_accuracy, "\n\n")
precision_per_class <- metrics$byClass[, 'Pos Pred Value']
cat("Precision (PPV) per class:\n")
print(precision_per_class)
cat("\n")
recall_per_class <- metrics$byClass[, 'Sensitivity']
cat("Recall (Sensitivity) per class:\n")
print(recall_per_class)
cat("\n")
predicted_probabilities <- as.data.frame(qda_pred$posterior)
multiclass_auc <- multiclass.roc(response = Y, predictor = predicted_probabilities)
overall_multiclass_auc <- multiclass_auc$auc
cat("Overall Multi-class AUC (Hand and Till):", overall_multiclass_auc, "\n")


decisionplot(qda_model, pc_data, class=Y)
#same error again 


## lasso 

library(glmnet)

fit1 <- glmnet(x=X, y=y, family="multinomial", alpha=1)
plot(fit1,col=1:9, lwd=3)
legend("center",legend=names(data1) [2:10],col=1:9,ltyrep(1,9),cex=.8, lwd=3)
  plot(fit1, xvar = "lambda", lwd=3, label = TRUE)
plot(fit1, xvar = "dev", lwd=3, label = TRUE)
cvfit <- cv.glmnet(as.matrix(X), as.factor(y), family="multinomial")
print(cvfit)
plot(cvfit)
cvfit$lambda.min
coef(cvfit, s = "lambda.min")
predict(cvfit, newx = as.matrix(X[1:5,]), s = "lambda.min", type="class")
y[1:5]
cvfit2 <- cv.glmnet(as.matrix(X), as.factor(y), family="multinomial", type.measure = "default", nfolds = 5)
print(cvfit2)
cvfit2$lambda.min
plot(cvfit2)

fit.multi <- cv.glmnet(X, y, alpha = 1, family = "multinomial")
predict(fit.multi, newx = X, type = "response")

## logistic regression 

head(data1)
data_lasso <- data1[, -c(1, 4, 11, 15, 19, 22, 25)]
data_lasso <- data_lasso |>
  rename(y = Target,
         x1 = "Application mode", 
          x2 = "Application order", 
          x3 = "Previous qualification", 
          x4 = "Previous qualification grade", 
          x5 = "Mother's qualification", 
          x6 = "Father's qualification", 
          x7 = "Admission grade", 
          x8 = "Displaced", 
          x9 = "Debtor", 
          x10 = "Gender", 
          x11 = "Scholarship holder", 
          x12 = "International", 
          x13 = "Semester 1 enrolled units", 
          x14 = "Semester 1 approved units", 
          x15 = "Semester 2 enrolled units", 
          x16 = "Semester 2 approved units", 
          x17 = "Unemployment rate", 
          x18 = "Inflation rate")
head(data_lasso)

set.seed(100)
idx_lasso <- sample(nrow(data_lasso), floor(0.8*nrow(data_lasso)))
dat.train.lasso <- data_lasso[idx_lasso,]
dat.test.lasso <- data_lasso[-idx_lasso,]

library(tidyverse)
library(caret)
library(nnet)
library(pROC)
library(dplyr)

#one vs. all 
y.uni <- as.character(unique(data_lasso$y))
y.uni

#create an empty list to hold each binary classifier 
classifiers_ova <- list()

#loop to train a classifier for each class 
for (k in y.uni) {
  y_binary <- ifelse(dat.train.lasso$y == k, 1, 0)
  assign("dat.train.lasso", within(dat.train.lasso, assign(k, y_binary)))
  formula.glm <- as.formula(paste(k,"~ ."))
  fit <- glm(formula.glm, data = dat.train.lasso, family = binomial)
  classifiers_ova[[k]] <- fit
}
#error here, which appeared during the homework also 

#AI help which worked for the homework 
classifiers_ova <- lapply(y.uni, function(k) {
  y_binary <- ifelse(dat.train.lasso$y == k, 1, 0)
  temp_data <- data.frame(y_binary = y_binary, x1 = dat.train.lasso$x1, 
                          x2 = dat.train.lasso$x2, 
                          x3 = dat.train.lasso$x3, 
                          x4 = dat.train.lasso$x4, 
                          x5 = dat.train.lasso$x5, 
                          x6 = dat.train.lasso$x6, 
                          x7 = dat.train.lasso$x7, 
                          x8 = dat.train.lasso$x8, 
                          x9 = dat.train.lasso$x9, 
                          x10 = dat.train.lasso$x10, 
                          x11 = dat.train.lasso$x11, 
                          x12 = dat.train.lasso$x12, 
                          x13 = dat.train.lasso$x13, 
                          x14 = dat.train.lasso$x14, 
                          x15 = dat.train.lasso$x15, 
                          x16 = dat.train.lasso$x16, 
                          x17 = dat.train.lasso$x17, 
                          x18 = dat.train.lasso$x18)
  glm(y_binary ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18, data = temp_data, family = binomial)
})

names(classifiers_ova) <- y.uni

#prediction function for OvA 
predict_OvA <- function(newdata, classifiers) {
  scores <- sapply(classifiers, function(fit) predict(fit, newdata = data.frame(newdata), type = "response"))
  return(y.uni[apply(scores, 1, which.max)])
}

#test the prediction function 
OvA_pred <- predict_OvA(dat.test.lasso, classifiers_ova)

mean(OvA_pred == dat.test.lasso$y)

#confusion matrix 
conf.ova <- confusionMatrix(as.factor(OvA_pred), dat.test.lasso$y)
# at this point I have another error here 

# AI attempt 
library(tidyverse)
library(caret)
library(nnet)
library(pROC)
library(dplyr)

data_lasso <- data1[, -c(1, 4, 11, 15, 19, 22, 25)] 
data_lasso <- data_lasso |>
  rename(y = Target) 

y.uni <- as.character(unique(dat.train.lasso$y))

predictor_vars <- names(dat.train.lasso)[names(dat.train.lasso) != "y"]
formula_str <- paste("y_binary ~", paste(paste0("`", predictor_vars, "`"), collapse = " + "))
formula.glm <- as.formula(formula_str)

classifiers_ova <- lapply(y.uni, function(k) {
  temp_data <- dat.train.lasso |>
    mutate(y_binary = ifelse(y == k, 1, 0))
    glm(formula.glm, data = temp_data, family = binomial(link = "logit"))
})

names(classifiers_ova) <- y.uni

predict_OvA <- function(newdata, classifiers) {
  scores <- sapply(classifiers, function(fit) predict(fit, newdata = newdata, type = "response"))
  
  predictions <- y.uni[apply(scores, 1, which.max)]
  return(factor(predictions, levels = all_possible_levels))
}

OvA_pred <- predict_OvA(dat.test.lasso, classifiers_ova)

mean(OvA_pred == dat.test.lasso$y)
conf.ova <- confusionMatrix(OvA_pred, dat.test.lasso$y)

print(conf.ova)

# using KFA 
library(tidyverse)
library(caret)
library(nnet)
library(pROC)
library(dplyr)

y.uni <- as.character(unique(kfa.dat.train$y))

predictor_vars <- names(kfa.dat.train)[names(kfa.dat.train) != "y"]
formula_str <- paste("y_binary ~", paste(paste0("`", predictor_vars, "`"), collapse = " + "))
formula.glm <- as.formula(formula_str)

classifiers_ova <- lapply(y.uni, function(k) {
  temp_data <- kfa.dat.train |>
    mutate(y_binary = ifelse(y == k, 1, 0))
  glm(formula.glm, data = temp_data, family = binomial(link = "logit"))
})

names(classifiers_ova) <- y.uni

predict_OvA <- function(newdata, classifiers) {
  scores <- sapply(classifiers, function(fit) predict(fit, newdata = newdata, type = "response"))
  
  predictions <- y.uni[apply(scores, 1, which.max)]
  return(factor(predictions, levels = all_possible_levels))
}

OvA_pred <- predict_OvA(kfa.dat.test, classifiers_ova)
#errors begin here 

mean(OvA_pred == kfa.dat.test$y)
conf.ova <- confusionMatrix(OvA_pred, kfa.dat.test$y)

print(conf.ova)

# Prediction using AI 
predict_OvA_scores <- function(newdata, classifiers) {
  scores <- sapply(classifiers, function(fit) predict(fit, newdata = newdata, type = "response"))
  return(scores)
}

OvA_probabilities <- predict_OvA_scores(kfa.dat.test, classifiers_ova)

average_probabilities_per_class <- colMeans(OvA_probabilities)

print(average_probabilities_per_class)


# AUC using AI 
mean(logit_pred == dat.test.lasso$y)
conf.logit <- confusionMatrix(as.factor(logit_pred), dat.test.lasso$y)

print(conf.logit)

library(pROC) 

predict_OvA_scores <- function(newdata, classifiers) {
  scores <- sapply(classifiers, function(fit) predict(fit, newdata = newdata, type = "response"))
  return(scores)
}

OvA_probabilities <- predict_OvA_scores(dat.test.lasso, classifiers_ova)

multiclass_auc_result <- multiclass.roc(
  response = dat.test.lasso$y, 
  predictor = OvA_probabilities
)
overall_auc <- multiclass_auc_result$auc
print(paste("Multi-class AUC:", round(overall_auc, 3)))

## trees 

library(MASS)
library(data.table)

library(rpart)
library(rpart.plot)
library(randomForest)
library(ada)

library(ggplot2)
library(ggplotify)
library(ggrepel)

head(dat.train)

rpart.plot::rpart.plot(tree)

tree.model <- rpart(y ~ ., data=dat.train)
rpart.plot(tree.model)
importance <- varImp(tree.model)
barplot(importance)
#error 

# AI 

importance <- tree$variable.importance
print(importance)

barplot(importance, main="Variable Importance", 
        ylab="Importance", las=2, cex.names=0.5)

plotcp(tree)
optimal_cp <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
pruned_tree <- prune(tree, cp = optimal_cp)
rpart.plot(pruned_tree, type=2, cex=0.5, extra=104, fallen.leaves=TRUE)
print(pruned_tree$variable.importance)

library(caret) 

head(dat.train)

tree.model <- rpart(y ~ ., data = dat.train, method = "class")
rpart.plot(tree.model)

importance_scores <- tree.model$variable.importance
print(importance_scores)

barplot(importance_scores, 
        main = "Variable Importance", 
        ylab = "Importance", 
        las = 2,
        cex.names = 0.7)

plotcp(tree.model)

optimal_cp <- tree.model$cptable[which.min(tree.model$cptable[,"xerror"]),"CP"]
print(paste("Optimal CP for pruning:", optimal_cp))

pruned_tree <- prune(tree.model, cp = optimal_cp)
rpart.plot(pruned_tree, type = 2, cex = 0.6, extra = 104, fallen.leaves = TRUE, main = "Pruned Decision Tree") 
print(pruned_tree$variable.importance)

predictions <- predict(pruned_tree, newdata = dat.test, type = "class")

conf_matrix <- confusionMatrix(predictions, as.factor(dat.test$y))

print(conf_matrix)

test_accuracy <- mean(predictions == dat.test$y)
print(paste("Test Set Accuracy:", round(test_accuracy, 3)))

# prediction using AI 

library(rpart)
predictions_prob_matrix <- predict(pruned_tree, newdata = dat.test, type = "prob")
head(predictions_prob_matrix)
average_probabilities_per_class <- colMeans(predictions_prob_matrix)
print(average_probabilities_per_class)


library(pROC) 
predictions_class <- predict(pruned_tree, newdata = dat.test, type = "class")
head(predictions_class)
predictions_prob_matrix <- predict(pruned_tree, newdata = dat.test, type = "prob")
head(predictions_prob_matrix)

# AUC 
actual_response <- as.factor(dat.test$y)

multiclass_auc_result <- multiclass.roc(
  response = actual_response,
  predictor = predictions_prob_matrix
)

overall_auc <- multiclass_auc_result$auc
print(paste("Multi-class AUC:", round(overall_auc, 3)))

## random forests 
library(randomForest)
library(caret)
library(dplyr)

set.seed(100)
idx <- sample(nrow(data), floor(0.8*nrow(data)))
train_data <- data[idx,]
test_data <- data[-idx,]

rf_model <- randomForest(
  as.factor(y) ~ .,
  data = train_data,
  ntree = 500,
  mtry = sqrt(ncol(train_data) - 1), 
  importance = TRUE
)

print(rf_model)

predictions <- predict(rf_model, newdata = test_data)
conf_matrix <- table(Actual = test_data$y, Predicted = predictions)
print(conf_matrix)

accuracy <- mean(predictions == test_data$Species)
cat(paste("Accuracy:", round(accuracy * 100, 2), "%\n"))
varImpPlot(rf_model)

library(randomForest)
library(caret)
library(pROC)
library(dplyr)

train_data$y <- as.factor(train_data$y)
test_data$y <- as.factor(test_data$y)
Y <- test_data$y 

rf_model <- randomForest(
  y ~ .,
  data = train_data,
  ntree = 500,
  importance = TRUE
)

rf_pred_classes <- predict(rf_model, newdata = test_data, type = "class")

rf_pred_probs <- predict(rf_model, newdata = test_data, type = "prob")

generic_pred_object <- list(
  class = rf_pred_classes,
  posterior = rf_pred_probs # pROC expects a data frame of probabilities
)

conf_matrix <- table("RF Predicted" = generic_pred_object$class, "True" = Y)
print(conf_matrix)

metrics <- confusionMatrix(data = generic_pred_object$class, reference = Y, mode = "everything")
overall_accuracy <- metrics$overall['Accuracy']
cat("\nOverall Accuracy:", overall_accuracy, "\n\n")
precision_per_class <- metrics$byClass[, 'Pos Pred Value']
cat("Precision (PPV) per class:\n")
print(precision_per_class)
cat("\n")
recall_per_class <- metrics$byClass[, 'Sensitivity']
cat("Recall (Sensitivity) per class:\n")
print(recall_per_class)
cat("\n")

predicted_probabilities <- as.data.frame(generic_pred_object$posterior)
multiclass_auc <- multiclass.roc(response = Y, predictor = predicted_probabilities)
overall_multiclass_auc <- multiclass_auc$auc
cat("Overall Multi-class AUC (Hand and Till):", overall_multiclass_auc, "\n")

importance_data <- importance(rf_model, type = 2)
importance_df <- as.data.frame(importance_data)

importance_df$Feature <- rownames(importance_df)
importance_df_sorted <- importance_df[order(importance_df$MeanDecreaseGini, decreasing = TRUE), ]
print(importance_df_sorted)

feature_importance_list <- setNames(importance_df_sorted$MeanDecreaseGini, importance_df_sorted$Feature)
print(feature_importance_list)


## boosting 

# response should be binary (0/1), so need to do 1 v all and find max probability 

library(gbm)
X <- model.matrix(y ~ ., data = data)[, -1]
y <- factor(data$y)
dat.gbm <- data.frame(y = data$y, X)

# generate pseudoresponses 
View(dat.gbm)
dat.gbm$y_dropout <- ifelse(dat.gbm$y == "Dropout", 1, 0)
dat.gbm$y_enrolled <- ifelse(dat.gbm$y == "Enrolled", 1, 0)
dat.gbm$y_graduate <- ifelse(dat.gbm$y == "Graduate", 1, 0)
dat.gbm <- dat.gbm[, -1] 
View(dat.gbm) 

dat.dropout <- dat.gbm[, -c(31,32)]
dat.enrolled <- dat.gbm[, -c(30,32)]
dat.graduate <- dat.gbm[, -c(30,31)]


# AI help from here 
library(xgboost)
library(dplyr)
X <- as.matrix(dat.dropout[, -which(names(dat.dropout) == "y")])
y_label <- as.numeric(dat.dropout$y) 
dtrain <- xgb.DMatrix(data = X, label = y_label)
## getting an error here--did I format the data incorrectly? 


xgb_model <- xgboost(
  data = dtrain, 
  objective = "binary:logistic", 
  nrounds = 50,                  
  verbose = 0                    
)

probabilities_class_1 <- predict(xgb_model, newdata = X)
probabilities_class_0 <- 1 - probabilities_class_1

predictions_df <- data.frame(
  Prob_Class_0 = probabilities_class_0,
  Prob_Class_1 = probabilities_class_1
)

max_probabilities <- apply(predictions_df, 1, max)

predicted_class <- apply(predictions_df, 1, function(row) {
  colnames(predictions_df)[which.max(row)]
})

results_df <- data.frame(
  Actual_Class = mtcars$vs,
  Max_Prob_Value = max_probabilities,
  Predicted_Class = predicted_class
)

print(head(results_df))







# below this point is the previous attempt 

fit.gbm <- gbm(
  formula = y ~ .,
  data = dat.gbm,
  distribution = "multinomial",
  n.trees = 100,
  shrinkage = 0.05,
  interaction.depth = 3,
  cv.folds = 5
)

summary(fit.gbm)
summary(fit.gbm, cex.axis = .5)
#customizations are not working here to make the feature names smaller 

set.seed(100)
idx <- sample(nrow(data), floor(0.8*nrow(data)))
dat.train <- data[idx,]
dat.test <- data[-idx,]

#AI 
# --- Data Preparation (Ensure 'y' is a factor) ---
# Assuming 'data' is your original data frame
dat.dropout$y <- as.factor(dat.dropout$y) 

# Assuming dat.train and dat.test are defined from 'data' somewhere previously
# set.seed(100); idx <- sample(nrow(data), floor(0.8*nrow(data))); dat.train <- data[idx,]; dat.test <- data[-idx,]

# --- 1. Training OvA GBM Classifiers ---

y.uni <- levels(dat.train$y) # Get all unique class labels
classifiers_ova_gbm <- list()

# Dynamically get predictor names (use backticks `` for names with spaces)
predictor_vars <- names(dat.train)[names(dat.train) != "y"]
formula_str <- paste("y_binary ~", paste(paste0("`", predictor_vars, "`"), collapse = " + "))
formula.gbm <- as.formula(formula_str)

# Loop to train a separate binary GBM classifier for each class
for (k in y.uni) {
  # Create a temporary binary target column (1 for current class, 0 otherwise)
  temp_train_data <- dat.train %>%
    mutate(y_binary = ifelse(y == k, 1, 0))
  
  # Fit GBM with "bernoulli" distribution (binary logistic regression)
  fit <- gbm(
    formula = formula.gbm,
    data = temp_train_data,
    distribution = "bernoulli", # Key change from multinomial
    n.trees = 500,             # Number of trees (can be tuned)
    shrinkage = 0.01,          # Learning rate
    interaction.depth = 3,     # Tree complexity
    cv.folds = 5               # Cross-validation folds for optimal trees
  )
  classifiers_ova_gbm[[k]] <- fit
  message(paste("Trained OvA GBM classifier for class:", k))
}
names(classifiers_ova_gbm) <- y.uni


# --- 2. Prediction Function (Winner Takes All) ---

predict_OvA_gbm <- function(newdata, classifiers) {
  scores <- sapply(classifiers, function(fit) {
    # Find the optimal number of trees via OOB estimation
    best_trees <- gbm.perf(fit, method = "OOB", plot = FALSE) 
    # Predict probabilities using optimal trees
    predict(fit, newdata = newdata, n.trees = best_trees, type = "response")
  })
  
  # Select the class label with the highest probability
  final_predictions <- colnames(scores)[apply(scores, 1, which.max)]
  
  return(factor(final_predictions, levels = names(classifiers)))
}


# --- 3. Evaluate on the Test Set ---

OvA_gbm_pred <- predict_OvA_gbm(dat.test, classifiers_ova_gbm)
# error here--do not know how to move forward. Troubleshooting shows that no common mistakes are happening. I will wait to meet 


# Calculate Accuracy
accuracy <- mean(OvA_gbm_pred == dat.test$y)
print(paste("Test Set Accuracy:", round(accuracy, 3)))

# Generate Confusion Matrix
conf.ova.gbm <- confusionMatrix(OvA_gbm_pred, dat.test$y)
print(conf.ova.gbm)

