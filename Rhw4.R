
knitr::opts_chunk$set(echo = TRUE)

options(repos = c(CRAN = "https://cran.rstudio.com/"))

install.packages("ggfortify")
install.packages("mvnormtest")
install.packages("datarium")
install.packages("ggplot2")
install.packages("caret")
install.packages("mvtnorm")
install.packages("pROC")
install.packages("tinytex")

update.packages(ask = FALSE, checkBuilt = TRUE)
tinytex::tlmgr_update()


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

data <- read.csv("C:/Users/harip/Downloads/adult/adult.data")
data

data <- select(data, X39, X13, X40, X..50K) 
names(data) <- c("age", "education-num", "hours-per-week", "y")
data 

data$y <- ifelse(data$y == " >50K", 2, 1)
data

X1 <- filter(data, y == 1)
X1 
X2 <- filter(data, y == 2)
X2 

X <- rbind(X1, X2)
X
y <- factor(c(rep(0, nrow(X1)), rep(1, nrow(X2)))) 
y

data <- data.table(x1 = X[, 1], x2 = X[, 2], y = y)
head(data)

ggplot(data, aes(x = x1, y = x2, color = y)) +
  geom_point(size = 2, alpha = 0.7) +
  scale_shape_manual(values = c(16, 17, 18, 19, 20, 21)) +
  ggtitle("") +
  labs(color = "True Class", shape = "Predicted Class") +
  theme(legend.position = "bottom")

#LDA model and decision boundary 

lda_model <- lda(y ~ x1 + x2, data = data)
lda_pred <- predict(lda_model, data)
table("LDA" = lda_pred$class, "True" = data$y)

decisionplot(lda_model, data, class="y")

#QDA and comparison 

qda_model <- qda(y ~ x1 + x2, data = data)
qda_pred <- predict(qda_model, data)
table("QDA" = qda_pred$class, "True" = data$y)

decisionplot(qda_model, data, class="y")

#RDA 
library(mvtnorm)

K <- 2

mu_list <- lapply(1:K, function(k) {
  colMeans(data[y == k, .(x1, x2)])
})

n_k <- as.numeric(table(y))
prior <- prop.table(n_k)

cov_list <- lapply(1:K, function(k) {
  X_k <- data[y == k, .(x1, x2)]
  cov(X_k)
})

Sigma_pooled <- Reduce("+", lapply(1:2, function(k) (n_k[k]-1)*cov_list[[k]])) / (sum(n_k) - K)

sigma2_hat <- mean(sapply(cov_list, function(S) mean(diag(S))))

#compute sigma_k^tilde and final RDA covariances 
lambda <- 0.5
gamma <- 0.5

tilde_covs <- lapply(1:K, function(k) {
  lambda * cov_list[[k]] + (1 - lambda) * sigma2_hat * diag(2)
})

rda_covs <- lapply(1:K, function(k) {
  gamma * tilde_covs[[k]] + (1 - gamma) * Sigma_pooled
})

#RDA prediction function 
rda_predict <- function(Xnew, mu_list, cov_list, prior) {
  log_post <- sapply(1:length(mu_list), function(k) {
    dmvnorm(Xnew, mean = mu_list[[k]], sigma = cov_list[[k]], log = TRUE) + log(prior[k])
  })
  apply(log_post, 1, which.max)
}

#plot 
grid_x1 <- seq(min(data$x1), max(data$x1), length.out = 200)
grid_x2 <- seq(min(data$x2), max(data$x2), length.out = 200)
grid <- as.data.table(expand.grid(x1 = grid_x1, x2 = grid_x2))

yhat <- rda_predict(as.matrix(grid), mu_list, rda_covs, prior)
grid[, yhat := as.factor(yhat)]

ggplot() +
  geom_point(data = grid, aes(x1, x2, color = yhat), alpha = 0.05, shape = 15) +
  geom_point(data = data, aes(x1, x2, color = y), shape = 1) +
  labs(title = "Regularized Discriminant Analysis", color = "Class") +
  theme_minimal()

#computation for QDA via eigen-decomposition 
#compute eigen-decomposition for sigma_k
cov1 <- cov(X1)
eig1 <- eigen(cov1)

cat("Eigenvalues (class 1):", eig1$values, "\n")
cat("Orthonormal matrix U_k (class 1):\n")
print(eig1$vectors)

#reduced-rank LDA 
#mean vectors for each class 
x3 <- X[, 3]
x4 <- X[, 4]

means <- by(data[, .(x1, x2, x3, x4)], data$y, colMeans)
means_matrix <- do.call(rbind, means)

#compute between-class and within-class scatter matrices 
grand_mean <- colMeans(data[, .(x1, x2, x3, x4)])
B <- t(means_matrix - grand_mean) %*% (means_matrix - grand_mean)
B
W <- var(X)
W

#eigen-decomposition of W^{-1}B 
eig <- eigen(solve(W) %*% B)
eig$vectors  # Discriminant directions

#single linear regression 
linear_model <- lm(as.numeric(y) ~ x1 + x2, data = data)
linear_pred <- as.numeric(predict(linear_model, data))

linear_pred_class <- ifelse(linear_pred < 1.5, 1, ifelse(linear_pred < 2.5, 2, 3))
table("Linear Regression" = linear_pred_class, "True" = data$y)

result1 <- cbind(data,Linear_Regression=as.factor(linear_pred_class))
ggplot(result1, aes(x = x1, y = x2, color = y)) +
  geom_point(aes(shape = Linear_Regression), size = 2, alpha = 0.7) +
  scale_shape_manual(values = c(16, 17, 18, 19, 20, 21)) +
  ggtitle("Single Linear Regression") +
  labs(color = "True Class", shape = "Predicted Class") +
  theme(legend.position = "bottom")

#one-vs-all linear regression 

#linear regression for class 1 
linear_model_1 <- lm(I(y == 1) ~ x1 + x2, data = data)
linear_pred_1 <- predict(linear_model_1, data)

#for class 2 
linear_model_2 <- lm(I(y == 2) ~ x1 + x2, data = data)
linear_pred_2 <- predict(linear_model_2, data)

#combine
linear_pred <- data.frame(
  class1 = linear_pred_1,
  class2 = linear_pred_2
)

#highest predicted value 
linear_pred_class <- apply(linear_pred, 1, function(x) names(x)[which.max(x)])
linear_pred_class <- gsub("class","",linear_pred_class)

table("Linear Regression" = linear_pred_class, "True" = data$y)

result2 <- cbind(data,Linear_Regression=linear_pred_class)
ggplot(result2, aes(x = x1, y = x2, color = y)) +
  geom_point(aes(shape = Linear_Regression), size = 2, alpha = 0.7) +
  scale_shape_manual(values = c(16, 17, 18, 19, 20, 21)) +
  ggtitle("One-vs-all Linear Regression") +
  labs(color = "True Class", shape = "Predicted Class") +
  theme(legend.position = "bottom")

#confusion matrices 
dc_data <- as.matrix(data[,1:2])%*%as.matrix(lda_model$scaling)
dc_data <- as.data.frame(dc_data)
dc_data$True <- data$y
dc_data$LDA <- lda_pred$class
dc_data$Linear_Regression <- factor(linear_pred_class)
head(dc_data)

#error: object 'LD2' not found
#ggplot(dc_data, aes(x = LD1, y = LD2, color = True)) +
#   geom_point() +
#  geom_point(aes( shape = LDA), size = 4, alpha = 0.3) +
#  geom_point(aes(shape = Linear_Regression), size = 2, alpha = 0.7) +
#  scale_shape_manual(values = c(16, 17, 18, 19, 20, 21)) +
#  ggtitle("Comparison of LDA and Linear Regression") +
#  labs(color = "True Class", shape = "Predicted Class") +
#  theme(legend.position = "bottom")

#logistic regression for multinomial 
library(tidyverse)
library(caret)
library(nnet)
library(pROC)

str(data)

set.seed(100)
train.sample1 <- sample(seq_len(nrow(data)), size=0.8*nrow(data))

length(train.sample1)

set.seed(100)
train.sample2 <- sample(c(TRUE, FALSE), nrow(data), replace=TRUE, prob=c(0.8,0.2))
sum(train.sample2)

train.data <- data[train.sample1, ]
dim(train.data)
test.data <- data[-train.sample1, ] 
dim(test.data)

#one vs. all 
y.uni <- as.character(unique(data$y))
y.uni

#create an empty list to hold each binary classifier 
classifiers_ova <- list()
train.data2 <- train.data
head(train.data2)
names(train.data2) <- 
  
  #loop to train a classifier for each class 
  #for (k in y.uni) {
  #  y_binary <- ifelse(train.data2$y == k, 1, 0)
  #  assign("train.data2", within(train.data2, assign(k, y_binary)))
  #  formula.glm <- paste(k,"~ x1 + x2")
  #  fit <- glm(formula.glm, data = train.data2, family = binomial)
  #  classifiers_ova[[k]] <- fit
  #}
  
  for (k in y.uni) {
    y_binary <- ifelse(train.data2$y == k, 1, 0)
    formula_str <- paste("y_binary ~ x1 + x2")
    temp_data_for_glm <- data.frame(
      y_binary = y_binary,
      x1 = train.data2$x1,
      x2 = train.data2$x2
    )
    fit <- glm(as.formula(formula_str), data = temp_data_for_glm, family = binomial)
    classifiers_ova[[k]] <- fit
    rm(temp_data_for_glm)
  }

#prediction function for OvA 
predict_OvA <- function(newdata, classifiers) {
  scores <- sapply(classifiers, function(fit) predict(fit, newdata = data.frame(newdata), type = "response"))
  return(y.uni[apply(scores, 1, which.max)])
}

#test the prediction function 
OvA_pred <- predict_OvA(test.data, classifiers_ova)

mean(OvA_pred == test.data$y)

#confusion matrix 
conf.ova <- confusionMatrix(as.factor(OvA_pred), test.data$y)
print(conf.ova)

#multinomial/softmaxx 
logit_model <- nnet::multinom(y ~., data = train.data)
summary(logit_model)

logit_pred <- logit_model %>% predict(test.data)
head(logit_pred)

#levels 
mean(logit_pred == test.data$y)
conf.logit <- confusionMatrix(as.factor(logit_pred), test.data$y)
print(conf.logit)

#LDA 
lda_model <- lda(y ~ ., data = train.data)

lda_pred <- lda_model %>% 
  predict(test.data) %>%
  `[[`("class")

lda_pred <- lda_model %>% 
  predict(test.data) %>%
  pluck("class")

mean(lda_pred==test.data$y)
conf.lda <- confusionMatrix(as.factor(lda_pred), test.data$y)
print(conf.lda)

#linear 
linear_model_1 <- lm(I(y == 1) ~ ., data = train.data)
linear_model_2 <- lm(I(y == 2) ~ ., data = train.data)

linear_pred_1 <- predict(linear_model_1, test.data)
linear_pred_2 <- predict(linear_model_2, test.data)

linear_pred <- data.frame(
  1 == linear_pred_1,
  2 == linear_pred_2
)

linear_pred_class <- apply(linear_pred, 1, function(x) names(x)[which.max(x)])
mean(linear_pred_class==test.data$y)

#plot 
dc_data <- as.matrix(test.data[,1:2])%*%as.matrix(lda_model$scaling)
dc_data <- as.data.frame(dc_data)
dc_data$True <- test.data$y
dc_data$Logit <- logit_pred
dc_data$LDA <- lda_pred
dc_data$Linear <- factor(linear_pred_class)
head(dc_data)

#error: object LD2 not found 
#ggplot(dc_data, aes(x = LD1, y = LD2, color = True)) +
#  geom_point(aes(shape = Logit),  size = 4, alpha = 0.3) +
#  geom_point(aes(shape = LDA),  size = 2, alpha = 0.9) +
#  # geom_point(aes(shape = Linear), size = 2, alpha = 0.7) +
#  scale_shape_manual(values = c(16, 17, 18, 19, 20, 21)) +
#  ggtitle("PCA Visualization of Actual and Predicted Classes") +
#  labs(shape = "Predicted Class",
#       color = "Actual Class") +
#  theme(legend.position = "bottom")


#logistic regression for multinomial 
library(tidyverse)
library(caret)
library(nnet)
library(pROC)

data <- read.csv("C:/Users/harip/Downloads/predict+students+dropout+and+academic+success/data.csv", sep=";")
data
data <- select(data, Previous.qualification..grade., Admission.grade, Target)

str(data)

set.seed(100)
train.sample1 <- sample(seq_len(nrow(data)), size=0.8*nrow(data))

length(train.sample1)

set.seed(100)
train.sample2 <- sample(c(TRUE, FALSE), nrow(data), replace=TRUE, prob=c(0.8,0.2))
sum(train.sample2)

train.data <- data[train.sample1, ]
dim(train.data)
test.data <- data[-train.sample1, ] 
dim(test.data)

#one vs. all 
Target.uni <- as.character(unique(data$Target))
Target.uni

#create an empty list to hold each binary classifier 
classifiers_ova <- list()
train.data2 <- train.data
head(train.data2)

#loop to train a classifier for each class 
for (k in Target.uni) {
  Target_binary <- ifelse(train.data2$Target == k, 1, 0)
  assign("train.data2", within(train.data2, assign(k, Target_binary)))
  formula.glm <- paste(k,"~ Previous.qualification..grade. + Admission.grade")
  fit <- glm(formula.glm, data = train.data2, family = binomial)
  classifiers_ova[[k]] <- fit
}

#prediction function for OvA 
predict_OvA <- function(newdata, classifiers) {
  scores <- sapply(classifiers, function(fit) predict(fit, newdata = data.frame(newdata), type = "response"))
  return(Target.uni[apply(scores, 1, which.max)])
}

#test the prediction function 
OvA_pred <- predict_OvA(test.data, classifiers_ova)
OvA_pred 
test.data$Target

mean(OvA_pred == test.data$Target)

all_possible_classes <- c("Dropout", "Enrolled", "Graduate")
OvA_pred <- factor(OvA_pred, levels = all_possible_classes)
test.data$Target <- factor(test.data$Target, levels = all_possible_classes)

#confusion matrix 
conf.ova <- confusionMatrix(as.factor(OvA_pred), as.factor(test.data$Target)) 
print(conf.ova)

#multinomial/softmaxx 
logit_model <- nnet::multinom(Target ~., data = train.data)
summary(logit_model)

logit_pred <- logit_model %>% predict(test.data)
head(logit_pred)

#levels 
mean(logit_pred == test.data$Target)
conf.logit <- confusionMatrix(as.factor(logit_pred), test.data$Target)
print(conf.logit)

#LDA 
lda_model <- lda(Target ~ ., data = train.data)

lda_pred <- lda_model %>% 
  predict(test.data) %>%
  `[[`("class")

lda_pred <- lda_model %>% 
  predict(test.data) %>%
  pluck("class")

mean(lda_pred==test.data$Target)
conf.lda <- confusionMatrix(as.factor(lda_pred), test.data$Target)
print(conf.lda)

#linear 
linear_model_Dropout <- lm(I(Target == "Dropout") ~ ., data = train.data)
linear_model_Enrolled <- lm(I(Target == "Enrolled") ~ ., data = train.data)
linear_model_Graduate <- lm(I(Target == "Graduate") ~ ., data = train.data)

linear_pred_Dropout <- predict(linear_model_Dropout, test.data)
linear_pred_Enrolled <- predict(linear_model_Enrolled, test.data)
linear_pred_Graduate <- predict(linear_model_Graduate, test.data)

linear_pred <- data.frame(
  Dropout = linear_pred_Dropout,
  Enrolled = linear_pred_Enrolled,
  Graduate = linear_pred_Graduate
)

linear_pred_class <- apply(linear_pred, 1, function(x) names(x)[which.max(x)])
mean(linear_pred_class==test.data$Target)

#plot 
dc_data <- as.matrix(test.data[,1:2])%*%as.matrix(lda_model$scaling)
dc_data <- as.data.frame(dc_data)
dc_data$True <- test.data$Target
dc_data$Logit <- logit_pred
dc_data$LDA <- lda_pred
dc_data$Linear <- factor(linear_pred_class)
head(dc_data)

ggplot(dc_data, aes(x = LD1, y = LD2, color = True)) +
  geom_point(aes(shape = Logit),  size = 4, alpha = 0.3) +
  geom_point(aes(shape = LDA),  size = 2, alpha = 0.9) +
  geom_point(aes(shape = Linear), size = 2, alpha = 0.7) +
  scale_shape_manual(values = c(16, 17, 18, 19, 20, 21)) +
  ggtitle("PCA Visualization of Actual and Predicted Classes") +
  labs(shape = "Predicted Class",
       color = "Actual Class") +
  theme(legend.position = "bottom")
