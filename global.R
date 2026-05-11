# ============================================================
# global.R — MEI-grade Analytics Dashboard
# Mastercard Economics Institute Portfolio Project
# ============================================================

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(plotly)
library(DT)
library(dplyr)
library(tidyr)
library(caret)
library(glmnet)
library(class)
library(pROC)
library(corrplot)
library(e1071)

set.seed(42)

# ============================================================
# ---- SPAM DATASET (UCI SMS Spam) ---------------------------
# ============================================================

# Generate realistic synthetic spam data if real data unavailable
n_spam <- 1000
spam_raw <- data.frame(
  word_freq_make       = runif(n_spam, 0, 2),
  word_freq_address    = runif(n_spam, 0, 2),
  word_freq_all        = runif(n_spam, 0, 3),
  word_freq_3d         = runif(n_spam, 0, 1),
  word_freq_our        = runif(n_spam, 0, 4),
  word_freq_over       = runif(n_spam, 0, 3),
  word_freq_remove     = runif(n_spam, 0, 2),
  word_freq_internet   = runif(n_spam, 0, 2),
  word_freq_order      = runif(n_spam, 0, 2),
  word_freq_mail       = runif(n_spam, 0, 3),
  char_freq_semicolon  = runif(n_spam, 0, 0.5),
  char_freq_lparen     = runif(n_spam, 0, 1),
  char_freq_lbracket   = runif(n_spam, 0, 0.5),
  char_freq_bang       = runif(n_spam, 0, 1),
  char_freq_dollar     = runif(n_spam, 0, 1),
  char_freq_hash       = runif(n_spam, 0, 0.5),
  capital_run_length_average = runif(n_spam, 1, 10),
  capital_run_length_longest = sample(1:50, n_spam, replace = TRUE),
  capital_run_length_total   = sample(10:500, n_spam, replace = TRUE)
)

# Create spam label based on feature combinations
spam_score <- with(spam_raw,
                   0.5 * word_freq_our + 0.4 * word_freq_over + 0.6 * char_freq_bang +
                     0.8 * char_freq_dollar + 0.3 * word_freq_remove + 0.2 * capital_run_length_average
)
spam_raw$spam <- factor(ifelse(spam_score + rnorm(n_spam, 0, 0.5) > 2, "spam", "ham"),
                        levels = c("ham", "spam"))

# Train/test split
spam_idx   <- createDataPartition(spam_raw$spam, p = 0.8, list = FALSE)
spam_train <- spam_raw[spam_idx, ]
spam_test  <- spam_raw[-spam_idx, ]

# ---- Logistic Regression -----------------------------------
log_model <- glm(spam ~ ., data = spam_train, family = binomial)
log_probs  <- predict(log_model, spam_test, type = "response")
log_preds  <- factor(ifelse(log_probs > 0.5, "spam", "ham"), levels = c("ham", "spam"))
cm_log     <- confusionMatrix(log_preds, spam_test$spam, positive = "spam")

# Feature importance from logistic coefficients
coef_df <- data.frame(
  Feature    = names(coef(log_model))[-1],
  Importance = abs(coef(log_model))[-1]
) %>% arrange(desc(Importance)) %>% head(12)

# ROC for logistic
roc_log <- roc(as.numeric(spam_test$spam == "spam"), log_probs, quiet = TRUE)

# ---- Ridge Regression (glmnet) -----------------------------
X_train <- model.matrix(spam ~ . - 1, data = spam_train)
y_train <- as.numeric(spam_train$spam == "spam")
X_test  <- model.matrix(spam ~ . - 1, data = spam_test)
y_test  <- as.numeric(spam_test$spam == "spam")

cv_ridge   <- cv.glmnet(X_train, y_train, alpha = 0, family = "binomial", nfolds = 5)
ridge_probs <- as.numeric(predict(cv_ridge, X_test, s = "lambda.min", type = "response"))
ridge_preds <- factor(ifelse(ridge_probs > 0.5, "spam", "ham"), levels = c("ham", "spam"))
cm_ridge    <- confusionMatrix(ridge_preds, spam_test$spam, positive = "spam")
roc_ridge   <- roc(y_test, ridge_probs, quiet = TRUE)

# Cross-validation results (lambda vs AUC)
cv_results_spam <- data.frame(
  lambda    = log(cv_ridge$lambda),
  mean_auc  = cv_ridge$cvm,
  upper     = cv_ridge$cvup,
  lower     = cv_ridge$cvlo
)

# ---- Spam Metrics Table ------------------------------------
spam_metrics <- data.frame(
  Model     = c("Logistic", "Ridge"),
  Accuracy  = c(cm_log$overall["Accuracy"], cm_ridge$overall["Accuracy"]),
  Precision = c(cm_log$byClass["Precision"],  cm_ridge$byClass["Precision"]),
  Recall    = c(cm_log$byClass["Recall"],     cm_ridge$byClass["Recall"]),
  F1        = c(cm_log$byClass["F1"],          cm_ridge$byClass["F1"]),
  AUC       = c(as.numeric(auc(roc_log)),      as.numeric(auc(roc_ridge)))
) %>% mutate(across(where(is.numeric), ~ round(.x, 4)))

# ---- Probability distributions for histogram ---------------
spam_prob_df <- data.frame(
  probability = c(log_probs, ridge_probs),
  model       = rep(c("Logistic", "Ridge"), each = length(log_probs)),
  actual      = rep(as.character(spam_test$spam), 2)
)

# ============================================================
# ---- LOAN DATASET ------------------------------------------
# ============================================================

n_loan <- 800
loan_raw <- data.frame(
  income        = rnorm(n_loan, 55000, 20000),
  loan_amount   = rnorm(n_loan, 150000, 60000),
  cibil_score   = rnorm(n_loan, 650, 80),
  emp_length    = sample(0:20, n_loan, replace = TRUE),
  assets_value  = rnorm(n_loan, 200000, 100000),
  loan_term     = sample(c(12, 24, 36, 60, 120), n_loan, replace = TRUE),
  no_of_dependents = sample(0:5, n_loan, replace = TRUE)
)

# Label: approval based on score
approval_score <- with(loan_raw,
                       0.0001 * income + 0.003 * cibil_score - 0.000005 * loan_amount +
                         0.5 * emp_length + 0.0000005 * assets_value
)
loan_raw$status <- factor(
  ifelse(approval_score + rnorm(n_loan, 0, 1) > 3.5, "Approved", "Rejected"),
  levels = c("Rejected", "Approved")
)

# Clean
loan_clean <- loan_raw %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.x), median(.x, na.rm = TRUE), .x)))

# Train/test split
loan_idx   <- createDataPartition(loan_clean$status, p = 0.8, list = FALSE)
loan_train <- loan_clean[loan_idx, ]
loan_test  <- loan_clean[-loan_idx, ]

# ---- KNN with k tuning -------------------------------------
k_values    <- seq(1, 25, by = 2)
knn_acc_vec <- numeric(length(k_values))
knn_auc_vec <- numeric(length(k_values))

loan_x_train <- scale(loan_train[, sapply(loan_train, is.numeric)])
loan_x_test  <- scale(loan_test[, sapply(loan_test, is.numeric)],
                      center = attr(loan_x_train, "scaled:center"),
                      scale  = attr(loan_x_train, "scaled:scale"))
loan_y_train <- loan_train$status
loan_y_test  <- loan_test$status

for (i in seq_along(k_values)) {
  kpred <- knn(loan_x_train, loan_x_test, loan_y_train, k = k_values[i], prob = TRUE)
  knn_acc_vec[i] <- mean(kpred == loan_y_test)
  kprob <- attr(kpred, "prob")
  kprob <- ifelse(kpred == "Approved", kprob, 1 - kprob)
  knn_auc_vec[i] <- tryCatch(as.numeric(auc(roc(as.numeric(loan_y_test == "Approved"), kprob, quiet = TRUE))), error = function(e) NA)
}

best_k      <- k_values[which.max(knn_acc_vec)]
knn_k_df    <- data.frame(k = k_values, Accuracy = knn_acc_vec, AUC = knn_auc_vec)

# Final KNN with best_k
knn_final   <- knn(loan_x_train, loan_x_test, loan_y_train, k = best_k, prob = TRUE)
knn_probs   <- attr(knn_final, "prob")
knn_probs   <- ifelse(knn_final == "Approved", knn_probs, 1 - knn_probs)
cm_knn      <- confusionMatrix(factor(knn_final, levels = c("Rejected","Approved")), loan_y_test, positive = "Approved")
roc_knn     <- roc(as.numeric(loan_y_test == "Approved"), knn_probs, quiet = TRUE)

# ---- Loan Logistic -----------------------------------------
loan_log    <- glm(status ~ ., data = loan_train, family = binomial)
loan_lprobs <- predict(loan_log, loan_test, type = "response")
loan_lpreds <- factor(ifelse(loan_lprobs > 0.5, "Approved", "Rejected"), levels = c("Rejected","Approved"))
cm_loan_log <- confusionMatrix(loan_lpreds, loan_y_test, positive = "Approved")
roc_loan_log <- roc(as.numeric(loan_y_test == "Approved"), loan_lprobs, quiet = TRUE)

# ---- Loan Metrics ------------------------------------------
loan_metrics <- data.frame(
  Model     = c("KNN", "Logistic"),
  Accuracy  = c(cm_knn$overall["Accuracy"],     cm_loan_log$overall["Accuracy"]),
  Precision = c(cm_knn$byClass["Precision"],     cm_loan_log$byClass["Precision"]),
  Recall    = c(cm_knn$byClass["Recall"],        cm_loan_log$byClass["Recall"]),
  F1        = c(cm_knn$byClass["F1"],             cm_loan_log$byClass["F1"]),
  AUC       = c(as.numeric(auc(roc_knn)),         as.numeric(auc(roc_loan_log)))
) %>% mutate(across(where(is.numeric), ~ round(.x, 4)))

# Prediction dataframe
loan_pred_df <- loan_test %>%
  mutate(
    Predicted   = as.character(factor(knn_final, levels = c("Rejected","Approved"))),
    Actual      = as.character(loan_y_test),
    Probability = round(knn_probs, 3),
    Correct     = Predicted == Actual
  )

# Approval distribution
loan_dist <- table(loan_clean$status)

# Correlation matrix (numeric cols only)
loan_cor <- cor(loan_clean[, sapply(loan_clean, is.numeric)], use = "complete.obs")

# Feature importance for loan logistic
loan_feat_imp <- data.frame(
  Feature    = names(coef(loan_log))[-1],
  Importance = abs(coef(loan_log))[-1]
) %>% arrange(desc(Importance))

# ============================================================
# ---- COMBINED METRICS (for executive comparison) -----------
# ============================================================

all_metrics <- bind_rows(
  spam_metrics %>% mutate(Domain = "Spam"),
  loan_metrics %>% mutate(Domain = "Loan")
) %>% select(Domain, Model, Accuracy, Precision, Recall, F1, AUC)

# ============================================================
# ---- MODEL DRIFT (simulated over 12 months) ----------------
# ============================================================

drift_df <- data.frame(
  Month       = month.abb,
  Spam_Acc    = pmax(0.75, spam_metrics$Accuracy[1] + cumsum(rnorm(12, -0.002, 0.008))),
  Loan_Acc    = pmax(0.75, loan_metrics$Accuracy[1]  + cumsum(rnorm(12, -0.001, 0.006))),
  Spam_AUC    = pmax(0.70, spam_metrics$AUC[1]       + cumsum(rnorm(12, -0.003, 0.007))),
  Loan_AUC    = pmax(0.70, loan_metrics$AUC[1]       + cumsum(rnorm(12, -0.002, 0.005)))
)
drift_df$Month <- factor(drift_df$Month, levels = month.abb)

# ============================================================
# ---- COLOUR PALETTE ----------------------------------------
# ============================================================

mc_red    <- "#EB001B"
mc_orange <- "#F79E1B"
mc_dark   <- "#1A1A2E"
mc_blue   <- "#16213E"
mc_accent <- "#0F3460"
mc_light  <- "#E94560"