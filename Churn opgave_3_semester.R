
library(tidyverse)
library(readxl)
library(caret)
library(MASS)


# Indlæs data

churn <- read_excel("data/data_og_analyse_opgave1.xlsx")

# Overblik
glimpse(churn)
colSums(is.na(churn))


# Definér target

target_var <- "succesfuldt_køb"

# Rens data


# Fjern NA i target
churn <- churn %>%
  filter(!is.na(.data[[target_var]]))

# Imputér kontakt
churn$kontakt[is.na(churn$kontakt)] <- median(churn$kontakt, na.rm = TRUE)

# Target som faktor
churn[[target_var]] <- as.factor(churn[[target_var]])

# Behold kun modelvariable (anti-leakage)
model_data <- churn %>%
  dplyr::select(
    succesfuldt_køb,
    abonnent_tidligere,
    abonnent_nu,
    kontakt
  )


# Train/test split (80/20 stratified)

set.seed(123)

train_index <- createDataPartition(
  model_data[[target_var]],
  p = 0.8,
  list = FALSE
)

train_model <- model_data[train_index, ]
test_model  <- model_data[-train_index, ]


# Logistisk regression

log_model <- glm(
  succesfuldt_køb ~ .,
  data = train_model,
  family = binomial
)

prob_log <- predict(
  log_model,
  newdata = test_model,
  type = "response"
)

# Cost-funktion (original)
cost_function <- function(actual, predicted,
                          fn_cost = 300,
                          fp_cost = 60,
                          tp_cost = 60) {
  
  cm <- table(actual, predicted)
  
  TN <- ifelse(!is.na(cm[1,1]), cm[1,1], 0)
  FP <- ifelse(!is.na(cm[1,2]), cm[1,2], 0)
  FN <- ifelse(!is.na(cm[2,1]), cm[2,1], 0)
  TP <- ifelse(!is.na(cm[2,2]), cm[2,2], 0)
  
  cost <- FN*fn_cost + FP*fp_cost + TP*tp_cost
  
  return(cost / length(actual))
}

actual <- test_model$succesfuldt_køb

# Optimal threshold — Logistic

thresholds <- seq(0.01, 0.99, by = 0.01)
costs_log <- numeric(length(thresholds))

for(i in seq_along(thresholds)) {
  
  pred_class <- ifelse(prob_log > thresholds[i], 1, 0)
  pred_class <- factor(pred_class, levels = levels(actual))
  
  costs_log[i] <- cost_function(actual, pred_class)
}

best_log_cost <- min(costs_log)
best_log_threshold <- thresholds[which.min(costs_log)]

best_log_cost
best_log_threshold

# LDA
lda_model <- lda(
  succesfuldt_køb ~ .,
  data = train_model
)

prob_lda <- predict(lda_model, newdata = test_model)$posterior[,2]

costs_lda <- numeric(length(thresholds))

for(i in seq_along(thresholds)) {
  
  pred_class <- ifelse(prob_lda > thresholds[i], 1, 0)
  pred_class <- factor(pred_class, levels = levels(actual))
  
  costs_lda[i] <- cost_function(actual, pred_class)
}

best_lda_cost <- min(costs_lda)
best_lda_cost


# Baseline (virksomheden: ingen churn)

baseline_pred <- factor(rep(0, length(actual)),
                        levels = levels(actual))

baseline_cost <- cost_function(actual, baseline_pred)
baseline_cost


# Model-sammenligning

results <- data.frame(
  Model = c("Baseline", "Logistic (optimal)", "LDA"),
  Cost_per_customer = c(baseline_cost,
                        best_log_cost,
                        best_lda_cost)
)

results

best_model_cost <- min(best_log_cost, best_lda_cost)


# Besparelse (500.000 kunder)

customers <- 500000

savings <- (baseline_cost - best_model_cost) * customers
savings


cost_function_120 <- function(actual, predicted) {
  cost_function(actual, predicted,
                fn_cost = 300,
                fp_cost = 120,
                tp_cost = 120)
}

costs_log_120 <- numeric(length(thresholds))

for(i in seq_along(thresholds)) {
  
  pred_class <- ifelse(prob_log > thresholds[i], 1, 0)
  pred_class <- factor(pred_class, levels = levels(actual))
  
  costs_log_120[i] <- cost_function_120(actual, pred_class)
}

best_log_cost_120 <- min(costs_log_120)
best_log_cost_120



#Visualisering

library(ggplot2)

plot_data <- data.frame(
  threshold = thresholds,
  cost = costs_log
)

ggplot(plot_data, aes(x = threshold, y = cost)) +
  geom_line() +
  geom_vline(xintercept = best_log_threshold,
             linetype = "dashed",
             color = "red") +
  labs(
    title = "Pris per kunde vs. threshold",
    x = "Threshold",
    y = "Pris per kunde"
  ) +
  theme_minimal()

#Søjlediagram
model_results <- data.frame(
  Model = c("Baseline", "Logistic", "LDA"),
  Cost = c(baseline_cost, best_log_cost, best_lda_cost)
)

ggplot(model_results, aes(x = Model, y = Cost)) +
  geom_col() +
  labs(
    title = "Cost comparison across models",
    y = "Cost per customer"
  ) +
  theme_minimal()

#Roc-kurve

library(pROC)
roc_obj <- roc(actual, prob_log)

plot(roc_obj,
     col = "#2C7FB8",
     lwd = 3,
     main = "ROC Curve – Logistic Regression")

auc(roc_obj)
