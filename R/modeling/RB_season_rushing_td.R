################################################################################
# Load Libraries
################################################################################

library(tidyverse)
library(lubridate)
library(caret)
library(boot)
options(dplyr.summarise.inform = FALSE)

################################################################################
# Load Data
################################################################################

modeling_data <- data.table::fread("data/processed/season/rb_season_stat_modeling_data.csv")
external_data <- data.frame(data.table::fread("data/processed/season/rb_season_external.csv"))

################################################################################
# Split
################################################################################

# Create indicies of split
set.seed(42)
train_split_prop <- 0.8
train_indices <- sample(nrow(modeling_data), nrow(modeling_data) * train_split_prop)

# Separate out train data
X_train_raw <- modeling_data[train_indices, ] %>% 
  dplyr::select(-matches("^target")) %>% 
  data.frame()

y_train <- modeling_data[train_indices, ] %>% 
  dplyr::select(matches("target_rushing_td")) %>% 
  data.frame()

# Separate out test data
X_test_raw <- modeling_data[-train_indices, ] %>% 
  dplyr::select(-matches("^target")) %>% 
  data.frame()

y_test <- modeling_data[-train_indices, ] %>% 
  dplyr::select(matches("target_rushing_td")) %>% 
  data.frame()


################################################################################
# Preprocessing
################################################################################

# Get numeric columns
numeric_cols <- names(X_train_raw)[sapply(X_train_raw, is.numeric)]

# Create Preprocessing Steps
preprocess_steps <- caret::preProcess(X_train_raw[,numeric_cols], 
                                      method = c("medianImpute", "center", "scale"))

# Preprocess Data
X_train_processed <- predict(preprocess_steps, X_train_raw)
X_test_processed <- predict(preprocess_steps, X_test_raw)
X_external <- predict(preprocess_steps, external_data)

# Create datesets
train_data <- cbind(X_train_processed, y_train)
test_data <- cbind(X_test_processed, y_test)

# Create formula
formula_str <- paste("target_rushing_td ~", paste0(numeric_cols, collapse = " + "))
formula <- as.formula(formula_str)

################################################################################
# Create CV Folds
################################################################################
n_cv <- 10
cv_folds <- caret::trainControl(method = "cv",
                               number = n_cv, 
                               savePredictions = TRUE)

################################################################################
# Create Linear Model
################################################################################
lm_model <- caret::train(formula, 
                         data = train_data, 
                         method = "glm", 
                         family = "poisson", 
                         trControl = cv_folds)

print(lm_model)

################################################################################
# Get CV Predictions
################################################################################

cv_preds <- lm_model$pred

cv_pred_range <- range(cv_preds$pred, cv_preds$obs)

cv_preds %>% 
  ggplot() +
    geom_point(aes(x = obs, 
                   y = pred)) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  xlim(0, cv_pred_range[2]) +
  ylim(0, cv_pred_range[2]) +
  labs(x = 'Observed Rushing TD',
       y = 'Predicted Rushing TD',
       title = 'Rushing Yards Prediction Performance')

################################################################################
# Get Test Predictions
################################################################################

# Separate Final Model
final_lm_model <- lm_model$finalModel

# # Create Test Predictions
# test_predictions <- data.frame(predict(final_lm_model, test_data, interval = 'prediction'))
# 
# # Create Prediction Object
# prediction_df <- cbind(test_data[c('player_id', 'player_display_name', 'target_rushing_td')],
#                        test_predictions) %>% 
#   rename(obs_rushing_td = target_rushing_td,
#          pred_rushing_td = fit,
#          pred_lower_limit = lwr,
#          pred_upper_limit = upr)
# 
# ################################################################################
# # Model Performance
# ################################################################################
# 
# # Statistics
# reg_r2 <- summary(lm("pred_rushing_td ~ obs_rushing_td", data = prediction_df))$r.squared
# rmse <- caret::RMSE(prediction_df$pred_rushing_td, prediction_df$obs_rushing_td)
# 
# print(paste0("Regression R-Squared: ", round(reg_r2, 2)))
# print(paste0("RMSE: ", round(rmse, 2)))
# 
# # Plotting
# pred_range <- range(prediction_df$pred_rushing_td, prediction_df$obs_rushing_td)
# 
# prediction_df %>% 
#   ggplot() +
#   geom_point(aes(x = round(obs_rushing_td), 
#                  y = round(pred_rushing_td))) +
#   geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
#   xlim(0, pred_range[2]) +
#   ylim(0, pred_range[2]) +
#   labs(x = 'Observed Rushing Yards',
#        y = 'Predicted Rushing Yards',
#        title = 'Rushing Yards Prediction Performance')
# 
# ################################################################################
# # Create External Predictions
# ################################################################################
# 
# # Create predictions for 2023
# external_predictions <- data.frame(predict(final_lm_model, X_external, interval = 'prediction'))
# 
# external_data <- external_data %>% 
#   data.frame()
# 
# external_prediction_df <- cbind(external_data[c('player_id', 'player_display_name', 'target_season')],
#                                 external_predictions) %>% 
#   rename(pred_rushing_td = fit,
#          pred_lower_limit = lwr,
#          pred_upper_limit = upr)

################################################################################
# Use Bootstrapping to Create Prediction CI
################################################################################

# Create Bootstrapped Predictions
player_df <- model_train(n_boot=100, 
                    train=modeling_data, 
                    process_func=preprocess_steps, 
                    train_split_prop=0.8, 
                    regression_model='poisson',
                    lm_formula=formula, 
                    n_cv=10, 
                    pred_ppt=external_data,
                    "target_rushing_td")

# Save
data.table::fwrite(player_df, "data/results/RB/exp_r_td_2023.csv")
