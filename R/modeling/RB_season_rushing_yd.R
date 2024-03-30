################################################################################
# Load Libraries
################################################################################

library(tidyverse)
library(lubridate)
library(caret)
library(boot)
options(dplyr.summarise.inform = FALSE)

#source("R/modeling/helper_functions.R")

################################################################################
# Load Data
################################################################################

modeling_data <- data.table::fread("data/processed/season/rb_season_stat_modeling_data.csv")
external_data <- data.frame(data.table::fread("data/processed/season/rb_season_external.csv"))

################################################################################
# Select relevant columns
################################################################################

modeling_columns <- c('team', 'player_id', 'player_display_name', 'target_season', 
                      'games_played', 'seasons_played', 
                      # Career
                      'career_carries', 'career_carries_pg', 'career_rushing_yd', 
                      'career_rushing_ypg', 'career_target_share', 'career_catch_rate', 'career_receiving_ypg', 
                      # Previous Season
                      'prev_season_games_played', 'prev_season_carries','prev_season_carries_pg', 
                      'prev_season_rushing_yd', 'prev_season_rushing_ypg', 'prev_season_rushing_fb', 
                      'prev_season_receptions', 'prev_season_target_share', 'prev_season_catch_rate', 
                      # Roster
                      'same_team', 'draft_round', 
                      # Coaching
                      'total_wins', 'total_losses', 'total_win_pct', 'avg_rush_td', 
                      'avg_1st_down_penalty', 'pct_early_down_pass_total', 'pct_early_down_rush_total', 
                      'pct_early_down_deep_pass_total', 'pct_early_down_short_pass_total', 
                      'total_yrds_gained_early_down_total', 'early_down_conversion_rate_total', 
                      'to_go_third_down', 'pct_third_down_pass', 'pct_third_down_rush', 
                      'mean_yrds_gained_third_down', 'third_down_conversion_rate', 
                      'third_down_failed_rate', 'to_go_fourth_down', 'pct_fourth_down_pass', 
                      'pct_fourth_down_rush', 'pct_fourth_down_deep_pass', 'pct_fourth_down_short_pass', 
                      'mean_yrds_gained_fourth_down', 'fourth_down_conversion_rate', 
                      'fourth_down_failed_rate',
                      # Power Rankings
                      'off_rush_ypg_rank', 'off_rush_td_rank', 'off_pass_ypg_rank', 
                      'off_pass_td_rank', 'def_rush_ypg_rank', 'def_rush_td_rank', 
                      'def_pass_ypg_rank', 'def_pass_td_rank')

# Targets
target_columns <- c('target_carries', 'target_rushing_yd', 'target_rushing_td', 
                    'target_rushing_fb_lst', 'target_receptions', 'target_receiving_yd', 
                    'target_receiving_td', 'target_receiving_fb_lst')

modeling_data <- modeling_data %>% dplyr::select(c(modeling_columns, target_columns))
external_data <- external_data %>% dplyr::select(modeling_columns) %>% filter(career_carries >= 100)

# Get factor columns
factor_columns <- c('same_team', 'draft_round', 'off_rush_ypg_rank', 'off_rush_td_rank',
                    'off_pass_ypg_rank', 'off_pass_td_rank', 'def_rush_ypg_rank', 'def_rush_td_rank',
                    'def_pass_ypg_rank', 'def_pass_td_rank')

# Get numeric columns
numeric_cols <- names(X_train_raw)[sapply(X_train_raw, is.numeric)]
numeric_cols <- numeric_cols[!numeric_cols %in% factor_columns]

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
  dplyr::select(matches("target_rushing_yd")) %>% 
  data.frame() %>% 
  mutate(target_rushing_yd = log(target_rushing_yd))

# Separate out test data
X_test_raw <- modeling_data[-train_indices, ] %>% 
  dplyr::select(-matches("^target")) %>% 
  data.frame()

y_test <- modeling_data[-train_indices, ] %>% 
  dplyr::select(matches("target_rushing_yd")) %>% 
  data.frame() %>% 
  mutate(target_rushing_yd = log(target_rushing_yd))


################################################################################
# Preprocessing
################################################################################

# Impute missing factor data
X_train_raw <- X_train_raw %>%
  mutate(across(all_of(factor_columns), as.character)) %>% 
  mutate(across(all_of(factor_columns), ~ replace_na(., table(.) %>% which.max() %>% as.character()))) %>%
  mutate(across(all_of(factor_columns), factor))


# Create Preprocessing Steps
preprocess_steps <- caret::preProcess(X_train_raw[,numeric_cols], 
                                      method = c("medianImpute", "center", "scale"))

# Preprocess Data
X_train_processed <- predict(preprocess_steps, X_train_raw)
X_test_processed <- predict(preprocess_steps, X_test_raw)

preproc_pipeline <- preProcess(X_train_processed, method = c("pca"), pcaComp = 5)  # Set the number of PCA components here
X_train_processed <- predict(preproc_pipeline, newdata = X_train_processed)
X_test_processed <- predict(preproc_pipeline, newdata = X_test_processed)

# Create datesets
train_data <- cbind(X_train_processed, y_train)
test_data <- cbind(X_test_processed, y_test)

################################################################################
# Create CV Folds
################################################################################
n_cv <- 5
cv_folds <- caret::trainControl(method = "cv",
                               number = n_cv, 
                               savePredictions = TRUE)

################################################################################
# Create Linear Model
################################################################################

# Create formula
lm_model_cols <- c("same_team", "draft_round", "PC1", "PC2", "PC3", "PC4", "PC5")
formula_str <- paste("target_rushing_yd ~", paste0(lm_model_cols, collapse = " + "))
formula <- as.formula(formula_str)

lm_model <- caret::train(formula, 
                         data = train_data, 
                         method = "lm", 
                         trControl = cv_folds)

print(lm_model)

################################################################################
# Get CV Predictions
################################################################################

cv_preds <- lm_model$pred

cv_pred_range <- range(exp(cv_preds$pred), exp(cv_preds$obs))

cv_preds %>% 
  ggplot() +
    geom_point(aes(x = exp(obs), 
                   y = exp(pred))) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  xlim(0, cv_pred_range[2]) +
  ylim(0, cv_pred_range[2]) +
  labs(x = 'Observed Rushing Yards',
       y = 'Predicted Rushing Yards',
       title = 'Rushing Yards Prediction Performance')

################################################################################
# Get Test Predictions
################################################################################

# Separate Final Model
final_lm_model <- lm_model$finalModel

# Create Test Predictions
test_predictions <- data.frame(predict(final_lm_model, test_data, interval = 'prediction'))

# Create Prediction Object
prediction_df <- cbind(test_data[c('player_id', 'player_display_name', 'target_rushing_yd')],
                       test_predictions) %>% 
  rename(obs_rushing_yd = target_rushing_yd,
         pred_rushing_yd = fit,
         pred_lower_limit = lwr,
         pred_upper_limit = upr)

################################################################################
# Model Performance
################################################################################

# Statistics
reg_r2 <- summary(lm("pred_rushing_yd ~ obs_rushing_yd", data = prediction_df))$r.squared
rmse <- caret::RMSE(prediction_df$pred_rushing_yd, prediction_df$obs_rushing_yd)

print(paste0("Regression R-Squared: ", round(reg_r2, 2)))
print(paste0("RMSE: ", round(rmse, 2)))

# Plotting
pred_range <- range(prediction_df$pred_rushing_yd, prediction_df$obs_rushing_yd)

prediction_df %>% 
  ggplot() +
  geom_point(aes(x = obs_rushing_yd, 
                 y = pred_rushing_yd)) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  xlim(0, pred_range[2]) +
  ylim(0, pred_range[2]) +
  labs(x = 'Observed Rushing Yards',
       y = 'Predicted Rushing Yards',
       title = 'Rushing Yards Prediction Performance')

################################################################################
# Create External Predictions
################################################################################

# Create predictions for 2023
external_predictions <- data.frame(predict(final_lm_model, X_external, interval = 'prediction'))

external_data <- external_data %>% 
  data.frame()

external_prediction_df <- cbind(external_data[c('player_id', 'player_display_name', 'target_season')],
                                external_predictions) %>% 
  rename(pred_rushing_yd = fit,
         pred_lower_limit = lwr,
         pred_upper_limit = upr)

################################################################################
# Use Bootstrapping to Create Prediction CI
################################################################################

# Create Bootstrapped Predictions
player_df <- model_train(n_boot=100, 
                    train=modeling_data, 
                    process_func=preprocess_steps, 
                    train_split_prop=0.8, 
                    regression_model='lm',
                    lm_formula=formula, 
                    n_cv=10, 
                    pred_ppt=external_data,
                    target_col_regex="target_rushing_yd")

# Save
data.table::fwrite(player_df, "data/results/RB/exp_r_yrd_2023.csv")
