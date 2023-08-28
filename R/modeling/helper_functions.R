test <- model_regression(modeling_data = modeling_data,
                         external_data = external_data,
                         regression_model = 'lm',
                         target_var = 'target_receiving_td',
                         n_cv = 10,
                         split_ratio = 0.8,
                         random_seed = 42)


model_regression <- function(modeling_data,
                             external_data,
                             regression_model='lm',
                             target_var,
                             n_cv,
                             split_ratio,
                             random_seed) {
  
  # Split data up (function 1)
  set.seed(random_seed)
  train_split_prop <- split_ratio
  train_indices <- sample(nrow(modeling_data), nrow(modeling_data) * train_split_prop)
  
  # Create datasets (function 2)
  # Separate out train data
  X_train_raw <- modeling_data[train_indices, ] %>% 
    dplyr::select(-matches("^target")) %>% 
    data.frame()
  
  y_train <- modeling_data[train_indices, ] %>% 
    dplyr::select(matches({{ target_var }})) %>% 
    data.frame()
  
  # Separate out test data
  X_test_raw <- modeling_data[-train_indices, ] %>% 
    dplyr::select(-matches("^target")) %>% 
    data.frame()
  
  y_test <- modeling_data[-train_indices, ] %>% 
    dplyr::select(matches({{ target_var }})) %>% 
    data.frame()
  
  ################################################################################
  # Preprocessing (function 3)
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
  
  # Create formula (function 4)
  formula_str <- paste(paste0(target_var, " ~"), 
                       paste0(numeric_cols, collapse = " + "))
  formula <- as.formula(formula_str)
  
  # Create CV
  cv_folds <- caret::trainControl(method = "cv",
                                  number = n_cv, 
                                  savePredictions = TRUE)
  
  # Create Linear Model
  if (regression_model == 'lm') {
    lm_model <- caret::train(formula, 
                             data = train_data, 
                             method = regression_model, 
                             trControl = cv_folds)
  } else if (regression_model == 'poisson') {
    tuneGrid <- data.frame(method = "poisson")
    
    lm_model <- caret::train(formula, 
                             data = train_data, 
                             method = 'glm', 
                             trControl = cv_folds)
  }
  
  # Separate Final Model
  final_lm_model <- lm_model$finalModel
  
  # Create predictions
  test_predictions <- data.frame(predict(final_lm_model, test_data, interval = 'prediction'))
  
  # Create predictions for 2023
  external_predictions <- data.frame(predict(final_lm_model, X_external, interval = 'prediction'))
  
  external_data <- external_data %>% 
    data.frame()
  
  external_prediction_df <- cbind(external_data[c('player_id', 'player_display_name', 'target_season')],
                                  external_predictions) %>% 
    rename(pred_receiving_td = fit,
           pred_lower_limit = lwr,
           pred_upper_limit = upr)
  
  return(list(lm_model = final_lm_model,
              lm_preds = test_predictions,
              train = train_data,
              test = test_data,
              external_X = X_external,
              external_preds = external_prediction_df,
              lm_formula = formula,
              process_func = preprocess_steps))
}

blah <- model_train(50,
                    test$train,
                    test$process_func,
                    0.8,
                    test$lm_formula,
                    10,
                    external_data,
                    'target_receiving_td')

model_train <- function(n_boot, 
                        train, 
                        process_func, 
                        train_split_prop, 
                        regression_model,
                        lm_formula, 
                        n_cv, 
                        pred_ppt, 
                        target_col_regex) {
  
  boot_list <- list()
  
  for (i in 1:n_boot) {
    # Resample
    set.seed(i)
    resamp_ind <- sample(1:nrow(train), size = nrow(train), replace = TRUE)
    train_tmp <- train[resamp_ind, ]
    
    # Create indices of split
    set.seed(i)
    train_indices <- sample(nrow(train_tmp), nrow(train_tmp) * train_split_prop)
    
    # Separate out train data
    X <- train_tmp[train_indices, ] %>% 
      dplyr::select(-matches("^target")) %>% 
      data.frame()
    
    y <- train_tmp[train_indices, ] %>% 
      dplyr::select(matches(target_col_regex)) %>% 
      data.frame()
    
    # Preprocess
    X <- predict(process_func,  X)
    pred_ppt_tmp <- predict(process_func,  pred_ppt)
    
    # Recombine
    Xy_data <- cbind(X, y)
    
    # Create CV Folds
    cv_folds <- caret::trainControl(method = "cv",
                                    number = n_cv, 
                                    savePredictions = FALSE)
    
    # Create LM
    if (regression_model == 'lm') {
      lm_model <- caret::train(formula, 
                               data = Xy_data, 
                               method = regression_model, 
                               trControl = cv_folds)
    } else if (regression_model == 'poisson') {
      tuneGrid <- data.frame(method = "poisson")
      
      lm_model <- caret::train(formula, 
                               data = Xy_data, 
                               method = 'glm', 
                               trControl = cv_folds)
    }
    
    # Create Test Predictions
    preds <- data.frame(predict(lm_model$finalModel, pred_ppt_tmp, interval = 'prediction')) %>% 
      mutate(boot = i)
    
    # Add Player Identifiers
    preds <- cbind(pred_ppt_tmp[c('player_id', 'player_display_name')],
                   preds)
    
    # Combine
    # add to list
    if (length(boot_list) == 0) {
      boot_list <- list(preds)
    } else {
      boot_list <- c(boot_list, list(preds))
    }
  }
  
  # Combine into dataframe
  pred_df <- do.call(rbind, boot_list)
  
  if (regression_model == 'poisson') {
    pred_df <- pred_df %>% 
      mutate(fit = pred_df[,3])
  }
  
  # Aggregate to player
  player_est <- pred_df %>% 
    mutate(fit = ifelse(fit < 0, 0, fit),
           var = target_col_regex) %>% 
    group_by(player_id, player_display_name, var) %>% 
    summarize(mean = median(fit),
              ci_l = quantile(fit, 0.05),
              ci_u = quantile(fit, 0.95)) %>% 
    ungroup(.)
  
  
  return(player_est)
}

















