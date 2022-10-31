# Model Training and Evaluation - 

# select features and split into training, validation and test set
select_and_split <- function(df, use_combined_features = FALSE, split_fraction) {
  subset <- df %>%
    # select only kaggle "training" set
    filter(!is.na(churn)) 
  if (use_combined_features) {
    subset <- subset %>%
      select(-source, -area_code)
  } else {
    subset <- subset %>%
      select(state, account_length, international_plan:churn)
  }
  
  split_index <- initial_split(subset, prop = split_fraction, strata = churn)
  
  return(split_index)
  
}


# create general formula
create_formula <- function(df_train) {
  form <- recipe(churn ~ ., data = df_train) %>%
    step_center(all_numeric_predictors()) %>%
    step_scale(all_numeric_predictors()) %>%
    step_dummy(all_nominal_predictors()) %>%
    step_nzv(all_predictors()) 
  
  return(form)
}


# extract optimal hyperparameter combination based on grid search
extract_parameters <- function(models, workflow_name, model_name) {
  best_params <- models %>%
    extract_workflow_set_result(workflow_name) %>%
    select_best("f_meas") %>%
    mutate(type = model_name)
  
  return(best_params)
}


final_prediction <- function(model_spec, best_params, base_recipe, data_split) {
  
  final_model <- finalize_model(
    model_spec,
    best_params
  )
  
  final_results <- workflow() %>%
    add_recipe(base_recipe) %>%
    add_model(final_model) %>%
    last_fit(data_split, metrics = metric_set(f_meas))
  
  return(list(model = final_model, results = final_results))
}









