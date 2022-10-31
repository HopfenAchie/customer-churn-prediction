# Run script

source("01_packages.R")
source("02_load_and_inspect.R")
source("03_preprocessing_feature_engineering.R")
source("04_eda.R")
source("05_modeling.R")

set.seed(123)

# load data ---
joined_df_raw <- load_and_merge(folder_name = "data_raw")

# inspect data ----
inspect_dimensionality(joined_df_raw)
inspect_data_type(joined_df_raw)
inspect_missings(joined_df_raw)  # 750 missings of churn feature are expected as they're not included in test set.
inspect_unique_categorical_count(joined_df_raw)

# alternative summary function (use in future!)
skim(joined_df_raw)

# feature processing and engineering ----
joined_df_processed <- joined_df_raw %>% 
  # convert categorical features
  convert_to_factor() %>%
  # add total calls, minutes and charge
  add_totals() %>%
  # add minutes and charge per call (avg.)
  add_charge_and_minutes_per_call() %>%
  # add average minutes, number of calls and charge per month (i.e. avg. per membership length) -> do not utilize based on EDA -> calls, charge and minutes likely NOT (additively) aggregated across time
  add_utilization_and_charge_per_membership_length()
  
# EDA ----

# categorical features
plot_barplots(joined_df_processed)

# numerical features
plot_histograms(joined_df_processed)

plot_boxplots(joined_df_processed)

joined_df_processed %>%
  select(churn, starts_with(match = "avg_price")) %>%
  pivot_longer(cols = !churn, values_to = "value", names_to = "metric") %>%
  filter(!is.infinite(value) & !is.na(churn)) %>%
  ggplot(mapping = aes(x = metric, y = value, colour = churn)) +
  geom_boxplot(show.legend = TRUE) +
  facet_wrap(~metric, scales = "free") +
  theme_bw()
  

# account length vs. utilizatin (is the data really aggregated?)
plot_scatterplot(joined_df_processed)

# raw data correlations (non-significant correlations are blank)
plot_correlations(joined_df_raw,
                  filter_churn = FALSE,
                  is_raw = TRUE,
                  select_feature_subset = FALSE)

# processed data (incl. new features)
plot_correlations(joined_df_processed, 
                  filter_churn = FALSE, 
                  is_raw = FALSE, 
                  select_feature_subset = FALSE) 

# processed data (only generated features due to visibility)
plot_correlations(joined_df_processed,
                  filter_churn = FALSE,
                  is_raw = FALSE,
                  select_feature_subset = TRUE)

# Thoughts:
# 1.) Strong correlation between minutes & charge -> Idea: Retain one to avoid multicollinearity (Select model type that does not suffer from it?)
# 2.) --> Investigate pricing strategy by looking at charge = minutes * price -> price = charge/minutes
# i.) Some customers did not have either day/evening/night or international calls -> NaNs/Inf introduced during feature engineering (div by 0) -> Use total measures (e.g. average price pre total minutes to avoid this)
# 2.) Area code less informative in comparison to more granular state indicator. Differences are observed across state.
# 3.) Customers tend to churn with a higher probability if they were in the international plan.
# 4.) Customers tend to churn with a higher probability if they were not in the voice mail plan.
# 5.) Customers who churn tend to have more service calls on average (This may represent complaints)
# 6.) Although total number of calls are relatively similar on average between churners and non-churners,
#     Customers who churn tend to have longer calls measured by average total minutes, resulting in higher average charge in total
# 7.) According to IQR, we observe high amount of outliers for call numbers, minutes and charge.
# 8.) Number of calls, minutes (and charge) should be aggregated (based on naming convention). -> Hence they should depend on membership length (account length)
#     However, calls, charge and minutes are almost constant across account length.
# 9.) Calculation of charge per minutes across day, night, evening and international calls yields unexpected results
# --> Average price per minute during night and evening is substantially lower (~ 5 and 9 cents per minute respectively). Phone calls during the day are more expensive
#     (~ 17 cents per minute). International calls are (as expected) most expensive (~ 27 cents).
# --> Use avg_price_per_total_minutes as an proxy for contracting cost (Potentially compute weighted average taking differing costs per call-type into account)
# --> On average customers who churn have relatively higher average prices per minute compared to non-churners.
# --> Sanity Question: What kind of telecom contract assigns different costs-per-minute depending on the time of the day?
# --> Does using net minutes, calls and charge improve model performance in comparison to using the time-of-the-day based measures?


# split into training and test data (baseline)
set.seed(868453)
split_data <- select_and_split(joined_df_processed, use_combined_features = F, split_fraction = 3/4)
train_base <- training(split_data)

# set up 10-Fold CV set based on training data for validation ----
set.seed(42)
train_folds <- vfold_cv(train_base, v = 10, strata = churn)

# set up general formula ----
churn_recipe <- create_formula(train_base)

# set up model types (+hyperparameters to be tuned) ----

# logistic regression
log_lasso_spec<- logistic_reg(
  mode = "classification",
  engine = "glmnet",
  penalty = tune(),
  mixture = 1  # lasso penalty
)


# decision tree
decision_tree_spec <- decision_tree(
  mode = "classification",
  engine = "rpart",
  cost_complexity = tune(),
  tree_depth = tune(),
  min_n = tune()
)


# random forest
rf_spec <- rand_forest(
  mode = "classification",
  engine = "ranger",
  trees = tune(),
  mtry = tune(),
  min_n = tune()
) 


# support vector machine (linear kernal)
svm_spec <- svm_linear(
  mode = "classification",
  engine = "LiblineaR",
  cost = tune()
)


# boosted trees (xgboost)
xgb_spec <- boost_tree(
  mode = "classification",
  engine = "xgboost",
  trees = tune(),
  mtry = tune(),
  tree_depth = tune(),
  min_n = tune(),
  learn_rate = 0.01
)


# set up joined model workflow for hyperparameter tuning
set.seed(420)
doParallel::registerDoParallel()

all_models_res <- workflow_set(
  # use standard formula and preprocessing steps
  preproc = list(standard_recipe = churn_recipe),
  # specify models
  models = list(log_reg = log_lasso_spec,
                decision_tree = decision_tree_spec,
                random_forest = rf_spec,
                svm = svm_spec,
                xgb = xgb_spec),
  cross = TRUE
) %>%
  workflow_map(
    # use 10-Fold CV for validation
    resamples = train_folds,
    # auto-create grid with meaningful values based on model type
    grid = 25,
    # use f1-score as evaluation metric
    metrics = metric_set(f_meas),
    verbose = TRUE
    )

# investigate overall results
autoplot(all_models_res)

# investigate impact of hyperparameters on model performance

# logistic regression (lasso)
all_models_res %>%
  extract_workflow_set_result("standard_recipe_log_reg") %>%
  collect_metrics() %>%
  ggplot(mapping = aes(x = penalty, y = mean, colour = .metric)) +
  geom_point(show.legend = FALSE) + 
  geom_line(alpha = 0.5, size = 2, show.legend = FALSE) +
  labs(x = "Penalty Parameter (Lasso)", y = "Average F1/Accuracy across Folds", title = "Logisitic Regression (Lasso)") +
  facet_wrap(~.metric, scales = "free_y") +
  theme_bw()

best_lasso_params <- extract_parameters(all_models_res, "standard_recipe_log_reg", "Logistic Regression (Lasso)")
  
# decision tree
all_models_res %>%
  extract_workflow_set_result("standard_recipe_decision_tree") %>%
  collect_metrics() %>%
  select(cost_complexity, tree_depth, min_n, .metric, mean) %>%
  pivot_longer(!c(.metric, mean), names_to = "parameter", values_to = "parameter_value") %>%
  filter(.metric == "f_meas") %>%
  ggplot(mapping = aes(x = parameter_value, y = mean, colour = parameter)) +
  geom_point(show.legend = F) +
  geom_line(alpha = 0.5, size = 2, show.legend = FALSE) +
  labs(x = "Parameter Value", y = "Average F1 across Folds", title = "Decision Tree") +
  facet_wrap(~parameter, scales = "free") +
  theme_bw() 

best_decision_tree_params <- extract_parameters(all_models_res, "standard_recipe_decision_tree", "Decision Tree")

# random forest
all_models_res %>%
  extract_workflow_set_result("standard_recipe_random_forest") %>%
  collect_metrics() %>%
  select(mtry, trees, min_n, .metric, mean) %>%
  pivot_longer(!c(.metric, mean), names_to = "parameter", values_to = "parameter_value") %>%
  filter(.metric == "f_meas") %>%
  ggplot(mapping = aes(x = parameter_value, y = mean, colour = parameter)) +
  geom_point(show.legend = F) +
  geom_line(alpha = 0.5, size = 2, show.legend = FALSE) +
  labs(x = "Parameter Value", y = "Average F1 across Folds", title = "Random Forest") +
  facet_wrap(~parameter, scales = "free") +
  theme_bw()


best_rf_params <- extract_parameters(all_models_res, "standard_recipe_random_forest", "Random Forest")

# svm (linear kernel)
all_models_res %>%
  extract_workflow_set_result("standard_recipe_svm") %>%
  collect_metrics() %>%
  filter(.metric == "f_meas") %>%
  ggplot(mapping = aes(x = cost, y = mean, colour = "blue")) +
  geom_point(show.legend = FALSE) +
  geom_line(alpha = 0.5, size = 1.5, show.legend = FALSE) +
  labs(x = "Cost Parameter", y = "Average F1 across Folds", title = "Support Vector Machine (Linear Kernel)") +
  theme_bw() 

best_svm_params <- extract_parameters(all_models_res, "standard_recipe_svm", "Support Vector Machine (Linear Kernel)")

# xgboost
all_models_res %>%
  extract_workflow_set_result("standard_recipe_xgb") %>%
  collect_metrics() %>%
  filter(.metric == "f_meas") %>%
  select(mtry, trees, min_n, tree_depth, mean) %>%
  pivot_longer(!mean, names_to = "parameter", values_to = "parameter_value") %>%
  ggplot(mapping = aes(x = parameter_value, y = mean, colour = parameter)) +
  geom_point(show.legend = FALSE) +
  geom_line(alpha = 0.5, size = 1.5, show.legend = FALSE) +
  labs(x = "Parameter Value", y = "Average F1 across Folds", title = "XGBoost") +
  facet_wrap(~parameter, scales = "free") +
  theme_bw()

best_xgb_params <- extract_parameters(all_models_res, "standard_recipe_xgb", "XGBoost")

# fit best performing model with hyperparameter selection and evaluate on test data ----
final_log_lasso <- finalize_model(
  log_lasso_spec,
  best_lasso_params
) 

final_res_log_lasso <- workflow() %>%
  add_recipe(churn_recipe) %>%
  add_model(final_log_lasso) %>%
  last_fit(split_data, metrics = metric_set(f_meas))

#----

final_decision_tree <- finalize_model(
  decision_tree_spec,
  best_decision_tree_params
)

final_res_decision_tree <- workflow() %>%
  add_recipe(churn_recipe) %>%
  add_model(final_decision_tree) %>%
  last_fit(split_data, metrics = metric_set(f_meas))
# ----

final_rf <- finalize_model(
  rf_spec,
  best_rf_params
)

final_res_rf <- workflow() %>%
  add_recipe(churn_recipe) %>%
  add_model(final_rf) %>%
  last_fit(split_data, metrics = metric_set(f_meas))
# ----

final_svm <- finalize_model(
  svm_spec,
  best_svm_params
)

final_res_svm <- workflow() %>%
  add_recipe(churn_recipe) %>%
  add_model(final_svm) %>%
  last_fit(split_data, metrics = metric_set(f_meas))
# ----

final_xgb <- finalize_model(
  xgb_spec,
  best_xgb_params
)

final_res_xgb <- workflow() %>%
  add_recipe(churn_recipe) %>%
  add_model(final_xgb) %>%
  last_fit(split_data, metrics = metric_set(f_meas))

# Benchmark the results of the different models according to performance on test data

benchmark_results_on_test <- bind_rows(
  final_res_log_lasso %>% collect_metrics() %>% select(.estimate) %>% mutate(type = "Logistic Regression"),
  final_res_decision_tree %>% collect_metrics() %>% select(.estimate) %>% mutate(type = "CART"),
  final_res_svm %>% collect_metrics() %>% select(.estimate) %>% mutate(type = "Support Vector Machine"),
  final_res_rf %>% collect_metrics() %>% select(.estimate) %>% mutate(type = "Random Forest"),
  final_res_xgb %>% collect_metrics() %>% select(.estimate) %>% mutate(type = "XGBoost")
)

benchmark_results_on_test %>%
  ggplot(mapping = aes(x = fct_reorder(type, -.estimate), y = .estimate)) +
  geom_col() +
  geom_text(
    aes(label = round(.estimate, 4)*100),
    colour = "white", size = 3,
    vjust = 1.5, position = position_dodge(.9)
  ) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Model Type", y = "F1-Score") +
  theme_bw()
  

# Thoughts: According to the F1-score on test data, the random forest model performs best.


# Investigate variable importance and partial dependence of random forest model












