# customer-churn-prediction
This repository includes the code for a project focused on customer churn prediction. The data was initially offered within a Kaggle competition, hosted in the year 2020.

The project goal is to create a model that is able to predict whethere a customer churns based on historical data.
Given the imbalance of the target feature (85.93% churn = no; 14.07% churn = yes), accuracy is not a valid evaluation metric (assuming no resampling is performed to account for imbalanced data). Thus, the core evaluation metric applied to this binary classification problem is F1-Score. (Other may be investigate during the workflow)

The different model algorithms that are applied are as follows:

- Logistic Regression
- Decision Trees
- Random Forrests
- KNN
- XGBoosted Trees
- SVM

Although project goal aims at predictive accuracy (vs. inference), less complex models such as Logistic Regression and Decision Trees, may be used to increase knowledge about the drivers of customer churn behaviour.

The project steps are as follows:

- Exploratory Data Analysis
- Data Inspection and Transformation
- Feature Engineering
- Feature Selection
- Model Training
- Model Evaluation
- Optional: Apply model stacking and observe potential performance increase (using the R-package stacks)

In addition to the above mentioned project steps, the entire project will be conducted using R in order to gain additional knowledge and experience in the tidymodels workflow. 


