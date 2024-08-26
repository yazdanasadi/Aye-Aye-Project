library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(caret)
library(xgboost)
library(randomForest)    
data <- read_csv("C:/Users/yazda/Desktop/AyeAye2024/Aye-Aye-Project/data/LEK/LEK_data_grouped.csv")

data <- data %>%
  mutate(edu.level = replace_na(edu.level, "NoEdu"))

selected_columns_dm <- data %>%
  select(Clove.benefit.score, Region, Gender, Age,
         edu.level, Ethnic.group, Migration.Experience, Farms.Cloves,
         encountered.binary, ID.vis.Aye.aye, ID.nom,
         Knowledge.non.benefit.score.max.6, DK.score,
         Diet.Insects, Knowledge.signs, Remoteness, village.size,
         School, Forest.cover2017, Forest.cover1990, Forestloss_total,
         Forestloss_relative, Farms.Paddy.Rice, Farms.Tavy.Rice,
         Extracts.Medical.plants, FES.provisioning, FES.regulating,
         FES.cultural, FES.supporting, ID.vis.Indri, ID.vis.Microcebus,
         Aye.aye.protected)

label_encode_columns <- function(df) {
  df <- df %>% 
    mutate(across(where(is.character), ~as.integer(factor(.x))))
  return(df)
}


calculate_and_plot_correlations <- function(d, target_col, top_n = 10) {
  d <- label_encode_columns(d)
  
  X <- d %>% select(-all_of(target_col))
  y <- d[[target_col]]
  
  # Standardize the Data
  X_scaled <- as.data.frame(scale(X))
  
  # Calculate Pearson Correlation for each feature with the target
  correlations <- sapply(X_scaled, function(col) cor(col, y, use = "complete.obs"))
  
  # Convert to DataFrame and sort by absolute correlation value
  corr_df <- data.frame(Feature = names(correlations), 
                        Correlation = correlations) %>%
    mutate(AbsCorrelation = abs(Correlation)) %>%
    arrange(desc(AbsCorrelation)) %>%
    head(top_n)
  
  # Plot the top_n features
  ggplot(corr_df, aes(x = Correlation, y = reorder(Feature, Correlation))) +
    geom_bar(stat = "identity", fill = "viridis") +
    ggtitle(paste("Top", top_n, "Features by Pearson Correlation with", target_col)) +
    theme_minimal()
  
  return(corr_df$Feature)
}

# Feature Selection with Top Features
feature_selection_with_top_features <- function(d, target_col, top_features, n_features = 8) {
  d <- label_encode_columns(d)
  
  X <- d %>% select(all_of(top_features))
  y <- d[[target_col]]
  
  # Standardize the Data
  X_scaled <- as.data.frame(scale(X))
  
  results <- list()
  
  # XGBoost
  xgb_model <- xgboost(data = as.matrix(X_scaled), label = y, nrounds = 100, objective = "reg:squarederror", verbose = FALSE)
  xgb_importance <- xgb.importance(model = xgb_model)
  results[["XGBoost"]] <- list(Feature = xgb_importance$Feature, Importance = xgb_importance$Gain)
  
  # Random Forest
  rf_model <- randomForest(X_scaled, y, ntree = 100)
  rf_importance <- data.frame(Feature = names(rf_model$importance), Importance = rf_model$importance[,1]) %>%
    arrange(desc(Importance))
  results[["Random Forest"]] <- list(Feature = rf_importance$Feature, Importance = rf_importance$Importance)
  
  # Logistic Regression
  lr_model <- glm(y ~ ., data = cbind(X_scaled, y), family = binomial(link = "logit"))
  lr_importance <- data.frame(Feature = names(coef(lr_model)), Importance = abs(coef(lr_model))) %>%
    arrange(desc(Importance))
  results[["Logistic Regression"]] <- list(Feature = lr_importance$Feature, Importance = lr_importance$Importance)
  
  # Plot the selected features for each method
  for (method in names(results)) {
    importance_df <- data.frame(Feature = results[[method]]$Feature, 
                                Importance = results[[method]]$Importance)
    ggplot(importance_df, aes(x = Importance, y = reorder(Feature, Importance))) +
      geom_bar(stat = "identity", fill = "muted") +
      ggtitle(paste("Selected Features by", method)) +
      theme_minimal() +
      xlab("Importance") +
      ylab("Feature") +
      print()
  }
  
  return(results)
}
top_features <- calculate_and_plot_correlations(selected_columns_dm, target_col = "Clove.benefit.score", top_n = 10)

