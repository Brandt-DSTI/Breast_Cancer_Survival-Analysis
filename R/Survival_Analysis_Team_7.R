# Load required libraries conditionally
required_packages <- c("survival", "survminer", "ggplot2", "dplyr", "splines", "randomForestSRC", 
                       "caret", "reshape2", "corrplot", "naniar", "broom")

new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]

if(length(new_packages)) install.packages(new_packages)

lapply(required_packages, library, character.only = TRUE)

# Data Loading - ensure data is in your working directory or modify path
data_path <- file.path(getwd(), "breast_cancer_data.csv")
data_clean <- read.csv(data_path)

# Display the structure and summary statistics of the dataset
str(data_clean)
summary(data_clean)

# Enhanced visualization for missing values
missing_data <- data_clean %>% select(where(~ any(is.na(.))))
vis_miss(missing_data) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  scale_fill_manual(values = c("skyblue", "grey50")) +
  labs(title = "Missing Data Visualization (Columns with Missing Data)", 
       x = "Features", 
       y = "Observations")

# Define a function for repeated histogram plotting
plot_histogram <- function(data, column, binwidth, title, x_label, y_label) {
  ggplot(data, aes_string(x = column)) +
    geom_histogram(binwidth = binwidth, fill = "skyblue", color = "black") +
    labs(title = title, x = x_label, y = y_label) +
    theme_minimal()
}

# Plotting histograms
plot_histogram(data_clean, "Age.at.Diagnosis", 5, "Distribution of Age at Diagnosis", "Age", "Frequency")

# Additional Data Visualizations
visualize_bar <- function(data, column, title, x_label, y_label, fill_color = "skyblue") {
  data %>%
    filter(!is.na(!!sym(column)) & !!sym(column) != "") %>%
    ggplot(aes_string(x = column)) +
    geom_bar(fill = fill_color, color = "black") +
    labs(title = title, x = x_label, y = y_label) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

visualize_bar(data_clean, "Type.of.Breast.Surgery", "Distribution of Breast Surgery Types", "Surgery Type", "Count")
visualize_bar(data_clean, "Cancer.Type", "Distribution of Cancer Types", "Cancer Type", "Count", fill_color = "lightgreen")

# Cancer Type Detailed Distribution
data_clean %>%
  filter(!is.na(Cancer.Type.Detailed) & Cancer.Type.Detailed != "") %>%
  count(Cancer.Type.Detailed) %>%
  ggplot(aes(x = reorder(Cancer.Type.Detailed, n), y = n)) +
  geom_bar(stat = "identity", fill = "pink", color = "black") +
  labs(title = "Distribution of Detailed Cancer Types", x = "Cancer Type", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

# Plotting additional histograms
plot_histogram(data_clean, "Overall.Survival..Months.", 12, "Distribution of Overall Survival Time", "Months", "Count")
visualize_bar(data_clean, "Overall.Survival.Status", "Distribution of Overall Survival Status", "Status", "Count", fill_color = "salmon")
plot_histogram(data_clean, "Relapse.Free.Status..Months.", 12, "Distribution of Relapse Free Time", "Months", "Count")
visualize_bar(data_clean, "Relapse.Free.Status", "Distribution of Relapse Free Status", "Status", "Count", fill_color = "lightyellow")
visualize_bar(data_clean, "ER.Status", "Distribution of ER Status", "Status", "Count", fill_color = "lightyellow")

# Data Cleansing
important_vars <- c("Age.at.Diagnosis", "ER.Status", "PR.Status", "HER2.Status", 
                    "Overall.Survival..Months.", "Overall.Survival.Status", "Cancer.Type",
                    "Neoplasm.Histologic.Grade", "Lymph.nodes.examined.positive")

data_analysis <- data_clean[complete.cases(data_clean[important_vars]), ]

# Convert categorical variables to factors
data_analysis <- data_analysis %>%
  mutate(across(c(ER.Status, PR.Status, HER2.Status, Neoplasm.Histologic.Grade), as.factor)) %>%
  mutate(Overall.Survival.Status = ifelse(Overall.Survival.Status == "Deceased", 1, 0))

# Feature Correlation Heatmap
numeric_vars <- data_analysis %>% select(where(is.numeric))
cor_matrix <- cor(numeric_vars, use = "complete.obs")

corrplot(cor_matrix, method = "color", type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, tl.cex = 0.8, mar=c(0,0,1,0))

# Survival Curves by Key Factors
fit_er <- survfit(Surv(Overall.Survival..Months., Overall.Survival.Status) ~ ER.Status, data = data_analysis)
ggsurvplot(fit_er, data = data_analysis, pval = TRUE, risk.table = TRUE, 
           title = "Survival Curves by ER Status", palette = c("#E7B800", "#2E9FDF"))

fit_grade <- survfit(Surv(Overall.Survival..Months., Overall.Survival.Status) ~ Neoplasm.Histologic.Grade, data = data_analysis)
ggsurvplot(fit_grade, data = data_analysis, pval = TRUE, risk.table = TRUE, 
           title = "Survival Curves by Neoplasm Histologic Grade", 
           palette = c("#00AFBB", "#E7B800", "#FC4E07"))

# Cox Proportional Hazards Model
cox_model <- coxph(Surv(Overall.Survival..Months., Overall.Survival.Status) ~ 
                     Age.at.Diagnosis + ER.Status + 
                     Neoplasm.Histologic.Grade + Lymph.nodes.examined.positive, 
                   data = data_analysis)
summary(cox_model)

# Check Proportional Hazards Assumption
ph_test <- cox.zph(cox_model)
print(ph_test)
plot(ph_test, col = c("red", "green", "blue", "purple"))

# Stratified Cox Model
strat_model <- coxph(Surv(Overall.Survival..Months., Overall.Survival.Status) ~ 
                       Age.at.Diagnosis + Neoplasm.Histologic.Grade + 
                       Lymph.nodes.examined.positive + strata(ER.Status), 
                     data = data_analysis)
summary(strat_model)

# Time-dependent Cox Model
td_model <- coxph(Surv(Overall.Survival..Months., Overall.Survival.Status) ~ 
                    Age.at.Diagnosis + ER.Status + Neoplasm.Histologic.Grade + 
                    Lymph.nodes.examined.positive + 
                    tt(Age.at.Diagnosis) + tt(Lymph.nodes.examined.positive), 
                  data = data_analysis)
summary(td_model)

# Random Survival Forest
rsf_model <- rfsrc(Surv(Overall.Survival..Months., Overall.Survival.Status) ~ 
                     Age.at.Diagnosis + ER.Status + Neoplasm.Histologic.Grade + 
                     Lymph.nodes.examined.positive, 
                   data = data_analysis, ntree = 1000)
print(rsf_model)

# Variable importance for RSF
vimp <- vimp(rsf_model)
print(vimp$importance)

# Custom Forest Plot of Hazard Ratios
model_summary <- tidy(strat_model, exponentiate = TRUE, conf.int = TRUE)

ggplot(model_summary, aes(y = term, x = estimate, xmin = conf.low, xmax = conf.high)) +
  geom_point(color = "blue") +
  geom_errorbarh(height = 0.2, color = "blue") +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  scale_x_log10() +
  labs(x = "Hazard Ratio (95% CI)", y = "Variables",
       title = "Forest Plot of Hazard Ratios") +
  theme_minimal() +
  theme(axis.text.y = element_text(hjust = 0))

# Partial Effect Plots
ggplot(data_analysis, aes(x = Age.at.Diagnosis, y = predict(strat_model, type = "risk"))) +
  geom_smooth(color = "blue", fill = "lightblue") +
  labs(title = "Partial Effect of Age at Diagnosis on Hazard",
       x = "Age at Diagnosis", y = "Predicted Hazard")

ggplot(data_analysis, aes(x = Lymph.nodes.examined.positive, y = predict(strat_model, type = "risk"))) +
  geom_smooth(color = "green", fill = "lightgreen") +
  labs(title = "Partial Effect of Positive Lymph Nodes on Hazard",
       x = "Number of Positive Lymph Nodes", y = "Predicted Hazard")

# Boxplots of Continuous Variables by Survival Status
ggplot(data_analysis, aes(x = factor(Overall.Survival.Status), y = Age.at.Diagnosis)) +
  geom_boxplot(fill = c("#00AFBB", "#FC4E07")) +
  labs(title = "Age at Diagnosis by Survival Status",
       x = "Survival Status (0 = Survived, 1 = Deceased)", y = "Age at Diagnosis")

ggplot(data_analysis, aes(x = factor(Overall.Survival.Status), y = Lymph.nodes.examined.positive)) +
  geom_boxplot(fill = c("#00AFBB", "#FC4E07")) +
  labs(title = "Positive Lymph Nodes by Survival Status",
       x = "Survival Status (0 = Survived, 1 = Deceased)", y = "Number of Positive Lymph Nodes")

# Scatter Plot Matrix
pairs(~ Age.at.Diagnosis + Lymph.nodes.examined.positive + Overall.Survival..Months., data = data_analysis,
      main = "Scatter Plot Matrix of Key Continuous Variables", col = "blue")

# Variable Importance Plot from Random Survival Forest
plot(vimp(rsf_model), col = "blue")

# Residuals Analysis
mart_res <- residuals(cox_model, type="martingale")
plot(predict(cox_model), mart_res, 
     xlab="Linear Predictor", ylab="Martingale Residuals",
     main="Martingale Residuals vs. Linear Predictor", col = "blue")
abline(h=0, col="red")

dev_res <- residuals(cox_model, type="deviance")
plot(predict(cox_model), dev_res,
     xlab="Linear Predictor", ylab="Deviance Residuals",
     main="Deviance Residuals vs. Linear Predictor", col = "blue")
abline(h=0, col="red")

# Model Selection and Validation
# Compare AIC
AIC(cox_model, strat_model, td_model)

# Cross-validation Function
perform_cv <- function(model_formula, data, k=5, stratified=FALSE) {
  set.seed(123)
  folds <- createFolds(data$Overall.Survival.Status, k = k, list = TRUE)
  
  cv_results <- numeric(length(folds))
  for (i in seq_along(folds)) {
    test_indices <- folds[[i]]
    train_data <- data[-test_indices, ]
    test_data <- data[test_indices, ]
    
    if (stratified) {
      predictions <- numeric(nrow(test_data))
      for (status in levels(data$ER.Status)) {
        train_subset <- train_data[train_data$ER.Status == status, ]
        test_subset <- test_data[test_data$ER.Status == status, ]
        if (nrow(train_subset) > 0 && nrow(test_subset) > 0) {
          model <- coxph(model_formula, data = train_subset)
          predictions[test_data$ER.Status == status] <- predict(model, newdata = test_subset, type = "risk")
        }
      }
    } else {
      model <- coxph(model_formula, data = train_data)
      predictions <- predict(model, newdata = test_data, type = "risk")
    }
    
    actual_survival <- Surv(test_data$Overall.Survival..Months., test_data$Overall.Survival.Status)
    cv_results[i] <- concordance(actual_survival ~ predictions)$concordance
  }
  
  return(list(c_index = cv_results, mean = mean(cv_results), sd = sd(cv_results)))
}

# Perform cross-validation for each model
cv_cox <- perform_cv(Surv(Overall.Survival..Months., Overall.Survival.Status) ~ 
                       Age.at.Diagnosis + 
                       Neoplasm.Histologic.Grade + Lymph.nodes.examined.positive, 
                     data_analysis)

cv_strat <- perform_cv(Surv(Overall.Survival..Months., Overall.Survival.Status) ~ 
                         Age.at.Diagnosis + Neoplasm.Histologic.Grade + 
                         Lymph.nodes.examined.positive, 
                       data_analysis, stratified=TRUE)

cv_td <- perform_cv(Surv(Overall.Survival..Months., Overall.Survival.Status) ~ 
                      Age.at.Diagnosis + ER.Status + Neoplasm.Histologic.Grade + 
                      Lymph.nodes.examined.positive + 
                      tt(Age.at.Diagnosis) + tt(Lymph.nodes.examined.positive), 
                    data_analysis)

# Print cross-validation results
cv_results <- list(Cox = cv_cox, Stratified = cv_strat, TimeDep = cv_td)
print(cv_results)

# Cross-validation Visualizations
cv_results_df <- data.frame(
  Cox = cv_cox$c_index,
  Stratified = cv_strat$c_index,
  TimeDep = cv_td$c_index
)

cv_results_melted <- melt(cv_results_df)

# Boxplot of C-index values
ggplot(cv_results_melted, aes(x = variable, y = value)) +
  geom_boxplot(fill = "lightblue", color = "blue") +
  labs(title = "Distribution of C-index Values Across Models",
       x = "Model", y = "C-index") +
  theme_minimal()

# Bar plot of mean C-index values with error bars
mean_cv <- data.frame(
  Model = c("Cox", "Stratified", "TimeDep"),
  Mean = c(cv_cox$mean, cv_strat$mean, cv_td$mean),
  SD = c(cv_cox$sd, cv_strat$sd, cv_td$sd)
)

ggplot(mean_cv, aes(x = Model, y = Mean, fill = Model)) +
  geom_bar(stat = "identity", color = "blue") +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.2, color = "blue") +
  labs(title = "Mean C-index Values with Standard Deviation",
       x = "Model", y = "Mean C-index") +
  theme_minimal()

# Density plot of C-index distributions
ggplot(cv_results_melted, aes(x = value, fill = variable)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of C-index Distributions",
       x = "C-index", y = "Density") +
  theme_minimal()

# Print summary statistics
print(summary(cv_results_df))

# Final Model Selection and Interpretation
final_model <- strat_model
summary(final_model)

# Visualize the effect of a continuous variable
ggplot(data_analysis, aes(x = Age.at.Diagnosis, y = predict(final_model, type = "risk"))) +
  geom_smooth(color = "blue", fill = "lightblue") +
  labs(title = "Effect of Age at Diagnosis on Hazard",
       x = "Age at Diagnosis", y = "Predicted Hazard")

# Additional interpretation plots
ggsurvplot(
  survfit(final_model, data = data_analysis, 
          newdata = data.frame(Age.at.Diagnosis = 60,
                               Neoplasm.Histologic.Grade = c("1", "2", "3"),
                               Lymph.nodes.examined.positive = 0,
                               ER.Status = "Positive")),
  data = data_analysis,
  title = "Survival Curves by Neoplasm Histologic Grade",
  legend.title = "Grade",
  risk.table = TRUE,
  palette = c("#00AFBB", "#E7B800", "#FC4E07")
)

ggplot(data_analysis, aes(x = Lymph.nodes.examined.positive, y = predict(final_model, type = "risk"))) +
  geom_smooth(color = "green", fill = "lightgreen") +
  labs(title = "Effect of Positive Lymph Nodes on Hazard",
       x = "Number of Positive Lymph Nodes", y = "Predicted Hazard")

# Interaction plot
interaction_model <- coxph(Surv(Overall.Survival..Months., Overall.Survival.Status) ~ 
                             Age.at.Diagnosis * Lymph.nodes.examined.positive + 
                             Neoplasm.Histologic.Grade + strata(ER.Status), 
                           data = data_analysis)

summary(interaction_model)

if (summary(interaction_model)$coefficients["Age.at.Diagnosis:Lymph.nodes.examined.positive", "Pr(>|z|)"] < 0.05) {
  age_range <- seq(min(data_analysis$Age.at.Diagnosis), max(data_analysis$Age.at.Diagnosis), length.out = 100)
  lymph_range <- c(0, 5, 10, 15, 20)
  grid <- expand.grid(Age.at.Diagnosis = age_range, 
                      Lymph.nodes.examined.positive = lymph_range,
                      Neoplasm.Histologic.Grade = "2",
                      ER.Status = "Positive")
  
  grid$hazard <- predict(interaction_model, newdata = grid, type = "risk")
  
  ggplot(grid, aes(x = Age.at.Diagnosis, y = hazard, color = factor(Lymph.nodes.examined.positive))) +
    geom_line(linewidth = 1) +
    scale_color_viridis_d(name = "Positive\nLymph Nodes") +
    labs(title = "Interaction Effect of Age and Positive Lymph Nodes on Hazard",
         x = "Age at Diagnosis", y = "Predicted Hazard") +
    theme_minimal() +
    theme(legend.position = "right",
          plot.title = element_text(hjust = 0.5, face = "bold")) +
    scale_x_continuous(breaks = seq(20, 100, by = 10)) +
    scale_y_continuous(labels = scales::comma)
}

# Final model performance metrics
concordance(final_model)

