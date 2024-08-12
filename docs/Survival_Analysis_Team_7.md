Survival_Analysis_Team_7
================
Team 7
2024-08-11

Below is the code to accompany the Survival Analysis Team 7 Report.
Note: Not all of the below visulisations were included in the final
report.

``` r
# Load required libraries conditionally
required_packages <- c("survival", "survminer", "ggplot2", "dplyr", "splines", "randomForestSRC", 
                       "caret", "reshape2", "corrplot", "naniar", "broom")

new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]

if(length(new_packages)) install.packages(new_packages)

lapply(required_packages, library, character.only = TRUE)
```

    ## [[1]]
    ##  [1] "broom"           "naniar"          "corrplot"        "reshape2"       
    ##  [5] "caret"           "lattice"         "randomForestSRC" "splines"        
    ##  [9] "dplyr"           "survminer"       "ggpubr"          "ggplot2"        
    ## [13] "survival"        "MatrixModels"    "Matrix"          "stats"          
    ## [17] "graphics"        "grDevices"       "datasets"        "utils"          
    ## [21] "methods"         "base"           
    ## 
    ## [[2]]
    ##  [1] "broom"           "naniar"          "corrplot"        "reshape2"       
    ##  [5] "caret"           "lattice"         "randomForestSRC" "splines"        
    ##  [9] "dplyr"           "survminer"       "ggpubr"          "ggplot2"        
    ## [13] "survival"        "MatrixModels"    "Matrix"          "stats"          
    ## [17] "graphics"        "grDevices"       "datasets"        "utils"          
    ## [21] "methods"         "base"           
    ## 
    ## [[3]]
    ##  [1] "broom"           "naniar"          "corrplot"        "reshape2"       
    ##  [5] "caret"           "lattice"         "randomForestSRC" "splines"        
    ##  [9] "dplyr"           "survminer"       "ggpubr"          "ggplot2"        
    ## [13] "survival"        "MatrixModels"    "Matrix"          "stats"          
    ## [17] "graphics"        "grDevices"       "datasets"        "utils"          
    ## [21] "methods"         "base"           
    ## 
    ## [[4]]
    ##  [1] "broom"           "naniar"          "corrplot"        "reshape2"       
    ##  [5] "caret"           "lattice"         "randomForestSRC" "splines"        
    ##  [9] "dplyr"           "survminer"       "ggpubr"          "ggplot2"        
    ## [13] "survival"        "MatrixModels"    "Matrix"          "stats"          
    ## [17] "graphics"        "grDevices"       "datasets"        "utils"          
    ## [21] "methods"         "base"           
    ## 
    ## [[5]]
    ##  [1] "broom"           "naniar"          "corrplot"        "reshape2"       
    ##  [5] "caret"           "lattice"         "randomForestSRC" "splines"        
    ##  [9] "dplyr"           "survminer"       "ggpubr"          "ggplot2"        
    ## [13] "survival"        "MatrixModels"    "Matrix"          "stats"          
    ## [17] "graphics"        "grDevices"       "datasets"        "utils"          
    ## [21] "methods"         "base"           
    ## 
    ## [[6]]
    ##  [1] "broom"           "naniar"          "corrplot"        "reshape2"       
    ##  [5] "caret"           "lattice"         "randomForestSRC" "splines"        
    ##  [9] "dplyr"           "survminer"       "ggpubr"          "ggplot2"        
    ## [13] "survival"        "MatrixModels"    "Matrix"          "stats"          
    ## [17] "graphics"        "grDevices"       "datasets"        "utils"          
    ## [21] "methods"         "base"           
    ## 
    ## [[7]]
    ##  [1] "broom"           "naniar"          "corrplot"        "reshape2"       
    ##  [5] "caret"           "lattice"         "randomForestSRC" "splines"        
    ##  [9] "dplyr"           "survminer"       "ggpubr"          "ggplot2"        
    ## [13] "survival"        "MatrixModels"    "Matrix"          "stats"          
    ## [17] "graphics"        "grDevices"       "datasets"        "utils"          
    ## [21] "methods"         "base"           
    ## 
    ## [[8]]
    ##  [1] "broom"           "naniar"          "corrplot"        "reshape2"       
    ##  [5] "caret"           "lattice"         "randomForestSRC" "splines"        
    ##  [9] "dplyr"           "survminer"       "ggpubr"          "ggplot2"        
    ## [13] "survival"        "MatrixModels"    "Matrix"          "stats"          
    ## [17] "graphics"        "grDevices"       "datasets"        "utils"          
    ## [21] "methods"         "base"           
    ## 
    ## [[9]]
    ##  [1] "broom"           "naniar"          "corrplot"        "reshape2"       
    ##  [5] "caret"           "lattice"         "randomForestSRC" "splines"        
    ##  [9] "dplyr"           "survminer"       "ggpubr"          "ggplot2"        
    ## [13] "survival"        "MatrixModels"    "Matrix"          "stats"          
    ## [17] "graphics"        "grDevices"       "datasets"        "utils"          
    ## [21] "methods"         "base"           
    ## 
    ## [[10]]
    ##  [1] "broom"           "naniar"          "corrplot"        "reshape2"       
    ##  [5] "caret"           "lattice"         "randomForestSRC" "splines"        
    ##  [9] "dplyr"           "survminer"       "ggpubr"          "ggplot2"        
    ## [13] "survival"        "MatrixModels"    "Matrix"          "stats"          
    ## [17] "graphics"        "grDevices"       "datasets"        "utils"          
    ## [21] "methods"         "base"           
    ## 
    ## [[11]]
    ##  [1] "broom"           "naniar"          "corrplot"        "reshape2"       
    ##  [5] "caret"           "lattice"         "randomForestSRC" "splines"        
    ##  [9] "dplyr"           "survminer"       "ggpubr"          "ggplot2"        
    ## [13] "survival"        "MatrixModels"    "Matrix"          "stats"          
    ## [17] "graphics"        "grDevices"       "datasets"        "utils"          
    ## [21] "methods"         "base"

``` r
# Data Loading - ensure data is in your working directory or modify path
data_path <- file.path(getwd(), "breast_cancer_data.csv")
data_clean <- read.csv(data_path)

# Display the structure and summary statistics of the dataset
str(data_clean)
```

    ## 'data.frame':    2509 obs. of  34 variables:
    ##  $ Patient.ID                    : chr  "MB-0000" "MB-0002" "MB-0005" "MB-0006" ...
    ##  $ Age.at.Diagnosis              : num  75.7 43.2 48.9 47.7 77 ...
    ##  $ Type.of.Breast.Surgery        : chr  "Mastectomy" "Breast Conserving" "Mastectomy" "Mastectomy" ...
    ##  $ Cancer.Type                   : chr  "Breast Cancer" "Breast Cancer" "Breast Cancer" "Breast Cancer" ...
    ##  $ Cancer.Type.Detailed          : chr  "Breast Invasive Ductal Carcinoma" "Breast Invasive Ductal Carcinoma" "Breast Invasive Ductal Carcinoma" "Breast Mixed Ductal and Lobular Carcinoma" ...
    ##  $ Cellularity                   : chr  "" "High" "High" "Moderate" ...
    ##  $ Chemotherapy                  : chr  "No" "No" "Yes" "Yes" ...
    ##  $ Pam50...Claudin.low.subtype   : chr  "claudin-low" "LumA" "LumB" "LumB" ...
    ##  $ Cohort                        : num  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ ER.status.measured.by.IHC     : chr  "Positve" "Positve" "Positve" "Positve" ...
    ##  $ ER.Status                     : chr  "Positive" "Positive" "Positive" "Positive" ...
    ##  $ Neoplasm.Histologic.Grade     : num  3 3 2 2 3 3 2 3 2 3 ...
    ##  $ HER2.status.measured.by.SNP6  : chr  "Neutral" "Neutral" "Neutral" "Neutral" ...
    ##  $ HER2.Status                   : chr  "Negative" "Negative" "Negative" "Negative" ...
    ##  $ Tumor.Other.Histologic.Subtype: chr  "Ductal/NST" "Ductal/NST" "Ductal/NST" "Mixed" ...
    ##  $ Hormone.Therapy               : chr  "Yes" "Yes" "Yes" "Yes" ...
    ##  $ Inferred.Menopausal.State     : chr  "Post" "Pre" "Pre" "Pre" ...
    ##  $ Integrative.Cluster           : chr  "4ER+" "4ER+" "3" "9" ...
    ##  $ Primary.Tumor.Laterality      : chr  "Right" "Right" "Right" "Right" ...
    ##  $ Lymph.nodes.examined.positive : num  10 0 1 3 8 0 1 NA 1 11 ...
    ##  $ Mutation.Count                : num  NA 2 2 1 2 4 4 NA 1 5 ...
    ##  $ Nottingham.prognostic.index   : num  6.04 4.02 4.03 4.05 6.08 ...
    ##  $ Oncotree.Code                 : chr  "IDC" "IDC" "IDC" "MDLC" ...
    ##  $ Overall.Survival..Months.     : num  140.5 84.6 163.7 164.9 41.4 ...
    ##  $ Overall.Survival.Status       : chr  "Living" "Living" "Deceased" "Living" ...
    ##  $ PR.Status                     : chr  "Negative" "Positive" "Positive" "Positive" ...
    ##  $ Radio.Therapy                 : chr  "Yes" "Yes" "No" "Yes" ...
    ##  $ Relapse.Free.Status..Months.  : num  138.7 83.5 151.3 162.8 18.6 ...
    ##  $ Relapse.Free.Status           : chr  "Not Recurred" "Not Recurred" "Recurred" "Not Recurred" ...
    ##  $ Sex                           : chr  "Female" "Female" "Female" "Female" ...
    ##  $ X3.Gene.classifier.subtype    : chr  "ER-/HER2-" "ER+/HER2- High Prolif" "" "" ...
    ##  $ Tumor.Size                    : num  22 10 15 25 40 31 10 65 29 34 ...
    ##  $ Tumor.Stage                   : num  2 1 2 2 2 4 2 3 2 2 ...
    ##  $ Patient.s.Vital.Status        : chr  "Living" "Living" "Died of Disease" "Living" ...

``` r
summary(data_clean)
```

    ##   Patient.ID        Age.at.Diagnosis Type.of.Breast.Surgery Cancer.Type       
    ##  Length:2509        Min.   :21.93    Length:2509            Length:2509       
    ##  Class :character   1st Qu.:50.92    Class :character       Class :character  
    ##  Mode  :character   Median :61.11    Mode  :character       Mode  :character  
    ##                     Mean   :60.42                                             
    ##                     3rd Qu.:70.00                                             
    ##                     Max.   :96.29                                             
    ##                     NA's   :11                                                
    ##  Cancer.Type.Detailed Cellularity        Chemotherapy      
    ##  Length:2509          Length:2509        Length:2509       
    ##  Class :character     Class :character   Class :character  
    ##  Mode  :character     Mode  :character   Mode  :character  
    ##                                                            
    ##                                                            
    ##                                                            
    ##                                                            
    ##  Pam50...Claudin.low.subtype     Cohort    ER.status.measured.by.IHC
    ##  Length:2509                 Min.   :1.0   Length:2509              
    ##  Class :character            1st Qu.:1.0   Class :character         
    ##  Mode  :character            Median :3.0   Mode  :character         
    ##                              Mean   :2.9                            
    ##                              3rd Qu.:4.0                            
    ##                              Max.   :9.0                            
    ##                              NA's   :11                             
    ##   ER.Status         Neoplasm.Histologic.Grade HER2.status.measured.by.SNP6
    ##  Length:2509        Min.   :1.000             Length:2509                 
    ##  Class :character   1st Qu.:2.000             Class :character            
    ##  Mode  :character   Median :3.000             Mode  :character            
    ##                     Mean   :2.412                                         
    ##                     3rd Qu.:3.000                                         
    ##                     Max.   :3.000                                         
    ##                     NA's   :121                                           
    ##  HER2.Status        Tumor.Other.Histologic.Subtype Hormone.Therapy   
    ##  Length:2509        Length:2509                    Length:2509       
    ##  Class :character   Class :character               Class :character  
    ##  Mode  :character   Mode  :character               Mode  :character  
    ##                                                                      
    ##                                                                      
    ##                                                                      
    ##                                                                      
    ##  Inferred.Menopausal.State Integrative.Cluster Primary.Tumor.Laterality
    ##  Length:2509               Length:2509         Length:2509             
    ##  Class :character          Class :character    Class :character        
    ##  Mode  :character          Mode  :character    Mode  :character        
    ##                                                                        
    ##                                                                        
    ##                                                                        
    ##                                                                        
    ##  Lymph.nodes.examined.positive Mutation.Count   Nottingham.prognostic.index
    ##  Min.   : 0.000                Min.   : 1.000   Min.   :1.000              
    ##  1st Qu.: 0.000                1st Qu.: 3.000   1st Qu.:3.048              
    ##  Median : 0.000                Median : 5.000   Median :4.044              
    ##  Mean   : 1.951                Mean   : 5.579   Mean   :4.029              
    ##  3rd Qu.: 2.000                3rd Qu.: 7.000   3rd Qu.:5.040              
    ##  Max.   :45.000                Max.   :80.000   Max.   :7.200              
    ##  NA's   :266                   NA's   :152      NA's   :222                
    ##  Oncotree.Code      Overall.Survival..Months. Overall.Survival.Status
    ##  Length:2509        Min.   :  0.00            Length:2509            
    ##  Class :character   1st Qu.: 60.87            Class :character       
    ##  Mode  :character   Median :116.47            Mode  :character       
    ##                     Mean   :125.24                                   
    ##                     3rd Qu.:185.13                                   
    ##                     Max.   :355.20                                   
    ##                     NA's   :528                                      
    ##   PR.Status         Radio.Therapy      Relapse.Free.Status..Months.
    ##  Length:2509        Length:2509        Min.   :  0.00              
    ##  Class :character   Class :character   1st Qu.: 40.56              
    ##  Mode  :character   Mode  :character   Median : 99.09              
    ##                                        Mean   :108.84              
    ##                                        3rd Qu.:167.64              
    ##                                        Max.   :384.21              
    ##                                        NA's   :121                 
    ##  Relapse.Free.Status     Sex            X3.Gene.classifier.subtype
    ##  Length:2509         Length:2509        Length:2509               
    ##  Class :character    Class :character   Class :character          
    ##  Mode  :character    Mode  :character   Mode  :character          
    ##                                                                   
    ##                                                                   
    ##                                                                   
    ##                                                                   
    ##    Tumor.Size      Tumor.Stage    Patient.s.Vital.Status
    ##  Min.   :  1.00   Min.   :0.000   Length:2509           
    ##  1st Qu.: 17.00   1st Qu.:1.000   Class :character      
    ##  Median : 22.41   Median :2.000   Mode  :character      
    ##  Mean   : 26.22   Mean   :1.714                         
    ##  3rd Qu.: 30.00   3rd Qu.:2.000                         
    ##  Max.   :182.00   Max.   :4.000                         
    ##  NA's   :149      NA's   :721

``` r
# Enhanced visualization for missing values
missing_data <- data_clean %>% select(where(~ any(is.na(.))))
vis_miss(missing_data) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  scale_fill_manual(values = c("skyblue", "grey50")) +
  labs(title = "Missing Data Visualization (Columns with Missing Data)", 
       x = "Features", 
       y = "Observations")
```

![](Survival_Analysis_Team_7_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
# Define a function for repeated histogram plotting
plot_histogram <- function(data, column, binwidth, title, x_label, y_label) {
  ggplot(data, aes_string(x = column)) +
    geom_histogram(binwidth = binwidth, fill = "skyblue", color = "black") +
    labs(title = title, x = x_label, y = y_label) +
    theme_minimal()
}

# Plotting histograms
plot_histogram(data_clean, "Age.at.Diagnosis", 5, "Distribution of Age at Diagnosis", "Age", "Frequency")
```

![](Survival_Analysis_Team_7_files/figure-gfm/unnamed-chunk-1-2.png)<!-- -->

``` r
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
```

![](Survival_Analysis_Team_7_files/figure-gfm/unnamed-chunk-1-3.png)<!-- -->

``` r
visualize_bar(data_clean, "Cancer.Type", "Distribution of Cancer Types", "Cancer Type", "Count", fill_color = "lightgreen")
```

![](Survival_Analysis_Team_7_files/figure-gfm/unnamed-chunk-1-4.png)<!-- -->

``` r
# Cancer Type Detailed Distribution
data_clean %>%
  filter(!is.na(Cancer.Type.Detailed) & Cancer.Type.Detailed != "") %>%
  count(Cancer.Type.Detailed) %>%
  ggplot(aes(x = reorder(Cancer.Type.Detailed, n), y = n)) +
  geom_bar(stat = "identity", fill = "pink", color = "black") +
  labs(title = "Distribution of Detailed Cancer Types", x = "Cancer Type", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
```

![](Survival_Analysis_Team_7_files/figure-gfm/unnamed-chunk-1-5.png)<!-- -->

``` r
# Plotting additional histograms
plot_histogram(data_clean, "Overall.Survival..Months.", 12, "Distribution of Overall Survival Time", "Months", "Count")
```

![](Survival_Analysis_Team_7_files/figure-gfm/unnamed-chunk-1-6.png)<!-- -->

``` r
visualize_bar(data_clean, "Overall.Survival.Status", "Distribution of Overall Survival Status", "Status", "Count", fill_color = "salmon")
```

![](Survival_Analysis_Team_7_files/figure-gfm/unnamed-chunk-1-7.png)<!-- -->

``` r
plot_histogram(data_clean, "Relapse.Free.Status..Months.", 12, "Distribution of Relapse Free Time", "Months", "Count")
```

![](Survival_Analysis_Team_7_files/figure-gfm/unnamed-chunk-1-8.png)<!-- -->

``` r
visualize_bar(data_clean, "Relapse.Free.Status", "Distribution of Relapse Free Status", "Status", "Count", fill_color = "lightyellow")
```

![](Survival_Analysis_Team_7_files/figure-gfm/unnamed-chunk-1-9.png)<!-- -->

``` r
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
```

![](Survival_Analysis_Team_7_files/figure-gfm/unnamed-chunk-1-10.png)<!-- -->

``` r
# Survival Curves by Key Factors
fit_er <- survfit(Surv(Overall.Survival..Months., Overall.Survival.Status) ~ ER.Status, data = data_analysis)
ggsurvplot(fit_er, data = data_analysis, pval = TRUE, risk.table = TRUE, 
           title = "Survival Curves by ER Status", palette = c("#E7B800", "#2E9FDF"))
```

![](Survival_Analysis_Team_7_files/figure-gfm/unnamed-chunk-1-11.png)<!-- -->

``` r
fit_grade <- survfit(Surv(Overall.Survival..Months., Overall.Survival.Status) ~ Neoplasm.Histologic.Grade, data = data_analysis)
ggsurvplot(fit_grade, data = data_analysis, pval = TRUE, risk.table = TRUE, 
           title = "Survival Curves by Neoplasm Histologic Grade", 
           palette = c("#00AFBB", "#E7B800", "#FC4E07"))
```

![](Survival_Analysis_Team_7_files/figure-gfm/unnamed-chunk-1-12.png)<!-- -->

``` r
# Cox Proportional Hazards Model
cox_model <- coxph(Surv(Overall.Survival..Months., Overall.Survival.Status) ~ 
                     Age.at.Diagnosis + ER.Status + 
                     Neoplasm.Histologic.Grade + Lymph.nodes.examined.positive, 
                   data = data_analysis)
summary(cox_model)
```

    ## Call:
    ## coxph(formula = Surv(Overall.Survival..Months., Overall.Survival.Status) ~ 
    ##     Age.at.Diagnosis + ER.Status + Neoplasm.Histologic.Grade + 
    ##         Lymph.nodes.examined.positive, data = data_analysis)
    ## 
    ##   n= 1833, number of events= 1054 
    ## 
    ##                                    coef exp(coef)  se(coef)      z Pr(>|z|)    
    ## Age.at.Diagnosis               0.038964  1.039733  0.002809 13.870  < 2e-16 ***
    ## ER.StatusPositive             -0.355501  0.700822  0.080822 -4.399 1.09e-05 ***
    ## Neoplasm.Histologic.Grade2     0.179279  1.196355  0.125730  1.426  0.15389    
    ## Neoplasm.Histologic.Grade3     0.355502  1.426897  0.126990  2.799  0.00512 ** 
    ## Lymph.nodes.examined.positive  0.061302  1.063220  0.005082 12.064  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ##                               exp(coef) exp(-coef) lower .95 upper .95
    ## Age.at.Diagnosis                 1.0397     0.9618    1.0340    1.0455
    ## ER.StatusPositive                0.7008     1.4269    0.5982    0.8211
    ## Neoplasm.Histologic.Grade2       1.1964     0.8359    0.9351    1.5307
    ## Neoplasm.Histologic.Grade3       1.4269     0.7008    1.1125    1.8302
    ## Lymph.nodes.examined.positive    1.0632     0.9405    1.0527    1.0739
    ## 
    ## Concordance= 0.65  (se = 0.009 )
    ## Likelihood ratio test= 320.5  on 5 df,   p=<2e-16
    ## Wald test            = 359.3  on 5 df,   p=<2e-16
    ## Score (logrank) test = 377.4  on 5 df,   p=<2e-16

``` r
# Check Proportional Hazards Assumption
ph_test <- cox.zph(cox_model)
print(ph_test)
```

    ##                                chisq df       p
    ## Age.at.Diagnosis               82.77  1 < 2e-16
    ## ER.Status                      94.82  1 < 2e-16
    ## Neoplasm.Histologic.Grade      45.30  2 1.5e-10
    ## Lymph.nodes.examined.positive   7.34  1  0.0068
    ## GLOBAL                        152.44  5 < 2e-16

``` r
plot(ph_test, col = c("red", "green", "blue", "purple"))
```

![](Survival_Analysis_Team_7_files/figure-gfm/unnamed-chunk-1-13.png)<!-- -->![](Survival_Analysis_Team_7_files/figure-gfm/unnamed-chunk-1-14.png)<!-- -->![](Survival_Analysis_Team_7_files/figure-gfm/unnamed-chunk-1-15.png)<!-- -->![](Survival_Analysis_Team_7_files/figure-gfm/unnamed-chunk-1-16.png)<!-- -->

``` r
# Stratified Cox Model
strat_model <- coxph(Surv(Overall.Survival..Months., Overall.Survival.Status) ~ 
                       Age.at.Diagnosis + Neoplasm.Histologic.Grade + 
                       Lymph.nodes.examined.positive + strata(ER.Status), 
                     data = data_analysis)
summary(strat_model)
```

    ## Call:
    ## coxph(formula = Surv(Overall.Survival..Months., Overall.Survival.Status) ~ 
    ##     Age.at.Diagnosis + Neoplasm.Histologic.Grade + Lymph.nodes.examined.positive + 
    ##         strata(ER.Status), data = data_analysis)
    ## 
    ##   n= 1833, number of events= 1054 
    ## 
    ##                                   coef exp(coef) se(coef)      z Pr(>|z|)    
    ## Age.at.Diagnosis              0.039239  1.040019 0.002806 13.986  < 2e-16 ***
    ## Neoplasm.Histologic.Grade2    0.199961  1.221355 0.125737  1.590  0.11176    
    ## Neoplasm.Histologic.Grade3    0.380458  1.462954 0.127006  2.996  0.00274 ** 
    ## Lymph.nodes.examined.positive 0.059600  1.061412 0.005064 11.769  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ##                               exp(coef) exp(-coef) lower .95 upper .95
    ## Age.at.Diagnosis                  1.040     0.9615    1.0343     1.046
    ## Neoplasm.Histologic.Grade2        1.221     0.8188    0.9546     1.563
    ## Neoplasm.Histologic.Grade3        1.463     0.6835    1.1406     1.876
    ## Lymph.nodes.examined.positive     1.061     0.9421    1.0509     1.072
    ## 
    ## Concordance= 0.66  (se = 0.01 )
    ## Likelihood ratio test= 316.8  on 4 df,   p=<2e-16
    ## Wald test            = 354.1  on 4 df,   p=<2e-16
    ## Score (logrank) test = 372.5  on 4 df,   p=<2e-16

``` r
# Time-dependent Cox Model
td_model <- coxph(Surv(Overall.Survival..Months., Overall.Survival.Status) ~ 
                    Age.at.Diagnosis + ER.Status + Neoplasm.Histologic.Grade + 
                    Lymph.nodes.examined.positive + 
                    tt(Age.at.Diagnosis) + tt(Lymph.nodes.examined.positive), 
                  data = data_analysis)
summary(td_model)
```

    ## Call:
    ## coxph(formula = Surv(Overall.Survival..Months., Overall.Survival.Status) ~ 
    ##     Age.at.Diagnosis + ER.Status + Neoplasm.Histologic.Grade + 
    ##         Lymph.nodes.examined.positive + tt(Age.at.Diagnosis) + 
    ##         tt(Lymph.nodes.examined.positive), data = data_analysis)
    ## 
    ##   n= 1833, number of events= 1054 
    ## 
    ##                                         coef  exp(coef)   se(coef)      z
    ## Age.at.Diagnosis                   0.0368051  1.0374908  0.0028389 12.964
    ## ER.StatusPositive                 -0.3258520  0.7219120  0.0811680 -4.015
    ## Neoplasm.Histologic.Grade2         0.1537613  1.1662124  0.1258831  1.221
    ## Neoplasm.Histologic.Grade3         0.3319694  1.3937102  0.1270461  2.613
    ## Lymph.nodes.examined.positive      0.1026404  1.1080929  0.0094279 10.887
    ## tt(Age.at.Diagnosis)               0.0004365  1.0004366  0.0001315  3.318
    ## tt(Lymph.nodes.examined.positive) -0.0021544  0.9978479  0.0005714 -3.770
    ##                                   Pr(>|z|)    
    ## Age.at.Diagnosis                   < 2e-16 ***
    ## ER.StatusPositive                 5.96e-05 ***
    ## Neoplasm.Histologic.Grade2        0.221912    
    ## Neoplasm.Histologic.Grade3        0.008976 ** 
    ## Lymph.nodes.examined.positive      < 2e-16 ***
    ## tt(Age.at.Diagnosis)              0.000906 ***
    ## tt(Lymph.nodes.examined.positive) 0.000163 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ##                                   exp(coef) exp(-coef) lower .95 upper .95
    ## Age.at.Diagnosis                     1.0375     0.9639    1.0317    1.0433
    ## ER.StatusPositive                    0.7219     1.3852    0.6157    0.8464
    ## Neoplasm.Histologic.Grade2           1.1662     0.8575    0.9112    1.4926
    ## Neoplasm.Histologic.Grade3           1.3937     0.7175    1.0865    1.7878
    ## Lymph.nodes.examined.positive        1.1081     0.9025    1.0878    1.1288
    ## tt(Age.at.Diagnosis)                 1.0004     0.9996    1.0002    1.0007
    ## tt(Lymph.nodes.examined.positive)    0.9978     1.0022    0.9967    0.9990
    ## 
    ## Concordance= 0.658  (se = 0.01 )
    ## Likelihood ratio test= 360.3  on 7 df,   p=<2e-16
    ## Wald test            = 410  on 7 df,   p=<2e-16
    ## Score (logrank) test = 465.1  on 7 df,   p=<2e-16

``` r
# Random Survival Forest
rsf_model <- rfsrc(Surv(Overall.Survival..Months., Overall.Survival.Status) ~ 
                     Age.at.Diagnosis + ER.Status + Neoplasm.Histologic.Grade + 
                     Lymph.nodes.examined.positive, 
                   data = data_analysis, ntree = 1000)
print(rsf_model)
```

    ##                          Sample size: 1833
    ##                     Number of deaths: 1054
    ##                      Number of trees: 1000
    ##            Forest terminal node size: 15
    ##        Average no. of terminal nodes: 86.141
    ## No. of variables tried at each split: 2
    ##               Total no. of variables: 4
    ##        Resampling used to grow trees: swor
    ##     Resample size used to grow trees: 1158
    ##                             Analysis: RSF
    ##                               Family: surv
    ##                       Splitting rule: logrank *random*
    ##        Number of random split points: 10
    ##                           (OOB) CRPS: 53.64742736
    ##                    (OOB) stand. CRPS: 0.15284167
    ##    (OOB) Requested performance error: 0.33848381

``` r
# Variable importance for RSF
vimp <- vimp(rsf_model)
print(vimp$importance)
```

    ##              Age.at.Diagnosis                     ER.Status 
    ##                    0.19437694                    0.02060200 
    ##     Neoplasm.Histologic.Grade Lymph.nodes.examined.positive 
    ##                    0.03328381                    0.17061409

``` r
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
```

![](Survival_Analysis_Team_7_files/figure-gfm/unnamed-chunk-1-17.png)<!-- -->

``` r
# Partial Effect Plots
ggplot(data_analysis, aes(x = Age.at.Diagnosis, y = predict(strat_model, type = "risk"))) +
  geom_smooth(color = "blue", fill = "lightblue") +
  labs(title = "Partial Effect of Age at Diagnosis on Hazard",
       x = "Age at Diagnosis", y = "Predicted Hazard")
```

![](Survival_Analysis_Team_7_files/figure-gfm/unnamed-chunk-1-18.png)<!-- -->

``` r
ggplot(data_analysis, aes(x = Lymph.nodes.examined.positive, y = predict(strat_model, type = "risk"))) +
  geom_smooth(color = "green", fill = "lightgreen") +
  labs(title = "Partial Effect of Positive Lymph Nodes on Hazard",
       x = "Number of Positive Lymph Nodes", y = "Predicted Hazard")
```

![](Survival_Analysis_Team_7_files/figure-gfm/unnamed-chunk-1-19.png)<!-- -->

``` r
# Boxplots of Continuous Variables by Survival Status
ggplot(data_analysis, aes(x = factor(Overall.Survival.Status), y = Age.at.Diagnosis)) +
  geom_boxplot(fill = c("#00AFBB", "#FC4E07")) +
  labs(title = "Age at Diagnosis by Survival Status",
       x = "Survival Status (0 = Survived, 1 = Deceased)", y = "Age at Diagnosis")
```

![](Survival_Analysis_Team_7_files/figure-gfm/unnamed-chunk-1-20.png)<!-- -->

``` r
ggplot(data_analysis, aes(x = factor(Overall.Survival.Status), y = Lymph.nodes.examined.positive)) +
  geom_boxplot(fill = c("#00AFBB", "#FC4E07")) +
  labs(title = "Positive Lymph Nodes by Survival Status",
       x = "Survival Status (0 = Survived, 1 = Deceased)", y = "Number of Positive Lymph Nodes")
```

![](Survival_Analysis_Team_7_files/figure-gfm/unnamed-chunk-1-21.png)<!-- -->

``` r
# Scatter Plot Matrix
pairs(~ Age.at.Diagnosis + Lymph.nodes.examined.positive + Overall.Survival..Months., data = data_analysis,
      main = "Scatter Plot Matrix of Key Continuous Variables", col = "blue")
```

![](Survival_Analysis_Team_7_files/figure-gfm/unnamed-chunk-1-22.png)<!-- -->

``` r
# Variable Importance Plot from Random Survival Forest
plot(vimp(rsf_model), col = "blue")
```

![](Survival_Analysis_Team_7_files/figure-gfm/unnamed-chunk-1-23.png)<!-- -->

    ## 
    ##                                 Importance   Relative Imp
    ## Age.at.Diagnosis                    0.1944         1.0000
    ## Lymph.nodes.examined.positive       0.1704         0.8763
    ## Neoplasm.Histologic.Grade           0.0324         0.1668
    ## ER.Status                           0.0204         0.1051

``` r
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
```

![](Survival_Analysis_Team_7_files/figure-gfm/unnamed-chunk-1-24.png)<!-- -->

``` r
# Model Selection and Validation
# Compare AIC
AIC(cox_model, strat_model, td_model)
```

    ##             df      AIC
    ## cox_model    5 14035.75
    ## strat_model  4 12898.40
    ## td_model     7 14000.00

``` r
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
```

    ## $Cox
    ## $Cox$c_index
    ## [1] 0.3440418 0.3828378 0.3162432 0.3783075 0.3800137
    ## 
    ## $Cox$mean
    ## [1] 0.3602888
    ## 
    ## $Cox$sd
    ## [1] 0.02926681
    ## 
    ## 
    ## $Stratified
    ## $Stratified$c_index
    ## [1] 0.3371831 0.3716527 0.3486980 0.3463764 0.3705555
    ## 
    ## $Stratified$mean
    ## [1] 0.3548931
    ## 
    ## $Stratified$sd
    ## [1] 0.01541716
    ## 
    ## 
    ## $TimeDep
    ## $TimeDep$c_index
    ## [1] 0.3313200 0.3731735 0.3039269 0.3650552 0.3554059
    ## 
    ## $TimeDep$mean
    ## [1] 0.3457763
    ## 
    ## $TimeDep$sd
    ## [1] 0.02817518

``` r
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
```

![](Survival_Analysis_Team_7_files/figure-gfm/unnamed-chunk-1-25.png)<!-- -->

``` r
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
```

![](Survival_Analysis_Team_7_files/figure-gfm/unnamed-chunk-1-26.png)<!-- -->

``` r
# Density plot of C-index distributions
ggplot(cv_results_melted, aes(x = value, fill = variable)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of C-index Distributions",
       x = "C-index", y = "Density") +
  theme_minimal()
```

![](Survival_Analysis_Team_7_files/figure-gfm/unnamed-chunk-1-27.png)<!-- -->

``` r
# Print summary statistics
print(summary(cv_results_df))
```

    ##       Cox           Stratified        TimeDep      
    ##  Min.   :0.3162   Min.   :0.3372   Min.   :0.3039  
    ##  1st Qu.:0.3440   1st Qu.:0.3464   1st Qu.:0.3313  
    ##  Median :0.3783   Median :0.3487   Median :0.3554  
    ##  Mean   :0.3603   Mean   :0.3549   Mean   :0.3458  
    ##  3rd Qu.:0.3800   3rd Qu.:0.3706   3rd Qu.:0.3651  
    ##  Max.   :0.3828   Max.   :0.3717   Max.   :0.3732

``` r
# Final Model Selection and Interpretation
final_model <- strat_model
summary(final_model)
```

    ## Call:
    ## coxph(formula = Surv(Overall.Survival..Months., Overall.Survival.Status) ~ 
    ##     Age.at.Diagnosis + Neoplasm.Histologic.Grade + Lymph.nodes.examined.positive + 
    ##         strata(ER.Status), data = data_analysis)
    ## 
    ##   n= 1833, number of events= 1054 
    ## 
    ##                                   coef exp(coef) se(coef)      z Pr(>|z|)    
    ## Age.at.Diagnosis              0.039239  1.040019 0.002806 13.986  < 2e-16 ***
    ## Neoplasm.Histologic.Grade2    0.199961  1.221355 0.125737  1.590  0.11176    
    ## Neoplasm.Histologic.Grade3    0.380458  1.462954 0.127006  2.996  0.00274 ** 
    ## Lymph.nodes.examined.positive 0.059600  1.061412 0.005064 11.769  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ##                               exp(coef) exp(-coef) lower .95 upper .95
    ## Age.at.Diagnosis                  1.040     0.9615    1.0343     1.046
    ## Neoplasm.Histologic.Grade2        1.221     0.8188    0.9546     1.563
    ## Neoplasm.Histologic.Grade3        1.463     0.6835    1.1406     1.876
    ## Lymph.nodes.examined.positive     1.061     0.9421    1.0509     1.072
    ## 
    ## Concordance= 0.66  (se = 0.01 )
    ## Likelihood ratio test= 316.8  on 4 df,   p=<2e-16
    ## Wald test            = 354.1  on 4 df,   p=<2e-16
    ## Score (logrank) test = 372.5  on 4 df,   p=<2e-16

``` r
# Visualize the effect of a continuous variable
ggplot(data_analysis, aes(x = Age.at.Diagnosis, y = predict(final_model, type = "risk"))) +
  geom_smooth(color = "blue", fill = "lightblue") +
  labs(title = "Effect of Age at Diagnosis on Hazard",
       x = "Age at Diagnosis", y = "Predicted Hazard")
```

![](Survival_Analysis_Team_7_files/figure-gfm/unnamed-chunk-1-28.png)<!-- -->

``` r
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
```

![](Survival_Analysis_Team_7_files/figure-gfm/unnamed-chunk-1-29.png)<!-- -->

``` r
ggplot(data_analysis, aes(x = Lymph.nodes.examined.positive, y = predict(final_model, type = "risk"))) +
  geom_smooth(color = "green", fill = "lightgreen") +
  labs(title = "Effect of Positive Lymph Nodes on Hazard",
       x = "Number of Positive Lymph Nodes", y = "Predicted Hazard")
```

![](Survival_Analysis_Team_7_files/figure-gfm/unnamed-chunk-1-30.png)<!-- -->

``` r
# Interaction plot
interaction_model <- coxph(Surv(Overall.Survival..Months., Overall.Survival.Status) ~ 
                             Age.at.Diagnosis * Lymph.nodes.examined.positive + 
                             Neoplasm.Histologic.Grade + strata(ER.Status), 
                           data = data_analysis)

summary(interaction_model)
```

    ## Call:
    ## coxph(formula = Surv(Overall.Survival..Months., Overall.Survival.Status) ~ 
    ##     Age.at.Diagnosis * Lymph.nodes.examined.positive + Neoplasm.Histologic.Grade + 
    ##         strata(ER.Status), data = data_analysis)
    ## 
    ##   n= 1833, number of events= 1054 
    ## 
    ##                                                      coef  exp(coef)   se(coef)
    ## Age.at.Diagnosis                                0.0437939  1.0447670  0.0030835
    ## Lymph.nodes.examined.positive                   0.1882920  1.2071860  0.0341561
    ## Neoplasm.Histologic.Grade2                      0.2054133  1.2280326  0.1257390
    ## Neoplasm.Histologic.Grade3                      0.3902065  1.4772858  0.1269920
    ## Age.at.Diagnosis:Lymph.nodes.examined.positive -0.0020637  0.9979385  0.0005516
    ##                                                     z Pr(>|z|)    
    ## Age.at.Diagnosis                               14.202  < 2e-16 ***
    ## Lymph.nodes.examined.positive                   5.513 3.53e-08 ***
    ## Neoplasm.Histologic.Grade2                      1.634 0.102333    
    ## Neoplasm.Histologic.Grade3                      3.073 0.002121 ** 
    ## Age.at.Diagnosis:Lymph.nodes.examined.positive -3.741 0.000183 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ##                                                exp(coef) exp(-coef) lower .95
    ## Age.at.Diagnosis                                  1.0448     0.9572    1.0385
    ## Lymph.nodes.examined.positive                     1.2072     0.8284    1.1290
    ## Neoplasm.Histologic.Grade2                        1.2280     0.8143    0.9598
    ## Neoplasm.Histologic.Grade3                        1.4773     0.6769    1.1518
    ## Age.at.Diagnosis:Lymph.nodes.examined.positive    0.9979     1.0021    0.9969
    ##                                                upper .95
    ## Age.at.Diagnosis                                   1.051
    ## Lymph.nodes.examined.positive                      1.291
    ## Neoplasm.Histologic.Grade2                         1.571
    ## Neoplasm.Histologic.Grade3                         1.895
    ## Age.at.Diagnosis:Lymph.nodes.examined.positive     0.999
    ## 
    ## Concordance= 0.662  (se = 0.01 )
    ## Likelihood ratio test= 329.3  on 5 df,   p=<2e-16
    ## Wald test            = 355.1  on 5 df,   p=<2e-16
    ## Score (logrank) test = 373.2  on 5 df,   p=<2e-16

``` r
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
```

![](Survival_Analysis_Team_7_files/figure-gfm/unnamed-chunk-1-31.png)<!-- -->

``` r
# Final model performance metrics
concordance(final_model)
```

    ## Call:
    ## concordance.coxph(object = final_model)
    ## 
    ## n= 1833 
    ## Concordance= 0.6596 se= 0.00984
    ##          concordant discordant tied.x tied.y tied.xy
    ## Negative      41476      28527      4     12       0
    ## Positive     438264     219095     16     64       0
