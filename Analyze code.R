###------------------R package checking, installation, loading------------------
if (!require("dplyr")) install.packages("dplyr");library("dplyr") # Data processing
if (!require("lubridate")) install.packages("lubridate");library("lubridate") #ymd()
if (!require("haven")) install.packages("haven");library("haven") # Remove labels
if (!require("ggplot2")) install.packages("ggplot2");library("ggplot2") # Plotting
if (!require("lcmm")) install.packages("lcmm");library("lcmm") # LCGM model hlme(), summarytable()
if (!require("purrr")) install.packages("purrr");library("purrr") # Average posterior probability: imap_dfr(), map()
if (!require("devtools")) install.packages("devtools");library("devtools")
if (!require("LCTMtools")) devtools::install_github("hlennon/LCTMtools");library("LCTMtools") # LCTMtools
if (!require("psych")) install.packages("psych");library("psych") # correlation analysis
if (!require("corrplot")) install.packages("corrplot");library("corrplot") # correlation heatmaps
if (!require("dlookr")) install.packages("dlookr");library("dlookr") # missing-data visualization, plots
if (!require("officer")) install.packages("officer");library("officer") # export to Word
if (!require("flextable")) install.packages("flextable");library("flextable") # export to Word
if (!require("missForest")) install.packages("missForest");library("missForest") # missing-value imputation: missForest()
if (!require("caret")) install.packages("caret");library("caret") # data split: createDataPartition(); scaling: preProcess()
if (!require("mlr3")) install.packages("mlr3");library("mlr3") # core ML framework
if (!require("mlr3verse")) install.packages("mlr3verse");library("mlr3verse") # mlr3 ecosystem extensions
if (!require("mlr3extralearners")) devtools::install_github("https://github.com/mlr-org/mlr3extralearners");library("mlr3extralearners") # extra algorithms for mlr3
if (!require("ranger")) install.packages("ranger");library("ranger") # random forest (mlr3 does not bundle ranger)
if (!require("xgboost")) install.packages("xgboost");library("xgboost") # XGBoost (mlr3 does not bundle xgboost)
if (!require("e1071")) install.packages("e1071");library("e1071") # SVM (mlr3 does not bundle e1071)
if (!require("mlr3learners")) install.packages("mlr3learners");library("mlr3learners") # Algorithm packages
library(mlr3tuning) # Hyperparameter optimization package
if (!require("kknn")) install.packages("kknn");library("kknn") # knn (mlr3 does not bundle knn)
if (!require("pROC")) install.packages("pROC");library("pROC") # ROC curves with 95% CI
if (!require("patchwork")) install.packages("patchwork");library("patchwork") # combine ggplot figures
if (!require("rmda")) install.packages("rmda");library("rmda") # decision curve analysis
if (!require("kernelshap")) install.packages("kernelshap");library("kernelshap") # SHAP value computation
if (!require("shapviz")) install.packages("shapviz");library("shapviz") # SHAP visualizations
if (!require("gtsummary")) install.packages("gtsummary");library("gtsummary") # baseline characteristics tables
if (!require("cardx")) install.packages("cardx");library("cardx") # baseline characteristics tables
if (!require("rmda")) install.packages("rmda");library("rmda") # decision curve analysis
if (!require("riskRegression")) install.packages("riskRegression");library("riskRegression") # calibration plots
if (!require("tidyverse")) install.packages("tidyverse");library("tidyverse") # data extraction
if (!require("ggradar")) devtools::install_github("ricardo-bion/ggradar");library("ggradar") # radar charts
if (!require("cowplot")) install.packages("cowplot");library("cowplot") # Combining plots
if (!require("mice")) install.packages("mice");library("mice") # multiple interpolation
if (!require("ggsankey")) devtools::install_github("davidsjoberg/ggsankey");library("ggsankey") # Draw Sankey diagram

# shiny app dependencies
install.packages(c("shiny", "DT", "plotly", "iml", "shinydashboard", "shinyjs", "progressr"))
install.packages("rsconnect") # cloud deployment


# Import data
load("data.RData")

# Filter data from 2017 to 2024
mj17_24 <- data %>%
  filter(exdate >= as.Date("2017-01-01"),
         exdate <= as.Date("2024-12-31"))

# Convert to data frame
mj <- as.data.frame(mj17_24)

# Remove haven label attributes
mj <- haven::zap_labels(mj)
mj <- haven::zap_label(mj)
mj[] <- lapply(mj, function(x) {
  if (inherits(x, "haven_labelled")) unclass(x) else x
})

# Create variable name mapping table
variable_dict <- data.frame(
  raw_name = c(
    "accountid","exdate","age","sex","scholar","occupation","marriage",
    "yearincome","BloodType","smokeornot","drinkornot","foodtime",
    "sleeptime","FirstSportFrequ","SameAgeHealth","nopsick","psick01",
    "psick02","psick03","psick04","psick05","psick06","psick07",
    "psick08","psick25","Cancer","psick10","psick23","psick14",
    "psick11","psick12","psick13","psick15","psick16","psick17",
    "psick18","Liver diseases","psick19","psick20","psick21",
    "psick22","ESLLS","ESRLS","Vision","LHearing","RHearing",
    "Hearing","PressSR1","PressSR2","Height","Weight","BMI",
    "WaWidth","HipWidth","BodyFat","UrEw","UrS","Insulin","BloodDep",
    "HS_CRP","Ushba1c2","BgChol","BgHdlc","BgLdlc","BgTg",
    "UfBun","Ushba1c1","UfUa","BloodWBC","BloodHB","BloodMCV",
    "BloodPLT","BloodHCT","UfCre"
  ),
  analysis_name = c(
    "accountid","exdate","Age","sex","Education","Occupation","Marital_status",
    "Personal_annual_income","Blood_type","Smoking","Drinking","Regular_eating",
    "Nighttime_sleep","Exercise_frequency","Self_rated_health","nopsick","psick01",
    "psick02","psick03","psick04","psick05","psick06","psick07",
    "psick08","psick25","Cancer","DM","Arthritis","Asthma",
    "Cerebrovascular_diseases","Cardiovascular_diseases","Thyroid_diseases",
    "Tuberculosis","Digestive_diseases","Hepatitis","Cirrhosis",
    "Liver_diseases","Kidney_diseases","Urolithiasis","Gout",
    "Anemia","ESLLS","ESRLS","Vision","LHearing","RHearing",
    "Hearing","SBP","DBP","Height","Weight","BMI",
    "Waist_circumference","Hip_circumference","BFP","Urine_protein","Urine_glucose",
    "Insulin","SpO2","HS_CRP","HbA1c","TC","HDL_C","LDL_C","TG",
    "BUN","FBG","UA","WBC","HB","MCV","PLT","HCT","Creatinine"
  ),
  display_name = c(
    "accountid","exdate","Age","Sex","Education","Occupation","Marital status",
    "Personal annual income","Blood type","Smoking","Drinking","Regular eating",
    "Nighttime sleep","Exercise frequency","Self-rated health","nopsick","psick01",
    "psick02","psick03","psick04","psick05","psick06","psick07",
    "psick08","psick25","Cancer","DM","Arthritis","Asthma",
    "Cerebrovascular diseases","Cardiovascular diseases","Thyroid diseases",
    "Tuberculosis","Digestive diseases","Hepatitis","Cirrhosis",
    "Liver diseases","Kidney diseases","Urolithiasis","Gout",
    "Anemia","ESLLS","ESRLS","Vision","LHearing","RHearing",
    "Hearing","SBP","DBP","Height","Weight","BMI",
    "Waist circumference","Hip circumference","BFP","Urine protein","Urine glucose",
    "Insulin","SpO2","HS-CRP","HbA1c","TC","HDL-C","LDL-C","TG",
    "BUN","FBG","UA","WBC","HB","MCV","PLT","HCT","Creatinine"
  ),
  stringsAsFactors = FALSE
)

# Variable name mapping
idx <- match(names(mj), variable_dict$raw_name)
colnames(mj)[!is.na(idx)] <- variable_dict$analysis_name[idx[!is.na(idx)]]

###------------------eGFR, CMDs coding------------------
## Age: continuous variable; Sex: 1=Man, 2=Woman
mj <- mj %>%
  mutate(
    Age = as.numeric(Age), # Convert Age column to numeric
    Sex = as.numeric(sex), # Convert sex column to numeric and rename to Sex
    Sex = factor(Sex, levels = c(1, 2), labels = c("Man", "Woman")) # Convert to labelled factor
  )

##------------------Calculate eGFR (CKD-EPI 2021, creatinine-only)------------------
# Convert creatinine to numeric
mj$Creatinine <- as.numeric(mj$Creatinine) # Creatinine: unit μmol/L

# Creatinine: NA=(exclude physiologically impossible values: 20--1500 μmol/L), μmol/88.4→mg/dL
mj$Creatinine <- ifelse(mj$Creatinine < 20 | mj$Creatinine > 1500, NA, mj$Creatinine / 88.4)

# Define CKD-EPI 2021 creatinine-only formula
eGFR_2021 <- function(Scr_mgdl, age, sex) {
  k <- ifelse(sex == "Woman", 0.7, 0.9) # Female 0.7, Male 0.9
  a <- ifelse(sex == "Woman", -0.241, -0.302) # Female -0.241, Male -0.302
  C <- ifelse(sex == "Woman", 1.012, 1) # Female 1.012, Male 1
  ratio <- Scr_mgdl / k
  eGFR <- 142 *
    (pmin(ratio, 1) ^ a) *
    (pmax(ratio, 1) ^ -1.200) *
    (0.9938 ^ age) *
    C
  return(eGFR)
}

# Calculate eGFR
mj$eGFR <- eGFR_2021(
  Scr_mgdl = mj$Creatinine,
  age = mj$Age,
  sex = mj$Sex
)

# eGFR: NA=(exclude physiologically impossible values: >150ml/min/1.73m²)
mj$eGFR <- ifelse(mj$eGFR > 150, NA, mj$eGFR)

##------------------Disease variable coding: no disease=1, disease=2------------------
# Define disease coding function
encode01 <- function(x){
  x <- as.character(x)
  out <- rep(NA_real_, length(x))
  out[x %in% c("T", "TRUE")] <- 2 # Disease present
  out[x %in% c("F", "FALSE")] <- 1 # No disease
  return(out)
}

mj$Kidney_diseases <- encode01(mj$Kidney_diseases) # Kidney disease
mj$DM <- encode01(mj$DM) # Diabetes
mj$Cardiovascular_diseases <- encode01(mj$Cardiovascular_diseases) # Cardiovascular disease (heart disease)
mj$Cerebrovascular_diseases <- encode01(mj$Cerebrovascular_diseases) # Cerebrovascular disease (stroke)

##------------------Find the first physical examination for each accountid = baseline------------------
# Group by accountid, calculate the minimum exdate for each accountid (ignore NA)
baseline_info <- aggregate(
  exdate ~ accountid,
  data = mj,
  FUN = function(x) min(x, na.rm = TRUE)
)

# Rename exdate column to baseline_date
names(baseline_info)[names(baseline_info) == "exdate"] <- "baseline_date"

# Extract year and create a new column
baseline_info$baseline_year <- as.integer(format(baseline_info$baseline_date, "%Y"))

# Merge back into mj to get mj2
mj2 <- merge(
  mj,
  baseline_info,
  by = "accountid",
  all.x = TRUE
)

# Extract the baseline row for each accountid
baseline0 <- mj2[mj2$exdate == mj2$baseline_date, ]

###------------------Inclusion/Exclusion: Total N=117537------------------
length(unique(baseline0$accountid))

## (1) Baseline age not missing and ≥45 years (n=42003)
baseline1 <- baseline0[!is.na(baseline0$Age) & baseline0$Age >= 45, ]
length(unique(baseline1$accountid))

## (2) Non-missing eGFR count from baseline ≥3 times (= baseline + ≥2 follow-ups) (n=12862)
# First, keep only those individuals from baseline1 and all records after their respective baseline_date
tmp_all <- mj2[
  mj2$accountid %in% baseline1$accountid &
    mj2$exdate >= mj2$baseline_date,
]

# Total non-missing eGFR count from baseline
egfr_from_baseline <- aggregate(
  eGFR ~ accountid,
  data = tmp_all,
  FUN = function(x) sum(!is.na(x))
)

# Rename eGFR column to n_egfr_from_baseline
names(egfr_from_baseline)[names(egfr_from_baseline) == "eGFR"] <- "n_egfr_from_baseline"

# Keep only accountids with total eGFR count from baseline ≥3
ids_step2 <- egfr_from_baseline$accountid[egfr_from_baseline$n_egfr_from_baseline >= 3]

# Filter rows from baseline1 where accountid belongs to ids_step2, save as baseline2.
baseline2 <- baseline1[baseline1$accountid %in% ids_step2, ]
length(unique(baseline2$accountid))

## (3) Exclude those with baseline history of CMDs (diabetes, stroke, cardiovascular disease) (n=11170)
baseline3 <- baseline2[
  !is.na(baseline2$DM) & baseline2$DM == 1 & # No diabetes
    !is.na(baseline2$Cerebrovascular_diseases) & baseline2$Cerebrovascular_diseases == 1 & # No stroke
    !is.na(baseline2$Cardiovascular_diseases) & baseline2$Cardiovascular_diseases == 1 # No cardiovascular disease
  , ]
length(unique(baseline3$accountid))

## (4) Exclude kidney disease (NS=2) or eGFR <60 (n=10969)
baseline4 <- baseline3[
  !is.na(baseline3$Kidney_diseases) & baseline3$Kidney_diseases == 1 & # No kidney disease
    !is.na(baseline3$eGFR) & baseline3$eGFR >= 60 # eGFR ≥60
  , ]
length(unique(baseline4$accountid))

# Enrolled accountids
ids_step4 <- baseline4$accountid

## Construct longitudinal follow-up data: all physical examination records from baseline
# Filter mj2 for final accountids and exdate >= baseline_date
mj3 <- mj2[
  mj2$accountid %in% ids_step4 &
    mj2$exdate >= mj2$baseline_date,
]

# Re-extract "year" directly from the date, do not use previous year variable
mj3$year <- as.integer(format(mj3$exdate, "%Y"))
mj3$baseline_year <- as.integer(format(mj3$baseline_date, "%Y"))

# Continuous time from baseline (years)
mj3$time <- as.numeric(
  difftime(mj3$exdate, mj3$baseline_date, units = "days")
) / 365.25

## (4) Exclude accountids with NA in the outcome CMDs during follow-up (n=10967)
# CMDs_event: 1=not occurred, 2=occurred, NA=cannot be determined
mj3$CMDs_event <- with(mj3, ifelse(
  DM == 2 | Cardiovascular_diseases == 2 | Cerebrovascular_diseases == 2, 2L, # Any disease present
  ifelse(
    DM == 1 & Cardiovascular_diseases == 1 & Cerebrovascular_diseases == 1, 1L, # All three absent
    NA_integer_ # Other cases (has NA)
  )
))

# Exclude accountids where CMDs_event ever had NA
valid_ids <- unique(mj3$accountid[is.na(mj3$CMDs_event)])
final17_24 <- mj3[!mj3$accountid %in% valid_ids, ]
length(unique(final17_24$accountid))

###------------------Trajectory identification: LCGM------------------
# Convert accountid to numeric
final17_24$accountid <- as.numeric(final17_24$accountid)

## (1) Model fitting
# Polynomial degree (quadratic): First build a 1-class base model
set.seed(2025)
LCGM1 <- hlme(
  fixed = eGFR ~ 1 + time + I(time^2), # Fixed effect part of the model (main formula), set to quadratic (linear: eGFR ~ 1 + time, cubic: eGFR ~ 1 + time + I(time^2) + I(time^3))
  ng = 1, # Number of latent classes
  subject = "accountid", # Subject id for nested structure
  data = final17_24 # Dataset name
)

## 2-5 class models
# Store each LCGM
LCGMs <- list()

# LCGM loop
for (k in 2:5) {
  LCGMs[[paste0("LCGM", k)]] <- hlme(
    fixed = eGFR ~ 1 + time + I(time^2),
    mixture = ~ 1 + time + I(time^2), # Mixture effect: required when number of classes > 1 (parameters may differ across latent classes)
    ng = k,
    subject = "accountid",
    data = final17_24,
    B = LCGM1 # Initial model
  )
}

### (2) Model fit evaluation: AIC, BIC, entropy, class proportions, Avepp, OCC
## ① BIC (lower absolute value), AIC (lower absolute value), entropy (close to 1), class proportion (>5%)
LCGM_result <- summarytable(LCGMs$LCGM2, LCGMs$LCGM3, LCGMs$LCGM4, LCGMs$LCGM5,
                            which = c("G", "AIC", "BIC", "SABIC", "entropy", "%class"))

# Convert LCGM_result to data frame, get the minimum class proportion into a new column
LCGM_result <- LCGM_result %>%
  as.data.frame() %>%
  mutate(
    `Smallest Class (%)` = pmin(`%class1`, `%class2`, `%class3`, `%class4`, `%class5`,
                                na.rm = TRUE)
  ) %>%
  select(-starts_with("%class")) %>%
  rename(Model = G, Entropy = entropy)

## ② Average posterior probability Avepp > 0.7; Odds of correct classification (OCC) > 5
# First define the list of models
LCGM_models <- list(
  LCGM2 = LCGMs$LCGM2,
  LCGM3 = LCGMs$LCGM3,
  LCGM4 = LCGMs$LCGM4,
  LCGM5 = LCGMs$LCGM5
)

# Run LCTMtoolkit for each model
Avepp_OCC_results_list <- lapply(names(LCGM_models), function(name) {
  cat("Model being processed:", name, "\n")
  mod <- LCGM_models[[name]]
  # Safely call, won't interrupt the loop if error occurs
  tryCatch({
    res <- LCTMtoolkit(mod)
    list(name = name, result = res)
  }, error = function(e) {
    cat("??? Model", name, "Error:", e$message, "\n")
    NULL
  })
})

# Extract Avepp results
appa_results <- bind_rows(Avepp_OCC_results_list[[1]][["result"]][["appa"]],
                          Avepp_OCC_results_list[[2]][["result"]][["appa"]],
                          Avepp_OCC_results_list[[3]][["result"]][["appa"]],
                          Avepp_OCC_results_list[[4]][["result"]][["appa"]])

# Get the minimum Avepp into a new column
appa_results <- appa_results %>%
  mutate(
    `Smallest avepp` = pmin(Class_1, Class_2, Class_3, Class_4, Class_5, na.rm = TRUE)
  )

# Extract OCC results
occ_results <- bind_rows(Avepp_OCC_results_list[[1]][["result"]][["occ"]],
                         Avepp_OCC_results_list[[2]][["result"]][["occ"]],
                         Avepp_OCC_results_list[[3]][["result"]][["occ"]],
                         Avepp_OCC_results_list[[4]][["result"]][["occ"]])

# Get the minimum OCC into a new column
occ_results <- occ_results %>%
  mutate(
    `Smallest OCC` = pmin(Class_1, Class_2, Class_3, Class_4, Class_5, na.rm = TRUE)
  )

# Combine all results
LCGM_results <- cbind(
  LCGM_result,
  `Smallest avepp` = appa_results[ , "Smallest avepp"],
  `Smallest OCC` = occ_results[ , "Smallest OCC"]
)

# Keep two decimal places
LCGM_results[ , -1] <- round(LCGM_results[ , -1], 2)

### (3) LCGM trajectory plot
# Construct time grid
newdata <- data.frame(time = seq(
  from = min(final17_24$time, na.rm = TRUE),
  to = max(final17_24$time, na.rm = TRUE),
  length.out = 100
))

# Extract model predictions
plotpred <- predictY(LCGMs$LCGM3, newdata, var.time = "time", draws = TRUE, marg = TRUE)

# Create data frame
pred_dfci <- data.frame(
  time = plotpred[["times"]]$time,
  class1 = plotpred[["pred"]][, "Ypred_class1"],
  class2 = plotpred[["pred"]][, "Ypred_class2"],
  class3 = plotpred[["pred"]][, "Ypred_class3"],
  # Add confidence interval data
  class1_lower = plotpred[["pred"]][, "lower.Ypred_class1"],
  class1_upper = plotpred[["pred"]][, "upper.Ypred_class1"],
  class2_lower = plotpred[["pred"]][, "lower.Ypred_class2"],
  class2_upper = plotpred[["pred"]][, "upper.Ypred_class2"],
  class3_lower = plotpred[["pred"]][, "lower.Ypred_class3"],
  class3_upper = plotpred[["pred"]][, "upper.Ypred_class3"]
)

# Directly plot 3 curves and confidence intervals
LCGM_plotci <- ggplot(pred_dfci, aes(x = time)) +
  # Draw confidence intervals (ribbon)
  geom_ribbon(aes(ymin = class1_lower, ymax = class1_upper, fill = "Class 1"), alpha = 0.2) +
  geom_ribbon(aes(ymin = class2_lower, ymax = class2_upper, fill = "Class 2"), alpha = 0.2) +
  geom_ribbon(aes(ymin = class3_lower, ymax = class3_upper, fill = "Class 3"), alpha = 0.2) +
  # Draw predicted lines
  geom_line(aes(y = class1, color = "Class 1"), size = 0.5) +
  geom_line(aes(y = class2, color = "Class 2"), size = 0.5) +
  geom_line(aes(y = class3, color = "Class 3"), size = 0.5) +
  labs(
    x = "Years since baseline",
    y = expression(eGFR~"(ml/min/1.73"~m^2*")"),
    color = "Trajectory",
    fill = "Trajectory"
  ) +
  scale_color_manual(values = c(
    "Class 1" = "#9E9AC8",
    "Class 2" = "#FDAE6B",
    "Class 3" = "#6BAED6"
  ),
  labels = c(
    "Class 1" = "Low decline (9.17%)",
    "Class 2" = "Unstable (34.60%)",
    "Class 3" = "High-stable (56.22%)"
  )) +
  scale_fill_manual(values = c(
    "Class 1" = "#9E9AC8",
    "Class 2" = "#FDAE6B",
    "Class 3" = "#6BAED6"),
    labels = c(
      "Class 1" = "Low decline (9.17%)",
      "Class 2" = "Unstable (34.60%)",
      "Class 3" = "High-stable (56.22%)"
    )) +
  scale_y_continuous(limits = c(70, 110)) + # y-axis range
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = NA), # Overall image background, border color none
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.2), # Add border
    panel.grid.major = element_line(linewidth = 0.2), # Adjust major grid line thickness
    axis.ticks.x = element_line(color = "black"), # x-axis tick marks
    axis.ticks.y = element_line(color = "black"), # y-axis tick marks
    axis.ticks.length = unit(0.05, "cm"), # Tick mark length and placement outside
    axis.text = element_text(size = 20), # Axis tick label font size
    axis.title = element_text(size = 30), # Axis title font size
    axis.ticks = element_line(size = 0.2), # Axis tick line thickness
    legend.margin = margin(0, 0, -0.2, 0, "cm"), # Adjust margins
    legend.key.height = unit(0.2, "cm"), # Bar legend height
    legend.key.width = unit(0.4, "cm"), # Bar legend width
    legend.position = "top",
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20)
  ) +
  guides(color = guide_legend(nrow = 2))

# Export trajectory plot
ggsave(
  filename = "LCGM_trajectory_plot.png",
  plot = LCGM_plotci,
  width = 7,
  height = 7,
  units = "cm",
  dpi = 600
)

### (4) Trajectory grouping
LCGM_group <- LCGMs$LCGM3$pprob[, c(1, 2)] # Extract accountid and class columns
names(LCGM_group)[names(LCGM_group) == "class"] <- "eGFR_group" # Rename

### (5) Merge with other data
data_traj <- merge(final17_24, LCGM_group, by = "accountid")

# For each accountid, if there is a 2 in CMDs_event, code new variable CMDs as 2, otherwise 1
data_traj2 <- data_traj %>%
  group_by(accountid) %>%
  mutate(
    CMDs = if_else(any(CMDs_event == 2, na.rm = TRUE), 2L, 1L),
    CMDs = factor(CMDs, levels = c(1, 2), labels = c("No", "Yes")) # Convert to labelled factor
  ) %>%
  arrange(exdate, .by_group = TRUE) %>% # Sort by date (early -> late) within each accountid
  dplyr::slice(1) %>% # Take the earliest row
  ungroup()

###------------------Other predictor coding------------------
data_traj2 <- data_traj2 %>%
  mutate(
    across(c(5:48, 50, 53:56, 60:63, 89:117, 128:130), ~ as.numeric(.x)), # Convert these columns from character to numeric
    
    # Education level Education: NA=(0), 1=Illiterate (scholar: 1), 2=Primary school (scholar: 2), 3=Middle school and above (scholar: 3-7)
    Education = case_when(
      Education == 0 ~ NA_real_,
      Education == 1 ~ 1,
      Education == 2 ~ 2,
      Education >= 3 ~ 3),
    Education = factor(
      Education,
      levels = c(1, 2, 3),
      labels = c("Illiterate", "Primary school", "Middle school and above")),
    
    # Occupation: NA=(0), 1=Non-agricultural work (occupation: 1-19,21-26), 2=Agricultural work (occupation: 20)
    Occupation = case_when(
      Occupation == 0 ~ NA_real_,
      Occupation == 20 ~ 2,
      Occupation %in% c(1:19, 21:26) ~ 1),
    Occupation = factor(
      Occupation,
      levels = c(1, 2),
      labels = c("Non-agricultural work", "Agricultural work")),
    
    # Marital status: NA=(0), 1=Other (marriage: 1,3,4), 2=Married (marriage: 2)
    Marital_status = case_when(
      Marital_status == 0 ~ NA_real_,
      Marital_status == 2 ~ 2,
      Marital_status %in% c(1, 3, 4) ~ 1),
    Marital_status = factor(
      Marital_status,
      levels = c(1, 2),
      labels = c("Other", "Married")),
    
    # Personal annual income: NA=(0), 1=Less than 100,000 yuan (yearincome:1-2), 2=100,000-200,000 yuan (yearincome:3-4), 3=More than 200,000 yuan (yearincome:5-6)
    Personal_annual_income = case_when(
      Personal_annual_income == 0 ~ NA_real_,
      Personal_annual_income %in% c(1, 2) ~ 1,
      Personal_annual_income %in% c(3, 4) ~ 2,
      Personal_annual_income %in% c(5, 6) ~ 3),
    Personal_annual_income = factor(
      Personal_annual_income,
      levels = c(1, 2, 3),
      labels = c(
        "Less than 100,000 yuan",
        "100,000-200,000 yuan",
        "More than 200,000 yuan")),
    
    # Blood type: NA=(0/5), 1=A (BloodType:1), 2=B (BloodType:2), 3=O (BloodType:3), 4=AB (BloodType:4)
    Blood_type = case_when(
      Blood_type %in% c(0, 5) ~ NA_real_,
      TRUE ~ Blood_type),
    Blood_type = factor(
      Blood_type,
      levels = c(1, 2, 3, 4),
      labels = c("A", "B", "O", "AB")),
    
    # Smoking status: NA=(0), 1=No (smokeornot: 1-2), 2=Yes (smokeornot: 3-5)
    Smoking = case_when(
      Smoking == 0 ~ NA_real_,
      Smoking %in% c(1, 2) ~ 1,
      Smoking %in% c(3, 4, 5) ~ 2),
    Smoking = factor(
      Smoking,
      levels = c(1, 2),
      labels = c("No", "Yes")),
    
    # Drinking status: NA=(0), 1=No (drinkornot: 1-2), 2=Yes (drinkornot: 3-5)
    Drinking = case_when(
      Drinking == 0 ~ NA_real_,
      Drinking %in% c(1, 2) ~ 1,
      Drinking %in% c(3, 4, 5) ~ 2),
    Drinking = factor(
      Drinking,
      levels = c(1, 2),
      labels = c("No", "Yes")),
    
    # Regular eating: NA=(0), 1=No (foodtime: 1), 2=Yes (foodtime: 2)
    Regular_eating = case_when(
      Regular_eating == 0 ~ NA_real_,
      TRUE ~ Regular_eating),
    Regular_eating = factor(
      Regular_eating,
      levels = c(1, 2),
      labels = c("No", "Yes")),
    
    # Nighttime sleep: 1= < 6h (sleeptime: 1-2), 2= 6-8h (sleeptime: 3-4), 3= > 8h (sleeptime: 5-6)
    Nighttime_sleep = case_when(
      Nighttime_sleep == 0 ~ NA_real_,
      Nighttime_sleep %in% c(1, 2) ~ 1,
      Nighttime_sleep %in% c(3, 4) ~ 2,
      Nighttime_sleep %in% c(5, 6) ~ 3),
    Nighttime_sleep = factor(
      Nighttime_sleep,
      levels = c(1, 2, 3),
      labels = c("<6 h", "6-8 h", ">8 h")),
    
    # Exercise frequency: NA=(0), 1=No exercise or very rarely (FirstSportFrequ:5), 2=Once per week (FirstSportFrequ:4), 3=2--3 times per week (FirstSportFrequ:3), 4=Once or more times per day (FirstSportFrequ:1-2)
    Exercise_frequency = case_when(
      Exercise_frequency == 0 ~ NA_real_,
      Exercise_frequency == 5 ~ 1,
      Exercise_frequency == 4 ~ 2,
      Exercise_frequency == 3 ~ 3,
      Exercise_frequency %in% c(1,2)~ 4),
    Exercise_frequency = factor(
      Exercise_frequency,
      levels = c(1, 2, 3, 4),
      labels = c(
        "No exercise or very rarely",
        "Once per week",
        "2-3 times per week",
        "Once or more times per day")),
    
    # Self-rated health: NA=(0), 1=Poor(SameAgeHealth: 4+5), 2=Fair(SameAgeHealth: 3), 3=Good(SameAgeHealth: 1+2)
    Self_rated_health = case_when(
      Self_rated_health == 0 ~ NA_real_,
      Self_rated_health %in% c(4, 5) ~ 1,
      Self_rated_health == 3 ~ 2,
      Self_rated_health %in% c(1, 2) ~ 3),
    Self_rated_health = factor(
      Self_rated_health,
      levels = c(1, 2, 3),
      labels = c("Poor", "Fair", "Good")),
    
    # Disease category conversion Arthritis, Asthma, nopsick, psick01:psick26: 1=No(F/FALSE), 2=Yes(T/TRUE)
    across(c(Arthritis, Asthma, nopsick, psick01:psick26),
           ~ factor(case_when(
             . %in% c("T", "TRUE", "2") ~ 2,
             . %in% c("F", "FALSE", "1") ~ 1,
             TRUE ~ NA_real_),
             levels = c(1, 2),
             labels = c("No", "Yes")
           )
    )
  )

# Select cancer variables
Cancer_vars <- c("psick01","psick02","psick03","psick04",
                 "psick05","psick06","psick07","psick08","psick25")
data_traj2 <- data_traj2 %>%
  rowwise() %>%
  mutate(
    # Cancer: 1=No (psick01-08, psick25: all 1 or nopsick == 2), 2=Yes (psick01-08, psick25: any 2)
    Cancer = case_when(
      any(c_across(all_of(Cancer_vars)) == "Yes") ~ 2,
      all(c_across(all_of(Cancer_vars)) == "No") | nopsick == "Yes" ~ 1,
      TRUE ~ NA_real_),
    Cancer = factor(
      Cancer,
      levels = c(1, 2),
      labels = c("No", "Yes")),
    
    # Liver diseases: 1=No (psick17-18: all 1 or nopsick == 2), 2=Yes (psick17-18 any 2)
    Liver_diseases = case_when(
      any(c_across(all_of(c("Hepatitis", "Cirrhosis"))) == "Yes") ~ 2,
      all(c_across(all_of(c("Hepatitis", "Cirrhosis"))) == "No") | nopsick == "Yes" ~ 1,
      TRUE ~ NA_real_),
    Liver_diseases = factor(
      Liver_diseases,
      levels = c(1, 2),
      labels = c("No", "Yes"))
  ) %>%
  ungroup()

data_traj2 <- data_traj2 %>%
  mutate(
    # Convert variables < 0 to NA
    across(
      c(
        ESLLS, ESRLS, LHearing, RHearing,
        SBP, DBP, Height, Weight, BMI,
        Waist_circumference, Hip_circumference, BFP,
        Urine_protein, Urine_glucose, Insulin, SpO2,
        HS_CRP, HbA1c, TC, HDL_C, LDL_C, TG,
        BUN, FBG, UA, WBC, HB, MCV, PLT, HCT
      ),
      ~ if_else(. < 0, NA_real_, .)),
    
    # Vision: create new variable Vision by taking the maximum of ESLLS, ESRLS
    Vision = pmax(ESLLS, ESRLS, na.rm = TRUE),
    
    # Hearing: create new variable Hearing by taking the maximum of LHearing, RHearing
    Hearing = pmax(LHearing, RHearing, na.rm = TRUE),
    
    # Recalculate BMI (kg / m^2)
    BMI = if_else(
      !is.na(Height) & !is.na(Weight) & Height > 0 & Weight > 0,
      Weight / (Height / 100)^2,
      NA_real_
    ),
    # BMI classification
    BMI = case_when(
      BMI < 18.5 ~ 1,
      BMI >= 18.5 & BMI < 24 ~ 2,
      BMI >= 24 ~ 3,
      TRUE ~ NA_real_
    ),
    BMI = factor(
      BMI,
      levels = c(1, 2, 3),
      labels = c("<18.5", "18.5-24", "≥24")),
    
    # Urine protein classification
    Urine_protein = case_when(
      Urine_protein == 1 ~ 1,
      Urine_protein == 2 ~ 2,
      Urine_protein %in% 3:5 ~ 3,
      TRUE ~ NA_real_
    ),
    Urine_protein = factor(
      Urine_protein,
      levels = c(1, 2, 3),
      labels = c("Negative", "Weakly positive", "Positive")),
    
    # Urine glucose classification
    Urine_glucose = case_when(
      Urine_glucose == 1 ~ 1,
      Urine_glucose == 2 ~ 2,
      Urine_glucose %in% 3:5 ~ 3,
      TRUE ~ NA_real_
    ),
    Urine_glucose = factor(
      Urine_glucose,
      levels = c(1, 2, 3),
      labels = c("Negative", "Weakly positive", "Positive")),
    SpO2 = case_when(
      SpO2 > 100 | SpO2 < 70 ~ NA_real_,
      TRUE ~ SpO2
    )
  )

# Extract needed variables
vars_needed <- c(
  "Age","Sex","Education","Marital_status","Smoking","Drinking",
  "Cancer","Arthritis","Asthma","Thyroid_diseases","Tuberculosis",
  "Digestive_diseases","Liver_diseases","Urolithiasis","Gout","Anemia",
  "Vision","Hearing","SBP","DBP","BMI","Waist_circumference",
  "Hip_circumference","BFP","Urine_protein","Urine_glucose",
  "HbA1c","TC","HDL_C","LDL_C","TG","BUN","FBG","UA","WBC",
  "HB","MCV","PLT","HCT","eGFR_group", "CMDs"
)

# Final analysis dataset
data_analyze <- data_traj2 %>%
  dplyr::select(all_of(vars_needed))

# Copy dataset for variable name replacement
data_analyze1 <- data_analyze
idx2 <- match(names(data_analyze1), variable_dict$analysis_name)
colnames(data_analyze1)[!is.na(idx2)] <- variable_dict$display_name[idx2[!is.na(idx2)]]

###-------------------Baseline characteristics-------------------
# Data dictionary units
data_dict <- data.frame(rbind(
  c("Age", "Age (years), mean ± SD"),
  c("Sex", "Sex, n (%)"),
  c("Education", "Education, n (%)"),
  c("Marital_status", "Marital status, n (%)"),
  c("Smoking", "Smoking, n (%)"),
  c("Drinking", "Drinking, n (%)"),
  c("Cancer", "Cancer, n (%)"),
  c("Arthritis", "Arthritis, n (%)"),
  c("Asthma", "Asthma, n (%)"),
  c("Thyroid_diseases", "Thyroid diseases, n (%)"),
  c("Tuberculosis", "Tuberculosis, n (%)"),
  c("Digestive_diseases", "Digestive diseases, n (%)"),
  c("Liver_diseases", "Liver diseases, n (%)"),
  c("Urolithiasis", "Urolithiasis, n (%)"),
  c("Gout", "Gout, n (%)"),
  c("Anemia", "Anemia, n (%)"),
  c("Vision", "Vision, mean ± SD"),
  c("Hearing", "Hearing (dB), mean ± SD"),
  c("SBP", "SBP (mmHg), mean ± SD"),
  c("DBP", "DBP (mmHg), mean ± SD"),
  c("BMI", "BMI, n (%)"),
  c("Waist_circumference", "Waist circumference (cm), mean ± SD"),
  c("Hip_circumference", "Hip circumference (cm), mean ± SD"),
  c("BFP", "BFP (%), mean ± SD"),
  c("Urine_protein", "Urine protein, n (%)"),
  c("Urine_glucose", "Urine glucose, n (%)"),
  c("HbA1c", "HbA1c (%), mean ± SD"),
  c("TC", "TC (mmol/L), mean ± SD"),
  c("HDL_C", "HDL-C (mmol/L), mean ± SD"),
  c("LDL_C", "LDL-C (mmol/L), mean ± SD"),
  c("TG", "TG (mmol/L), mean ± SD"),
  c("BUN", "BUN (mmol/L), mean ± SD"),
  c("FBG", "FBG (mmol/L), mean ± SD"),
  c("UA", "UA (μmol/L), mean ± SD"),
  c("WBC", "WBC (×10^9/L), mean ± SD"),
  c("HB", "HB (g/L), mean ± SD"),
  c("MCV", "MCV (fL), mean ± SD"),
  c("PLT", "PLT (×10^9/L), mean ± SD"),
  c("HCT", "HCT (%), mean ± SD"),
  c("CMDs", "CMDs")
))
names(data_dict) <- c("short", "long")

# Remove eGFR_group
data_baseline <- data_analyze %>%
  select(-eGFR_group)

# Convert variable names
colnames(data_baseline) <- data_dict$long[match(names(data_baseline), data_dict$short)]

# Baseline characteristics comparison
baseline <- data_baseline %>%
  tbl_summary(by = "CMDs", # Group statistics by CMDs
              statistic = list(all_continuous() ~ "{mean} ± {sd}", # Continuous variable statistics: mean ± SD
                               all_categorical() ~ "{n} ({p})"), # Categorical variable statistics: frequency (percentage)
              digits = list(all_continuous() ~ 2, # Keep 2 decimal places for continuous variable mean and SD
                            all_categorical() ~ c(0, 2)), # Keep 2 decimal places for categorical variable percentage
              type = list(all_continuous() ~ "continuous", # Continuous variable display: mean ± SD
                          all_categorical() ~ "categorical"), # Categorical variable display all categories: frequency (percentage)
              missing = "no" # Do not display missing value rows
  ) %>%
  add_p(
    all_continuous() ~ "t.test",# Categorical variable automatically selects chi-square test or Fisher's exact test
    pvalue_fun = ~ style_pvalue(.x, digits = 3)) %>% # Add P-value and keep 3 decimal places
  add_overall() %>% # Add a column summarizing overall statistics
  modify_header(statistic ~ "t test/χ^2 (df)", p.value = "P value") %>%
  modify_table_styling(
    columns = statistic,
    fmt_fun = function(x) style_number(x, digits = 3)
  ) %>%
  modify_table_styling(
    columns = parameter,
    fmt_fun = function(x) style_number(x, digits = 0)
  ) %>%
  modify_column_merge(
    pattern = "{statistic} ({parameter})", # Format: statistic (degrees of freedom)
    rows = !is.na(statistic)
  )

# Export to Word
baseline %>%
  as_flex_table() %>%
  flextable::save_as_docx(baseline, path = "baseline characteristics.docx")

###-------------------Multicollinearity diagnosis-------------------
# Remove eGFR_group and CMDs
data_corr <- data_analyze1 %>%
  select(-eGFR_group, -CMDs)

# Convert all factor variables to numeric
data_corr_num <- data_corr %>%
  mutate(across(where(is.factor), ~ as.numeric(.)))

# Perform correlation test
corr_matrix <- corr.test(data_corr_num, method = "spearman")

# Extract correlation coefficients
r <- corr_matrix$r

# Plot
png("corrplot.png", width = 3500, height = 3000, res = 600)
corrplot.mixed(r,
               lower = "number", # Lower part graphic type as number
               number.cex = 1.5, # Correlation coefficient font size
               upper = "color", # Upper part as color blocks
               lower.col = "black", # Lower part color
               tl.pos = "lt", # Labels displayed on left and top
               tl.col = "black", # Label color black
               tl.srt = 30, # Top label display angle
               tl.cex = 2.5, # Adjust text label size
               diag = "l", # 'l' indicates display lower part content at the junction
               cl.cex = 2.5, # Legend text size
               cl.ratio = 0.1 # Legend width ratio
)
dev.off()

# Find variable pairs with absolute correlation coefficient > 0.9
r_id <- which(abs(r) > 0.9 & abs(r) < 1, arr.ind = TRUE)
high_corr <- data.frame(
  var1 = rownames(r)[r_id[,1]],
  var2 = colnames(r)[r_id[,2]],
  cor = r[r_id]
)

# Remove duplicates (keep only lower triangle)
high_corr <- high_corr[r_id[,1] < r_id[,2], ]

# Identify the first variable name to drop
corr_vars_drop <- unique(high_corr$var1)

###-------------------Missing value situation-------------------
# Calculate missing count and proportion
missing <- data.frame(
  # Add variable names
  Variable = names(data_corr),
  
  # Missing count
  Missing_Count <- sapply(
    data_corr,
    function(x) formatC(sum(is.na(x)), format = "d", big.mark = ",")
  ), # Missing proportion, keep two decimal places
  Missing_Percentage = sapply(
    data_corr,
    function(x) round(sum(is.na(x)) / nrow(data_corr) * 100, 2))
)

# Extract variable names from data_corr with missing percentage > 20%
missing_variable <- missing$Variable[missing$Missing_Percentage > 20]

# Delete variables with missing percentage > 20% and strongly correlated variables
# Find column indices of these variable names in data_analyze1
missing_columns <- which(names(data_analyze1) %in% c(missing_variable, corr_vars_drop))

# Delete these columns
data_analyze2 <- data_analyze[, -missing_columns]

## Organize missing value information into a table
# Determine variable type and add type column
missing$Type <- sapply(data_corr, function(x) if (is.factor(x)) "Categorical" else "Continuous")

# For categorical variables, output description in "code-label" format
missing$Description <- sapply(names(data_corr), function(x) {
  if (is.factor(data_corr[[x]])) {
    levels_x <- levels(data_corr[[x]]) # Get all levels of variable x and store in levels_x
    paste(paste(1:length(levels_x), levels_x, sep = ": "), collapse = ", ") # Variable coding: variable label
  }
  
  # Numerical variable unit description
  else if (x == "Age") {"years"}
  else if (x == "Hearing") {"dB"}
  else if (x == "SBP" | x == "DBP") {"mmHg"}
  else if (x == "Waist circumference" | x == "Hip circumference") {"cm"}
  else if (x == "BFP" | x == "SpO2" | x == "HbA1c" | x == "HCT") {"%"}
  else if (x == "Insulin") {"uU/ml"}
  else if (x == "HS-CRP") {"mg/L"}
  else if (x == "TC" | x == "HDL-C" | x == "LDL-C" | x == "TG" | x == "BUN" | x == "FBG") {"mmol/L"}
  else if (x == "UA") {"μmol/L"}
  else if (x == "WBC" | x == "PLT") {"×10^9/L"}
  else if (x == "HB") {"g/L"}
  else if (x == "MCV") {"fL"}
  else { "-" }
})

# Combine missing count and proportion
missing$"Missing data (n=10,967) [n (%)]" <- paste(missing$Missing_Count, " (", missing$Missing_Percentage, ")", sep = "")

# Select final needed columns
missing_description <- missing[, -c(2:3)]

###-------------------Missing value imputation: Random Forest imputation-------------------
# Convert to data frame
data_analyze2 <- as.data.frame(data_analyze2)

# Set random seed for reproducibility
set.seed(2025)

# Impute missing values in data_analyze2 using missForest()
data_missresult <- missForest(data_analyze2)

# Extract imputed data and convert to data frame
data_analyze3 <- as.data.frame(data_missresult$ximp)

###-------------------Data splitting-------------------
## Trajectory group split
# Low decline and High-stable groups
data_LH <- data_analyze3 %>%
  filter(eGFR_group %in% c(1, 3)) %>%
  select(-eGFR_group) # Delete eGFR_group column

# Unstable and High-stable groups
data_UH <- data_analyze3 %>%
  filter(eGFR_group %in% c(2, 3)) %>%
  select(-eGFR_group) # Delete eGFR_group column

## Split training and test sets for Low decline and High-stable groups
# Set random seed for reproducibility
set.seed(1217)

# createDataPartition() automatically takes equal proportions of data from each level of y, p indicates training/test set proportion, list = F returns a vector, times = 1 means perform split only once
trainindex_LH <- createDataPartition(data_LH$CMDs, p = 0.7, list = F, times = 1)

# Extract training set
data_LH_train <- data_LH[trainindex_LH, ]

# Extract test set
data_LH_test <- data_LH[-trainindex_LH, ]

## Split training and test sets for Unstable and High-stable groups
# Set random seed for reproducibility
set.seed(1217)

# createDataPartition() automatically takes equal proportions of data from each level of y, p indicates training/test set proportion, list = F returns a vector, times = 1 means perform split only once
trainindex_UH <- createDataPartition(data_UH$CMDs, p = 0.7, list = F, times = 1)

# Extract training set
data_UH_train <- data_UH[trainindex_UH, ]

# Extract test set
data_UH_test <- data_UH[-trainindex_UH, ]

###-------------------Variable standardization-------------------
## Standardize variables for Low decline and High-stable groups
# Use preProcess() to standardize (center and scale) the training set, making the mean 0 and SD 1 for each column
standardized_para_LH <- preProcess(data_LH_train, method = c("center", "scale"))

# Standardize training set
data_LH_train_std <- predict(standardized_para_LH, newdata = data_LH_train)

# Standardize test set using the standardization parameters from the training set
data_LH_test_std <- predict(standardized_para_LH, newdata = data_LH_test)

## Standardize variables for Unstable and High-stable groups
# Use preProcess() to standardize (center and scale) the training set, making the mean 0 and SD 1 for each column
standardized_para_UH <- preProcess(data_UH_train, method = c("center", "scale"))

# Standardize training set
data_UH_train_std <- predict(standardized_para_UH, newdata = data_UH_train)

# Standardize test set using the standardization parameters from the training set
data_UH_test_std <- predict(standardized_para_UH, newdata = data_UH_test)

## XGBoost, SVM cannot handle factor variables, convert dataset to numeric, except CMDs
# Define function
to_num_except_CMDs <- function(df) {
  df %>%
    mutate(
      across(
        where(is.factor) & -CMDs,
        ~ as.numeric(.)
      )
    )
}

# Perform conversion
data_LH_train_std_num <- to_num_except_CMDs(data_LH_train_std)
data_LH_test_std_num <- to_num_except_CMDs(data_LH_test_std)
data_UH_train_std_num <- to_num_except_CMDs(data_UH_train_std)
data_UH_test_std_num <- to_num_except_CMDs(data_UH_test_std)

LH_train_smotenc_num <- to_num_except_CMDs(LH_train_smotenc)
UH_train_smotenc_num <- to_num_except_CMDs(UH_train_smotenc)

###-------------------Model building: mlr3-------------------
# Suppress console output during training, 'warn' (output only warnings and errors)
lgr::get_logger("mlr3")$set_threshold("warn")
lgr::get_logger("bbotk")$set_threshold("warn")

##------------------Model building: Low decline and High-stable groups------------------
# Create training task
LH_train_task <- as_task_classif(data_LH_train_std_num, target = "CMDs")

# Create test task
LH_test_task <- as_task_classif(data_LH_test_std_num, target = "CMDs")

## Logistic Regression LR
# Select algorithm, predict output as probability
LH_lr_learner <- lrn("classif.log_reg", predict_type = "prob")

# Train on training set
LH_lr_learner$train(LH_train_task)

# Predict on training set
LH_train_lr_pred <- LH_lr_learner$predict(LH_train_task)

# Predict on test set
LH_test_lr_pred <- LH_lr_learner$predict(LH_test_task)

## Random Forest RF
# Select algorithm, predict output as probability
LH_rf_learner <- lrn("classif.ranger", predict_type = "prob")

# Select hyperparameters and their ranges for tuning
LH_rf_learner$param_set$values <- list(
  num.trees = to_tune(10, 100), # Number of trees: controls forest size, affects model stability and computational efficiency.
  max.depth = to_tune(1, 5), # Maximum tree depth: limits tree depth to prevent overfitting.
  min.node.size = to_tune(p_int(5, 10)), # Minimum node size: sets minimum samples in a node, controls tree splitting. p_int() defines integer parameter range, suitable for newer mlr3tuning
  mtry = to_tune(5, 10), # Number of random features to try at each split: controls randomness, balances diversity and accuracy.
  sample.fraction = to_tune(0.5, 1) # Proportion of samples to draw for each tree
)

# Set random seed for reproducibility
set.seed(123)

# Perform tuning
LH_rf <- tune(tuner = tnr("grid_search", resolution = 5), # Grid search; resolution = 5 means each parameter to tune is divided into 5 equally spaced values
              task = LH_train_task, # Specify tuning task
              learner = LH_rf_learner, # Specify learner to tune
              resampling = rsmp("cv", folds = 5), # 5-fold cross-validation
              measure = msr("classif.auc") # Model evaluation metric AUC
)

# Apply best hyperparameters to the learner
LH_rf_learner$param_set$values <- LH_rf$result_learner_param_vals

# Train on training set
LH_rf_learner$train(LH_train_task)

# Predict on training set
LH_train_rf_pred <- LH_rf_learner$predict(LH_train_task)

# Predict on test set
LH_test_rf_pred <- LH_rf_learner$predict(LH_test_task)

## XGBoost
LH_xgb_learner <- lrn("classif.xgboost", predict_type = "prob")
LH_xgb_learner$param_set$values <- list(
  nrounds = to_tune(500, 1000), # Number of trees: controls number of boosting iterations, determines learning extent.
  max_depth = to_tune(1, 10), # Maximum tree depth: controls tree complexity to prevent overfitting.
  eta = to_tune(0.001, 0.05), # Learning rate: controls learning speed, affects convergence and stability.
  min_child_weight = to_tune(1, 10), # Minimum sum of instance weights in a child node: restricts splitting to prevent overfitting.
  subsample = to_tune(0.1, 0.5) # Subsample ratio: provides randomness, reduces overfitting risk, increases diversity.
)

set.seed(123)
LH_xgb <- tune(tuner = tnr("grid_search", resolution = 5),
               task = LH_train_task,
               learner = LH_xgb_learner,
               resampling = rsmp("cv", folds = 5),
               measure = msr("classif.auc")
)
LH_xgb_learner$param_set$values <- LH_xgb$result_learner_param_vals
LH_xgb_learner$train(LH_train_task)
LH_train_xgb_pred <- LH_xgb_learner$predict(LH_train_task)
LH_test_xgb_pred <- LH_xgb_learner$predict(LH_test_task)

## Support Vector Machine SVM
LH_svm_learner <- lrn("classif.svm", predict_type = "prob")
LH_svm_learner$param_set$values <- list(
  type = "C-classification", # Classification problem
  kernel = "radial", # Radial Basis Function (RBF) kernel: handles complex nonlinear classification problems
  cost = to_tune(0.001, 1), # C or penalty parameter: penalty for misclassification
  gamma = to_tune(0.001, 1) # gamma parameter: defines width of RBF kernel, affects distribution of data mapped to high-dimensional feature space
)

set.seed(123)
LH_svm <- tune(tuner = tnr("grid_search", resolution = 5),
               task = LH_train_task,
               learner = LH_svm_learner,
               resampling = rsmp("cv", folds = 5),
               measure = msr("classif.auc")
)
LH_svm_learner$param_set$values <- LH_svm$result_learner_param_vals
LH_svm_learner$train(LH_train_task)
LH_train_svm_pred <- LH_svm_learner$predict(LH_train_task)
LH_test_svm_pred <- LH_svm_learner$predict(LH_test_task)

## K-Nearest Neighbors KNN
LH_knn_learner <- lrn("classif.kknn", predict_type = "prob")
LH_knn_learner$param_set$values <- list(
  k = to_tune(500, 1000) # Number of nearest neighbors
)

set.seed(123)
LH_knn <- tune(tuner = tnr("grid_search", resolution = 5),
               task = LH_train_task,
               learner = LH_knn_learner,
               resampling = rsmp("cv", folds = 5),
               measure = msr("classif.auc")
)
LH_knn_learner$param_set$values <- LH_knn$result_learner_param_vals
LH_knn_learner$train(LH_train_task)
LH_train_knn_pred <- LH_knn_learner$predict(LH_train_task)
LH_test_knn_pred <- LH_knn_learner$predict(LH_test_task)

## Decision Tree DT
LH_dt_learner <- lrn("classif.rpart", predict_type = "prob")
LH_dt_learner$param_set$values <- list(
  minsplit = to_tune(20, 50), # Minimum number of observations for a split
  minbucket = to_tune(10, 20), # Minimum number of observations in a terminal node
  cp = to_tune(0.01, 0.1), # Complexity parameter
  maxdepth = to_tune(3, 10) # Maximum tree depth
)

set.seed(123)
LH_dt <- tune(tuner = tnr("grid_search", resolution = 5),
              task = LH_train_task,
              learner = LH_dt_learner,
              resampling = rsmp("cv", folds = 5),
              measure = msr("classif.auc")
)
LH_dt_learner$param_set$values <- LH_dt$result_learner_param_vals
LH_dt_learner$train(LH_train_task)
LH_train_dt_pred <- LH_dt_learner$predict(LH_train_task)
LH_test_dt_pred <- LH_dt_learner$predict(LH_test_task)

## Artificial Neural Network ANN
LH_ann_learner <- lrn("classif.nnet", predict_type = "prob")
LH_ann_learner$param_set$values <- list(
  size = to_tune(3, 10), # Number of neurons in the hidden layer, larger size -> stronger model capacity, can fit more complex nonlinear relationships
  decay = to_tune(0.001, 0.01) # Weight decay coefficient / L2 regularization strength, larger decay -> stronger penalty on weights, weights are shrunk -> less prone to overfitting, but too large can cause severe underfitting
)

set.seed(123)
LH_ann <- tune(tuner = tnr("grid_search", resolution = 5),
               task = LH_train_task,
               learner = LH_ann_learner,
               resampling = rsmp("cv", folds = 5),
               measure = msr("classif.auc")
)
LH_ann_learner$param_set$values <- LH_ann$result_learner_param_vals
LH_ann_learner$train(LH_train_task)
LH_train_ann_pred <- LH_ann_learner$predict(LH_train_task)
LH_test_ann_pred <- LH_ann_learner$predict(LH_test_task)

##------------------Model building: Unstable and High-stable groups------------------
# Create training task
UH_train_task <- as_task_classif(data_UH_train_std_num, target = "CMDs")

# Create test task
UH_test_task <- as_task_classif(data_UH_test_std_num, target = "CMDs")

## Logistic Regression LR
# Select algorithm, predict output as probability
UH_lr_learner <- lrn("classif.log_reg", predict_type = "prob")

# Train on training set
UH_lr_learner$train(UH_train_task)

# Predict on training set
UH_train_lr_pred <- UH_lr_learner$predict(UH_train_task)

# Predict on test set
UH_test_lr_pred <- UH_lr_learner$predict(UH_test_task)

## Random Forest RF
# Select algorithm, predict output as probability
UH_rf_learner <- lrn("classif.ranger", predict_type = "prob")

# Select hyperparameters and their ranges for tuning
UH_rf_learner$param_set$values <- list(
  num.trees = to_tune(10, 100), # Number of trees: controls forest size, affects model stability and computational efficiency.
  max.depth = to_tune(1, 5), # Maximum tree depth: limits tree depth to prevent overfitting.
  min.node.size = to_tune(p_int(1, 5)), # Minimum node size: sets minimum samples in a node, controls tree splitting. p_int() defines integer parameter range, suitable for newer mlr3tuning
  mtry = to_tune(1, 5), # Number of random features to try at each split: controls randomness, balances diversity and accuracy.
  sample.fraction = to_tune(0.3, 1) # Proportion of samples to draw for each tree
)

# Set random seed for reproducibility
set.seed(123)

# Perform tuning
UH_rf <- tune(tuner = tnr("grid_search", resolution = 5), # Grid search; resolution = 5 means each parameter to tune is divided into 5 equally spaced values
              task = UH_train_task, # Specify tuning task
              learner = UH_rf_learner, # Specify learner to tune
              resampling = rsmp("cv", folds = 5), # 5-fold cross-validation
              measure = msr("classif.auc") # Model evaluation metric AUC
)

# Apply best hyperparameters to the learner
UH_rf_learner$param_set$values <- UH_rf$result_learner_param_vals

# Train on training set
UH_rf_learner$train(UH_train_task)

# Predict on training set
UH_train_rf_pred <- UH_rf_learner$predict(UH_train_task)

# Predict on test set
UH_test_rf_pred <- UH_rf_learner$predict(UH_test_task)

## XGBoost
UH_xgb_learner <- lrn("classif.xgboost", predict_type = "prob")
UH_xgb_learner$param_set$values <- list(
  nrounds = to_tune(100, 500), # Number of trees: controls number of boosting iterations, determines learning extent.
  max_depth = to_tune(1, 10), # Maximum tree depth: controls tree complexity to prevent overfitting.
  eta = to_tune(0.001, 0.05), # Learning rate: controls learning speed, affects convergence and stability.
  min_child_weight = to_tune(1, 10), # Minimum sum of instance weights in a child node: restricts splitting to prevent overfitting.
  subsample = to_tune(0.5, 1) # Subsample ratio: provides randomness, reduces overfitting risk, increases diversity.
)

set.seed(123)
UH_xgb <- tune(tuner = tnr("grid_search", resolution = 5),
               task = UH_train_task,
               learner = UH_xgb_learner,
               resampling = rsmp("cv", folds = 5),
               measure = msr("classif.auc")
)
UH_xgb_learner$param_set$values <- UH_xgb$result_learner_param_vals
UH_xgb_learner$train(UH_train_task)
UH_train_xgb_pred <- UH_xgb_learner$predict(UH_train_task)
UH_test_xgb_pred <- UH_xgb_learner$predict(UH_test_task)

## Support Vector Machine SVM
UH_svm_learner <- lrn("classif.svm", predict_type = "prob")
UH_svm_learner$param_set$values <- list(
  type = "C-classification", # Classification problem
  kernel = "radial", # Radial Basis Function (RBF) kernel: handles complex nonlinear classification problems
  cost = to_tune(1, 10), # C or penalty parameter: penalty for misclassification
  gamma = to_tune(0.001, 1) # gamma parameter: defines width of RBF kernel, affects distribution of data mapped to high-dimensional feature space
)

set.seed(123)
UH_svm <- tune(tuner = tnr("grid_search", resolution = 5),
               task = UH_train_task,
               learner = UH_svm_learner,
               resampling = rsmp("cv", folds = 5),
               measure = msr("classif.auc")
)
UH_svm_learner$param_set$values <- UH_svm$result_learner_param_vals
UH_svm_learner$train(UH_train_task)
UH_train_svm_pred <- UH_svm_learner$predict(UH_train_task)
UH_test_svm_pred <- UH_svm_learner$predict(UH_test_task)

## K-Nearest Neighbors KNN
UH_knn_learner <- lrn("classif.kknn", predict_type = "prob")
UH_knn_learner$param_set$values <- list(
  k = to_tune(100, 1000) # Number of nearest neighbors
)

set.seed(123)
UH_knn <- tune(tuner = tnr("grid_search", resolution = 5),
               task = UH_train_task,
               learner = UH_knn_learner,
               resampling = rsmp("cv", folds = 5),
               measure = msr("classif.auc")
)
UH_knn_learner$param_set$values <- UH_knn$result_learner_param_vals
UH_knn_learner$train(UH_train_task)
UH_train_knn_pred <- UH_knn_learner$predict(UH_train_task)
UH_test_knn_pred <- UH_knn_learner$predict(UH_test_task)

## Decision Tree DT
UH_dt_learner <- lrn("classif.rpart", predict_type = "prob")
UH_dt_learner$param_set$values <- list(
  minsplit = to_tune(20, 50), # Minimum number of observations for a split
  minbucket = to_tune(10, 20), # Minimum number of observations in a terminal node
  cp = to_tune(0.01, 0.1), # Complexity parameter
  maxdepth = to_tune(3, 10) # Maximum tree depth
)

set.seed(123)
UH_dt <- tune(tuner = tnr("grid_search", resolution = 5),
              task = UH_train_task,
              learner = UH_dt_learner,
              resampling = rsmp("cv", folds = 5),
              measure = msr("classif.auc")
)
UH_dt_learner$param_set$values <- UH_dt$result_learner_param_vals
UH_dt_learner$train(UH_train_task)
UH_train_dt_pred <- UH_dt_learner$predict(UH_train_task)
UH_test_dt_pred <- UH_dt_learner$predict(UH_test_task)

## Artificial Neural Network ANN
UH_ann_learner <- lrn("classif.nnet", predict_type = "prob")
UH_ann_learner$param_set$values <- list(
  size = to_tune(1, 10), # Number of neurons in the hidden layer, larger size -> stronger model capacity, can fit more complex nonlinear relationships
  decay = to_tune(0.001, 0.01) # Weight decay coefficient / L2 regularization strength, larger decay -> stronger penalty on weights, weights are shrunk -> less prone to overfitting, but too large can cause severe underfitting
)

set.seed(123)
UH_ann <- tune(tuner = tnr("grid_search", resolution = 5),
               task = UH_train_task,
               learner = UH_ann_learner,
               resampling = rsmp("cv", folds = 5),
               measure = msr("classif.auc")
)
UH_ann_learner$param_set$values <- UH_ann$result_learner_param_vals
UH_ann_learner$train(UH_train_task)
UH_train_ann_pred <- UH_ann_learner$predict(UH_train_task)
UH_test_ann_pred <- UH_ann_learner$predict(UH_test_task)

###-------------------Model performance evaluation: 35 factors-------------------
##------------------Model performance evaluation: Low decline and High-stable groups------------------
# Create lists of model predictions for training and test sets
LH_train_preds <- list(
  LR = LH_train_lr_pred,
  RF = LH_train_rf_pred,
  XGBoost = LH_train_xgb_pred,
  SVM = LH_train_svm_pred,
  KNN = LH_train_knn_pred,
  DT = LH_train_dt_pred,
  ANN = LH_train_ann_pred
)

LH_test_preds <- list(
  LR = LH_test_lr_pred,
  RF = LH_test_rf_pred,
  XGBoost = LH_test_xgb_pred,
  SVM = LH_test_svm_pred,
  KNN = LH_test_knn_pred,
  DT = LH_test_dt_pred,
  ANN = LH_test_ann_pred
)

# Create empty data frames for storing performance evaluation results for training and test sets
LH_train_metrics <- data.frame()
LH_test_metrics <- data.frame()

# Create empty data frames for storing ROC data for training and test sets
LH_train_roc_data <- data.frame()
LH_test_roc_data <- data.frame()

# Create lists for storing ROC() results for training and test sets for DeLong test
LH_train_roc_auc <- list()
LH_test_roc_auc <- list()

# Create lists for storing model prediction probability data for training and test sets (calibration curve)
LH_train_prob_data <- list()
LH_test_prob_data <- list()

# Create empty data frames for storing DCA data for training and test sets
LH_train_dca_data <- data.frame()
LH_test_dca_data <- data.frame()

## Define performance metric calculation function
calculate_metrics <- function(pred_prob, true_class, threshold) {
  # Generate predicted class based on threshold
  pred_class <- ifelse(pred_prob >= threshold, "1", "0")
  
  # Build confusion matrix, ensure all classes are present
  cm <- table(
    factor(pred_class, levels = c("0", "1")), # Ensure predicted classes include No and Yes
    factor(true_class, levels = c("0", "1")) # Ensure actual classes include No and Yes
  )
  
  # Extract confusion matrix elements, prevent errors if some categories are missing
  TP <- cm["1", "1"] # True Positive
  TN <- cm["0", "0"] # True Negative
  FP <- cm["1", "0"] # False Positive
  FN <- cm["0", "1"] # False Negative
  
  # Calculate metrics
  sensitivity <- TP / (TP + FN) # Sensitivity / Recall
  specificity <- TN / (TN + FP) # Specificity
  PPV <- TP / (TP + FP) # Positive Predictive Value / Precision
  NPV <- TN / (TN + FN) # Negative Predictive Value
  CCR <- (TP + TN) / sum(cm) # Accuracy
  f1_score <- 2 * (PPV * sensitivity) / (PPV + sensitivity) # F1-score
  
  # Return results
  list(Sensitivity = sensitivity, Specificity = specificity, PPV = PPV, NPV = NPV, CCR = CCR, F1_score = f1_score)
}

# Bootstrap confidence interval function
bootstrap_ci <- function(pred_prob, true_class, threshold, B = 1000) {
  n <- length(true_class)
  
  boot_metrics <- data.frame(
    Sensitivity = numeric(B),
    Specificity = numeric(B),
    PPV = numeric(B),
    NPV = numeric(B),
    CCR = numeric(B),
    F1_score = numeric(B),
    Brier_score = numeric(B)
  )
  
  for (b in 1:B) {
    idx <- sample(1:n, n, replace = TRUE)
    boot_prob <- pred_prob[idx]
    boot_true <- true_class[idx]
    metrics <- calculate_metrics(boot_prob, boot_true, threshold)
    boot_metrics[b, "Sensitivity"] <- metrics$Sensitivity
    boot_metrics[b, "Specificity"] <- metrics$Specificity
    boot_metrics[b, "PPV"] <- metrics$PPV
    boot_metrics[b, "NPV"] <- metrics$NPV
    boot_metrics[b, "CCR"] <- metrics$CCR
    boot_metrics[b, "F1_score"] <- metrics$F1_score
    boot_metrics[b, "Brier_score"] <- mean((boot_prob - boot_true)^2)
  }
  apply(boot_metrics, 2, quantile, probs = c(0.025, 0.975), na.rm = TRUE)
}

format_ci <- function(estimate, ci_vec) {
  sprintf("%.3f\n(%.3f-%.3f)", estimate, ci_vec[1], ci_vec[2])
}

# Calculate performance metrics for training set
for (model_name in names(LH_train_preds)) {
  # Get prediction results for training set model
  pred_train <- LH_train_preds[[model_name]]
  
  # Get predicted probabilities from training set model
  pred_prob_train <- pred_train$prob[, "Yes"]
  
  # Store predicted probabilities for training set model in list
  LH_train_prob_data[[model_name]] <- pred_prob_train
  
  # Get actual class labels for training set
  true_class_train <- data_LH_train_std_num$CMDs
  true_class_train <- ifelse(true_class_train == "Yes", 1, 0)
  roc_train <- roc(true_class_train, pred_prob_train)
  auc_train <- roc_train$auc
  ci_train <- ci(roc_train)
  
  # Combine AUC and 95%CI, keep three decimal places
  auc_with_ci_train <- sprintf("%.3f\n(%.3f-%.3f)", auc_train, ci_train[1], ci_train[3])
  
  # Store roc() result in list
  LH_train_roc_auc[[model_name]] <- roc_train
  
  # Extract data for ROC curve and add to data frame
  roc_data_train <- data.frame(
    FPR = 1-roc_train$specificities, # False Positive Rate: 1-specificities
    TPR = roc_train$sensitivities, # True Positive Rate: sensitivities
    model = model_name # Model name
  )
  
  # Order model names
  roc_data_train$model <- factor(roc_data_train$model, levels = c("LR", "RF", "XGBoost", "SVM", "KNN", "DT", "ANN"))
  
  # Combine ROC data for all models for test set
  LH_train_roc_data <- rbind(LH_train_roc_data, roc_data_train)
  
  ## Calculate best threshold for training set
  # Define threshold range
  threshold <- seq(0, 1, by = 0.001)
  
  # Calculate performance metrics for different thresholds
  metrics_list_train <- sapply(threshold, function(t) {
    calculate_metrics(pred_prob_train, true_class_train, t)
  }, simplify = F)
  distances <- sapply(metrics_list_train, function(metrics) {
    sqrt((1-metrics$Sensitivity)^2 + (1-metrics$Specificity)^2)
  })
  
  # Find best threshold for training set
  best_threshold_train <- threshold[which.min(distances)]
  
  # Calculate performance metrics for training set using the best threshold
  best_metrics_train <- calculate_metrics(pred_prob_train, true_class_train, best_threshold_train)
  
  # Calculate Brier score
  brier_score_train <- mean((pred_prob_train - true_class_train)^2)
  
  # Bootstrap CI
  set.seed(123)
  ci_train_metrics <- bootstrap_ci(pred_prob_train, true_class_train, best_threshold_train, B = 1000)
  Sensitivity_CI <- format_ci(best_metrics_train$Sensitivity, ci_train_metrics[, "Sensitivity"])
  Specificity_CI <- format_ci(best_metrics_train$Specificity, ci_train_metrics[, "Specificity"])
  PPV_CI <- format_ci(best_metrics_train$PPV, ci_train_metrics[, "PPV"])
  NPV_CI <- format_ci(best_metrics_train$NPV, ci_train_metrics[, "NPV"])
  CCR_CI <- format_ci(best_metrics_train$CCR, ci_train_metrics[, "CCR"])
  F1_score_CI <- format_ci(best_metrics_train$F1_score, ci_train_metrics[, "F1_score"])
  Brier_score_CI <- format_ci(brier_score_train, ci_train_metrics[, "Brier_score"])
  
  # Summarize training set model results
  train_metrics_result <- data.frame(
    Model = model_name,
    Dataset = "Training cohort",
    AUC_CI = auc_with_ci_train,
    Threshold = round(best_threshold_train, 3),
    Sensitivity_CI = Sensitivity_CI,
    Specificity_CI = Specificity_CI,
    PPV_CI = PPV_CI,
    NPV_CI = NPV_CI,
    CCR_CI = CCR_CI,
    F1_score_CI = F1_score_CI,
    Brier_score_CI = Brier_score_CI
  )
  
  # Add results for each model for training set to data frame
  LH_train_metrics <- rbind(LH_train_metrics, train_metrics_result)
  
  ## Extract DCA data for training set
  # Construct data frame: truth and predicted probability
  train_truth_p <- data.frame(
    truth = true_class_train, # Convert truth to 0/1
    p = pred_prob_train # Extract predicted probability
  )
  
  # Calculate decision curve
  train_dca_result <- decision_curve(truth ~ p, data = train_truth_p, family = binomial, thresholds = seq(0, 1, by = 0.01))
  
  # Extract data and add to data frame
  train_thresholds <- train_dca_result$derived.data$thresholds
  train_net_benefit <- train_dca_result$derived.data$sNB
  train_model <- train_dca_result$derived.data$model
  train_model <- ifelse(train_model == "truth ~ p", model_name, train_model)
  
  # Combine decision data
  LH_train_dca_data <- rbind(LH_train_dca_data, data.frame(
    threshold = train_thresholds,
    net_benefit = train_net_benefit,
    model = train_model
  ))
  
  # Order model names
  LH_train_dca_data$model <- factor(LH_train_dca_data$model, levels = c("LR", "RF", "XGBoost", "SVM", "KNN", "DT", "ANN", "All", "None"))
}

# Calculate performance metrics for test set
for (model_name in names(LH_test_preds)) {
  # Get prediction results for test set model
  pred_test <- LH_test_preds[[model_name]]
  
  # Get predicted probabilities from test set model
  pred_prob_test <- pred_test$prob[, "Yes"]
  
  # Store predicted probabilities for test set model in list
  LH_test_prob_data[[model_name]] <- pred_prob_test
  
  # Get actual class labels for test set
  true_class_test <- data_LH_test_std_num$CMDs
  true_class_test <- ifelse(true_class_test == "Yes", 1, 0)
  
  # Calculate test set AUC and 95%CI
  roc_test <- roc(true_class_test, pred_prob_test)
  auc_test <- roc_test$auc
  ci_test <- ci(roc_test)
  
  # Combine AUC and 95%CI, keep three decimal places
  auc_with_ci_test <- sprintf("%.3f\n(%.3f-%.3f)", auc_test, ci_test[1], ci_test[3])
  
  # Store roc() result in list
  LH_test_roc_auc[[model_name]] <- roc_test
  
  # Extract data for ROC curve and add to data frame
  roc_data_test <- data.frame(
    FPR = 1-roc_test$specificities, # False Positive Rate: 1-specificities
    TPR = roc_test$sensitivities, # True Positive Rate: sensitivities
    model = model_name # Model name
  )
  
  # Order model names
  roc_data_test$model <- factor(roc_data_test$model, levels = c("LR", "RF", "XGBoost", "SVM", "KNN", "DT", "ANN"))
  
  # Combine ROC data for all models for test set
  LH_test_roc_data <- rbind(LH_test_roc_data, roc_data_test)
  
  # Get best threshold from training set
  best_threshold_train <- LH_train_metrics[LH_train_metrics$Model == model_name, "Threshold"]
  
  # Calculate test set performance using training set's best threshold
  best_metrics_test <- calculate_metrics(pred_prob_test, true_class_test, best_threshold_train)
  
  # Calculate test set Brier score
  brier_score_test <- mean((pred_prob_test - true_class_test)^2)
  
  # Bootstrap CI
  set.seed(123)
  ci_test_metrics <- bootstrap_ci(pred_prob_test, true_class_test, best_threshold_train, B = 1000)
  Sensitivity_CI <- format_ci(best_metrics_test$Sensitivity, ci_test_metrics[, "Sensitivity"])
  Specificity_CI <- format_ci(best_metrics_test$Specificity, ci_test_metrics[, "Specificity"])
  PPV_CI <- format_ci(best_metrics_test$PPV, ci_test_metrics[, "PPV"])
  NPV_CI <- format_ci(best_metrics_test$NPV, ci_test_metrics[, "NPV"])
  CCR_CI <- format_ci(best_metrics_test$CCR, ci_test_metrics[, "CCR"])
  F1_score_CI <- format_ci(best_metrics_test$F1_score, ci_test_metrics[, "F1_score"])
  Brier_score_CI <- format_ci(brier_score_test, ci_test_metrics[, "Brier_score"])
  
  # Summarize test set model results into data frame
  test_metrics_result <- data.frame(
    Model = model_name,
    Dataset = "Testing cohort",
    AUC_CI = auc_with_ci_test,
    Threshold = round(best_threshold_train, 3), # Use training set's best threshold
    Sensitivity_CI = Sensitivity_CI,
    Specificity_CI = Specificity_CI,
    PPV_CI = PPV_CI,
    NPV_CI = NPV_CI,
    CCR_CI = CCR_CI,
    F1_score_CI = F1_score_CI,
    Brier_score_CI = Brier_score_CI
  )
  
  # Combine results for each model for test set
  LH_test_metrics <- rbind(LH_test_metrics, test_metrics_result)
  
  ## Extract DCA data for test set
  # Construct data frame: truth and predicted probability
  test_truth_p <- data.frame(
    truth = true_class_test, # Convert truth to 0/1
    p = pred_prob_test # Extract predicted probability
  )
  
  # Calculate decision curve
  test_dca_result <- decision_curve(truth ~ p, data = test_truth_p, family = binomial, thresholds = seq(0, 1, by = 0.01))
  
  # Extract data and add to data frame
  test_thresholds <- test_dca_result$derived.data$thresholds
  test_net_benefit <- test_dca_result$derived.data$sNB
  test_model <- test_dca_result$derived.data$model
  test_model <- ifelse(test_model == "truth ~ p", model_name, test_model)
  
  # Combine decision data
  LH_test_dca_data <- rbind(LH_test_dca_data, data.frame(
    threshold = test_thresholds,
    net_benefit = test_net_benefit,
    model = test_model
  ))
  
  # Order model names
  LH_test_dca_data$model <- factor(LH_test_dca_data$model, levels = c("LR", "RF", "XGBoost", "SVM", "KNN", "DT", "ANN", "All", "None"))
}

##------------------DeLong test------------------
# Initialize an empty data frame for storing test set DeLong test results
LH_test_auc_comparisons <- data.frame()

# Get combinations of model names for the training set
model_names <- names(LH_test_roc_auc)

# P-value formatting function
pvalue_format <- Vectorize(function(p) {
  if (is.na(p)) return("")
  if (p < 0.001) {
    return("<0.001")
  } else {
    return(sprintf("%.3f", p))
  }
})

# Pairwise ROC curve comparisons, extract Z-scores and p-values for test set
for (i in 1:(length(model_names) - 1)) {
  for (j in (i + 1):length(model_names)) {
    # Get ROC curve data for the two models
    test_roc1 <- LH_test_roc_auc[[model_names[i]]]
    test_roc2 <- LH_test_roc_auc[[model_names[j]]]
    
    # DeLong test
    test_delong_test <- roc.test(test_roc1, test_roc2, method = "delong")
    
    # Extract Z-score and p-value, rounding Z-score to 3 decimals and formatting p-value
    test_Z_score <- round(test_delong_test$statistic, 3)
    test_P_value <- pvalue_format(test_delong_test$p.value)
    
    # Add result to data frame and combine
    LH_test_auc_comparisons <- rbind(LH_test_auc_comparisons, data.frame(
      Model_comparison = paste(model_names[i], model_names[j], sep = " vs. "),
      LH_test_Z_score = test_Z_score,
      LH_test_P_value = test_P_value
    ))
  }
}

#------------------Plot ROC curves for all models on the test set------------------
LH_test_roc <- ggplot(LH_test_roc_data, aes(x = FPR, y = TPR, color = model)) +
  geom_line(size = 0.1) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.2)) + # Set x-axis ticks
  scale_y_continuous(breaks = seq(0, 1, by = 0.2)) + # Set y-axis ticks
  
  # Custom colors and legend labels
  scale_color_manual(
    values = c("#4DAF9C", "#807DBA", "#E34A33", "#3182BD", "#A63603", "#31A354", "#8C6D31"),
    labels = c(
      expression("LR (AUC=0.791)"),
      expression("RF (AUC=0.801, " * italic(P) * "=0.380)"),
      expression("XGBoost (AUC=0.806, " * italic(P) * "=0.133)"),
      expression("SVM (AUC=0.778, " * italic(P) * "=0.291)"),
      expression("KNN (AUC=0.763, " * italic(P) * "=0.026)"),
      expression("DT (AUC=0.665, " * italic(P) * "<0.001)"),
      expression("ANN (AUC=0.776, " * italic(P) * "=0.114)"))
  ) +
  labs(x = "1-Specificity",
       y = "Sensitivity",
       title = "ROC Curve", # Plot title
       color = "") + # Legend title blank
  theme_minimal() + # Set chart theme
  theme(
    plot.background = element_rect(fill = "white", color = NA), # Overall image background, border color none
    panel.grid = element_blank(), # Remove grid lines
    panel.border = element_rect(color = "black", fill = NA), # Add border
    axis.ticks.x = element_line(color = "black"), # x-axis tick marks
    axis.ticks.y = element_line(color = "black"), # y-axis tick marks
    axis.ticks.length = unit(0.1, "cm"), # Tick mark length and placement outside
    legend.position = c(0.75, 0.22), # Legend placed at lower right
    legend.key.height = unit(0.2, "cm"), # Control height of each legend item
    legend.key.width = unit(0.4, "cm"), # Control width of each legend item
    axis.title = element_text(size = 60), # Axis title font size
    axis.text = element_text(size = 50), # Axis tick label font size
    legend.text = element_text(size = 25), # Legend text font size
    plot.title = element_text(size = 60, hjust = 0.5), # Plot title font, center alignment
    plot.tag = element_text(size = 90, face = "bold") # Set tag text size
  ) +
  coord_fixed(ratio = 5/7) + # Set plot aspect ratio
  annotate(geom = "segment", x = 0, y = 0, xend = 1, yend = 1, color = "black", size = 0.2, linetype = "dotted") # Add diagonal line

##------------------Plot calibration curves for test set------------------
# Extract test task data
LH_test_task_data <- LH_test_task$data()
LH_test_task_data$CMDs <- factor(ifelse(LH_test_task_data$CMDs == "Yes", 1, 0))

# Score the models
LH_test_score <- Score(list(LR = LH_test_prob_data[["LR"]],
                            RF = LH_test_prob_data[["RF"]],
                            XGBoost = LH_test_prob_data[["XGBoost"]],
                            SVM = LH_test_prob_data[["SVM"]],
                            KNN = LH_test_prob_data[["KNN"]],
                            DT = LH_test_prob_data[["DT"]],
                            ANN = LH_test_prob_data[["ANN"]]),
                       formula = CMDs ~ 1, # Model evaluation formula
                       null.model = F, # Do not use null model for comparison
                       plots = "calibration", # Plot calibration curve
                       data = LH_test_task_data)

# Extract calibration curve data
LH_test_calibration_plot <- plotCalibration(LH_test_score, method = "nne", bandwidth = 0.05, plot = FALSE)
LH_test_calibration_data <- imap_dfr(LH_test_calibration_plot$plotFrames, ~ {
  .x %>%
    as_tibble() %>%
    mutate(model = .y) # .y is the list element name (lr/rf/svm etc.)
})
LH_test_calibration_data$model <- factor(LH_test_calibration_data$model, levels = c("LR", "RF", "XGBoost", "SVM", "KNN", "DT", "ANN"))

# Create a custom segment data for the ideal calibration line
segment_data <- data.frame(
  x = 0, y = 0, xend = 1, yend = 1,
  segment_type = "Ideal" # For legend labeling
)

# Plot calibration curve
LH_test_calibration <- ggplot(LH_test_calibration_data, aes(x = Pred, y = Obs, color = model)) +
  geom_line(linewidth = 0.1) + # Set line thickness
  
  # Use geom_segment() to draw the ideal calibration line
  geom_segment(data = segment_data, aes(x = x, y = y, xend = xend, yend = yend, color = segment_type), linewidth = 0.2, linetype = "dotted") +
  scale_x_continuous(limits = c(0, 1), # Limit x-axis range
                     breaks = seq(0, 1, by = 0.2), name = "Predicted Probability") + # Set x-axis ticks and labels
  scale_y_continuous(limits = c(0, 1), # Limit y-axis range
                     breaks = seq(0, 1, by = 0.2), name = "Actual Probability") + # Set y-axis ticks and labels
  
  # Custom colors and legend labels
  scale_color_manual(
    values = c("#4DAF9C", "#807DBA", "#E34A33", "#3182BD", "#A63603", "#31A354", "#8C6D31", "black"),
    labels = c("LR", "RF", "XGBoost", "SVM", "KNN", "DT", "ANN", "Ideal")
  ) +
  labs(title = "Calibration Curve", # Plot title
       color = "") + # Legend title blank
  theme_minimal() + # Set chart theme
  theme(
    plot.background = element_rect(fill = "white", color = NA), # Overall image background, border color none
    panel.grid = element_blank(), # Remove grid lines
    panel.border = element_rect(color = "black", fill = NA), # Add border
    axis.ticks.x = element_line(color = "black"), # x-axis tick marks
    axis.ticks.y = element_line(color = "black"), # y-axis tick marks
    axis.ticks.length = unit(0.1, "cm"), # Tick mark length and placement outside
    legend.position = c(0.15, 0.82), # Legend placed at upper left
    legend.key.height = unit(0.2, "cm"), # Control height of each legend item
    legend.key.width = unit(0.4, "cm"), # Control width of each legend item
    axis.title = element_text(size = 60), # Axis title font size
    axis.text = element_text(size = 50), # Axis tick label font size
    legend.text = element_text(size = 30), # Legend text font size
    plot.title = element_text(size = 60, hjust = 0.5), # Plot title font, center alignment
    plot.tag = element_text(size = 90, face = "bold") # Set tag text size
  ) +
  coord_fixed(ratio = 5/7) # Set plot aspect ratio

#------------------Plot DCA for test set------------------
LH_test_dca <- ggplot(LH_test_dca_data, aes(x = threshold, y = net_benefit, color = model)) +
  geom_line(linewidth = 0.1) + # Set line thickness
  scale_x_continuous(limits = c(0, 0.8), # Limit x-axis range
                     breaks = seq(0, 0.8, by = 0.2), # Set x-axis ticks
                     name = "High Risk Threshold") + # Set x-axis label
  scale_y_continuous(limits = c(-0.2, 0.8), # Limit y-axis range
                     breaks = seq(-0.2, 0.8, by = 0.2), # Set y-axis ticks
                     name = "Standardized Net Benefit") + # Set y-axis label
  scale_color_manual(values = c("#4DAF9C", "#807DBA", "#E34A33", "#3182BD",
                                "#A63603", "#31A354", "#8C6D31", "gray", "black")) + # Custom colors
  labs(title = "DCA", # Plot title
       color = "") + # Legend title blank
  theme_minimal() + # Set chart theme
  theme(
    plot.background = element_rect(fill = "white", color = NA), # Overall image background, border color none
    panel.grid = element_blank(), # Remove grid lines
    panel.border = element_rect(color = "black", fill = NA), # Add border
    axis.ticks.x = element_line(color = "black"), # x-axis tick marks
    axis.ticks.y = element_line(color = "black"), # y-axis tick marks
    axis.ticks.length = unit(0.1, "cm"), # Tick mark length and placement outside
    legend.position = c(0.86, 0.79), # Legend placed at upper right
    legend.key.height = unit(0.2, "cm"), # Control height of each legend item
    legend.key.width = unit(0.4, "cm"), # Control width of each legend item
    axis.title = element_text(size = 60), # Axis title font size
    axis.text = element_text(size = 50), # Axis tick label font size
    legend.text = element_text(size = 30), # Legend text font size
    plot.title = element_text(size = 60, hjust = 0.5), # Plot title font, center alignment
    plot.tag = element_text(size = 90, face = "bold") # Set tag text size
  ) +
  coord_fixed(ratio = 4/7) # Set plot aspect ratio

##------------------Plot radar charts for test set------------------
# Convert data to long format suitable for radar charts
LH_test_radar_data <- LH_test_metrics %>%
  select(3, 5:11) %>%
  mutate(across(ends_with("_CI"),
                ~ as.numeric(sub("\\s*\\(.*$", "", .x))))
LH_test_radar_data <- t(LH_test_radar_data) # Transpose data frame
LH_test_radar_data <- as.data.frame(LH_test_radar_data) # Convert back to data frame
colnames(LH_test_radar_data) <- c("LR", "RF", "XGBoost", "SVM", "KNN", "DT", "ANN") # Add column names
LH_test_radar_data$metrics <- c("AUC", "Sensitivity", "Specificity", "PPV", "NPV", "CCR", "F1 score", "Brier score") # Add performance column
LH_test_radar_data <- LH_test_radar_data[, c("metrics", "LR", "RF", "XGBoost", "SVM", "KNN", "DT", "ANN")] # Reorder columns
LH_test_radar_data$metrics <- factor(LH_test_radar_data$metrics, levels=c("AUC", "Sensitivity", "Specificity", "PPV", "NPV", "CCR", "F1 score", "Brier score")) # Convert to factor

# Plot radar chart - AUC
LH_test_AUC <- ggradar(LH_test_radar_data[1,],
                       grid.line.width = 0.4, # Grid line width
                       grid.label.size = 11, # Grid label font size
                       axis.label.size = 9, # Axis label font size
                       grid.min = 0, # Grid minimum value
                       grid.mid = 0.45, # Grid mid value
                       grid.max = 0.9, # Grid maximum value
                       values.radar = c(0, 0.45, 0.9), # Axis label display
                       background.circle.colour = "white", # Background color
                       group.line.width = 0.5, # Line width
                       group.point.size = 1, # Point size
                       fill = T, # Fill color
                       fill.alpha = 0.3, # Fill alpha transparency
                       group.colours = "#95B0E0") +
  ggtitle("AUC") + # Add title
  theme(plot.title = element_text(hjust = 0.5, size = 40), # Center title, set label text size
        plot.margin = margin(0, 0, 0, 0, "cm"), # Adjust margins
        plot.tag = element_text(size = 50, face = "bold") # Upper right corner label font size
  ) +
  coord_fixed(ratio = 1) # Set plot to square

# Plot radar chart - Sensitivity
LH_test_Sensitivity <- ggradar(LH_test_radar_data[2,],
                               grid.line.width = 0.4, # Grid line width
                               grid.label.size = 11, # Grid label font size
                               axis.label.size = 9, # Axis label font size
                               grid.min = 0, # Grid minimum value
                               grid.mid = 0.4, # Grid mid value
                               grid.max = 0.8, # Grid maximum value
                               values.radar = c(0, 0.4, 0.8), # Axis label display
                               background.circle.colour = "white", # Background color
                               group.line.width = 0.5, # Line width
                               group.point.size = 1, # Point size
                               fill = T, # Fill color
                               fill.alpha = 0.3, # Fill alpha transparency
                               group.colours = "#56AEDE") +
  ggtitle("Sensitivity") + # Add title
  theme(plot.title = element_text(hjust = 0.5, size = 40), # Center title, set label text size
        plot.margin = margin(0, 0, 0, 0, "cm"), # Adjust margins
        plot.tag = element_text(size = 50, face = "bold") # Upper right corner label font size
  ) +
  coord_fixed(ratio = 1) # Set plot to square

# Plot radar chart - Specificity
LH_test_Specificity <- ggradar(LH_test_radar_data[3,],
                               grid.line.width = 0.4, # Grid line width
                               grid.label.size = 11, # Grid label font size
                               axis.label.size = 9, # Axis label font size
                               grid.min = 0, # Grid minimum value
                               grid.mid = 0.5, # Grid mid value
                               grid.max = 1, # Grid maximum value
                               values.radar = c(0, 0.5, 1), # Axis label display
                               background.circle.colour = "white", # Background color
                               group.line.width = 0.5, # Line width
                               group.point.size = 1, # Point size
                               fill = T, # Fill color
                               fill.alpha = 0.2, # Fill alpha transparency
                               group.colours = "#EE7A5F") +
  ggtitle("Specificity") + # Add title
  theme(plot.title = element_text(hjust = 0.5, size = 40), # Center title, set label text size
        plot.margin = margin(0, 0, 0, 0, "cm"), # Adjust margins
        plot.tag = element_text(size = 50, face = "bold") # Upper right corner label font size
  ) +
  coord_fixed(ratio = 1) # Set plot to square

# Plot radar chart - PPV
LH_test_PPV <- ggradar(LH_test_radar_data[4,],
                       grid.line.width = 0.4, # Grid line width
                       grid.label.size = 11, # Grid label font size
                       axis.label.size = 9, # Axis label font size
                       grid.min = 0, # Grid minimum value
                       grid.mid = 0.3, # Grid mid value
                       grid.max = 0.6, # Grid maximum value
                       values.radar = c(0, 0.3, 0.6), # Axis label display
                       background.circle.colour = "white", # Background color
                       group.line.width = 0.5, # Line width
                       group.point.size = 1, # Point size
                       fill = T, # Fill color
                       fill.alpha = 0.2, # Fill alpha transparency
                       group.colours = "#FEAF8A") +
  ggtitle("PPV") + # Add title
  theme(plot.title = element_text(hjust = 0.5, size = 40), # Center title, set label text size
        plot.margin = margin(0, 0, 0, 0, "cm"), # Adjust margins
        plot.tag = element_text(size = 50, face = "bold") # Upper right corner label font size
  ) +
  coord_fixed(ratio = 1) # Set plot to square

# Plot radar chart - NPV
LH_test_NPV <- ggradar(LH_test_radar_data[5,],
                       grid.line.width = 0.4, # Grid line width
                       grid.label.size = 11, # Grid label font size
                       axis.label.size = 9, # Axis label font size
                       grid.min = 0, # Grid minimum value
                       grid.mid = 0.5, # Grid mid value
                       grid.max = 1, # Grid maximum value
                       values.radar = c(0, 0.5, 1), # Axis label display
                       background.circle.colour = "white", # Background color
                       group.line.width = 0.5, # Line width
                       group.point.size = 1, # Point size
                       fill = T, # Fill color
                       fill.alpha = 0.3, # Fill alpha transparency
                       group.colours = "#F6B7C6") +
  ggtitle("NPV") + # Add title
  theme(plot.title = element_text(hjust = 0.5, size = 40), # Center title, set label text size
        plot.margin = margin(0, 0, 0, 0, "cm"), # Adjust margins
        plot.tag = element_text(size = 50, face = "bold") # Upper right corner label font size
  ) +
  coord_fixed(ratio = 1) # Set plot to square

# Plot radar chart - CCR
LH_test_CCR <- ggradar(LH_test_radar_data[6,],
                       grid.line.width = 0.4, # Grid line width
                       grid.label.size = 11, # Grid label font size
                       axis.label.size = 9, # Axis label font size
                       grid.min = 0, # Grid minimum value
                       grid.mid = 0.5, # Grid mid value
                       grid.max = 1, # Grid maximum value
                       values.radar = c(0, 0.5, 1), # Axis label display
                       background.circle.colour = "white", # Background color
                       group.line.width = 0.5, # Line width
                       group.point.size = 1, # Point size
                       fill = T, # Fill color
                       fill.alpha = 0.3, # Fill alpha transparency
                       group.colours = "#D8CBF0") +
  ggtitle("CCR") + # Add title
  theme(plot.title = element_text(hjust = 0.5, size = 40), # Center title, set label text size
        plot.margin = margin(0, 0, 0, 0, "cm"), # Adjust margins
        plot.tag = element_text(size = 50, face = "bold") # Upper right corner label font size
  ) +
  coord_fixed(ratio = 1) # Set plot to square

# Plot radar chart - F1 score
LH_test_F1_score <- ggradar(LH_test_radar_data[7,],
                            grid.line.width = 0.4, # Grid line width
                            grid.label.size = 11, # Grid label font size
                            axis.label.size = 9, # Axis label font size
                            grid.min = 0, # Grid minimum value
                            grid.mid = 0.25, # Grid mid value
                            grid.max = 0.5, # Grid maximum value
                            values.radar = c(0, 0.25, 0.5), # Axis label display
                            background.circle.colour = "white", # Background color
                            group.line.width = 0.5, # Line width
                            group.point.size = 1, # Point size
                            fill = T, # Fill color
                            fill.alpha = 0.3, # Fill alpha transparency
                            group.colours = "#9FCDC9") +
  ggtitle("F1 score") + # Add title
  theme(plot.title = element_text(hjust = 0.5, size = 40), # Center title, set label text size
        plot.margin = margin(0, 0, 0, 0, "cm"), # Adjust margins
        plot.tag = element_text(size = 50, face = "bold") # Upper right corner label font size
  ) +
  coord_fixed(ratio = 1) # Set plot to square

# Plot radar chart - Brier score
LH_test_Brier_score <- ggradar(LH_test_radar_data[8,],
                               grid.line.width = 0.4, # Grid line width
                               grid.label.size = 11, # Grid label font size
                               axis.label.size = 9, # Axis label font size
                               grid.min = 0, # Grid minimum value
                               grid.mid = 0.05, # Grid mid value
                               grid.max = 0.1, # Grid maximum value
                               values.radar = c(0, 0.05, 0.1), # Axis label display
                               background.circle.colour = "white", # Background color
                               group.line.width = 0.5, # Line width
                               group.point.size = 1, # Point size
                               fill = T, # Fill color
                               fill.alpha = 0.2, # Fill alpha transparency
                               group.colours = "#7EB87B") +
  ggtitle("Brier score") + # Add title
  theme(plot.title = element_text(hjust = 0.5, size = 40), # Center title, set label text size
        plot.margin = margin(0, 0, 0, 0, "cm"), # Adjust margins
        plot.tag = element_text(size = 50, face = "bold") # Upper right corner label font size
  ) +
  coord_fixed(ratio = 1) # Set plot to square

# Combine radar charts for test set
LH_test_radar <- (LH_test_AUC + LH_test_Sensitivity + LH_test_Specificity + LH_test_PPV +
                    LH_test_NPV + LH_test_CCR + LH_test_F1_score + LH_test_Brier_score) +
  plot_annotation(tag_levels = "A") + # Automatically add A and B labels
  plot_layout(ncol = 4, nrow = 2) # 2*4 layout

# Export radar charts for test set
ggsave(plot = LH_test_radar,
       filename = "LH_test_radar.png",
       width = 17,
       height = 8.5,
       units = "cm",
       dpi = 600)

##------------------Model performance evaluation: Unstable and High-stable groups------------------
# Create lists of model predictions for training and test sets
UH_train_preds <- list(
  LR = UH_train_lr_pred,
  RF = UH_train_rf_pred,
  XGBoost = UH_train_xgb_pred,
  SVM = UH_train_svm_pred,
  KNN = UH_train_knn_pred,
  DT = UH_train_dt_pred,
  ANN = UH_train_ann_pred
)

UH_test_preds <- list(
  LR = UH_test_lr_pred,
  RF = UH_test_rf_pred,
  XGBoost = UH_test_xgb_pred,
  SVM = UH_test_svm_pred,
  KNN = UH_test_knn_pred,
  DT = UH_test_dt_pred,
  ANN = UH_test_ann_pred
)

# Create empty data frames for storing performance evaluation results for training and test sets
UH_train_metrics <- data.frame()
UH_test_metrics <- data.frame()

# Create empty data frames for storing ROC data for training and test sets
UH_train_roc_data <- data.frame()
UH_test_roc_data <- data.frame()

# Create lists for storing ROC() results for training and test sets for DeLong test
UH_train_roc_auc <- list()
UH_test_roc_auc <- list()

# Create lists for storing model prediction probability data for training and test sets (calibration curve)
UH_train_prob_data <- list()
UH_test_prob_data <- list()

# Create empty data frames for storing DCA data for training and test sets
UH_train_dca_data <- data.frame()
UH_test_dca_data <- data.frame()

# Calculate performance metrics for training set
for (model_name in names(UH_train_preds)) {
  # Get prediction results for training set model
  pred_train <- UH_train_preds[[model_name]]
  
  # Get predicted probabilities from training set model
  pred_prob_train <- pred_train$prob[, "Yes"]
  
  # Store predicted probabilities for training set model in list
  UH_train_prob_data[[model_name]] <- pred_prob_train
  
  # Get actual class labels for training set
  true_class_train <- pred_train$truth
  true_class_train <- ifelse(true_class_train == "Yes", 1, 0)
  roc_train <- roc(true_class_train, pred_prob_train)
  auc_train <- roc_train$auc
  ci_train <- ci(roc_train)
  
  # Combine AUC and 95%CI, keep three decimal places
  auc_with_ci_train <- sprintf("%.3f\n(%.3f-%.3f)", auc_train, ci_train[1], ci_train[3])
  
  # Store roc() result in list
  UH_train_roc_auc[[model_name]] <- roc_train
  
  # Extract data for ROC curve and add to data frame
  roc_data_train <- data.frame(
    FPR = 1-roc_train$specificities, # False Positive Rate: 1-specificities
    TPR = roc_train$sensitivities, # True Positive Rate: sensitivities
    model = model_name # Model name
  )
  
  # Order model names
  roc_data_train$model <- factor(roc_data_train$model, levels = c("LR", "RF", "XGBoost", "SVM", "KNN", "DT", "ANN"))
  
  # Combine ROC data for all models for test set
  UH_train_roc_data <- rbind(UH_train_roc_data, roc_data_train)
  
  ## Calculate best threshold for training set
  # Define threshold range
  threshold <- seq(0, 1, by = 0.001)
  
  # Calculate performance metrics for different thresholds
  metrics_list_train <- sapply(threshold, function(t) {
    calculate_metrics(pred_prob_train, true_class_train, t)
  }, simplify = F)
  distances <- sapply(metrics_list_train, function(metrics) {
    sqrt((1-metrics$Sensitivity)^2 + (1-metrics$Specificity)^2)
  })
  
  # Find best threshold for training set
  best_threshold_train <- threshold[which.min(distances)]
  
  # Calculate performance metrics for training set using the best threshold
  best_metrics_train <- calculate_metrics(pred_prob_train, true_class_train, best_threshold_train)
  
  # Calculate Brier score
  brier_score_train <- mean((pred_prob_train - true_class_train)^2)
  
  # Bootstrap CI
  set.seed(123)
  ci_train_metrics <- bootstrap_ci(pred_prob_train, true_class_train, best_threshold_train, B = 1000)
  Sensitivity_CI <- format_ci(best_metrics_train$Sensitivity, ci_train_metrics[, "Sensitivity"])
  Specificity_CI <- format_ci(best_metrics_train$Specificity, ci_train_metrics[, "Specificity"])
  PPV_CI <- format_ci(best_metrics_train$PPV, ci_train_metrics[, "PPV"])
  NPV_CI <- format_ci(best_metrics_train$NPV, ci_train_metrics[, "NPV"])
  CCR_CI <- format_ci(best_metrics_train$CCR, ci_train_metrics[, "CCR"])
  F1_score_CI <- format_ci(best_metrics_train$F1_score, ci_train_metrics[, "F1_score"])
  Brier_score_CI <- format_ci(brier_score_train, ci_train_metrics[, "Brier_score"])
  
  # Summarize training set model results
  train_metrics_result <- data.frame(
    Model = model_name,
    Dataset = "Training cohort",
    AUC_CI = auc_with_ci_train,
    Threshold = round(best_threshold_train, 3),
    Sensitivity_CI = Sensitivity_CI,
    Specificity_CI = Specificity_CI,
    PPV_CI = PPV_CI,
    NPV_CI = NPV_CI,
    CCR_CI = CCR_CI,
    F1_score_CI = F1_score_CI,
    Brier_score_CI = Brier_score_CI
  )
  
  # Add results for each model for training set to data frame
  UH_train_metrics <- rbind(UH_train_metrics, train_metrics_result)
  
  ## Extract DCA data for training set
  # Construct data frame: truth and predicted probability
  train_truth_p <- data.frame(
    truth = true_class_train, # Convert truth to 0/1
    p = pred_prob_train # Extract predicted probability
  )
  
  # Calculate decision curve
  train_dca_result <- decision_curve(truth ~ p, data = train_truth_p, family = binomial, thresholds = seq(0, 1, by = 0.01))
  
  # Extract data and add to data frame
  train_thresholds <- train_dca_result$derived.data$thresholds
  train_net_benefit <- train_dca_result$derived.data$sNB
  train_model <- train_dca_result$derived.data$model
  train_model <- ifelse(train_model == "truth ~ p", model_name, train_model)
  
  # Combine decision data
  UH_train_dca_data <- rbind(UH_train_dca_data, data.frame(
    threshold = train_thresholds,
    net_benefit = train_net_benefit,
    model = train_model
  ))
  
  # Order model names
  UH_train_dca_data$model <- factor(UH_train_dca_data$model, levels = c("LR", "RF", "XGBoost", "SVM", "KNN", "DT", "ANN", "All", "None"))
}

# Calculate performance metrics for test set
for (model_name in names(UH_test_preds)) {
  # Get prediction results for test set model
  pred_test <- UH_test_preds[[model_name]]
  
  # Get predicted probabilities from test set model
  pred_prob_test <- pred_test$prob[, "Yes"]
  
  # Store predicted probabilities for test set model in list
  UH_test_prob_data[[model_name]] <- pred_prob_test
  
  # Get actual class labels for test set
  true_class_test <- pred_test$truth
  true_class_test <- ifelse(true_class_test == "Yes", 1, 0)
  
  # Calculate test set AUC and 95%CI
  roc_test <- roc(true_class_test, pred_prob_test)
  auc_test <- roc_test$auc
  ci_test <- ci(roc_test)
  
  # Combine AUC and 95%CI, keep three decimal places
  auc_with_ci_test <- sprintf("%.3f\n(%.3f-%.3f)", auc_test, ci_test[1], ci_test[3])
  
  # Store roc() result in list
  UH_test_roc_auc[[model_name]] <- roc_test
  
  # Extract data for ROC curve and add to data frame
  roc_data_test <- data.frame(
    FPR = 1-roc_test$specificities, # False Positive Rate: 1-specificities
    TPR = roc_test$sensitivities, # True Positive Rate: sensitivities
    model = model_name # Model name
  )
  
  # Order model names
  roc_data_test$model <- factor(roc_data_test$model, levels = c("LR", "RF", "XGBoost", "SVM", "KNN", "DT", "ANN"))
  
  # Combine ROC data for all models for test set
  UH_test_roc_data <- rbind(UH_test_roc_data, roc_data_test)
  
  # Get best threshold from training set
  best_threshold_train <- UH_train_metrics[UH_train_metrics$Model == model_name, "Threshold"]
  
  # Calculate test set performance using training set's best threshold
  best_metrics_test <- calculate_metrics(pred_prob_test, true_class_test, best_threshold_train)
  
  # Calculate test set Brier score
  brier_score_test <- mean((pred_prob_test - true_class_test)^2)
  
  # Bootstrap CI
  set.seed(123)
  ci_test_metrics <- bootstrap_ci(pred_prob_test, true_class_test, best_threshold_train, B = 1000)
  Sensitivity_CI <- format_ci(best_metrics_test$Sensitivity, ci_test_metrics[, "Sensitivity"])
  Specificity_CI <- format_ci(best_metrics_test$Specificity, ci_test_metrics[, "Specificity"])
  PPV_CI <- format_ci(best_metrics_test$PPV, ci_test_metrics[, "PPV"])
  NPV_CI <- format_ci(best_metrics_test$NPV, ci_test_metrics[, "NPV"])
  CCR_CI <- format_ci(best_metrics_test$CCR, ci_test_metrics[, "CCR"])
  F1_score_CI <- format_ci(best_metrics_test$F1_score, ci_test_metrics[, "F1_score"])
  Brier_score_CI <- format_ci(brier_score_test, ci_test_metrics[, "Brier_score"])
  
  # Summarize test set model results into data frame
  test_metrics_result <- data.frame(
    Model = model_name,
    Dataset = "Testing cohort",
    AUC_CI = auc_with_ci_test,
    Threshold = round(best_threshold_train, 3), # Use training set's best threshold
    Sensitivity_CI = Sensitivity_CI,
    Specificity_CI = Specificity_CI,
    PPV_CI = PPV_CI,
    NPV_CI = NPV_CI,
    CCR_CI = CCR_CI,
    F1_score_CI = F1_score_CI,
    Brier_score_CI = Brier_score_CI
  )
  
  # Combine results for each model for test set
  UH_test_metrics <- rbind(UH_test_metrics, test_metrics_result)
  
  ## Extract DCA data for test set
  # Construct data frame: truth and predicted probability
  test_truth_p <- data.frame(
    truth = true_class_test, # Convert truth to 0/1
    p = pred_prob_test # Extract predicted probability
  )
  
  # Calculate decision curve
  test_dca_result <- decision_curve(truth ~ p, data = test_truth_p, family = binomial, thresholds = seq(0, 1, by = 0.01))
  
  # Extract data and add to data frame
  test_thresholds <- test_dca_result$derived.data$thresholds
  test_net_benefit <- test_dca_result$derived.data$sNB
  test_model <- test_dca_result$derived.data$model
  test_model <- ifelse(test_model == "truth ~ p", model_name, test_model)
  
  # Combine decision data
  UH_test_dca_data <- rbind(UH_test_dca_data, data.frame(
    threshold = test_thresholds,
    net_benefit = test_net_benefit,
    model = test_model
  ))
  
  # Order model names
  UH_test_dca_data$model <- factor(UH_test_dca_data$model, levels = c("LR", "RF", "XGBoost", "SVM", "KNN", "DT", "ANN", "All", "None"))
}

##------------------DeLong test------------------
# Initialize an empty data frame for storing test set DeLong test results
UH_test_auc_comparisons <- data.frame()

# Get combinations of model names for the training set
model_names <- names(UH_test_roc_auc)

# Pairwise ROC curve comparisons, extract Z-scores and p-values for test set
for (i in 1:(length(model_names) - 1)) {
  for (j in (i + 1):length(model_names)) {
    # Get ROC curve data for the two models
    test_roc1 <- UH_test_roc_auc[[model_names[i]]]
    test_roc2 <- UH_test_roc_auc[[model_names[j]]]
    
    # DeLong test
    test_delong_test <- roc.test(test_roc1, test_roc2, method = "delong")
    
    # Extract Z-score and p-value, rounding Z-score to 3 decimals and formatting p-value
    test_Z_score <- round(test_delong_test$statistic, 3)
    test_P_value <- pvalue_format(test_delong_test$p.value)
    
    # Add result to data frame and combine
    UH_test_auc_comparisons <- rbind(UH_test_auc_comparisons, data.frame(
      Model_comparison = paste(model_names[i], model_names[j], sep = " vs. "),
      UH_test_Z_score = test_Z_score,
      UH_test_P_value = test_P_value
    ))
  }
}

#------------------Plot ROC curves for all models on the test set------------------
UH_test_roc <- ggplot(UH_test_roc_data, aes(x = FPR, y = TPR, color = model)) +
  geom_line(size = 0.1) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.2)) + # Set x-axis ticks
  scale_y_continuous(breaks = seq(0, 1, by = 0.2)) + # Set y-axis ticks
  
  # Custom colors and legend labels
  scale_color_manual(
    values = c("#4DAF9C", "#807DBA", "#E34A33", "#3182BD", "#A63603", "#31A354", "#8C6D31"),
    labels = c(
      expression("LR (AUC=0.797)"),
      expression("RF (AUC=0.804, " * italic(P) * "=0.422)"),
      expression("XGBoost (AUC=0.805, " * italic(P) * "=0.199)"),
      expression("SVM (AUC=0.732, " * italic(P) * "<0.001)"),
      expression("KNN (AUC=0.781, " * italic(P) * "=0.105)"),
      expression("DT (AUC=0.652, " * italic(P) * "<0.001)"),
      expression("ANN (AUC=0.794, " * italic(P) * "=0.091)"))
  ) +
  labs(x = "1-Specificity",
       y = "Sensitivity",
       title = "ROC Curve", # Plot title
       color = "") + # Legend title blank
  theme_minimal() + # Set chart theme
  theme(
    plot.background = element_rect(fill = "white", color = NA), # Overall image background, border color none
    panel.grid = element_blank(), # Remove grid lines
    panel.border = element_rect(color = "black", fill = NA), # Add border
    axis.ticks.x = element_line(color = "black"), # x-axis tick marks
    axis.ticks.y = element_line(color = "black"), # y-axis tick marks
    axis.ticks.length = unit(0.1, "cm"), # Tick mark length and placement outside
    legend.position = c(0.75, 0.22), # Legend placed at lower right
    legend.key.height = unit(0.2, "cm"), # Control height of each legend item
    legend.key.width = unit(0.4, "cm"), # Control width of each legend item
    axis.title = element_text(size = 60), # Axis title font size
    axis.text = element_text(size = 50), # Axis tick label font size
    legend.text = element_text(size = 25), # Legend text font size
    plot.title = element_text(size = 60, hjust = 0.5), # Plot title font, center alignment
    plot.tag = element_text(size = 90, face = "bold") # Set tag text size
  ) +
  coord_fixed(ratio = 5/7) + # Set plot aspect ratio
  annotate(geom = "segment", x = 0, y = 0, xend = 1, yend = 1, color = "black", size = 0.2, linetype = "dotted") # Add diagonal line

##------------------Plot calibration curves for test set------------------
# Extract test task data
UH_test_task_data <- UH_test_task$data()
UH_test_task_data$CMDs <- factor(ifelse(UH_test_task_data$CMDs == "Yes", 1, 0))

# Score the models
UH_test_score <- Score(list(LR = UH_test_prob_data[["LR"]],
                            RF = UH_test_prob_data[["RF"]],
                            XGBoost = UH_test_prob_data[["XGBoost"]],
                            SVM = UH_test_prob_data[["SVM"]],
                            KNN = UH_test_prob_data[["KNN"]],
                            DT = UH_test_prob_data[["DT"]],
                            ANN = UH_test_prob_data[["ANN"]]),
                       formula = CMDs ~ 1, # Model evaluation formula
                       null.model = F, # Do not use null model for comparison
                       plots = "calibration", # Plot calibration curve
                       data = UH_test_task_data)

# Extract calibration curve data
UH_test_calibration_plot <- plotCalibration(UH_test_score, method = "nne", bandwidth = 0.05, plot = FALSE)
UH_test_calibration_data <- imap_dfr(UH_test_calibration_plot$plotFrames, ~ {
  .x %>%
    as_tibble() %>%
    mutate(model = .y) # .y is the list element name (lr/rf/svm etc.)
})
UH_test_calibration_data$model <- factor(UH_test_calibration_data$model, levels = c("LR", "RF", "XGBoost", "SVM", "KNN", "DT", "ANN"))

# Plot calibration curve
UH_test_calibration <- ggplot(UH_test_calibration_data, aes(x = Pred, y = Obs, color = model)) +
  geom_line(linewidth = 0.1) + # Set line thickness
  
  # Use geom_segment() to draw the ideal calibration line
  geom_segment(data = segment_data, aes(x = x, y = y, xend = xend, yend = yend, color = segment_type), linewidth = 0.2, linetype = "dotted") +
  scale_x_continuous(limits = c(0, 1), # Limit x-axis range
                     breaks = seq(0, 1, by = 0.2), name = "Predicted Probability") + # Set x-axis ticks and labels
  scale_y_continuous(limits = c(0, 1), # Limit y-axis range
                     breaks = seq(0, 1, by = 0.2), name = "Actual Probability") + # Set y-axis ticks and labels
  
  # Custom colors and legend labels
  scale_color_manual(
    values = c("#4DAF9C", "#807DBA", "#E34A33", "#3182BD", "#A63603", "#31A354", "#8C6D31", "black"),
    labels = c("LR", "RF", "XGBoost", "SVM", "KNN", "DT", "ANN", "Ideal")
  ) +
  labs(title = "Calibration Curve", # Plot title
       color = "") + # Legend title blank
  theme_minimal() + # Set chart theme
  theme(
    plot.background = element_rect(fill = "white", color = NA), # Overall image background, border color none
    panel.grid = element_blank(), # Remove grid lines
    panel.border = element_rect(color = "black", fill = NA), # Add border
    axis.ticks.x = element_line(color = "black"), # x-axis tick marks
    axis.ticks.y = element_line(color = "black"), # y-axis tick marks
    axis.ticks.length = unit(0.1, "cm"), # Tick mark length and placement outside
    legend.position = c(0.15, 0.82), # Legend placed at upper left
    legend.key.height = unit(0.2, "cm"), # Control height of each legend item
    legend.key.width = unit(0.4, "cm"), # Control width of each legend item
    axis.title = element_text(size = 60), # Axis title font size
    axis.text = element_text(size = 50), # Axis tick label font size
    legend.text = element_text(size = 30), # Legend text font size
    plot.title = element_text(size = 60, hjust = 0.5), # Plot title font, center alignment
    plot.tag = element_text(size = 90, face = "bold") # Set tag text size
  ) +
  coord_fixed(ratio = 5/7) # Set plot aspect ratio

#------------------Plot DCA for test set------------------
UH_test_dca <- ggplot(UH_test_dca_data, aes(x = threshold, y = net_benefit, color = model)) +
  geom_line(linewidth = 0.1) + # Set line thickness
  scale_x_continuous(limits = c(0, 0.8), # Limit x-axis range
                     breaks = seq(0, 0.8, by = 0.2), # Set x-axis ticks
                     name = "High Risk Threshold") + # Set x-axis label
  scale_y_continuous(limits = c(-0.2, 0.8), # Limit y-axis range
                     breaks = seq(-0.2, 0.8, by = 0.2), # Set y-axis ticks
                     name = "Standardized Net Benefit") + # Set y-axis label
  scale_color_manual(values = c("#4DAF9C", "#807DBA", "#E34A33", "#3182BD",
                                "#A63603", "#31A354", "#8C6D31", "gray", "black")) + # Custom colors
  labs(title = "DCA", # Plot title
       color = "") + # Legend title blank
  theme_minimal() + # Set chart theme
  theme(
    plot.background = element_rect(fill = "white", color = NA), # Overall image background, border color none
    panel.grid = element_blank(), # Remove grid lines
    panel.border = element_rect(color = "black", fill = NA), # Add border
    axis.ticks.x = element_line(color = "black"), # x-axis tick marks
    axis.ticks.y = element_line(color = "black"), # y-axis tick marks
    axis.ticks.length = unit(0.1, "cm"), # Tick mark length and placement outside
    legend.position = c(0.86, 0.79), # Legend placed at upper right
    legend.key.height = unit(0.2, "cm"), # Control height of each legend item
    legend.key.width = unit(0.4, "cm"), # Control width of each legend item
    axis.title = element_text(size = 60), # Axis title font size
    axis.text = element_text(size = 50), # Axis tick label font size
    legend.text = element_text(size = 30), # Legend text font size
    plot.title = element_text(size = 60, hjust = 0.5), # Plot title font, center alignment
    plot.tag = element_text(size = 90, face = "bold") # Set tag text size
  ) +
  coord_fixed(ratio = 4/7) # Set plot aspect ratio

##------------------Plot radar charts for test set------------------
# Convert data to long format suitable for radar charts
UH_test_radar_data <- UH_test_metrics %>%
  select(3, 5:11) %>%
  mutate(across(ends_with("_CI"),
                ~ as.numeric(sub("\\s*\\(.*$", "", .x))))
UH_test_radar_data <- t(UH_test_radar_data) # Transpose data frame
UH_test_radar_data <- as.data.frame(UH_test_radar_data) # Convert back to data frame
colnames(UH_test_radar_data) <- c("LR", "RF", "XGBoost", "SVM", "KNN", "DT", "ANN") # Add column names
UH_test_radar_data$metrics <- c("AUC", "Sensitivity", "Specificity", "PPV", "NPV", "CCR", "F1 score", "Brier score") # Add performance column
UH_test_radar_data <- UH_test_radar_data[, c("metrics", "LR", "RF", "XGBoost", "SVM", "KNN", "DT", "ANN")] # Reorder columns
UH_test_radar_data$metrics <- factor(UH_test_radar_data$metrics, levels=c("AUC", "Sensitivity", "Specificity", "PPV", "NPV", "CCR", "F1 score", "Brier score")) # Convert to factor

# Plot radar chart - AUC
UH_test_AUC <- ggradar(UH_test_radar_data[1,],
                       grid.line.width = 0.4, # Grid line width
                       grid.label.size = 11, # Grid label font size
                       axis.label.size = 9, # Axis label font size
                       grid.min = 0, # Grid minimum value
                       grid.mid = 0.45, # Grid mid value
                       grid.max = 0.9, # Grid maximum value
                       values.radar = c(0, 0.45, 0.9), # Axis label display
                       background.circle.colour = "white", # Background color
                       group.line.width = 0.5, # Line width
                       group.point.size = 1, # Point size
                       fill = T, # Fill color
                       fill.alpha = 0.3, # Fill alpha transparency
                       group.colours = "#95B0E0") +
  ggtitle("AUC") + # Add title
  theme(plot.title = element_text(hjust = 0.5, size = 40), # Center title, set label text size
        plot.margin = margin(0, 0, 0, 0, "cm"), # Adjust margins
        plot.tag = element_text(size = 50, face = "bold") # Upper right corner label font size
  ) +
  coord_fixed(ratio = 1) # Set plot to square

# Plot radar chart - Sensitivity
UH_test_Sensitivity <- ggradar(UH_test_radar_data[2,],
                               grid.line.width = 0.4, # Grid line width
                               grid.label.size = 11, # Grid label font size
                               axis.label.size = 9, # Axis label font size
                               grid.min = 0, # Grid minimum value
                               grid.mid = 0.4, # Grid mid value
                               grid.max = 0.8, # Grid maximum value
                               values.radar = c(0, 0.4, 0.8), # Axis label display
                               background.circle.colour = "white", # Background color
                               group.line.width = 0.5, # Line width
                               group.point.size = 1, # Point size
                               fill = T, # Fill color
                               fill.alpha = 0.3, # Fill alpha transparency
                               group.colours = "#56AEDE") +
  ggtitle("Sensitivity") + # Add title
  theme(plot.title = element_text(hjust = 0.5, size = 40), # Center title, set label text size
        plot.margin = margin(0, 0, 0, 0, "cm"), # Adjust margins
        plot.tag = element_text(size = 50, face = "bold") # Upper right corner label font size
  ) +
  coord_fixed(ratio = 1) # Set plot to square

# Plot radar chart - Specificity
UH_test_Specificity <- ggradar(UH_test_radar_data[3,],
                               grid.line.width = 0.4, # Grid line width
                               grid.label.size = 11, # Grid label font size
                               axis.label.size = 9, # Axis label font size
                               grid.min = 0, # Grid minimum value
                               grid.mid = 0.5, # Grid mid value
                               grid.max = 1, # Grid maximum value
                               values.radar = c(0, 0.5, 1), # Axis label display
                               background.circle.colour = "white", # Background color
                               group.line.width = 0.5, # Line width
                               group.point.size = 1, # Point size
                               fill = T, # Fill color
                               fill.alpha = 0.2, # Fill alpha transparency
                               group.colours = "#EE7A5F") +
  ggtitle("Specificity") + # Add title
  theme(plot.title = element_text(hjust = 0.5, size = 40), # Center title, set label text size
        plot.margin = margin(0, 0, 0, 0, "cm"), # Adjust margins
        plot.tag = element_text(size = 50, face = "bold") # Upper right corner label font size
  ) +
  coord_fixed(ratio = 1) # Set plot to square

# Plot radar chart - PPV
UH_test_PPV <- ggradar(UH_test_radar_data[4,],
                       grid.line.width = 0.4, # Grid line width
                       grid.label.size = 11, # Grid label font size
                       axis.label.size = 9, # Axis label font size
                       grid.min = 0, # Grid minimum value
                       grid.mid = 0.3, # Grid mid value
                       grid.max = 0.6, # Grid maximum value
                       values.radar = c(0, 0.3, 0.6), # Axis label display
                       background.circle.colour = "white", # Background color
                       group.line.width = 0.5, # Line width
                       group.point.size = 1, # Point size
                       fill = T, # Fill color
                       fill.alpha = 0.2, # Fill alpha transparency
                       group.colours = "#FEAF8A") +
  ggtitle("PPV") + # Add title
  theme(plot.title = element_text(hjust = 0.5, size = 40), # Center title, set label text size
        plot.margin = margin(0, 0, 0, 0, "cm"), # Adjust margins
        plot.tag = element_text(size = 50, face = "bold") # Upper right corner label font size
  ) +
  coord_fixed(ratio = 1) # Set plot to square

# Plot radar chart - NPV
UH_test_NPV <- ggradar(UH_test_radar_data[5,],
                       grid.line.width = 0.4, # Grid line width
                       grid.label.size = 11, # Grid label font size
                       axis.label.size = 9, # Axis label font size
                       grid.min = 0, # Grid minimum value
                       grid.mid = 0.5, # Grid mid value
                       grid.max = 1, # Grid maximum value
                       values.radar = c(0, 0.5, 1), # Axis label display
                       background.circle.colour = "white", # Background color
                       group.line.width = 0.5, # Line width
                       group.point.size = 1, # Point size
                       fill = T, # Fill color
                       fill.alpha = 0.3, # Fill alpha transparency
                       group.colours = "#F6B7C6") +
  ggtitle("NPV") + # Add title
  theme(plot.title = element_text(hjust = 0.5, size = 40), # Center title, set label text size
        plot.margin = margin(0, 0, 0, 0, "cm"), # Adjust margins
        plot.tag = element_text(size = 50, face = "bold") # Upper right corner label font size
  ) +
  coord_fixed(ratio = 1) # Set plot to square

# Plot radar chart - CCR
UH_test_CCR <- ggradar(UH_test_radar_data[6,],
                       grid.line.width = 0.4, # Grid line width
                       grid.label.size = 11, # Grid label font size
                       axis.label.size = 9, # Axis label font size
                       grid.min = 0, # Grid minimum value
                       grid.mid = 0.5, # Grid mid value
                       grid.max = 1, # Grid maximum value
                       values.radar = c(0, 0.5, 1), # Axis label display
                       background.circle.colour = "white", # Background color
                       group.line.width = 0.5, # Line width
                       group.point.size = 1, # Point size
                       fill = T, # Fill color
                       fill.alpha = 0.3, # Fill alpha transparency
                       group.colours = "#D8CBF0") +
  ggtitle("CCR") + # Add title
  theme(plot.title = element_text(hjust = 0.5, size = 40), # Center title, set label text size
        plot.margin = margin(0, 0, 0, 0, "cm"), # Adjust margins
        plot.tag = element_text(size = 50, face = "bold") # Upper right corner label font size
  ) +
  coord_fixed(ratio = 1) # Set plot to square

# Plot radar chart - F1 score
UH_test_F1_score <- ggradar(UH_test_radar_data[7,],
                            grid.line.width = 0.4, # Grid line width
                            grid.label.size = 11, # Grid label font size
                            axis.label.size = 9, # Axis label font size
                            grid.min = 0, # Grid minimum value
                            grid.mid = 0.25, # Grid mid value
                            grid.max = 0.5, # Grid maximum value
                            values.radar = c(0, 0.25, 0.5), # Axis label display
                            background.circle.colour = "white", # Background color
                            group.line.width = 0.5, # Line width
                            group.point.size = 1, # Point size
                            fill = T, # Fill color
                            fill.alpha = 0.3, # Fill alpha transparency
                            group.colours = "#9FCDC9") +
  ggtitle("F1 score") + # Add title
  theme(plot.title = element_text(hjust = 0.5, size = 40), # Center title, set label text size
        plot.margin = margin(0, 0, 0, 0, "cm"), # Adjust margins
        plot.tag = element_text(size = 50, face = "bold") # Upper right corner label font size
  ) +
  coord_fixed(ratio = 1) # Set plot to square

# Plot radar chart - Brier score
UH_test_Brier_score <- ggradar(UH_test_radar_data[8,],
                               grid.line.width = 0.4, # Grid line width
                               grid.label.size = 11, # Grid label font size
                               axis.label.size = 9, # Axis label font size
                               grid.min = 0, # Grid minimum value
                               grid.mid = 0.05, # Grid mid value
                               grid.max = 0.1, # Grid maximum value
                               values.radar = c(0, 0.05, 0.1), # Axis label display
                               background.circle.colour = "white", # Background color
                               group.line.width = 0.5, # Line width
                               group.point.size = 1, # Point size
                               fill = T, # Fill color
                               fill.alpha = 0.2, # Fill alpha transparency
                               group.colours = "#7EB87B") +
  ggtitle("Brier score") + # Add title
  theme(plot.title = element_text(hjust = 0.5, size = 40), # Center title, set label text size
        plot.margin = margin(0, 0, 0, 0, "cm"), # Adjust margins
        plot.tag = element_text(size = 50, face = "bold") # Upper right corner label font size
  ) +
  coord_fixed(ratio = 1) # Set plot to square

# Combine radar charts for test set
UH_test_radar <- (UH_test_AUC + UH_test_Sensitivity + UH_test_Specificity + UH_test_PPV +
                    UH_test_NPV + UH_test_CCR + UH_test_F1_score + UH_test_Brier_score) +
  plot_annotation(tag_levels = "A") + # Automatically add A and B labels
  plot_layout(ncol = 4, nrow = 2) # 2*4 layout

# Export radar charts for test set
ggsave(plot = UH_test_radar,
       filename = "UH_test_radar.png",
       width = 17,
       height = 8.5,
       units = "cm",
       dpi = 600)

# Combine model performance results for both groups
test_metrics_37 <- rbind(LH_test_metrics, UH_test_metrics)

# Combine DeLong test results for both groups
test_roc_comparisons_37 <- merge(LH_test_auc_comparisons, UH_test_auc_comparisons,
                                 by = "Model_comparison")

# Combine ROC plots, calibration curve plots, DCA plots for both groups
ROC_calibration_dca_37 <- (LH_test_roc + UH_test_roc) /
  (LH_test_calibration + UH_test_calibration) /
  (LH_test_dca + UH_test_dca) +
  plot_annotation(tag_levels = "A") # Automatically add A and B labels

# Export combined ROC, calibration, DCA plots for both groups
ggsave(plot = ROC_calibration_dca_37,
       filename = "ROC_calibration_dca_37.png",
       width = 16,
       height = 20,
       units = "cm",
       dpi = 600)

## Combine radar charts for both groups
# Add tags to the plots
LH_test_AUC_tag <- LH_test_AUC +
  labs(tag = "A") +
  theme(plot.tag.position = c(0.02, 1),
        plot.tag = element_text(size = 60, face = "bold"))

UH_test_AUC_tag <- UH_test_AUC +
  labs(tag = "B") +
  theme(plot.tag.position = c(0.02, 1),
        plot.tag = element_text(size = 60, face = "bold"))

LH_UH_test_radar <- (
  LH_test_AUC_tag + LH_test_Sensitivity + LH_test_Specificity + LH_test_PPV +
    LH_test_NPV + LH_test_CCR + LH_test_F1_score + LH_test_Brier_score +
    UH_test_AUC_tag + UH_test_Sensitivity + UH_test_Specificity + UH_test_PPV +
    UH_test_NPV + UH_test_CCR + UH_test_F1_score + UH_test_Brier_score
) +
  plot_layout(ncol = 4, nrow = 4)

# Export combined radar charts for test sets
ggsave(plot = LH_UH_test_radar,
       filename = "LH_UH_test_radar.png",
       width = 17,
       height = 15,
       units = "cm",
       dpi = 600)

###------------------------Feature selection: RFE+SHAP--------------------------
##------------------Feature selection: Low decline and High-stable groups------------------
# Initial features (obtained from original data column names to avoid task name confusion)
current_feats <- setdiff(colnames(data_LH_train_std_num), "CMDs")

# Create output list
LH_round_outputs <- list()

# Iteration counter
iter <- 1

# Create function: given feature set → build task → tune → train → predict → calculate SHAP

LH_task_pred_shap <- function(train_df, test_df, feats) {
  # 1) Create task using "dataset after removal"
  train_task <- as_task_classif(train_df[, c("CMDs", feats), drop = FALSE], target = "CMDs")
  test_task <- as_task_classif(test_df[, c("CMDs", feats), drop = FALSE], target = "CMDs")
  
  # 2) Train model and predict
  ##XGBoost
  xgb_learner <- lrn("classif.xgboost", predict_type = "prob")
  xgb_learner$param_set$values <- list(
    nrounds = to_tune(500, 1000), # Number of trees: controls number of boosting iterations, determines learning extent.
    max_depth = to_tune(1, 10), # Maximum tree depth: controls tree complexity to prevent overfitting.
    eta = to_tune(0.001, 0.05), # Learning rate: controls learning speed, affects convergence and stability.
    min_child_weight = to_tune(1, 10), # Minimum sum of instance weights in a child node: restricts splitting to prevent overfitting.
    subsample = to_tune(0.1, 0.5) # Subsample ratio: provides randomness, reduces overfitting risk, increases diversity.
  )
  set.seed(123)
  xgb <- tune(tuner = tnr("grid_search", resolution = 5),
              task = train_task,
              learner = xgb_learner,
              resampling = rsmp("cv", folds = 5),
              measure = msr("classif.auc")
  )
  xgb_learner$param_set$values <- xgb$result_learner_param_vals
  xgb_learner$train(train_task)
  train_xgb_pred <- xgb_learner$predict(train_task)
  test_xgb_pred <- xgb_learner$predict(test_task)
  
  # 3) Calculate SHAP
  # Extract predictor variables for training and test sets
  train_x <- train_task$data(cols = train_task$feature_names)
  test_x <- test_task$data(cols = test_task$feature_names)
  
  # Randomly sample 200 rows from the training set
  set.seed(123)
  train_x_200 <- train_x %>%
    slice_sample(n = 200)
  
  # Calculate SHAP values: svm_learner trained model; X = test_x test dataset to explain; bg_X = train_x_200: background dataset for SHAP baseline; predict_type = "prob": specify model prediction type as probability; verbose = F to turn off detailed output.
  shap_value <- kernelshap(xgb_learner, X = test_x, bg_X = train_x_200, predict_type = "prob", verbose = T)
  
  # Return object as list
  list(
    xgb_learner = xgb_learner,
    train_xgb_pred = train_xgb_pred,
    test_xgb_pred = test_xgb_pred,
    shap_value = shap_value
  )
}

# While loop: each round does "train + SHAP + drop features" until few features remain
while (length(current_feats) > 4) {
  # Round based on current features: first synchronize "dataset after removal"
  train_df_i <- data_LH_train_std_num[, c("CMDs", current_feats), drop = FALSE]
  test_df_i <- data_LH_test_std_num[, c("CMDs", current_feats), drop = FALSE]
  
  # Train/Predict/SHAP (all based on this round's dataset after removal)
  out <- LH_task_pred_shap(train_df_i, test_df_i, current_feats)
  
  # Save the 4 objects for this round (also store current features for traceability)
  LH_round_outputs[[iter]] <- list(
    iter = iter,
    features = current_feats,
    xgb_learner = out$xgb_learner,
    train_xgb_pred = out$train_xgb_pred,
    test_xgb_pred = out$test_xgb_pred,
    shap_value = out$shap_value
  )
  
  ### Use SHAP ranking to decide dropping the bottom 5 features
  ## Calculate mean SHAP value per feature
  # 1) mean(|SHAP|) per feature
  mean_abs_shap <- colMeans(abs(out$shap_value[["S"]][["Yes"]]), na.rm = TRUE)
  
  # 2) Sort (most important to least important)
  mean_abs_shap_sorted <- sort(mean_abs_shap, decreasing = TRUE)
  
  # Update feature set (these columns will be removed from both datasets in the next round)
  drop_n <- min(5, length(current_feats) - 5)
  
  # If drop_n <= 0, break out of the while loop
  if (drop_n <= 0) break
  
  # Extract feature names to be dropped in this round
  drop_feats <- tail(names(mean_abs_shap_sorted), drop_n)
  
  # Remove drop_feats from current features to get features for the next round
  current_feats <- setdiff(current_feats, drop_feats)
  
  # Increment iteration counter
  iter <- iter + 1
}

##------------------Feature selection: Unstable and High-stable groups------------------
# Initial features (obtained from original data column names to avoid task name confusion)
current_feats <- setdiff(colnames(data_LH_train_std_num), "CMDs")

# Create output list
UH_round_outputs <- list()

# Iteration counter
iter <- 1

# Create function: given feature set → build task → tune → train → predict → calculate SHAP

UH_task_pred_shap <- function(train_df, test_df, feats) {
  # 1) Create task using "dataset after removal"
  train_task <- as_task_classif(train_df[, c("CMDs", feats), drop = FALSE], target = "CMDs")
  test_task <- as_task_classif(test_df[, c("CMDs", feats), drop = FALSE], target = "CMDs")
  
  # 2) Train model and predict
  ##XGBoost
  xgb_learner <- lrn("classif.xgboost", predict_type = "prob")
  xgb_learner$param_set$values <- list(
    nrounds = to_tune(100, 500), # Number of trees: controls number of boosting iterations, determines learning extent.
    max_depth = to_tune(1, 10), # Maximum tree depth: controls tree complexity to prevent overfitting.
    eta = to_tune(0.001, 0.05), # Learning rate: controls learning speed, affects convergence and stability.
    min_child_weight = to_tune(1, 10), # Minimum sum of instance weights in a child node: restricts splitting to prevent overfitting.
    subsample = to_tune(0.5, 1) # Subsample ratio: provides randomness, reduces overfitting risk, increases diversity.
  )
  set.seed(123)
  xgb <- tune(tuner = tnr("grid_search", resolution = 5),
              task = train_task,
              learner = xgb_learner,
              resampling = rsmp("cv", folds = 5),
              measure = msr("classif.auc")
  )
  xgb_learner$param_set$values <- xgb$result_learner_param_vals
  xgb_learner$train(train_task)
  train_xgb_pred <- xgb_learner$predict(train_task)
  test_xgb_pred <- xgb_learner$predict(test_task)
  
  # 3) Calculate SHAP
  # Extract predictor variables for training and test sets
  train_x <- train_task$data(cols = train_task$feature_names)
  test_x <- test_task$data(cols = test_task$feature_names)
  
  # Randomly sample 200 rows from the training set
  set.seed(123)
  train_x_200 <- train_x %>%
    slice_sample(n = 200)
  
  # Calculate SHAP values: svm_learner trained model; X = test_x test dataset to explain; bg_X = train_x_200: background dataset for SHAP baseline; predict_type = "prob": specify model prediction type as probability; verbose = F to turn off detailed output.
  shap_value <- kernelshap(xgb_learner, X = test_x, bg_X = train_x_200, predict_type = "prob", verbose = T)
  
  # Return object as list
  list(
    xgb_learner = xgb_learner,
    train_xgb_pred = train_xgb_pred,
    test_xgb_pred = test_xgb_pred,
    shap_value = shap_value
  )
}

# While loop: each round does "train + SHAP + drop features" until few features remain
while (length(current_feats) > 4) {
  # Round based on current features: first synchronize "dataset after removal"
  train_df_i <- data_UH_train_std_num[, c("CMDs", current_feats), drop = FALSE]
  test_df_i <- data_UH_test_std_num[, c("CMDs", current_feats), drop = FALSE]
  
  # Train/Predict/SHAP (all based on this round's dataset after removal)
  out <- UH_task_pred_shap(train_df_i, test_df_i, current_feats)
  
  # Save the 4 objects for this round (also store current features for traceability)
  UH_round_outputs[[iter]] <- list(
    iter = iter,
    features = current_feats,
    xgb_learner = out$xgb_learner,
    train_xgb_pred = out$train_xgb_pred,
    test_xgb_pred = out$test_xgb_pred,
    shap_value = out$shap_value
  )
  
  ### Use SHAP ranking to decide dropping the bottom 5 features
  ## Calculate mean SHAP value per feature
  # 1) mean(|SHAP|) per feature
  mean_abs_shap <- colMeans(abs(out$shap_value[["S"]][["Yes"]]), na.rm = TRUE)
  
  # 2) Sort (most important to least important)
  mean_abs_shap_sorted <- sort(mean_abs_shap, decreasing = TRUE)
  
  # Update feature set (these columns will be removed from both datasets in the next round)
  drop_n <- min(5, length(current_feats) - 5)
  
  # If drop_n <= 0, break out of the while loop
  if (drop_n <= 0) break
  
  # Extract feature names to be dropped in this round
  drop_feats <- tail(names(mean_abs_shap_sorted), drop_n)
  
  # Remove drop_feats from current features to get features for the next round
  current_feats <- setdiff(current_feats, drop_feats)
  
  # Increment iteration counter
  iter <- iter + 1
}

###-------------------Performance evaluation with different numbers of risk factors-------------------
##------------------Performance evaluation with different numbers of risk factors: Low decline and High-stable groups------------------
# Create lists of model predictions for training and test sets
LH_train_preds_xgb <- list(
  xgb_35 = LH_round_outputs[[1]][["train_xgb_pred"]],
  xgb_30 = LH_round_outputs[[2]][["train_xgb_pred"]],
  xgb_25 = LH_round_outputs[[3]][["train_xgb_pred"]],
  xgb_20 = LH_round_outputs[[4]][["train_xgb_pred"]],
  xgb_15 = LH_round_outputs[[5]][["train_xgb_pred"]],
  xgb_10 = LH_round_outputs[[6]][["train_xgb_pred"]],
  xgb_5 = LH_round_outputs[[7]][["train_xgb_pred"]]
)

LH_test_preds_xgb <- list(
  xgb_35 = LH_round_outputs[[1]][["test_xgb_pred"]],
  xgb_30 = LH_round_outputs[[2]][["test_xgb_pred"]],
  xgb_25 = LH_round_outputs[[3]][["test_xgb_pred"]],
  xgb_20 = LH_round_outputs[[4]][["test_xgb_pred"]],
  xgb_15 = LH_round_outputs[[5]][["test_xgb_pred"]],
  xgb_10 = LH_round_outputs[[6]][["test_xgb_pred"]],
  xgb_5 = LH_round_outputs[[7]][["test_xgb_pred"]]
)

# Create empty data frames for storing performance evaluation results for training and test sets
LH_train_metrics_xgb <- data.frame()
LH_test_metrics_xgb <- data.frame()

# Create empty data frames for storing ROC data for training and test sets
LH_train_roc_data_xgb <- data.frame()
LH_test_roc_data_xgb <- data.frame()

# Create lists for storing ROC() results for training and test sets for DeLong test
LH_train_roc_auc_xgb <- list()
LH_test_roc_auc_xgb <- list()

# Create lists for storing model prediction probability data for training and test sets (calibration curve)
LH_train_prob_data_xgb <- list()
LH_test_prob_data_xgb <- list()

# Create empty data frames for storing DCA data for training and test sets
LH_train_dca_data_xgb <- data.frame()
LH_test_dca_data_xgb <- data.frame()

# Calculate performance metrics for training set
for (model_name in names(LH_train_preds_xgb)) {
  # Get prediction results for training set model
  pred_train <- LH_train_preds_xgb[[model_name]]
  
  # Get predicted probabilities from training set model
  pred_prob_train <- pred_train$prob[, "Yes"]
  
  # Store predicted probabilities for training set model in list
  LH_train_prob_data_xgb[[model_name]] <- pred_prob_train
  
  # Get actual class labels for training set
  true_class_train <- pred_train$truth
  true_class_train <- ifelse(true_class_train == "Yes", 1, 0)
  roc_train <- roc(true_class_train, pred_prob_train)
  auc_train <- roc_train$auc
  ci_train <- ci(roc_train)
  
  # Combine AUC and 95%CI, keep three decimal places
  auc_with_ci_train <- sprintf("%.3f\n(%.3f-%.3f)", auc_train, ci_train[1], ci_train[3])
  
  # Store roc() result in list
  LH_train_roc_auc_xgb[[model_name]] <- roc_train
  
  # Extract data for ROC curve and add to data frame
  roc_data_train <- data.frame(
    FPR = 1-roc_train$specificities, # False Positive Rate: 1-specificities
    TPR = roc_train$sensitivities, # True Positive Rate: sensitivities
    model = model_name # Model name
  )
  
  # Order model names
  roc_data_train$model <- factor(roc_data_train$model, levels = c("xgb_35", "xgb_30", "xgb_25", "xgb_20", "xgb_15", "xgb_10", "xgb_5"))
  
  # Combine ROC data for all models for test set
  LH_train_roc_data_xgb <- rbind(LH_train_roc_data, roc_data_train)
  
  ## Calculate best threshold for training set
  # Define threshold range
  threshold <- seq(0, 1, by = 0.001)
  
  # Calculate performance metrics for different thresholds
  metrics_list_train <- sapply(threshold, function(t) {
    calculate_metrics(pred_prob_train, true_class_train, t)
  }, simplify = F)
  distances <- sapply(metrics_list_train, function(metrics) {
    sqrt((1-metrics$Sensitivity)^2 + (1-metrics$Specificity)^2)
  })
  
  # Find best threshold for training set
  best_threshold_train <- threshold[which.min(distances)]
  
  # Calculate performance metrics for training set using the best threshold
  best_metrics_train <- calculate_metrics(pred_prob_train, true_class_train, best_threshold_train)
  
  # Calculate Brier score
  brier_score_train <- mean((pred_prob_train - true_class_train)^2)
  
  # Bootstrap CI
  set.seed(123)
  ci_train_metrics <- bootstrap_ci(pred_prob_train, true_class_train, best_threshold_train, B = 1000)
  Sensitivity_CI <- format_ci(best_metrics_train$Sensitivity, ci_train_metrics[, "Sensitivity"])
  Specificity_CI <- format_ci(best_metrics_train$Specificity, ci_train_metrics[, "Specificity"])
  PPV_CI <- format_ci(best_metrics_train$PPV, ci_train_metrics[, "PPV"])
  NPV_CI <- format_ci(best_metrics_train$NPV, ci_train_metrics[, "NPV"])
  CCR_CI <- format_ci(best_metrics_train$CCR, ci_train_metrics[, "CCR"])
  F1_score_CI <- format_ci(best_metrics_train$F1_score, ci_train_metrics[, "F1_score"])
  Brier_score_CI <- format_ci(brier_score_train, ci_train_metrics[, "Brier_score"])
  
  # Summarize training set model results
  train_metrics_result <- data.frame(
    Model = model_name,
    Dataset = "Training cohort",
    AUC_CI = auc_with_ci_train,
    Threshold = round(best_threshold_train, 3),
    Sensitivity_CI = Sensitivity_CI,
    Specificity_CI = Specificity_CI,
    PPV_CI = PPV_CI,
    NPV_CI = NPV_CI,
    CCR_CI = CCR_CI,
    F1_score_CI = F1_score_CI,
    Brier_score_CI = Brier_score_CI
  )
  
  # Add results for each model for training set to data frame
  LH_train_metrics_xgb <- rbind(LH_train_metrics_xgb, train_metrics_result)
  
  ## Extract DCA data for training set
  # Construct data frame: truth and predicted probability
  train_truth_p <- data.frame(
    truth = true_class_train, # Convert truth to 0/1
    p = pred_prob_train # Extract predicted probability
  )
  
  # Calculate decision curve
  train_dca_result <- decision_curve(truth ~ p, data = train_truth_p, family = binomial, thresholds = seq(0, 1, by = 0.01))
  
  # Extract data and add to data frame
  train_thresholds <- train_dca_result$derived.data$thresholds
  train_net_benefit <- train_dca_result$derived.data$sNB
  train_model <- train_dca_result$derived.data$model
  train_model <- ifelse(train_model == "truth ~ p", model_name, train_model)
  
  # Combine decision data
  LH_train_dca_data_xgb <- rbind(LH_train_dca_data_xgb, data.frame(
    threshold = train_thresholds,
    net_benefit = train_net_benefit,
    model = train_model
  ))
  
  # Order model names
  LH_train_dca_data_xgb$model <- factor(LH_train_dca_data_xgb$model, levels = c("xgb_35", "xgb_30", "xgb_25", "xgb_20", "xgb_15", "xgb_10", "xgb_5", "All", "None"))
}

# Calculate performance metrics for test set
for (model_name in names(LH_test_preds_xgb)) {
  # Get prediction results for test set model
  pred_test <- LH_test_preds_xgb[[model_name]]
  
  # Get predicted probabilities from test set model
  pred_prob_test <- pred_test$prob[, "Yes"]
  
  # Store predicted probabilities for test set model in list
  LH_test_prob_data_xgb[[model_name]] <- pred_prob_test
  
  # Get actual class labels for test set
  true_class_test <- pred_test$truth
  true_class_test <- ifelse(true_class_test == "Yes", 1, 0)
  
  # Calculate test set AUC and 95%CI
  roc_test <- roc(true_class_test, pred_prob_test)
  auc_test <- roc_test$auc
  ci_test <- ci(roc_test)
  
  # Combine AUC and 95%CI, keep three decimal places
  auc_with_ci_test <- sprintf("%.3f\n(%.3f-%.3f)", auc_test, ci_test[1], ci_test[3])
  
  # Store roc() result in list
  LH_test_roc_auc_xgb[[model_name]] <- roc_test
  
  # Extract data for ROC curve and add to data frame
  roc_data_test <- data.frame(
    FPR = 1-roc_test$specificities, # False Positive Rate: 1-specificities
    TPR = roc_test$sensitivities, # True Positive Rate: sensitivities
    model = model_name # Model name
  )
  
  # Order model names
  roc_data_test$model <- factor(roc_data_test$model, levels = c("xgb_35", "xgb_30", "xgb_25", "xgb_20", "xgb_15", "xgb_10", "xgb_5"))
  
  # Combine ROC data for all models for test set
  LH_test_roc_data_xgb <- rbind(LH_test_roc_data_xgb, roc_data_test)
  
  # Get best threshold from training set
  best_threshold_train <- LH_train_metrics_xgb[LH_train_metrics_xgb$Model == model_name, "Threshold"]
  
  # Calculate test set performance using training set's best threshold
  best_metrics_test <- calculate_metrics(pred_prob_test, true_class_test, best_threshold_train)
  
  # Calculate test set Brier score
  brier_score_test <- mean((pred_prob_test - true_class_test)^2)
  
  # Bootstrap CI
  set.seed(123)
  ci_test_metrics <- bootstrap_ci(pred_prob_test, true_class_test, best_threshold_train, B = 1000)
  Sensitivity_CI <- format_ci(best_metrics_test$Sensitivity, ci_test_metrics[, "Sensitivity"])
  Specificity_CI <- format_ci(best_metrics_test$Specificity, ci_test_metrics[, "Specificity"])
  PPV_CI <- format_ci(best_metrics_test$PPV, ci_test_metrics[, "PPV"])
  NPV_CI <- format_ci(best_metrics_test$NPV, ci_test_metrics[, "NPV"])
  CCR_CI <- format_ci(best_metrics_test$CCR, ci_test_metrics[, "CCR"])
  F1_score_CI <- format_ci(best_metrics_test$F1_score, ci_test_metrics[, "F1_score"])
  Brier_score_CI <- format_ci(brier_score_test, ci_test_metrics[, "Brier_score"])
  
  # Summarize test set model results into data frame
  test_metrics_result <- data.frame(
    Model = model_name,
    Dataset = "Testing cohort",
    AUC_CI = auc_with_ci_test,
    Threshold = round(best_threshold_train, 3), # Use training set's best threshold
    Sensitivity_CI = Sensitivity_CI,
    Specificity_CI = Specificity_CI,
    PPV_CI = PPV_CI,
    NPV_CI = NPV_CI,
    CCR_CI = CCR_CI,
    F1_score_CI = F1_score_CI,
    Brier_score_CI = Brier_score_CI
  )
  # Combine results for each model for test set
  LH_test_metrics_xgb <- rbind(LH_test_metrics_xgb, test_metrics_result)
  
  ## Extract DCA data for test set
  # Construct data frame: truth and predicted probability
  test_truth_p <- data.frame(
    truth = true_class_test, # Convert truth to 0/1
    p = pred_prob_test # Extract predicted probability
  )
  
  # Calculate decision curve
  test_dca_result <- decision_curve(truth ~ p, data = test_truth_p, family = binomial, thresholds = seq(0, 1, by = 0.01))
  
  # Extract data and add to data frame
  test_thresholds <- test_dca_result$derived.data$thresholds
  test_net_benefit <- test_dca_result$derived.data$sNB
  test_model <- test_dca_result$derived.data$model
  test_model <- ifelse(test_model == "truth ~ p", model_name, test_model)
  
  # Combine decision data
  LH_test_dca_data_xgb <- rbind(LH_test_dca_data_xgb, data.frame(
    threshold = test_thresholds,
    net_benefit = test_net_benefit,
    model = test_model
  ))
  
  # Order model names
  LH_test_dca_data_xgb$model <- factor(LH_test_dca_data_xgb$model, levels = c("xgb_35", "xgb_30", "xgb_25", "xgb_20", "xgb_15", "xgb_10", "xgb_5", "All", "None"))
}

## DeLong test
# Initialize an empty data frame for storing test set DeLong test results
LH_test_auc_comparisons_xgb <- data.frame()

# Get combinations of model names for the training set
model_names <- names(LH_test_roc_auc_xgb)

# Pairwise ROC curve comparisons, extract Z-scores and p-values for test set
for (i in 1:(length(model_names) - 1)) {
  for (j in (i + 1):length(model_names)) {
    # Get ROC curve data for the two models
    test_roc1 <- LH_test_roc_auc_xgb[[model_names[i]]]
    test_roc2 <- LH_test_roc_auc_xgb[[model_names[j]]]
    
    # DeLong test
    test_delong_test <- roc.test(test_roc1, test_roc2, method = "delong")
    
    # Extract Z-score and p-value, rounding Z-score to 3 decimals and formatting p-value
    test_Z_score <- round(test_delong_test$statistic, 3)
    test_P_value <- pvalue_format(test_delong_test$p.value)
    
    # Add result to data frame and combine
    LH_test_auc_comparisons_xgb <- rbind(LH_test_auc_comparisons_xgb, data.frame(
      Model_comparison = paste(model_names[i], model_names[j], sep = " vs. "),
      LH_test_Z_score = test_Z_score,
      LH_test_P_value = test_P_value
    ))
  }
}

# Plot ROC curves for all models on the test set
LH_test_roc_xgb <- ggplot(LH_test_roc_data_xgb, aes(x = FPR, y = TPR, color = model)) +
  geom_line(size = 0.1) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.2)) + # Set x-axis ticks
  scale_y_continuous(breaks = seq(0, 1, by = 0.2)) + # Set y-axis ticks
  
  # Custom colors and legend labels
  scale_color_manual(
    values = c("#4DAF9C", "#807DBA", "#E34A33", "#3182BD", "#A63603", "#31A354", "#8C6D31"),
    labels = c(
      expression("35 features (AUC=0.806)"),
      expression("30 features (AUC=0.806, " * italic(P) * "=1.000)"),
      expression("25 features (AUC=0.806, " * italic(P) * "=1.000)"),
      expression("20 features (AUC=0.803, " * italic(P) * "=0.234)"),
      expression("15 features (AUC=0.807, " * italic(P) * "=0.484)"),
      expression("10 features (AUC=0.806, " * italic(P) * "=0.883)"),
      expression("5 features (AUC=0.805, " * italic(P) * "=0.867)"))
  ) +
  labs(x = "1-Specificity",
       y = "Sensitivity",
       title = "ROC Curve", # Plot title
       color = "") + # Legend title blank
  theme_minimal() + # Set chart theme
  theme(
    plot.background = element_rect(fill = "white", color = NA), # Overall image background, border color none
    panel.grid = element_blank(), # Remove grid lines
    panel.border = element_rect(color = "black", fill = NA), # Add border
    axis.ticks.x = element_line(color = "black"), # x-axis tick marks
    axis.ticks.y = element_line(color = "black"), # y-axis tick marks
    axis.ticks.length = unit(0.1, "cm"), # Tick mark length and placement outside
    legend.position = c(0.75, 0.22), # Legend placed at lower right
    legend.key.height = unit(0.2, "cm"), # Control height of each legend item
    legend.key.width = unit(0.4, "cm"), # Control width of each legend item
    axis.title = element_text(size = 60), # Axis title font size
    axis.text = element_text(size = 50), # Axis tick label font size
    legend.text = element_text(size = 25), # Legend text font size
    plot.title = element_text(size = 60, hjust = 0.5), # Plot title font, center alignment
    plot.tag = element_text(size = 90, face = "bold") # Set tag text size
  ) +
  coord_fixed(ratio = 5/7) + # Set plot aspect ratio
  annotate(geom = "segment", x = 0, y = 0, xend = 1, yend = 1, color = "black", size = 0.2, linetype = "dotted") # Add diagonal line

## Plot calibration curves for test set
# Extract test task data
LH_test_task_data <- LH_test_task$data()
LH_test_task_data$CMDs <- factor(ifelse(LH_test_task_data$CMDs == "Yes", 1, 0))

# Score the models
LH_test_score_xgb <- Score(list(xgb_35 = LH_test_prob_data_xgb[["xgb_35"]],
                                xgb_30 = LH_test_prob_data_xgb[["xgb_30"]],
                                xgb_25 = LH_test_prob_data_xgb[["xgb_25"]],
                                xgb_20 = LH_test_prob_data_xgb[["xgb_20"]],
                                xgb_15 = LH_test_prob_data_xgb[["xgb_15"]],
                                xgb_10 = LH_test_prob_data_xgb[["xgb_10"]],
                                xgb_5 = LH_test_prob_data_xgb[["xgb_5"]]),
                           formula = CMDs ~ 1, # Model evaluation formula
                           null.model = F, # Do not use null model for comparison
                           plots = "calibration", # Plot calibration curve
                           data = LH_test_task_data)

# Extract calibration curve data
LH_test_calibration_plot_xgb <- plotCalibration(LH_test_score_xgb, method = "nne", bandwidth = 0.05, plot = FALSE)
LH_test_calibration_data_xgb <- imap_dfr(LH_test_calibration_plot_xgb$plotFrames, ~ {
  .x %>%
    as_tibble() %>%
    mutate(model = .y) # .y is the list element name (lr/rf/svm etc.)
})
LH_test_calibration_data_xgb$model <- factor(LH_test_calibration_data_xgb$model, levels = c("xgb_35", "xgb_30", "xgb_25", "xgb_20", "xgb_15", "xgb_10", "xgb_5"))

# Plot calibration curve
LH_test_calibration_xgb <- ggplot(LH_test_calibration_data_xgb, aes(x = Pred, y = Obs, color = model)) +
  geom_line(linewidth = 0.1) + # Set line thickness
  # Use geom_segment() to draw the ideal calibration line
  geom_segment(data = segment_data, aes(x = x, y = y, xend = xend, yend = yend, color = segment_type), linewidth = 0.2, linetype = "dotted") +
  scale_x_continuous(limits = c(0, 1), # Limit x-axis range
                     breaks = seq(0, 1, by = 0.2), name = "Predicted Probability") + # Set x-axis ticks and labels
  scale_y_continuous(limits = c(0, 1), # Limit y-axis range
                     breaks = seq(0, 1, by = 0.2), name = "Actual Probability") + # Set y-axis ticks and labels
  # Custom colors and legend labels
  scale_color_manual(
    values = c("#4DAF9C", "#807DBA", "#E34A33", "#3182BD", "#A63603", "#31A354", "#8C6D31", "black"),
    labels = c("35 features", "30 features", "25 features","20 features",
               "15 features", "10 features", "5 features", "Ideal")
  ) +
  labs(title = "Calibration Curve", # Plot title
       color = "") + # Legend title blank
  theme_minimal() + # Set chart theme
  theme(
    plot.background = element_rect(fill = "white", color = NA), # Overall image background, border color none
    panel.grid = element_blank(), # Remove grid lines
    panel.border = element_rect(color = "black", fill = NA), # Add border
    axis.ticks.x = element_line(color = "black"), # x-axis tick marks
    axis.ticks.y = element_line(color = "black"), # y-axis tick marks
    axis.ticks.length = unit(0.1, "cm"), # Tick mark length and placement outside
    legend.position = c(0.15, 0.82), # Legend placed at upper left
    legend.key.height = unit(0.2, "cm"), # Control height of each legend item
    legend.key.width = unit(0.4, "cm"), # Control width of each legend item
    axis.title = element_text(size = 60), # Axis title font size
    axis.text = element_text(size = 50), # Axis tick label font size
    legend.text = element_text(size = 30), # Legend text font size
    plot.title = element_text(size = 60, hjust = 0.5), # Plot title font, center alignment
    plot.tag = element_text(size = 90, face = "bold") # Set tag text size
  ) +
  coord_fixed(ratio = 5/7) # Set plot aspect ratio

# Plot DCA for test set
LH_test_dca_xgb <- ggplot(LH_test_dca_data_xgb, aes(x = threshold, y = net_benefit, color = model)) +
  geom_line(linewidth = 0.1) + # Set line thickness
  scale_x_continuous(limits = c(0, 0.8), # Limit x-axis range
                     breaks = seq(0, 0.8, by = 0.2), # Set x-axis ticks
                     name = "High Risk Threshold") + # Set x-axis label
  scale_y_continuous(limits = c(-0.2, 0.8), # Limit y-axis range
                     breaks = seq(-0.2, 0.8, by = 0.2), # Set y-axis ticks
                     name = "Standardized Net Benefit") + # Set y-axis label
  scale_color_manual(values = c("#4DAF9C", "#807DBA", "#E34A33", "#3182BD",
                                "#A63603", "#31A354", "#8C6D31", "gray", "black"), # Custom colors
                     labels = c("35 features", "30 features", "25 features",
                                "20 features", "15 features", "10 features",
                                "5 features", "All", "None")
  ) +
  labs(title = "DCA", # Plot title
       color = "") + # Legend title blank
  theme_minimal() + # Set chart theme
  theme(
    plot.background = element_rect(fill = "white", color = NA), # Overall image background, border color none
    panel.grid = element_blank(), # Remove grid lines
    panel.border = element_rect(color = "black", fill = NA), # Add border
    axis.ticks.x = element_line(color = "black"), # x-axis tick marks
    axis.ticks.y = element_line(color = "black"), # y-axis tick marks
    axis.ticks.length = unit(0.1, "cm"), # Tick mark length and placement outside
    legend.position = c(0.86, 0.79), # Legend placed at upper right
    legend.key.height = unit(0.2, "cm"), # Control height of each legend item
    legend.key.width = unit(0.4, "cm"), # Control width of each legend item
    axis.title = element_text(size = 60), # Axis title font size
    axis.text = element_text(size = 50), # Axis tick label font size
    legend.text = element_text(size = 30), # Legend text font size
    plot.title = element_text(size = 60, hjust = 0.5), # Plot title font, center alignment
    plot.tag = element_text(size = 90, face = "bold") # Set tag text size
  ) +
  coord_fixed(ratio = 4/7) # Set plot aspect ratio

## Plot radar charts for test set
# Convert data to long format suitable for radar charts
LH_test_radar_data_xgb <- LH_test_metrics_xgb %>%
  select(3, 5:11) %>%
  mutate(across(ends_with("_CI"),
                ~ as.numeric(sub("\\s*\\(.*$", "", .x))))
LH_test_radar_data_xgb <- t(LH_test_radar_data_xgb) # Transpose data frame
LH_test_radar_data_xgb <- as.data.frame(LH_test_radar_data_xgb) # Convert back to data frame
colnames(LH_test_radar_data_xgb) <- c("35", "30", "25", "20", "15", "10", "5") # Add column names
LH_test_radar_data_xgb$metrics <- c("AUC", "Sensitivity", "Specificity", "PPV", "NPV", "CCR", "F1 score", "Brier score") # Add performance column
LH_test_radar_data_xgb <- LH_test_radar_data_xgb[, c("metrics", "35", "30", "25", "20", "15", "10", "5")] # Reorder columns
LH_test_radar_data_xgb$metrics <- factor(LH_test_radar_data_xgb$metrics, levels=c("AUC", "Sensitivity", "Specificity", "PPV", "NPV", "CCR", "F1 score", "Brier score")) # Convert to factor

# Plot radar chart - AUC
LH_test_AUC_xgb <- ggradar(LH_test_radar_data_xgb[1,],
                           grid.line.width = 0.4, # Grid line width
                           grid.label.size = 11, # Grid label font size
                           axis.label.size = 9, # Axis label font size
                           grid.min = 0, # Grid minimum value
                           grid.mid = 0.45, # Grid mid value
                           grid.max = 0.9, # Grid maximum value
                           values.radar = c(0, 0.45, 0.9), # Axis label display
                           background.circle.colour = "white", # Background color
                           group.line.width = 0.5, # Line width
                           group.point.size = 1, # Point size
                           fill = T, # Fill color
                           fill.alpha = 0.3, # Fill alpha transparency
                           group.colours = "#95B0E0") +
  ggtitle("AUC") + # Add title
  theme(plot.title = element_text(hjust = 0.5, size = 40), # Center title, set label text size
        plot.margin = margin(0, 0, 0, 0, "cm"), # Adjust margins
        plot.tag = element_text(size = 50, face = "bold") # Upper right corner label font size
  ) +
  coord_fixed(ratio = 1) # Set plot to square

# Plot radar chart - Sensitivity
LH_test_Sensitivity_xgb <- ggradar(LH_test_radar_data_xgb[2,],
                                   grid.line.width = 0.4, # Grid line width
                                   grid.label.size = 11, # Grid label font size
                                   axis.label.size = 9, # Axis label font size
                                   grid.min = 0, # Grid minimum value
                                   grid.mid = 0.4, # Grid mid value
                                   grid.max = 0.8, # Grid maximum value
                                   values.radar = c(0, 0.4, 0.8), # Axis label display
                                   background.circle.colour = "white", # Background color
                                   group.line.width = 0.5, # Line width
                                   group.point.size = 1, # Point size
                                   fill = T, # Fill color
                                   fill.alpha = 0.3, # Fill alpha transparency
                                   group.colours = "#56AEDE") +
  ggtitle("Sensitivity") + # Add title
  theme(plot.title = element_text(hjust = 0.5, size = 40), # Center title, set label text size
        plot.margin = margin(0, 0, 0, 0, "cm"), # Adjust margins
        plot.tag = element_text(size = 50, face = "bold") # Upper right corner label font size
  ) +
  coord_fixed(ratio = 1) # Set plot to square

# Plot radar chart - Specificity
LH_test_Specificity_xgb <- ggradar(LH_test_radar_data_xgb[3,],
                                   grid.line.width = 0.4, # Grid line width
                                   grid.label.size = 11, # Grid label font size
                                   axis.label.size = 9, # Axis label font size
                                   grid.min = 0, # Grid minimum value
                                   grid.mid = 0.4, # Grid mid value
                                   grid.max = 0.8, # Grid maximum value
                                   values.radar = c(0, 0.4, 0.8), # Axis label display
                                   background.circle.colour = "white", # Background color
                                   group.line.width = 0.5, # Line width
                                   group.point.size = 1, # Point size
                                   fill = T, # Fill color
                                   fill.alpha = 0.2, # Fill alpha transparency
                                   group.colours = "#EE7A5F") +
  ggtitle("Specificity") + # Add title
  theme(plot.title = element_text(hjust = 0.5, size = 40), # Center title, set label text size
        plot.margin = margin(0, 0, 0, 0, "cm"), # Adjust margins
        plot.tag = element_text(size = 50, face = "bold") # Upper right corner label font size
  ) +
  coord_fixed(ratio = 1) # Set plot to square

# Plot radar chart - PPV
LH_test_PPV_xgb <- ggradar(LH_test_radar_data_xgb[4,],
                           grid.line.width = 0.4, # Grid line width
                           grid.label.size = 11, # Grid label font size
                           axis.label.size = 9, # Axis label font size
                           grid.min = 0, # Grid minimum value
                           grid.mid = 0.15, # Grid mid value
                           grid.max = 0.3, # Grid maximum value
                           values.radar = c(0, 0.15, 0.3), # Axis label display
                           background.circle.colour = "white", # Background color
                           group.line.width = 0.5, # Line width
                           group.point.size = 1, # Point size
                           fill = T, # Fill color
                           fill.alpha = 0.2, # Fill alpha transparency
                           group.colours = "#FEAF8A") +
  ggtitle("PPV") + # Add title
  theme(plot.title = element_text(hjust = 0.5, size = 40), # Center title, set label text size
        plot.margin = margin(0, 0, 0, 0, "cm"), # Adjust margins
        plot.tag = element_text(size = 50, face = "bold") # Upper right corner label font size
  ) +
  coord_fixed(ratio = 1) # Set plot to square

# Plot radar chart - NPV
LH_test_NPV_xgb <- ggradar(LH_test_radar_data_xgb[5,],
                           grid.line.width = 0.4, # Grid line width
                           grid.label.size = 11, # Grid label font size
                           axis.label.size = 9, # Axis label font size
                           grid.min = 0, # Grid minimum value
                           grid.mid = 0.5, # Grid mid value
                           grid.max = 1, # Grid maximum value
                           values.radar = c(0, 0.5, 1), # Axis label display
                           background.circle.colour = "white", # Background color
                           group.line.width = 0.5, # Line width
                           group.point.size = 1, # Point size
                           fill = T, # Fill color
                           fill.alpha = 0.3, # Fill alpha transparency
                           group.colours = "#F6B7C6") +
  ggtitle("NPV") + # Add title
  theme(plot.title = element_text(hjust = 0.5, size = 40), # Center title, set label text size
        plot.margin = margin(0, 0, 0, 0, "cm"), # Adjust margins
        plot.tag = element_text(size = 50, face = "bold") # Upper right corner label font size
  ) +
  coord_fixed(ratio = 1) # Set plot to square

# Plot radar chart - CCR
LH_test_CCR_xgb <- ggradar(LH_test_radar_data_xgb[6,],
                           grid.line.width = 0.4, # Grid line width
                           grid.label.size = 11, # Grid label font size
                           axis.label.size = 9, # Axis label font size
                           grid.min = 0, # Grid minimum value
                           grid.mid = 0.4, # Grid mid value
                           grid.max = 0.8, # Grid maximum value
                           values.radar = c(0, 0.4, 0.8), # Axis label display
                           background.circle.colour = "white", # Background color
                           group.line.width = 0.5, # Line width
                           group.point.size = 1, # Point size
                           fill = T, # Fill color
                           fill.alpha = 0.3, # Fill alpha transparency
                           group.colours = "#D8CBF0") +
  ggtitle("CCR") + # Add title
  theme(plot.title = element_text(hjust = 0.5, size = 40), # Center title, set label text size
        plot.margin = margin(0, 0, 0, 0, "cm"), # Adjust margins
        plot.tag = element_text(size = 50, face = "bold") # Upper right corner label font size
  ) +
  coord_fixed(ratio = 1) # Set plot to square

# Plot radar chart - F1 score
LH_test_F1_score_xgb <- ggradar(LH_test_radar_data_xgb[7,],
                                grid.line.width = 0.4, # Grid line width
                                grid.label.size = 11, # Grid label font size
                                axis.label.size = 9, # Axis label font size
                                grid.min = 0, # Grid minimum value
                                grid.mid = 0.2, # Grid mid value
                                grid.max = 0.4, # Grid maximum value
                                values.radar = c(0, 0.2, 0.4), # Axis label display
                                background.circle.colour = "white", # Background color
                                group.line.width = 0.5, # Line width
                                group.point.size = 1, # Point size
                                fill = T, # Fill color
                                fill.alpha = 0.3, # Fill alpha transparency
                                group.colours = "#9FCDC9") +
  ggtitle("F1 score") + # Add title
  theme(plot.title = element_text(hjust = 0.5, size = 40), # Center title, set label text size
        plot.margin = margin(0, 0, 0, 0, "cm"), # Adjust margins
        plot.tag = element_text(size = 50, face = "bold") # Upper right corner label font size
  ) +
  coord_fixed(ratio = 1) # Set plot to square

# Plot radar chart - Brier score
LH_test_Brier_score_xgb <- ggradar(LH_test_radar_data_xgb[8,],
                                   grid.line.width = 0.4, # Grid line width
                                   grid.label.size = 11, # Grid label font size
                                   axis.label.size = 9, # Axis label font size
                                   grid.min = 0, # Grid minimum value
                                   grid.mid = 0.05, # Grid mid value
                                   grid.max = 0.1, # Grid maximum value
                                   values.radar = c(0, 0.05, 0.1), # Axis label display
                                   background.circle.colour = "white", # Background color
                                   group.line.width = 0.5, # Line width
                                   group.point.size = 1, # Point size
                                   fill = T, # Fill color
                                   fill.alpha = 0.2, # Fill alpha transparency
                                   group.colours = "#7EB87B") +
  ggtitle("Brier score") + # Add title
  theme(plot.title = element_text(hjust = 0.5, size = 40), # Center title, set label text size
        plot.margin = margin(0, 0, 0, 0, "cm"), # Adjust margins
        plot.tag = element_text(size = 50, face = "bold") # Upper right corner label font size
  ) +
  coord_fixed(ratio = 1) # Set plot to square

# Combine radar charts for test set
LH_test_radar_xgb <- (LH_test_AUC_xgb + LH_test_Sensitivity_xgb + LH_test_Specificity_xgb + LH_test_PPV_xgb +
                        LH_test_NPV_xgb + LH_test_CCR_xgb + LH_test_F1_score_xgb + LH_test_Brier_score_xgb) +
  plot_annotation(tag_levels = "A") + # Automatically add A and B labels
  plot_layout(ncol = 4, nrow = 2) # 2*4 layout

# Export radar charts for test set
ggsave(plot = LH_test_radar_xgb,
       filename = "LH_test_radar_xgb.png",
       width = 17,
       height = 8.5,
       units = "cm",
       dpi = 600)

##------------------Performance evaluation with different numbers of risk factors: Unstable and High-stable groups------------------
# Create lists of model predictions for training and test sets
UH_train_preds_xgb <- list(
  xgb_35 = UH_round_outputs[[1]][["train_xgb_pred"]],
  xgb_30 = UH_round_outputs[[2]][["train_xgb_pred"]],
  xgb_25 = UH_round_outputs[[3]][["train_xgb_pred"]],
  xgb_20 = UH_round_outputs[[4]][["train_xgb_pred"]],
  xgb_15 = UH_round_outputs[[5]][["train_xgb_pred"]],
  xgb_10 = UH_round_outputs[[6]][["train_xgb_pred"]],
  xgb_5 = UH_round_outputs[[7]][["train_xgb_pred"]]
)

UH_test_preds_xgb <- list(
  xgb_35 = UH_round_outputs[[1]][["test_xgb_pred"]],
  xgb_30 = UH_round_outputs[[2]][["test_xgb_pred"]],
  xgb_25 = UH_round_outputs[[3]][["test_xgb_pred"]],
  xgb_20 = UH_round_outputs[[4]][["test_xgb_pred"]],
  xgb_15 = UH_round_outputs[[5]][["test_xgb_pred"]],
  xgb_10 = UH_round_outputs[[6]][["test_xgb_pred"]],
  xgb_5 = UH_round_outputs[[7]][["test_xgb_pred"]]
)

# Create empty data frames for storing performance evaluation results for training and test sets
UH_train_metrics_xgb <- data.frame()
UH_test_metrics_xgb <- data.frame()

# Create empty data frames for storing ROC data for training and test sets
UH_train_roc_data_xgb <- data.frame()
UH_test_roc_data_xgb <- data.frame()

# Create lists for storing ROC() results for training and test sets for DeLong test
UH_train_roc_auc_xgb <- list()
UH_test_roc_auc_xgb <- list()

# Create lists for storing model prediction probability data for training and test sets (calibration curve)
UH_train_prob_data_xgb <- list()
UH_test_prob_data_xgb <- list()

# Create empty data frames for storing DCA data for training and test sets
UH_train_dca_data_xgb <- data.frame()
UH_test_dca_data_xgb <- data.frame()

# Calculate performance metrics for training set
for (model_name in names(UH_train_preds_xgb)) {
  # Get prediction results for training set model
  pred_train <- UH_train_preds_xgb[[model_name]]
  # Get predicted probabilities from training set model
  pred_prob_train <- pred_train$prob[, "Yes"]
  # Store predicted probabilities for training set model in list
  UH_train_prob_data_xgb[[model_name]] <- pred_prob_train
  # Get actual class labels for training set
  true_class_train <- pred_train$truth
  true_class_train <- ifelse(true_class_train == "Yes", 1, 0)
  roc_train <- roc(true_class_train, pred_prob_train)
  auc_train <- roc_train$auc
  ci_train <- ci(roc_train)
  # Combine AUC and 95%CI, keep three decimal places
  auc_with_ci_train <- sprintf("%.3f\n(%.3f-%.3f)", auc_train, ci_train[1], ci_train[3])
  # Store roc() result in list
  UH_train_roc_auc_xgb[[model_name]] <- roc_train
  # Extract data for ROC curve and add to data frame
  roc_data_train <- data.frame(
    FPR = 1-roc_train$specificities, # False Positive Rate: 1-specificities
    TPR = roc_train$sensitivities, # True Positive Rate: sensitivities
    model = model_name # Model name
  )
  # Order model names
  roc_data_train$model <- factor(roc_data_train$model, levels = c("xgb_35", "xgb_30", "xgb_25", "xgb_20", "xgb_15", "xgb_10", "xgb_5"))
  # Combine ROC data for all models for test set
  UH_train_roc_data_xgb <- rbind(UH_train_roc_data_xgb, roc_data_train)
  
  ## Calculate best threshold for training set
  # Define threshold range
  threshold <- seq(0, 1, by = 0.001)
  # Calculate performance metrics for different thresholds
  metrics_list_train <- sapply(threshold, function(t) {
    calculate_metrics(pred_prob_train, true_class_train, t)
  }, simplify = F)
  distances <- sapply(metrics_list_train, function(metrics) {
    sqrt((1-metrics$Sensitivity)^2 + (1-metrics$Specificity)^2)
  })
  # Find best threshold for training set
  best_threshold_train <- threshold[which.min(distances)]
  # Calculate performance metrics for training set using the best threshold
  best_metrics_train <- calculate_metrics(pred_prob_train, true_class_train, best_threshold_train)
  # Calculate Brier score
  brier_score_train <- mean((pred_prob_train - true_class_train)^2)
  # Bootstrap CI
  set.seed(123)
  ci_train_metrics <- bootstrap_ci(pred_prob_train, true_class_train, best_threshold_train, B = 1000)
  Sensitivity_CI <- format_ci(best_metrics_train$Sensitivity, ci_train_metrics[, "Sensitivity"])
  Specificity_CI <- format_ci(best_metrics_train$Specificity, ci_train_metrics[, "Specificity"])
  PPV_CI <- format_ci(best_metrics_train$PPV, ci_train_metrics[, "PPV"])
  NPV_CI <- format_ci(best_metrics_train$NPV, ci_train_metrics[, "NPV"])
  CCR_CI <- format_ci(best_metrics_train$CCR, ci_train_metrics[, "CCR"])
  F1_score_CI <- format_ci(best_metrics_train$F1_score, ci_train_metrics[, "F1_score"])
  Brier_score_CI <- format_ci(brier_score_train, ci_train_metrics[, "Brier_score"])
  # Summarize training set model results
  train_metrics_result <- data.frame(
    Model = model_name,
    Dataset = "Training cohort",
    AUC_CI = auc_with_ci_train,
    Threshold = round(best_threshold_train, 3),
    Sensitivity_CI = Sensitivity_CI,
    Specificity_CI = Specificity_CI,
    PPV_CI = PPV_CI,
    NPV_CI = NPV_CI,
    CCR_CI = CCR_CI,
    F1_score_CI = F1_score_CI,
    Brier_score_CI = Brier_score_CI
  )
  # Add results for each model for training set to data frame
  UH_train_metrics_xgb <- rbind(UH_train_metrics_xgb, train_metrics_result)
  
  ## Extract DCA data for training set
  # Construct data frame: truth and predicted probability
  train_truth_p <- data.frame(
    truth = true_class_train, # Convert truth to 0/1
    p = pred_prob_train # Extract predicted probability
  )
  # Calculate decision curve
  train_dca_result <- decision_curve(truth ~ p, data = train_truth_p, family = binomial, thresholds = seq(0, 1, by = 0.01))
  # Extract data and add to data frame
  train_thresholds <- train_dca_result$derived.data$thresholds
  train_net_benefit <- train_dca_result$derived.data$sNB
  train_model <- train_dca_result$derived.data$model
  train_model <- ifelse(train_model == "truth ~ p", model_name, train_model)
  # Combine decision data
  UH_train_dca_data_xgb <- rbind(UH_train_dca_data_xgb, data.frame(
    threshold = train_thresholds,
    net_benefit = train_net_benefit,
    model = train_model
  ))
  # Order model names
  UH_train_dca_data_xgb$model <- factor(UH_train_dca_data_xgb$model, levels = c("xgb_35", "xgb_30", "xgb_25", "xgb_20", "xgb_15", "xgb_10", "xgb_5", "All", "None"))
}

# Calculate performance metrics for test set
for (model_name in names(UH_test_preds_xgb)) {
  # Get prediction results for test set model
  pred_test <- UH_test_preds_xgb[[model_name]]
  # Get predicted probabilities from test set model
  pred_prob_test <- pred_test$prob[, "Yes"]
  # Store predicted probabilities for test set model in list
  UH_test_prob_data_xgb[[model_name]] <- pred_prob_test
  # Get actual class labels for test set
  true_class_test <- pred_test$truth
  true_class_test <- ifelse(true_class_test == "Yes", 1, 0)
  # Calculate test set AUC and 95%CI
  roc_test <- roc(true_class_test, pred_prob_test)
  auc_test <- roc_test$auc
  ci_test <- ci(roc_test)
  # Combine AUC and 95%CI, keep three decimal places
  auc_with_ci_test <- sprintf("%.3f\n(%.3f-%.3f)", auc_test, ci_test[1], ci_test[3])
  # Store roc() result in list
  UH_test_roc_auc_xgb[[model_name]] <- roc_test
  # Extract data for ROC curve and add to data frame
  roc_data_test <- data.frame(
    FPR = 1-roc_test$specificities, # False Positive Rate: 1-specificities
    TPR = roc_test$sensitivities, # True Positive Rate: sensitivities
    model = model_name # Model name
  )
  # Order model names
  roc_data_test$model <- factor(roc_data_test$model, levels = c("xgb_35", "xgb_30", "xgb_25", "xgb_20", "xgb_15", "xgb_10", "xgb_5"))
  # Combine ROC data for all models for test set
  UH_test_roc_data_xgb <- rbind(UH_test_roc_data_xgb, roc_data_test)
  # Get best threshold from training set
  best_threshold_train <- UH_train_metrics_xgb[UH_train_metrics_xgb$Model == model_name, "Threshold"]
  # Calculate test set performance using training set's best threshold
  best_metrics_test <- calculate_metrics(pred_prob_test, true_class_test, best_threshold_train)
  # Calculate test set Brier score
  brier_score_test <- mean((pred_prob_test - true_class_test)^2)
  # Bootstrap CI
  set.seed(123)
  ci_test_metrics <- bootstrap_ci(pred_prob_test, true_class_test, best_threshold_train, B = 1000)
  Sensitivity_CI <- format_ci(best_metrics_test$Sensitivity, ci_test_metrics[, "Sensitivity"])
  Specificity_CI <- format_ci(best_metrics_test$Specificity, ci_test_metrics[, "Specificity"])
  PPV_CI <- format_ci(best_metrics_test$PPV, ci_test_metrics[, "PPV"])
  NPV_CI <- format_ci(best_metrics_test$NPV, ci_test_metrics[, "NPV"])
  CCR_CI <- format_ci(best_metrics_test$CCR, ci_test_metrics[, "CCR"])
  F1_score_CI <- format_ci(best_metrics_test$F1_score, ci_test_metrics[, "F1_score"])
  Brier_score_CI <- format_ci(brier_score_test, ci_test_metrics[, "Brier_score"])
  # Summarize test set model results into data frame
  test_metrics_result <- data.frame(
    Model = model_name,
    Dataset = "Testing cohort",
    AUC_CI = auc_with_ci_test,
    Threshold = round(best_threshold_train, 3), # Use training set's best threshold
    Sensitivity_CI = Sensitivity_CI,
    Specificity_CI = Specificity_CI,
    PPV_CI = PPV_CI,
    NPV_CI = NPV_CI,
    CCR_CI = CCR_CI,
    F1_score_CI = F1_score_CI,
    Brier_score_CI = Brier_score_CI
  )
  # Combine results for each model for test set
  UH_test_metrics_xgb <- rbind(UH_test_metrics_xgb, test_metrics_result)
  
  ## Extract DCA data for test set
  # Construct data frame: truth and predicted probability
  test_truth_p <- data.frame(
    truth = true_class_test, # Convert truth to 0/1
    p = pred_prob_test # Extract predicted probability
  )
  # Calculate decision curve
  test_dca_result <- decision_curve(truth ~ p, data = test_truth_p, family = binomial, thresholds = seq(0, 1, by = 0.01))
  # Extract data and add to data frame
  test_thresholds <- test_dca_result$derived.data$thresholds
  test_net_benefit <- test_dca_result$derived.data$sNB
  test_model <- test_dca_result$derived.data$model
  test_model <- ifelse(test_model == "truth ~ p", model_name, test_model)
  # Combine decision data
  UH_test_dca_data_xgb <- rbind(UH_test_dca_data_xgb, data.frame(
    threshold = test_thresholds,
    net_benefit = test_net_benefit,
    model = test_model
  ))
  # Order model names
  UH_test_dca_data_xgb$model <- factor(UH_test_dca_data_xgb$model, levels = c("xgb_35", "xgb_30", "xgb_25", "xgb_20", "xgb_15", "xgb_10", "xgb_5", "All", "None"))
}

## DeLong test
# Initialize an empty data frame for storing test set DeLong test results
UH_test_auc_comparisons_xgb <- data.frame()

# Get combinations of model names for the training set
model_names <- names(UH_test_roc_auc_xgb)

# Pairwise ROC curve comparisons, extract Z-scores and p-values for test set
for (i in 1:(length(model_names) - 1)) {
  for (j in (i + 1):length(model_names)) {
    # Get ROC curve data for the two models
    test_roc1 <- UH_test_roc_auc_xgb[[model_names[i]]]
    test_roc2 <- UH_test_roc_auc_xgb[[model_names[j]]]
    # DeLong test
    test_delong_test <- roc.test(test_roc1, test_roc2, method = "delong")
    # Extract Z-score and p-value, rounding Z-score to 3 decimals and formatting p-value
    test_Z_score <- round(test_delong_test$statistic, 3)
    test_P_value <- pvalue_format(test_delong_test$p.value)
    # Add result to data frame and combine
    UH_test_auc_comparisons_xgb <- rbind(UH_test_auc_comparisons_xgb, data.frame(
      Model_comparison = paste(model_names[i], model_names[j], sep = " vs. "),
      UH_test_Z_score = test_Z_score,
      UH_test_P_value = test_P_value
    ))
  }
}

# Plot ROC curves for all models on the test set
UH_test_roc_xgb <- ggplot(UH_test_roc_data_xgb, aes(x = FPR, y = TPR, color = model)) +
  geom_line(size = 0.1) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.2)) + # Set x-axis ticks
  scale_y_continuous(breaks = seq(0, 1, by = 0.2)) + # Set y-axis ticks
  
  # Custom colors and legend labels
  scale_color_manual(
    values = c("#4DAF9C", "#807DBA", "#E34A33", "#3182BD", "#A63603", "#31A354", "#8C6D31"),
    labels = c(
      expression("35 features (AUC=0.805)"),
      expression("30 features (AUC=0.805, " * italic(P) * "=0.279)"),
      expression("25 features (AUC=0.805, " * italic(P) * "=0.429)"),
      expression("20 features (AUC=0.805, " * italic(P) * "=0.830)"),
      expression("15 features (AUC=0.802, " * italic(P) * "=0.360)"),
      expression("10 features (AUC=0.805, " * italic(P) * "=0.887)"),
      expression("5 features (AUC=0.802, " * italic(P) * "=0.617)"))
  ) +
  labs(x = "1-Specificity",
       y = "Sensitivity",
       title = "ROC Curve", # Plot title
       color = "") + # Legend title blank
  theme_minimal() + # Set chart theme
  theme(
    plot.background = element_rect(fill = "white", color = NA), # Overall image background, border color none
    panel.grid = element_blank(), # Remove grid lines
    panel.border = element_rect(color = "black", fill = NA), # Add border
    axis.ticks.x = element_line(color = "black"), # x-axis tick marks
    axis.ticks.y = element_line(color = "black"), # y-axis tick marks
    axis.ticks.length = unit(0.1, "cm"), # Tick mark length and placement outside
    legend.position = c(0.75, 0.22), # Legend placed at lower right
    legend.key.height = unit(0.2, "cm"), # Control height of each legend item
    legend.key.width = unit(0.4, "cm"), # Control width of each legend item
    axis.title = element_text(size = 60), # Axis title font size
    axis.text = element_text(size = 50), # Axis tick label font size
    legend.text = element_text(size = 25), # Legend text font size
    plot.title = element_text(size = 60, hjust = 0.5), # Plot title font, center alignment
    plot.tag = element_text(size = 90, face = "bold") # Set tag text size
  ) +
  coord_fixed(ratio = 5/7) + # Set plot aspect ratio
  annotate(geom = "segment", x = 0, y = 0, xend = 1, yend = 1, color = "black", size = 0.2, linetype = "dotted") # Add diagonal line

## Plot calibration curves for test set
# Score the models
UH_test_score_xgb <- Score(list(xgb_35 = UH_test_prob_data_xgb[["xgb_35"]],
                                xgb_30 = UH_test_prob_data_xgb[["xgb_30"]],
                                xgb_25 = UH_test_prob_data_xgb[["xgb_25"]],
                                xgb_20 = UH_test_prob_data_xgb[["xgb_20"]],
                                xgb_15 = UH_test_prob_data_xgb[["xgb_15"]],
                                xgb_10 = UH_test_prob_data_xgb[["xgb_10"]],
                                xgb_5 = UH_test_prob_data_xgb[["xgb_5"]]),
                           formula = CMDs ~ 1, # Model evaluation formula
                           null.model = F, # Do not use null model for comparison
                           plots = "calibration", # Plot calibration curve
                           data = UH_test_task_data)

# Extract calibration curve data
UH_test_calibration_plot_xgb <- plotCalibration(UH_test_score_xgb, method = "nne", bandwidth = 0.05, plot = FALSE)
UH_test_calibration_data_xgb <- imap_dfr(UH_test_calibration_plot_xgb$plotFrames, ~ {
  .x %>%
    as_tibble() %>%
    mutate(model = .y) # .y is the list element name (lr/rf/svm etc.)
})
UH_test_calibration_data_xgb$model <- factor(UH_test_calibration_data_xgb$model, levels = c("xgb_35", "xgb_30", "xgb_25", "xgb_20", "xgb_15", "xgb_10", "xgb_5"))

# Plot calibration curve
UH_test_calibration_xgb <- ggplot(UH_test_calibration_data_xgb, aes(x = Pred, y = Obs, color = model)) +
  geom_line(linewidth = 0.1) + # Set line thickness
  # Use geom_segment() to draw the ideal calibration line
  geom_segment(data = segment_data, aes(x = x, y = y, xend = xend, yend = yend, color = segment_type), linewidth = 0.2, linetype = "dotted") +
  scale_x_continuous(limits = c(0, 1), # Limit x-axis range
                     breaks = seq(0, 1, by = 0.2), name = "Predicted Probability") + # Set x-axis ticks and labels
  scale_y_continuous(limits = c(0, 1), # Limit y-axis range
                     breaks = seq(0, 1, by = 0.2), name = "Actual Probability") + # Set y-axis ticks and labels
  # Custom colors and legend labels
  scale_color_manual(
    values = c("#4DAF9C", "#807DBA", "#E34A33", "#3182BD", "#A63603", "#31A354", "#8C6D31", "black"),
    labels = c("35 features", "30 features", "25 features","20 features",
               "15 features", "10 features", "5 features", "Ideal")
  ) +
  labs(title = "Calibration Curve", # Plot title
       color = "") + # Legend title blank
  theme_minimal() + # Set chart theme
  theme(
    plot.background = element_rect(fill = "white", color = NA), # Overall image background, border color none
    panel.grid = element_blank(), # Remove grid lines
    panel.border = element_rect(color = "black", fill = NA), # Add border
    axis.ticks.x = element_line(color = "black"), # x-axis tick marks
    axis.ticks.y = element_line(color = "black"), # y-axis tick marks
    axis.ticks.length = unit(0.1, "cm"), # Tick mark length and placement outside
    legend.position = c(0.15, 0.82), # Legend placed at upper left
    legend.key.height = unit(0.2, "cm"), # Control height of each legend item
    legend.key.width = unit(0.4, "cm"), # Control width of each legend item
    axis.title = element_text(size = 60), # Axis title font size
    axis.text = element_text(size = 50), # Axis tick label font size
    legend.text = element_text(size = 30), # Legend text font size
    plot.title = element_text(size = 60, hjust = 0.5), # Plot title font, center alignment
    plot.tag = element_text(size = 90, face = "bold") # Set tag text size
  ) +
  coord_fixed(ratio = 5/7) # Set plot aspect ratio

# Plot DCA for test set
UH_test_dca_xgb <- ggplot(UH_test_dca_data_xgb, aes(x = threshold, y = net_benefit, color = model)) +
  geom_line(linewidth = 0.1) + # Set line thickness
  scale_x_continuous(limits = c(0, 0.8), # Limit x-axis range
                     breaks = seq(0, 0.8, by = 0.2), # Set x-axis ticks
                     name = "High Risk Threshold") + # Set x-axis label
  scale_y_continuous(limits = c(-0.2, 0.8), # Limit y-axis range
                     breaks = seq(-0.2, 0.8, by = 0.2), # Set y-axis ticks
                     name = "Standardized Net Benefit") + # Set y-axis label
  scale_color_manual(values = c("#4DAF9C", "#807DBA", "#E34A33", "#3182BD",
                                "#A63603", "#31A354", "#8C6D31", "gray", "black"), # Custom colors
                     labels = c("35 features", "30 features", "25 features",
                                "20 features", "15 features", "10 features",
                                "5 features", "All", "None")
  ) +
  labs(title = "DCA", # Plot title
       color = "") + # Legend title blank
  theme_minimal() + # Set chart theme
  theme(
    plot.background = element_rect(fill = "white", color = NA), # Overall image background, border color none
    panel.grid = element_blank(), # Remove grid lines
    panel.border = element_rect(color = "black", fill = NA), # Add border
    axis.ticks.x = element_line(color = "black"), # x-axis tick marks
    axis.ticks.y = element_line(color = "black"), # y-axis tick marks
    axis.ticks.length = unit(0.1, "cm"), # Tick mark length and placement outside
    legend.position = c(0.86, 0.79), # Legend placed at upper right
    legend.key.height = unit(0.2, "cm"), # Control height of each legend item
    legend.key.width = unit(0.4, "cm"), # Control width of each legend item
    axis.title = element_text(size = 60), # Axis title font size
    axis.text = element_text(size = 50), # Axis tick label font size
    legend.text = element_text(size = 30), # Legend text font size
    plot.title = element_text(size = 60, hjust = 0.5), # Plot title font, center alignment
    plot.tag = element_text(size = 90, face = "bold") # Set tag text size
  ) +
  coord_fixed(ratio = 4/7) # Set plot aspect ratio

## Plot radar charts for test set
# Convert data to long format suitable for radar charts
UH_test_radar_data_xgb <- UH_test_metrics_xgb %>%
  select(3, 5:11) %>%
  mutate(across(ends_with("_CI"),
                ~ as.numeric(sub("\\s*\\(.*$", "", .x))))
UH_test_radar_data_xgb <- t(UH_test_radar_data_xgb) # Transpose data frame
UH_test_radar_data_xgb <- as.data.frame(UH_test_radar_data_xgb) # Convert back to data frame
colnames(UH_test_radar_data_xgb) <- c("35", "30", "25", "20", "15", "10", "5") # Add column names
UH_test_radar_data_xgb$metrics <- c("AUC", "Sensitivity", "Specificity", "PPV", "NPV", "CCR", "F1 score", "Brier score") # Add performance column
UH_test_radar_data_xgb <- UH_test_radar_data_xgb[, c("metrics", "35", "30", "25", "20", "15", "10", "5")] # Reorder columns
UH_test_radar_data_xgb$metrics <- factor(UH_test_radar_data_xgb$metrics, levels=c("AUC", "Sensitivity", "Specificity", "PPV", "NPV", "CCR", "F1 score", "Brier score")) # Convert to factor

# Plot radar chart - AUC
UH_test_AUC_xgb <- ggradar(UH_test_radar_data_xgb[1,],
                           grid.line.width = 0.4, # Grid line width
                           grid.label.size = 11, # Grid label font size
                           axis.label.size = 9, # Axis label font size
                           grid.min = 0, # Grid minimum value
                           grid.mid = 0.45, # Grid mid value
                           grid.max = 0.9, # Grid maximum value
                           values.radar = c(0, 0.45, 0.9), # Axis label display
                           background.circle.colour = "white", # Background color
                           group.line.width = 0.5, # Line width
                           group.point.size = 1, # Point size
                           fill = T, # Fill color
                           fill.alpha = 0.3, # Fill alpha transparency
                           group.colours = "#95B0E0") +
  ggtitle("AUC") + # Add title
  theme(plot.title = element_text(hjust = 0.5, size = 40), # Center title, set label text size
        plot.margin = margin(0, 0, 0, 0, "cm"), # Adjust margins
        plot.tag = element_text(size = 50, face = "bold") # Upper right corner label font size
  ) +
  coord_fixed(ratio = 1) # Set plot to square

# Plot radar chart - Sensitivity
UH_test_Sensitivity_xgb <- ggradar(UH_test_radar_data_xgb[2,],
                                   grid.line.width = 0.4, # Grid line width
                                   grid.label.size = 11, # Grid label font size
                                   axis.label.size = 9, # Axis label font size
                                   grid.min = 0, # Grid minimum value
                                   grid.mid = 0.4, # Grid mid value
                                   grid.max = 0.8, # Grid maximum value
                                   values.radar = c(0, 0.4, 0.8), # Axis label display
                                   background.circle.colour = "white", # Background color
                                   group.line.width = 0.5, # Line width
                                   group.point.size = 1, # Point size
                                   fill = T, # Fill color
                                   fill.alpha = 0.3, # Fill alpha transparency
                                   group.colours = "#56AEDE") +
  ggtitle("Sensitivity") + # Add title
  theme(plot.title = element_text(hjust = 0.5, size = 40), # Center title, set label text size
        plot.margin = margin(0, 0, 0, 0, "cm"), # Adjust margins
        plot.tag = element_text(size = 50, face = "bold") # Upper right corner label font size
  ) +
  coord_fixed(ratio = 1) # Set plot to square

# Plot radar chart - Specificity
UH_test_Specificity_xgb <- ggradar(UH_test_radar_data_xgb[3,],
                                   grid.line.width = 0.4, # Grid line width
                                   grid.label.size = 11, # Grid label font size
                                   axis.label.size = 9, # Axis label font size
                                   grid.min = 0, # Grid minimum value
                                   grid.mid = 0.4, # Grid mid value
                                   grid.max = 0.8, # Grid maximum value
                                   values.radar = c(0, 0.4, 0.8), # Axis label display
                                   background.circle.colour = "white", # Background color
                                   group.line.width = 0.5, # Line width
                                   group.point.size = 1, # Point size
                                   fill = T, # Fill color
                                   fill.alpha = 0.2, # Fill alpha transparency
                                   group.colours = "#EE7A5F") +
  ggtitle("Specificity") + # Add title
  theme(plot.title = element_text(hjust = 0.5, size = 40), # Center title, set label text size
        plot.margin = margin(0, 0, 0, 0, "cm"), # Adjust margins
        plot.tag = element_text(size = 50, face = "bold") # Upper right corner label font size
  ) +
  coord_fixed(ratio = 1) # Set plot to square

# Plot radar chart - PPV
UH_test_PPV_xgb <- ggradar(UH_test_radar_data_xgb[4,],
                           grid.line.width = 0.4, # Grid line width
                           grid.label.size = 11, # Grid label font size
                           axis.label.size = 9, # Axis label font size
                           grid.min = 0, # Grid minimum value
                           grid.mid = 0.15, # Grid mid value
                           grid.max = 0.3, # Grid maximum value
                           values.radar = c(0, 0.15, 0.3), # Axis label display
                           background.circle.colour = "white", # Background color
                           group.line.width = 0.5, # Line width
                           group.point.size = 1, # Point size
                           fill = T, # Fill color
                           fill.alpha = 0.2, # Fill alpha transparency
                           group.colours = "#FEAF8A") +
  ggtitle("PPV") + # Add title
  theme(plot.title = element_text(hjust = 0.5, size = 40), # Center title, set label text size
        plot.margin = margin(0, 0, 0, 0, "cm"), # Adjust margins
        plot.tag = element_text(size = 50, face = "bold") # Upper right corner label font size
  ) +
  coord_fixed(ratio = 1) # Set plot to square

# Plot radar chart - NPV
UH_test_NPV_xgb <- ggradar(UH_test_radar_data_xgb[5,],
                           grid.line.width = 0.4, # Grid line width
                           grid.label.size = 11, # Grid label font size
                           axis.label.size = 9, # Axis label font size
                           grid.min = 0, # Grid minimum value
                           grid.mid = 0.5, # Grid mid value
                           grid.max = 1, # Grid maximum value
                           values.radar = c(0, 0.5, 1), # Axis label display
                           background.circle.colour = "white", # Background color
                           group.line.width = 0.5, # Line width
                           group.point.size = 1, # Point size
                           fill = T, # Fill color
                           fill.alpha = 0.3, # Fill alpha transparency
                           group.colours = "#F6B7C6") +
  ggtitle("NPV") + # Add title
  theme(plot.title = element_text(hjust = 0.5, size = 40), # Center title, set label text size
        plot.margin = margin(0, 0, 0, 0, "cm"), # Adjust margins
        plot.tag = element_text(size = 50, face = "bold") # Upper right corner label font size
  ) +
  coord_fixed(ratio = 1) # Set plot to square

# Plot radar chart - CCR
UH_test_CCR_xgb <- ggradar(UH_test_radar_data_xgb[6,],
                           grid.line.width = 0.4, # Grid line width
                           grid.label.size = 11, # Grid label font size
                           axis.label.size = 9, # Axis label font size
                           grid.min = 0, # Grid minimum value
                           grid.mid = 0.4, # Grid mid value
                           grid.max = 0.8, # Grid maximum value
                           values.radar = c(0, 0.4, 0.8), # Axis label display
                           background.circle.colour = "white", # Background color
                           group.line.width = 0.5, # Line width
                           group.point.size = 1, # Point size
                           fill = T, # Fill color
                           fill.alpha = 0.3, # Fill alpha transparency
                           group.colours = "#D8CBF0") +
  ggtitle("CCR") + # Add title
  theme(plot.title = element_text(hjust = 0.5, size = 40), # Center title, set label text size
        plot.margin = margin(0, 0, 0, 0, "cm"), # Adjust margins
        plot.tag = element_text(size = 50, face = "bold") # Upper right corner label font size
  ) +
  coord_fixed(ratio = 1) # Set plot to square

# Plot radar chart - F1 score
UH_test_F1_score_xgb <- ggradar(UH_test_radar_data_xgb[7,],
                                grid.line.width = 0.4, # Grid line width
                                grid.label.size = 11, # Grid label font size
                                axis.label.size = 9, # Axis label font size
                                grid.min = 0, # Grid minimum value
                                grid.mid = 0.2, # Grid mid value
                                grid.max = 0.4, # Grid maximum value
                                values.radar = c(0, 0.2, 0.4), # Axis label display
                                background.circle.colour = "white", # Background color
                                group.line.width = 0.5, # Line width
                                group.point.size = 1, # Point size
                                fill = T, # Fill color
                                fill.alpha = 0.3, # Fill alpha transparency
                                group.colours = "#9FCDC9") +
  ggtitle("F1 score") + # Add title
  theme(plot.title = element_text(hjust = 0.5, size = 40), # Center title, set label text size
        plot.margin = margin(0, 0, 0, 0, "cm"), # Adjust margins
        plot.tag = element_text(size = 50, face = "bold") # Upper right corner label font size
  ) +
  coord_fixed(ratio = 1) # Set plot to square

# Plot radar chart - Brier score
UH_test_Brier_score_xgb <- ggradar(UH_test_radar_data_xgb[8,],
                                   grid.line.width = 0.4, # Grid line width
                                   grid.label.size = 11, # Grid label font size
                                   axis.label.size = 9, # Axis label font size
                                   grid.min = 0, # Grid minimum value
                                   grid.mid = 0.05, # Grid mid value
                                   grid.max = 0.1, # Grid maximum value
                                   values.radar = c(0, 0.05, 0.1), # Axis label display
                                   background.circle.colour = "white", # Background color
                                   group.line.width = 0.5, # Line width
                                   group.point.size = 1, # Point size
                                   fill = T, # Fill color
                                   fill.alpha = 0.2, # Fill alpha transparency
                                   group.colours = "#7EB87B") +
  ggtitle("Brier score") + # Add title
  theme(plot.title = element_text(hjust = 0.5, size = 40), # Center title, set label text size
        plot.margin = margin(0, 0, 0, 0, "cm"), # Adjust margins
        plot.tag = element_text(size = 50, face = "bold") # Upper right corner label font size
  ) +
  coord_fixed(ratio = 1) # Set plot to square

# Combine radar charts for test set
UH_test_radar_xgb <- (UH_test_AUC_xgb + UH_test_Sensitivity_xgb + UH_test_Specificity_xgb + UH_test_PPV_xgb +
                        UH_test_NPV_xgb + UH_test_CCR_xgb + UH_test_F1_score_xgb + UH_test_Brier_score_xgb) +
  plot_annotation(tag_levels = "A") + # Automatically add A and B labels
  plot_layout(ncol = 4, nrow = 2) # 2*4 layout

# Export radar charts for test set
ggsave(plot = UH_test_radar_xgb,
       filename = "UH_test_radar_xgb.png",
       width = 17,
       height = 8.5,
       units = "cm",
       dpi = 600)

# Combine model performance results for both groups
LH_UH_test_metrics_xgb <- rbind(LH_test_metrics_xgb, UH_test_metrics_xgb)

# Combine DeLong test results for both groups
test_roc_comparisons_xgb <- merge(LH_test_auc_comparisons_xgb, UH_test_auc_comparisons_xgb,
                                  by = "Model_comparison")

# Combine ROC plots, calibration curve plots, DCA plots for both groups
ROC_calibration_dca_xgb <- (LH_test_roc_xgb + UH_test_roc_xgb) /
  (LH_test_calibration_xgb + UH_test_calibration_xgb) /
  (LH_test_dca_xgb + UH_test_dca_xgb) +
  plot_annotation(tag_levels = "A") # Automatically add A and B labels

# Export combined ROC, calibration, DCA plots for both groups
ggsave(plot = ROC_calibration_dca_xgb,
       filename = "ROC_calibration_dca_xgb.png",
       width = 16,
       height = 20,
       units = "cm",
       dpi = 600)

## Combine radar charts for both groups
# Add tags to the plots
LH_test_AUC_xgb_tag <- LH_test_AUC_xgb +
  labs(tag = "A") +
  theme(plot.tag.position = c(0.02, 1),
        plot.tag = element_text(size = 60, face = "bold"))

UH_test_AUC_xgb_tag <- UH_test_AUC_xgb +
  labs(tag = "B") +
  theme(plot.tag.position = c(0.02, 1),
        plot.tag = element_text(size = 60, face = "bold"))

LH_UH_test_radar_xgb <- (
  LH_test_AUC_xgb_tag + LH_test_Sensitivity_xgb + LH_test_Specificity_xgb + LH_test_PPV_xgb +
    LH_test_NPV_xgb + LH_test_CCR_xgb + LH_test_F1_score_xgb + LH_test_Brier_score_xgb +
    UH_test_AUC_xgb_tag + UH_test_Sensitivity_xgb + UH_test_Specificity_xgb + UH_test_PPV_xgb +
    UH_test_NPV_xgb + UH_test_CCR_xgb + UH_test_F1_score_xgb + UH_test_Brier_score_xgb
) +
  plot_layout(ncol = 4, nrow = 4)

# Export combined radar charts for test sets
ggsave(plot = LH_UH_test_radar_xgb,
       filename = "LH_UH_test_radar_xgb.png",
       width = 17,
       height = 15,
       units = "cm",
       dpi = 600)

###------------------------SHAP explanation--------------------------
##------------------SHAP explanation: Low decline and High-stable groups------------------
# Build SHAP visualization object
LH_sv_xgb <- shapviz(LH_round_outputs[[7]][["shap_value"]], which_class = 2)

# Map variable names using data dictionary
LH_shap_values_df_xgb <- as.data.frame(LH_sv_xgb$S)
colnames(LH_shap_values_df_xgb) <- variable_dict$display_name[match(names(LH_shap_values_df_xgb), variable_dict$analysis_name)]
LH_sv_xgb$S <- as.matrix(LH_shap_values_df_xgb)
colnames(LH_sv_xgb$X) <- variable_dict$display_name[match(names(LH_sv_xgb$X), variable_dict$analysis_name)]

# SHAP bar plot
LH_shap_bar <- sv_importance(LH_sv_xgb,
                             kind = "bar", # Bar plot
                             max_display = Inf, # Display all features
                             show_numbers = T, # Show numbers (i.e., feature importance scores)
                             fill = "#48EDFE") + # Bar fill color
  scale_x_continuous(limits = c(0, 0.06), # Limit x-axis range
                     breaks = seq(0, 0.06, by = 0.02)) + # Set x-axis ticks
  theme(
    axis.text.y = element_text(vjust = 0.5, hjust = 1, margin = margin(r = 1)),
    panel.background = element_rect(fill = "white"), # Panel background white
    plot.background = element_rect(fill = "white"), # Entire plot background white
    panel.grid.major.y = element_blank(), # Remove horizontal grid lines
    panel.grid.minor = element_blank(), # Remove minor grid lines
    axis.title = element_text(size = 40), # Axis title font size
    axis.text = element_text(size = 30), # Axis tick label font size
    plot.tag = element_text(size = 70, face = "bold"), # Set tag text size
    plot.margin = margin(0, 0, 0, 0, "cm"), # Adjust margins
    axis.ticks = element_line(size = 0.25), # y-axis tick line thickness
    axis.ticks.length = unit(0.05, "cm"), # Tick mark length and placement outside
    axis.ticks.y = element_blank() # Remove y-axis tick marks
  )

# Adjust font size for numbers on the SHAP bar plot
LH_shap_bar$layers <- lapply(LH_shap_bar$layers, function(layer) {
  if ("GeomText" %in% class(layer$geom)) {
    layer$aes_params <- modifyList(layer$aes_params, list(size = 10))
  }
  layer
})

# SHAP beeswarm plot
LH_shap_beeswarm <- sv_importance(LH_sv_xgb,
                                  kind = "beeswarm", # Beeswarm plot
                                  size = 0.2, # Scatter point size (beeswarm)
                                  bee_width = 0.3, # Horizontal spread width of points
                                  max_display = Inf) + # Display all features
  scale_color_gradient(low = "#6601F7", high = "#48EDFE") +
  scale_x_continuous(limits = c(-0.1, 0.6), # Limit x-axis range
                     breaks = seq(-0.1, 0.6, by = 0.1)) + # Set x-axis ticks
  theme(
    panel.background = element_rect(fill = "white"), # Panel background white
    plot.background = element_rect(fill = "white"), # Entire plot background white
    axis.title = element_text(size = 40), # Axis title font size
    axis.text = element_text(size = 30), # Axis tick label font size
    legend.title = element_text(size = 30), # Legend title font size
    legend.text = element_text(size = 20), # Legend text font size
    legend.key.height = unit(0.35, "cm"), # Bar legend height
    legend.key.width = unit(0.1, "cm"), # Bar legend width
    plot.tag = element_text(size = 70, face = "bold"), # Set tag text size
    plot.margin = margin(0, 0, 0, 0, "cm"), # Adjust margins
    axis.ticks = element_line(size = 0.25), # y-axis tick line thickness
    axis.ticks.length = unit(0.05, "cm"), # Tick mark length and placement outside
    axis.ticks.y = element_blank() # Remove y-axis tick marks
  )

# Combine SHAP bar plot and beeswarm plot
LH_shap_bar_beeswarm <- LH_shap_bar + LH_shap_beeswarm +
  plot_annotation(tag_levels = "A") # Automatically add A and B labels

# Export SHAP bar plot and beeswarm plot
ggsave(plot = LH_shap_bar_beeswarm,
       filename = "LH_shap_bar_beeswarm.png",
       width = 17,
       height = 3.5,
       units = "cm",
       bg = "white",
       dpi = 600)

# Create SHAP waterfall plot for the first observation
LH_shap_waterfall <- sv_waterfall(LH_sv_xgb, row_id = 1, size = 300,
                                  fill_colors = c("#48EDFE", "#6601F7"), # Colors
                                  annotation_size = 10) + # Annotation text font size
  labs(x = "SHAP value", tag = "C") + # Change x-axis title to "SHAP value", add tag C
  scale_x_continuous(limits = c(0.02, 0.12), # Limit x-axis range
                     breaks = seq(0.02, 0.12, by = 0.02)) + # Set x-axis ticks
  theme(
    axis.title = element_text(size = 40), # Axis title font size
    axis.text = element_text(size = 30), # Axis tick label font size
    axis.ticks = element_line(size = 0.25), # y-axis tick line thickness
    axis.ticks.length = unit(0.05, "cm"), # Tick mark length and placement outside
    axis.line.x = element_line(size = 0.25), # Set x-axis main line thickness
    panel.grid = element_blank(), # Remove grid lines
    plot.margin = margin(0, 0, 0, 0, "cm"), # Adjust margins
    plot.tag = element_text(size = 70, face = "bold") # Set tag text size
  ) +
  coord_fixed(ratio = 8/1035) # Set plot aspect ratio

# SHAP force plot (single sample)
LH_shap_force <- sv_force(LH_sv_xgb, row_id = 1, max_display = Inf, size = 30,
                          fill_colors = c("#48EDFE", "#6601F7"), # Colors
                          bar_label_size = 10, # Bar label font size
                          annotation_size = 10) + # Annotation text font size
  labs(x = "SHAP value", tag = "D") + # Change x-axis title to "SHAP value", add tag D
  theme(
    axis.title = element_text(size = 40), # Axis title font size
    axis.text = element_text(size = 30), # Axis tick label font size
    axis.ticks = element_line(size = 0.25), # y-axis tick line thickness
    axis.ticks.length = unit(0.05, "cm"), # Tick mark length and placement outside
    axis.line.x = element_line(size = 0.25), # Set x-axis main line thickness
    panel.grid = element_blank(), # Remove grid lines
    plot.margin = margin(0, 0, 0, 0, "cm"), # Adjust margins
    plot.tag = element_text(size = 70, face = "bold") # Set tag text size
  )

# Combine SHAP waterfall plot and force plot
LH_shap_waterfall_force <- LH_shap_waterfall + LH_shap_force

# Export SHAP waterfall and force plots
ggsave(plot = LH_shap_waterfall_force,
       filename = "LH_shap_waterfall_force.png",
       width = 17,
       height = 3.5,
       units = "cm",
       bg = "white",
       dpi = 600)

## SHAP dependence plots
# Define variables
LH_x <- c("Age", "Waist circumference", "FBG", "UA", "WBC")

# Plot
LH_dependence_plots <- lapply(LH_x, function(v) {
  sv_dependence(LH_sv_xgb, v = v, color_var = "auto", size = 0.03) +
    scale_color_gradient(low = "#6601F7", high = "#48EDFE") + # Colors
    theme_bw() +
    theme(
      axis.title = element_text(size = 40), # Axis title font size
      axis.text = element_text(size = 30), # Axis tick label font size
      legend.title = element_text(size = 25), # Legend title font size
      legend.text = element_text(size = 20), # Legend text font size
      legend.key.height = unit(0.6, "cm"), # Bar legend height
      legend.key.width = unit(0.1, "cm"), # Bar legend width
      plot.margin = margin(0, 0, 0, 0, "cm"), # Adjust margins
      axis.ticks = element_line(size = 0.2), # y-axis tick line thickness
      axis.ticks.length = unit(0.03, "cm"), # Tick mark length and placement outside
      aspect.ratio = 1, # Square plot
      legend.position = "right", # Legend on the right
      plot.tag = element_text(size = 70, face = "bold") # Set tag text size
    )
})

# Combine plots
LH_shap_dependence_plot <- wrap_plots(
  lapply(LH_dependence_plots, \(p) p + theme(plot.margin = margin(0, 5, 10, 0))), # top, right, bottom, left
  ncol = 3, nrow = 2) +
  plot_annotation(tag_levels = list("E"))

# Export SHAP dependence plots
ggsave(plot = LH_shap_dependence_plot,
       filename = "LH_shap_dependence_plot.png",
       width = 17,
       height = 9.5,
       units = "cm",
       bg = "white",
       dpi = 600)

##------------------SHAP explanation: Unstable and High-stable groups------------------
# Build SHAP visualization object
UH_sv_xgb <- shapviz(UH_round_outputs[[7]][["shap_value"]], which_class = 2)

# Map variable names using data dictionary
UH_shap_values_df_xgb <- as.data.frame(UH_sv_xgb$S)
colnames(UH_shap_values_df_xgb) <- variable_dict$display_name[match(names(UH_shap_values_df_xgb), variable_dict$analysis_name)]
UH_sv_xgb$S <- as.matrix(UH_shap_values_df_xgb)
colnames(UH_sv_xgb$X) <- variable_dict$display_name[match(names(UH_sv_xgb$X), variable_dict$analysis_name)]

# SHAP bar plot
UH_shap_bar <- sv_importance(UH_sv_xgb,
                             kind = "bar", # Bar plot
                             max_display = Inf, # Display all features
                             show_numbers = T, # Show numbers (i.e., feature importance scores)
                             fill = "#48EDFE") + # Bar fill color
  scale_x_continuous(limits = c(0, 0.06), # Limit x-axis range
                     breaks = seq(0, 0.06, by = 0.02)) + # Set x-axis ticks
  theme(
    axis.text.y = element_text(vjust = 0.5, hjust = 1, margin = margin(r = 1)),
    panel.background = element_rect(fill = "white"), # Panel background white
    plot.background = element_rect(fill = "white"), # Entire plot background white
    panel.grid.major.y = element_blank(), # Remove horizontal grid lines
    panel.grid.minor = element_blank(), # Remove minor grid lines
    axis.title = element_text(size = 40), # Axis title font size
    axis.text = element_text(size = 30), # Axis tick label font size
    plot.tag = element_text(size = 70, face = "bold"), # Set tag text size
    plot.margin = margin(0, 0, 0, 0, "cm"), # Adjust margins
    axis.ticks = element_line(size = 0.25), # y-axis tick line thickness
    axis.ticks.length = unit(0.05, "cm"), # Tick mark length and placement outside
    axis.ticks.y = element_blank() # Remove y-axis tick marks
  )

# Adjust font size for numbers on the SHAP bar plot
UH_shap_bar$layers <- lapply(UH_shap_bar$layers, function(layer) {
  if ("GeomText" %in% class(layer$geom)) {
    layer$aes_params <- modifyList(layer$aes_params, list(size = 10))
  }
  layer
})

# SHAP beeswarm plot
UH_shap_beeswarm <- sv_importance(UH_sv_xgb,
                                  kind = "beeswarm", # Beeswarm plot
                                  size = 0.2, # Scatter point size (beeswarm)
                                  bee_width = 0.3, # Horizontal spread width of points
                                  max_display = Inf) + # Display all features
  scale_color_gradient(low = "#6601F7", high = "#48EDFE") +
  scale_x_continuous(limits = c(-0.2, 0.8), # Limit x-axis range
                     breaks = seq(-0.2, 0.8, by = 0.2)) + # Set x-axis ticks
  theme(
    panel.background = element_rect(fill = "white"), # Panel background white
    plot.background = element_rect(fill = "white"), # Entire plot background white
    axis.title = element_text(size = 40), # Axis title font size
    axis.text = element_text(size = 30), # Axis tick label font size
    legend.title = element_text(size = 30), # Legend title font size
    legend.text = element_text(size = 20), # Legend text font size
    legend.key.height = unit(0.35, "cm"), # Bar legend height
    legend.key.width = unit(0.1, "cm"), # Bar legend width
    plot.tag = element_text(size = 70, face = "bold"), # Set tag text size
    plot.margin = margin(0, 0, 0, 0, "cm"), # Adjust margins
    axis.ticks = element_line(size = 0.25), # y-axis tick line thickness
    axis.ticks.length = unit(0.05, "cm"), # Tick mark length and placement outside
    axis.ticks.y = element_blank() # Remove y-axis tick marks
  )

# Combine SHAP bar plot and beeswarm plot
UH_shap_bar_beeswarm <- UH_shap_bar + UH_shap_beeswarm +
  plot_annotation(tag_levels = "A") # Automatically add A and B labels

# Export SHAP bar plot and beeswarm plot
ggsave(plot = UH_shap_bar_beeswarm,
       filename = "UH_shap_bar_beeswarm.png",
       width = 17,
       height = 3.5,
       units = "cm",
       bg = "white",
       dpi = 600)

# Create SHAP waterfall plot for the first observation
UH_shap_waterfall <- sv_waterfall(UH_sv_xgb, row_id = 1, size = 300,
                                  fill_colors = c("#48EDFE", "#6601F7"), # Colors
                                  annotation_size = 10) + # Annotation text font size
  labs(x = "SHAP value", tag = "C") + # Change x-axis title to "SHAP value", add tag C
  scale_x_continuous(limits = c(0.06, 0.13), # Limit x-axis range
                     breaks = seq(0.06, 0.13, by = 0.01)) + # Set x-axis ticks
  theme(
    axis.title = element_text(size = 40), # Axis title font size
    axis.text = element_text(size = 30), # Axis tick label font size
    axis.ticks = element_line(size = 0.25), # y-axis tick line thickness
    axis.ticks.length = unit(0.05, "cm"), # Tick mark length and placement outside
    axis.line.x = element_line(size = 0.25), # Set x-axis main line thickness
    panel.grid = element_blank(), # Remove grid lines
    plot.margin = margin(0, 0, 0, 0, "cm"), # Adjust margins
    plot.tag = element_text(size = 70, face = "bold") # Set tag text size
  ) +
  coord_fixed(ratio = 8/1780) # Set plot aspect ratio

# SHAP force plot (single sample)
UH_shap_force <- sv_force(UH_sv_xgb, row_id = 1, max_display = Inf, size = 30,
                          fill_colors = c("#48EDFE", "#6601F7"), # Colors
                          bar_label_size = 10, # Bar label font size
                          annotation_size = 10) + # Annotation text font size
  labs(x = "SHAP value", tag = "D") + # Change x-axis title to "SHAP value", add tag D
  theme(
    axis.title = element_text(size = 40), # Axis title font size
    axis.text = element_text(size = 30), # Axis tick label font size
    axis.ticks = element_line(size = 0.25), # y-axis tick line thickness
    axis.ticks.length = unit(0.05, "cm"), # Tick mark length and placement outside
    axis.line.x = element_line(size = 0.25), # Set x-axis main line thickness
    panel.grid = element_blank(), # Remove grid lines
    plot.margin = margin(0, 0, 0, 0, "cm"), # Adjust margins
    plot.tag = element_text(size = 70, face = "bold") # Set tag text size
  )

# Combine SHAP waterfall plot and force plot
UH_shap_waterfall_force <- UH_shap_waterfall + UH_shap_force

# Export SHAP waterfall and force plots
ggsave(plot = UH_shap_waterfall_force,
       filename = "UH_shap_waterfall_force.png",
       width = 17,
       height = 3.5,
       units = "cm",
       bg = "white",
       dpi = 600)

## SHAP dependence plots
# Define variables
UH_x <- c("Age", "HDL-C", "LDL-C", "FBG", "WBC")

# Plot
UH_dependence_plots <- lapply(UH_x, function(v) {
  sv_dependence(UH_sv_xgb, v = v, color_var = "auto", size = 0.03) +
    scale_color_gradient(low = "#6601F7", high = "#48EDFE") + # Colors
    theme_bw() +
    theme(
      axis.title = element_text(size = 40), # Axis title font size
      axis.text = element_text(size = 30), # Axis tick label font size
      legend.title = element_text(size = 25), # Legend title font size
      legend.text = element_text(size = 20), # Legend text font size
      legend.key.height = unit(0.56, "cm"), # Bar legend height
      legend.key.width = unit(0.1, "cm"), # Bar legend width
      plot.margin = margin(0, 0, 0, 0, "cm"), # Adjust margins
      axis.ticks = element_line(size = 0.2), # y-axis tick line thickness
      axis.ticks.length = unit(0.03, "cm"), # Tick mark length and placement outside
      aspect.ratio = 1, # Square plot
      legend.position = "right", # Legend on the right
      plot.tag = element_text(size = 70, face = "bold") # Set tag text size
    )
})

# Combine plots
UH_shap_dependence_plot <- wrap_plots(
  lapply(UH_dependence_plots, \(p) p + theme(plot.margin = margin(0, 5, 5, 0))), # top, right, bottom, left
  ncol = 3, nrow = 2) +
  plot_annotation(tag_levels = list("E"))

# Export SHAP dependence plots
ggsave(plot = UH_shap_dependence_plot,
       filename = "UH_shap_dependence_plot.png",
       width = 17,
       height = 9,
       units = "cm",
       bg = "white",
       dpi = 600)

###------------------------Sankey diagram--------------------------
##------------------Sankey diagram: Low decline and High-stable groups------------------
# Create dataset
LH_test_sankey <- data_LH_test %>%
  select(Age, Waist_circumference, FBG, UA, WBC, CMDs) %>%
  mutate(
    # Age classification: >=60 years and 45-59 years
    Age_group = ifelse(Age >= 60, "≥60 years", "45-59 years"),
    # Waist circumference grouped by quartiles
    Waist_circumference_group = cut(Waist_circumference,
                                    breaks = quantile(Waist_circumference,
                                                      probs = c(0, 0.25, 0.5, 0.75, 1),
                                                      na.rm = TRUE),
                                    labels = c("Q1", "Q2", "Q3", "Q4"),
                                    include.lowest = TRUE),
    # FBG grouped by quartiles
    FBG_group = cut(FBG,
                    breaks = quantile(FBG,
                                      probs = c(0, 0.25, 0.5, 0.75, 1),
                                      na.rm = TRUE),
                    labels = c("Q1", "Q2", "Q3", "Q4"),
                    include.lowest = TRUE),
    # UA grouped by quartiles
    UA_group = cut(UA,
                   breaks = quantile(UA,
                                     probs = c(0, 0.25, 0.5, 0.75, 1),
                                     na.rm = TRUE),
                   labels = c("Q1", "Q2", "Q3", "Q4"),
                   include.lowest = TRUE),
    # WBC grouped by quartiles
    WBC_group = cut(WBC,
                    breaks = quantile(WBC,
                                      probs = c(0, 0.25, 0.5, 0.75, 1),
                                      na.rm = TRUE),
                    labels = c("Q1", "Q2", "Q3", "Q4"),
                    include.lowest = TRUE)
  ) %>%
  # Select needed columns and rename for consistency
  select(Age_group,
         Waist_circumference_group,
         FBG_group,
         UA_group,
         WBC_group,
         CMDs) %>%
  # Rename columns, remove "_group" suffix (optional)
  rename(
    Age = Age_group,
    "Waist circumference" = Waist_circumference_group,
    FBG = FBG_group,
    UA = UA_group,
    WBC = WBC_group
  )

# Convert to long format data frame
data_LH_sankey <- LH_test_sankey %>%
  make_long(Age, "Waist circumference", FBG, UA, WBC, CMDs)

# Define node colors
cols <- c("#90EDFE", "#D8CBF0", "#A0E7E5","#B5EAD7","#F6B7C6","#FFDAC1",
          "#C1CEEA", "#EE7A5F")

# Plot
LH_sankey_plot <- ggplot(data_LH_sankey, aes(x = x,
                                             next_x = next_x,
                                             node = node,
                                             next_node = next_node,
                                             fill = factor(node),
                                             label = node)) + # Add label mapping for displaying text
  geom_sankey(flow.alpha = 0.6, # Set flow transparency
              node.color = "black", # Add black border to nodes
              node.linewidth = 0.1, # Set border thickness
              width = 0.2, # Adjust node width
              space = 0) +
  scale_fill_manual(values = cols) +
  labs(tag = "A") + # Add tag A
  theme_sankey(base_size = 16) + # Use ggsankey's built-in simple theme
  theme(axis.text = element_text(size = 35), # Axis tick label font size
        legend.text = element_text(size = 25, # Legend text font size
                                   margin = margin(l = 0.1, unit = "cm")), # Legend to text spacing
        legend.key.height = unit(0.2, "cm"), # Bar legend height
        legend.key.width = unit(0.2, "cm"), # Bar legend width
        plot.margin = margin(0, 0.5, 0, 0, "cm"), # Adjust margins
        legend.position = c(1, 0.5), # Legend on the right
        legend.margin = margin(0, 0, 0, 0, "cm"), # Legend margins
        plot.tag.position = c(0.03, 0.95), # Tag position
        plot.tag = element_text(size = 70, face = "bold") # Set tag text size
  ) +
  guides(fill = guide_legend(title = NULL)) + # Hide legend title
  labs(x = NULL, title = "") + # Remove extra axis labels
  coord_fixed(ratio = 5/3300) # Set plot aspect ratio

##------------------Sankey diagram: Unstable and High-stable groups------------------
# Create dataset
UH_test_sankey <- data_UH_test %>%
  select(Age, HDL_C, LDL_C, FBG, WBC, CMDs) %>%
  mutate(
    # Age classification: >=60 years and 45-59 years
    Age_group = ifelse(Age >= 60, "≥60 years", "45-59 years"),
    # HDL_C grouped by quartiles
    HDL_C_group = cut(HDL_C,
                      breaks = quantile(HDL_C,
                                        probs = c(0, 0.25, 0.5, 0.75, 1),
                                        na.rm = TRUE),
                      labels = c("Q1", "Q2", "Q3", "Q4"),
                      include.lowest = TRUE),
    # LDL_C grouped by quartiles
    LDL_C_group = cut(LDL_C,
                      breaks = quantile(LDL_C,
                                        probs = c(0, 0.25, 0.5, 0.75, 1),
                                        na.rm = TRUE),
                      labels = c("Q1", "Q2", "Q3", "Q4"),
                      include.lowest = TRUE),
    # FBG grouped by quartiles
    FBG_group = cut(FBG,
                    breaks = quantile(FBG,
                                      probs = c(0, 0.25, 0.5, 0.75, 1),
                                      na.rm = TRUE),
                    labels = c("Q1", "Q2", "Q3", "Q4"),
                    include.lowest = TRUE),
    # WBC grouped by quartiles
    WBC_group = cut(WBC,
                    breaks = quantile(WBC,
                                      probs = c(0, 0.25, 0.5, 0.75, 1),
                                      na.rm = TRUE),
                    labels = c("Q1", "Q2", "Q3", "Q4"),
                    include.lowest = TRUE)
  ) %>%
  # Select needed columns and rename for consistency
  select(Age_group,
         HDL_C_group,
         LDL_C_group,
         FBG_group,
         WBC_group,
         CMDs) %>%
  # Rename columns, remove "_group" suffix (optional)
  rename(
    Age = Age_group,
    "HDL-C" = HDL_C_group,
    "LDL-C" = LDL_C_group,
    FBG = FBG_group,
    WBC = WBC_group
  )

# Convert to long format data frame
data_UH_sankey <- UH_test_sankey %>%
  make_long(Age, "HDL-C", "LDL-C", FBG, WBC, CMDs)

# Plot
UH_sankey_plot <- ggplot(data_UH_sankey, aes(x = x,
                                             next_x = next_x,
                                             node = node,
                                             next_node = next_node,
                                             fill = factor(node),
                                             label = node)) + # Add label mapping for displaying text
  geom_sankey(flow.alpha = 0.6, # Set flow transparency
              node.color = "black", # Add black border to nodes
              node.linewidth = 0.1, # Set border thickness
              width = 0.2, # Adjust node width
              space = 0) +
  scale_fill_manual(values = cols) +
  labs(tag = "B") + # Add tag B
  theme_sankey(base_size = 16) + # Use ggsankey's built-in simple theme
  theme(axis.text = element_text(size = 35), # Axis tick label font size
        legend.text = element_text(size = 25, # Legend text font size
                                   margin = margin(l = 0.1, unit = "cm")), # Legend to text spacing
        legend.key.height = unit(0.2, "cm"), # Bar legend height
        legend.key.width = unit(0.2, "cm"), # Bar legend width
        plot.margin = margin(0, 0.5, 0, 0, "cm"), # Adjust margins
        legend.position = c(1, 0.5), # Legend on the right
        legend.margin = margin(0, 0, 0, 0, "cm"), # Legend margins
        plot.tag.position = c(0.03, 0.95), # Tag position
        plot.tag = element_text(size = 70, face = "bold") # Set tag text size
  ) +
  guides(fill = guide_legend(title = NULL)) + # Hide legend title
  labs(x = NULL, title = "") + # Remove extra axis labels
  coord_fixed(ratio = 5/4600) # Set plot aspect ratio

# Combine Sankey diagrams for both groups
LH_UH_sankey_plot <- LH_sankey_plot + UH_sankey_plot

# Export combined Sankey diagrams
ggsave(plot = LH_UH_sankey_plot,
       filename = "LH_UH_sankey_plot.png",
       width = 17,
       height = 5.5,
       units = "cm",
       bg = "white",
       dpi = 600)

###-------------------Sensitivity analysis-------------------
##------------------(1) Sensitivity analysis 1: Complete case analysis------------------
# Delete rows with missing values
sensitivity_data1 <- na.omit(data_analyze2)

###------------------Data splitting------------------
## Trajectory group split
# Low decline and High-stable groups
sensitivity_data1_LH <- sensitivity_data1 %>%
  filter(eGFR_group %in% c(1, 3)) %>%
  select(-eGFR_group) # Delete eGFR_group column

# Unstable and High-stable groups
sensitivity_data1_UH <- sensitivity_data1 %>%
  filter(eGFR_group %in% c(2, 3)) %>%
  select(-eGFR_group) # Delete eGFR_group column

## Split training and test sets for Low decline and High-stable groups
# Set random seed for reproducibility
set.seed(1217)

# createDataPartition() automatically takes equal proportions of data from each level of y, p indicates training/test set proportion, list = F returns a vector, times = 1 means perform split only once
sensitivity_data1_trainindex_LH <- createDataPartition(sensitivity_data1_LH$CMDs, p = 0.7, list = F, times = 1)

# Extract training set
sensitivity_data1_LH_train <- sensitivity_data1_LH[sensitivity_data1_trainindex_LH, ]

# Extract test set
sensitivity_data1_LH_test <- sensitivity_data1_LH[-sensitivity_data1_trainindex_LH, ]

## Split training and test sets for Unstable and High-stable groups
# Set random seed for reproducibility
set.seed(1217)

# createDataPartition() automatically takes equal proportions of data from each level of y, p indicates training/test set proportion, list = F returns a vector, times = 1 means perform split only once
sensitivity_data1_trainindex_UH <- createDataPartition(sensitivity_data1_UH$CMDs, p = 0.7, list = F, times = 1)

# Extract training set
sensitivity_data1_UH_train <- sensitivity_data1_UH[sensitivity_data1_trainindex_UH, ]

# Extract test set
sensitivity_data1_UH_test <- sensitivity_data1_UH[-sensitivity_data1_trainindex_UH, ]

###-------------------Variable standardization-------------------
## Standardize variables for Low decline and High-stable groups
# Use preProcess() to standardize (center and scale) the training set, making the mean 0 and SD 1 for each column
sensitivity_data1_std_para_LH <- preProcess(sensitivity_data1_LH_train, method = c("center", "scale"))

# Standardize training set
sensitivity_data1_LH_train_std <- predict(sensitivity_data1_std_para_LH, newdata = sensitivity_data1_LH_train)

# Standardize test set using the standardization parameters from the training set
sensitivity_data1_LH_test_std <- predict(sensitivity_data1_std_para_LH, newdata = sensitivity_data1_LH_test)

## Standardize variables for Unstable and High-stable groups
# Use preProcess() to standardize (center and scale) the training set, making the mean 0 and SD 1 for each column
sensitivity_data1_std_para_UH <- preProcess(sensitivity_data1_UH_train, method = c("center", "scale"))

# Standardize training set
sensitivity_data1_UH_train_std <- predict(sensitivity_data1_std_para_UH, newdata = sensitivity_data1_UH_train)

# Standardize test set using the standardization parameters from the training set
sensitivity_data1_UH_test_std <- predict(sensitivity_data1_std_para_UH, newdata = sensitivity_data1_UH_test)

## XGBoost, SVM cannot handle factor variables, convert dataset to numeric, except CMDs
sensitivity_data1_LH_train_std_num <- to_num_except_CMDs(sensitivity_data1_LH_train_std)
sensitivity_data1_LH_test_std_num <- to_num_except_CMDs(sensitivity_data1_LH_test_std)
sensitivity_data1_UH_train_std_num <- to_num_except_CMDs(sensitivity_data1_UH_train_std)
sensitivity_data1_UH_test_std_num <- to_num_except_CMDs(sensitivity_data1_UH_test_std)

# Extract selected features and outcome
sensitivity_data1_LH5_train <- sensitivity_data1_LH_train_std_num[, c(LH_round_outputs[[7]][["features"]], "CMDs")]
sensitivity_data1_LH5_test <- sensitivity_data1_LH_test_std_num[, c(LH_round_outputs[[7]][["features"]], "CMDs")]
sensitivity_data1_UH5_train <- sensitivity_data1_UH_train_std_num[, c(UH_round_outputs[[7]][["features"]], "CMDs")]
sensitivity_data1_UH5_test <- sensitivity_data1_UH_test_std_num[, c(UH_round_outputs[[7]][["features"]], "CMDs")]

##------------------Model building------------------
# Create tasks
sensitivity1_LH_train_task <- as_task_classif(sensitivity_data1_LH5_train, target = "CMDs")
sensitivity1_LH_test_task <- as_task_classif(sensitivity_data1_LH5_test, target = "CMDs")
sensitivity1_UH_train_task <- as_task_classif(sensitivity_data1_UH5_train, target = "CMDs")
sensitivity1_UH_test_task <- as_task_classif(sensitivity_data1_UH5_test, target = "CMDs")

## Train and predict
# Low decline and High-stable groups: XGBoost
sensitivity1_LH_xgb_learner <- lrn("classif.xgboost", predict_type = "prob", verbose = 0)
sensitivity1_LH_xgb_learner$param_set$values <- list(
  nrounds = to_tune(500, 1000), # Number of trees: controls number of boosting iterations, determines learning extent.
  max_depth = to_tune(1, 10), # Maximum tree depth: controls tree complexity to prevent overfitting.
  eta = to_tune(0.001, 0.05), # Learning rate: controls learning speed, affects convergence and stability.
  min_child_weight = to_tune(1, 10), # Minimum sum of instance weights in a child node: restricts splitting to prevent overfitting.
  subsample = to_tune(0.1, 0.5) # Subsample ratio: provides randomness, reduces overfitting risk, increases diversity.
)

set.seed(123)
sensitivity1_LH_xgb <- tune(tuner = tnr("grid_search", resolution = 5),
                            task = sensitivity1_LH_train_task,
                            learner = sensitivity1_LH_xgb_learner,
                            resampling = rsmp("cv", folds = 5),
                            measure = msr("classif.auc")
)
sensitivity1_LH_xgb_learner$param_set$values <- sensitivity1_LH_xgb$result_learner_param_vals
sensitivity1_LH_xgb_learner$train(sensitivity1_LH_train_task)
sensitivity1_LH_train_xgb_pred <- sensitivity1_LH_xgb_learner$predict(sensitivity1_LH_train_task)
sensitivity1_LH_test_xgb_pred <- sensitivity1_LH_xgb_learner$predict(sensitivity1_LH_test_task)

# Unstable and High-stable groups: XGBoost
sensitivity1_UH_xgb_learner <- lrn("classif.xgboost", predict_type = "prob", verbose = 0)
sensitivity1_UH_xgb_learner$param_set$values <- list(
  nrounds = to_tune(100, 500), # Number of trees: controls number of boosting iterations, determines learning extent.
  max_depth = to_tune(1, 10), # Maximum tree depth: controls tree complexity to prevent overfitting.
  eta = to_tune(0.001, 0.05), # Learning rate: controls learning speed, affects convergence and stability.
  min_child_weight = to_tune(1, 10), # Minimum sum of instance weights in a child node: restricts splitting to prevent overfitting.
  subsample = to_tune(0.5, 1) # Subsample ratio: provides randomness, reduces overfitting risk, increases diversity.
)

set.seed(123)
sensitivity1_UH_xgb <- tune(tuner = tnr("grid_search", resolution = 5),
                            task = sensitivity1_UH_train_task,
                            learner = sensitivity1_UH_xgb_learner,
                            resampling = rsmp("cv", folds = 5),
                            measure = msr("classif.auc")
)
sensitivity1_UH_xgb_learner$param_set$values <- sensitivity1_UH_xgb$result_learner_param_vals
sensitivity1_UH_xgb_learner$train(sensitivity1_UH_train_task)
sensitivity1_UH_train_xgb_pred <- sensitivity1_UH_xgb_learner$predict(sensitivity1_UH_train_task)
sensitivity1_UH_test_xgb_pred <- sensitivity1_UH_xgb_learner$predict(sensitivity1_UH_test_task)

###------------------Performance evaluation------------------
# Training set model performance evaluation function
train_performance <- function(train_pred, train_data, Dataset_name) {
  # Get predicted probabilities from training set model
  pred_prob_train <- train_pred$prob[, "Yes"]
  # Get actual class labels for training set
  true_class_train <- ifelse(train_data$CMDs == "Yes", 1, 0)
  roc_train <- roc(true_class_train, pred_prob_train)
  auc_train <- roc_train$auc
  ci_train <- ci(roc_train)
  # Combine AUC and 95%CI, keep three decimal places
  auc_with_ci_train <- sprintf("%.3f\n(%.3f-%.3f)", auc_train, ci_train[1], ci_train[3])
  
  ## Calculate best threshold for training set
  # Define threshold range
  threshold <- seq(0, 1, by = 0.001)
  # Calculate performance metrics for different thresholds
  metrics_list_train <- sapply(threshold, function(t) {
    calculate_metrics(pred_prob_train, true_class_train, t)
  }, simplify = F)
  distances <- sapply(metrics_list_train, function(metrics) {
    sqrt((1-metrics$Sensitivity)^2 + (1-metrics$Specificity)^2)
  })
  # Find best threshold for training set
  best_threshold_train <- threshold[which.min(distances)]
  # Calculate performance metrics for training set using the best threshold
  best_metrics_train <- calculate_metrics(pred_prob_train, true_class_train, best_threshold_train)
  # Calculate Brier score
  brier_score_train <- mean((pred_prob_train - true_class_train)^2)
  # Bootstrap CI
  set.seed(123)
  ci_train_metrics <- bootstrap_ci(pred_prob_train, true_class_train, best_threshold_train, B = 1000)
  Sensitivity_CI <- format_ci(best_metrics_train$Sensitivity, ci_train_metrics[, "Sensitivity"])
  Specificity_CI <- format_ci(best_metrics_train$Specificity, ci_train_metrics[, "Specificity"])
  PPV_CI <- format_ci(best_metrics_train$PPV, ci_train_metrics[, "PPV"])
  NPV_CI <- format_ci(best_metrics_train$NPV, ci_train_metrics[, "NPV"])
  CCR_CI <- format_ci(best_metrics_train$CCR, ci_train_metrics[, "CCR"])
  F1_score_CI <- format_ci(best_metrics_train$F1_score, ci_train_metrics[, "F1_score"])
  Brier_score_CI <- format_ci(brier_score_train, ci_train_metrics[, "Brier_score"])
  # Summarize training set model results
  train_metrics_result <- data.frame(
    Dataset_name = Dataset_name,
    AUC_CI = auc_with_ci_train,
    Threshold = round(best_threshold_train, 3),
    Sensitivity_CI = Sensitivity_CI,
    Specificity_CI = Specificity_CI,
    PPV_CI = PPV_CI,
    NPV_CI = NPV_CI,
    CCR_CI = CCR_CI,
    F1_score_CI = F1_score_CI,
    Brier_score_CI = Brier_score_CI
  )
  # Return result
  return(train_metrics_result)
}

# Test set model performance evaluation function
test_performance <- function(test_pred, test_data, train_threshold, Dataset_name) {
  # Get predicted probabilities from test set model
  pred_prob_test <- test_pred$prob[, "Yes"]
  # Get actual class labels for test set
  true_class_test <- ifelse(test_data$CMDs == "Yes", 1, 0)
  roc_test <- roc(true_class_test, pred_prob_test)
  auc_test <- roc_test$auc
  ci_test <- ci(roc_test)
  # Combine AUC and 95%CI, keep three decimal places
  auc_with_ci_test <- sprintf("%.3f\n(%.3f-%.3f)", auc_test, ci_test[1], ci_test[3])
  # Calculate test set performance using training set's best threshold
  best_metrics_test <- calculate_metrics(pred_prob_test, true_class_test, train_threshold)
  # Calculate test set Brier score
  brier_score_test <- mean((pred_prob_test - true_class_test)^2)
  # Bootstrap CI
  set.seed(123)
  ci_test_metrics <- bootstrap_ci(pred_prob_test, true_class_test, train_threshold, B = 1000)
  Sensitivity_CI <- format_ci(best_metrics_test$Sensitivity, ci_test_metrics[, "Sensitivity"])
  Specificity_CI <- format_ci(best_metrics_test$Specificity, ci_test_metrics[, "Specificity"])
  PPV_CI <- format_ci(best_metrics_test$PPV, ci_test_metrics[, "PPV"])
  NPV_CI <- format_ci(best_metrics_test$NPV, ci_test_metrics[, "NPV"])
  CCR_CI <- format_ci(best_metrics_test$CCR, ci_test_metrics[, "CCR"])
  F1_score_CI <- format_ci(best_metrics_test$F1_score, ci_test_metrics[, "F1_score"])
  Brier_score_CI <- format_ci(brier_score_test, ci_test_metrics[, "Brier_score"])
  # Summarize test set model results
  test_metrics_result <- data.frame(
    Dataset_name = Dataset_name,
    AUC_CI = auc_with_ci_test,
    Threshold = round(train_threshold, 3),
    Sensitivity_CI = Sensitivity_CI,
    Specificity_CI = Specificity_CI,
    PPV_CI = PPV_CI,
    NPV_CI = NPV_CI,
    CCR_CI = CCR_CI,
    F1_score_CI = F1_score_CI,
    Brier_score_CI = Brier_score_CI
  )
  # Return result
  return(list(
    roc_test = roc_test,
    test_metrics_result = test_metrics_result
  ))
}

##------------------Low decline and High-stable groups------------------
sensitivity1_LH_train_metrics <- train_performance(sensitivity1_LH_train_xgb_pred,
                                                   sensitivity_data1_LH_train_std_num,
                                                   Dataset_name = "sensitivity1_LH_train")
sensitivity1_LH_test_metrics <- test_performance(sensitivity1_LH_test_xgb_pred,
                                                 sensitivity_data1_LH_test_std_num,
                                                 train_threshold = sensitivity1_LH_train_metrics$Threshold,
                                                 Dataset_name = "sensitivity1_LH_test")

# DeLong test
sensitivity1_LH_test_delong_test <- roc.test(LH_test_roc_auc_xgb$xgb_5, sensitivity1_LH_test_metrics[["roc_test"]], method = "delong")
# Extract Z-score and p-value, rounding Z-score to 3 decimals and formatting p-value
sensitivity1_LH_test_Z_score <- round(sensitivity1_LH_test_delong_test$statistic, 3)
sensitivity1_LH_test_P_value <- pvalue_format(sensitivity1_LH_test_delong_test$p.value)

#------------------Unstable and High-stable groups------------------
sensitivity1_UH_train_metrics <- train_performance(sensitivity1_UH_train_xgb_pred,
                                                   sensitivity_data1_UH_train_std_num,
                                                   Dataset_name = "sensitivity1_UH_train")
sensitivity1_UH_test_metrics <- test_performance(sensitivity1_UH_test_xgb_pred,
                                                 sensitivity_data1_UH_test_std_num,
                                                 train_threshold = sensitivity1_UH_train_metrics$Threshold,
                                                 Dataset_name = "sensitivity1_UH_test")

# DeLong test
sensitivity1_UH_test_delong_test <- roc.test(UH_test_roc_auc_xgb$xgb_5, sensitivity1_UH_test_metrics[["roc_test"]], method = "delong")
# Extract Z-score and p-value, rounding Z-score to 3 decimals and formatting p-value
sensitivity1_UH_test_Z_score <- round(sensitivity1_UH_test_delong_test$statistic, 3)
sensitivity1_UH_test_P_value <- pvalue_format(sensitivity1_UH_test_delong_test$p.value)

# Add DeLong test results to data frame and combine
sensitivity1_test_auc_comparisons <- data.frame(
  Model = "sensitivity1",
  LH_test_Z_score = sensitivity1_LH_test_Z_score,
  LH_test_P_value = sensitivity1_LH_test_P_value,
  UH_test_Z_score = sensitivity1_UH_test_Z_score,
  UH_test_P_value = sensitivity1_UH_test_P_value
)

##------------------(2) Sensitivity analysis 2: Multiple imputation------------------
# Generate default imputation method template
meth <- make.method(data_analyze2)

# Variable type classification function
split_var_names <- function(df, max_cat_unique = 10, treat_int_small_unique_as_cat = TRUE) {
  continuous <- c()
  binary <- c()
  multiclass <- c()
  other <- c()
  for (col in names(df)) {
    x <- df[[col]]
    non_na <- x[!is.na(x)]
    n_unique <- length(unique(non_na))
    # All missing or constant column
    if (n_unique <= 1) {
      other <- c(other, col)
      next
    }
    # Logical treated as binary
    if (is.logical(x)) {
      binary <- c(binary, col)
      next
    }
    # Character / factor: treat based on unique=2 or >2
    if (is.character(x) || is.factor(x)) {
      if (n_unique == 2) binary <- c(binary, col) else multiclass <- c(multiclass, col)
      next
    }
    # Numeric (integer/double)
    if (is.numeric(x)) {
      # Check for binary first
      if (n_unique == 2) {
        binary <- c(binary, col)
        next
      }
      # Integer-like with few unique values can be treated as multiclass
      if (treat_int_small_unique_as_cat) {
        is_int_like <- all(abs(non_na - round(non_na)) < 1e-8)
        if (is_int_like && n_unique <= max_cat_unique) {
          multiclass <- c(multiclass, col)
          next
        }
      }
      # Otherwise continuous
      continuous <- c(continuous, col)
      next
    }
    # Other types (Date, POSIXct, list, etc.)
    other <- c(other, col)
  }
  return(list(
    continuous = continuous,
    binary = binary,
    multiclass = multiclass,
    other = other
  ))
}

# Perform classification
var_types <- split_var_names(data_analyze2, max_cat_unique = 10)

# Specify imputation method for continuous variables: pmm
meth[var_types$continuous] <- "pmm"

# Specify imputation method for binary variables: logreg
meth[var_types$binary] <- "logreg"

# Specify imputation method for multiclass variables: polyreg
meth[var_types$multiclass] <- "polyreg"

# Perform multiple imputation
set.seed(126)
mi <- mice(data_analyze2, m = 5, method = meth, maxit = 20)

# Extract the first imputed dataset
sensitivity_data2 <- complete(mi, action = 1)

###------------------Data splitting------------------
## Trajectory group split
# Low decline and High-stable groups
sensitivity_data2_LH <- sensitivity_data2 %>%
  filter(eGFR_group %in% c(1, 3)) %>%
  select(-eGFR_group) # Delete eGFR_group column

# Unstable and High-stable groups
sensitivity_data2_UH <- sensitivity_data2 %>%
  filter(eGFR_group %in% c(2, 3)) %>%
  select(-eGFR_group) # Delete eGFR_group column

## Split training and test sets for Low decline and High-stable groups
# Set random seed for reproducibility
set.seed(1217)

# createDataPartition() automatically takes equal proportions of data from each level of y, p indicates training/test set proportion, list = F returns a vector, times = 1 means perform split only once
sensitivity_data2_trainindex_LH <- createDataPartition(sensitivity_data2_LH$CMDs, p = 0.7, list = F, times = 1)

# Extract training set
sensitivity_data2_LH_train <- sensitivity_data2_LH[sensitivity_data2_trainindex_LH, ]

# Extract test set
sensitivity_data2_LH_test <- sensitivity_data2_LH[-sensitivity_data2_trainindex_LH, ]

## Split training and test sets for Unstable and High-stable groups
# Set random seed for reproducibility
set.seed(1217)

# createDataPartition() automatically takes equal proportions of data from each level of y, p indicates training/test set proportion, list = F returns a vector, times = 1 means perform split only once
sensitivity_data2_trainindex_UH <- createDataPartition(sensitivity_data2_UH$CMDs, p = 0.7, list = F, times = 1)

# Extract training set
sensitivity_data2_UH_train <- sensitivity_data2_UH[sensitivity_data2_trainindex_UH, ]

# Extract test set
sensitivity_data2_UH_test <- sensitivity_data2_UH[-sensitivity_data2_trainindex_UH, ]

###-------------------Variable standardization-------------------
## Standardize variables for Low decline and High-stable groups
# Use preProcess() to standardize (center and scale) the training set, making the mean 0 and SD 1 for each column
sensitivity_data2_std_para_LH <- preProcess(sensitivity_data2_LH_train, method = c("center", "scale"))

# Standardize training set
sensitivity_data2_LH_train_std <- predict(sensitivity_data2_std_para_LH, newdata = sensitivity_data2_LH_train)

# Standardize test set using the standardization parameters from the training set
sensitivity_data2_LH_test_std <- predict(sensitivity_data2_std_para_LH, newdata = sensitivity_data2_LH_test)

## Standardize variables for Unstable and High-stable groups
# Use preProcess() to standardize (center and scale) the training set, making the mean 0 and SD 1 for each column
sensitivity_data2_std_para_UH <- preProcess(sensitivity_data2_UH_train, method = c("center", "scale"))

# Standardize training set
sensitivity_data2_UH_train_std <- predict(sensitivity_data2_std_para_UH, newdata = sensitivity_data2_UH_train)

# Standardize test set using the standardization parameters from the training set
sensitivity_data2_UH_test_std <- predict(sensitivity_data2_std_para_UH, newdata = sensitivity_data2_UH_test)

## XGBoost, SVM cannot handle factor variables, convert dataset to numeric, except CMDs
sensitivity_data2_LH_train_std_num <- to_num_except_CMDs(sensitivity_data2_LH_train_std)
sensitivity_data2_LH_test_std_num <- to_num_except_CMDs(sensitivity_data2_LH_test_std)
sensitivity_data2_UH_train_std_num <- to_num_except_CMDs(sensitivity_data2_UH_train_std)
sensitivity_data2_UH_test_std_num <- to_num_except_CMDs(sensitivity_data2_UH_test_std)

# Extract selected features and outcome
sensitivity_data2_LH5_train <- sensitivity_data2_LH_train_std_num[, c(LH_round_outputs[[7]][["features"]], "CMDs")]
sensitivity_data2_LH5_test <- sensitivity_data2_LH_test_std_num[, c(LH_round_outputs[[7]][["features"]], "CMDs")]
sensitivity_data2_UH5_train <- sensitivity_data2_UH_train_std_num[, c(UH_round_outputs[[7]][["features"]], "CMDs")]
sensitivity_data2_UH5_test <- sensitivity_data2_UH_test_std_num[, c(UH_round_outputs[[7]][["features"]], "CMDs")]

##------------------Model building------------------
# Create tasks
sensitivity2_LH_train_task <- as_task_classif(sensitivity_data2_LH5_train, target = "CMDs")
sensitivity2_LH_test_task <- as_task_classif(sensitivity_data2_LH5_test, target = "CMDs")
sensitivity2_UH_train_task <- as_task_classif(sensitivity_data2_UH5_train, target = "CMDs")
sensitivity2_UH_test_task <- as_task_classif(sensitivity_data2_UH5_test, target = "CMDs")

## Train and predict
# Low decline and High-stable groups: XGBoost
sensitivity2_LH_xgb_learner <- lrn("classif.xgboost", predict_type = "prob", verbose = 0)
sensitivity2_LH_xgb_learner$param_set$values <- list(
  nrounds = to_tune(500, 1000), # Number of trees: controls number of boosting iterations, determines learning extent.
  max_depth = to_tune(1, 10), # Maximum tree depth: controls tree complexity to prevent overfitting.
  eta = to_tune(0.001, 0.05), # Learning rate: controls learning speed, affects convergence and stability.
  min_child_weight = to_tune(1, 10), # Minimum sum of instance weights in a child node: restricts splitting to prevent overfitting.
  subsample = to_tune(0.1, 0.5) # Subsample ratio: provides randomness, reduces overfitting risk, increases diversity.
)

set.seed(123)
sensitivity2_LH_xgb <- tune(tuner = tnr("grid_search", resolution = 5),
                            task = sensitivity2_LH_train_task,
                            learner = sensitivity2_LH_xgb_learner,
                            resampling = rsmp("cv", folds = 5),
                            measure = msr("classif.auc")
)
sensitivity2_LH_xgb_learner$param_set$values <- sensitivity2_LH_xgb$result_learner_param_vals
sensitivity2_LH_xgb_learner$train(sensitivity2_LH_train_task)
sensitivity2_LH_train_xgb_pred <- sensitivity2_LH_xgb_learner$predict(sensitivity2_LH_train_task)
sensitivity2_LH_test_xgb_pred <- sensitivity2_LH_xgb_learner$predict(sensitivity2_LH_test_task)

# Unstable and High-stable groups: XGBoost
sensitivity2_UH_xgb_learner <- lrn("classif.xgboost", predict_type = "prob", verbose = 0)
sensitivity2_UH_xgb_learner$param_set$values <- list(
  nrounds = to_tune(100, 500), # Number of trees: controls number of boosting iterations, determines learning extent.
  max_depth = to_tune(1, 10), # Maximum tree depth: controls tree complexity to prevent overfitting.
  eta = to_tune(0.001, 0.05), # Learning rate: controls learning speed, affects convergence and stability.
  min_child_weight = to_tune(1, 10), # Minimum sum of instance weights in a child node: restricts splitting to prevent overfitting.
  subsample = to_tune(0.5, 1) # Subsample ratio: provides randomness, reduces overfitting risk, increases diversity.
)

set.seed(123)
sensitivity2_UH_xgb <- tune(tuner = tnr("grid_search", resolution = 5),
                            task = sensitivity2_UH_train_task,
                            learner = sensitivity2_UH_xgb_learner,
                            resampling = rsmp("cv", folds = 5),
                            measure = msr("classif.auc")
)
sensitivity2_UH_xgb_learner$param_set$values <- sensitivity2_UH_xgb$result_learner_param_vals
sensitivity2_UH_xgb_learner$train(sensitivity2_UH_train_task)
sensitivity2_UH_train_xgb_pred <- sensitivity2_UH_xgb_learner$predict(sensitivity2_UH_train_task)
sensitivity2_UH_test_xgb_pred <- sensitivity2_UH_xgb_learner$predict(sensitivity2_UH_test_task)

###------------------Performance evaluation------------------
##------------------Low decline and High-stable groups------------------
sensitivity2_LH_train_metrics <- train_performance(sensitivity2_LH_train_xgb_pred,
                                                   sensitivity_data2_LH_train_std_num,
                                                   Dataset_name = "sensitivity2_LH_train")
sensitivity2_LH_test_metrics <- test_performance(sensitivity2_LH_test_xgb_pred,
                                                 sensitivity_data2_LH_test_std_num,
                                                 train_threshold = sensitivity2_LH_train_metrics$Threshold,
                                                 Dataset_name = "sensitivity2_LH_test")

# DeLong test
sensitivity2_LH_test_delong_test <- roc.test(LH_test_roc_auc_xgb$xgb_5, sensitivity2_LH_test_metrics[["roc_test"]], method = "delong")
# Extract Z-score and p-value, rounding Z-score to 3 decimals and formatting p-value
sensitivity2_LH_test_Z_score <- round(sensitivity2_LH_test_delong_test$statistic, 3)
sensitivity2_LH_test_P_value <- pvalue_format(sensitivity2_LH_test_delong_test$p.value)

#------------------Unstable and High-stable groups------------------
sensitivity2_UH_train_metrics <- train_performance(sensitivity2_UH_train_xgb_pred,
                                                   sensitivity_data2_UH_train_std_num,
                                                   Dataset_name = "sensitivity2_UH_train")
sensitivity2_UH_test_metrics <- test_performance(sensitivity2_UH_test_xgb_pred,
                                                 sensitivity_data2_UH_test_std_num,
                                                 train_threshold = sensitivity2_UH_train_metrics$Threshold,
                                                 Dataset_name = "sensitivity2_UH_test")

# DeLong test
sensitivity2_UH_test_delong_test <- roc.test(UH_test_roc_auc_xgb$xgb_5, sensitivity2_UH_test_metrics[["roc_test"]], method = "delong")
# Extract Z-score and p-value, rounding Z-score to 3 decimals and formatting p-value
sensitivity2_UH_test_Z_score <- round(sensitivity2_UH_test_delong_test$statistic, 3)
sensitivity2_UH_test_P_value <- pvalue_format(sensitivity2_UH_test_delong_test$p.value)

# Add DeLong test results to data frame and combine
sensitivity2_test_auc_comparisons <- data.frame(
  Model = "sensitivity2",
  LH_test_Z_score = sensitivity2_LH_test_Z_score,
  LH_test_P_value = sensitivity2_LH_test_P_value,
  UH_test_Z_score = sensitivity2_UH_test_Z_score,
  UH_test_P_value = sensitivity2_UH_test_P_value
)

##------------------(3) Sensitivity analysis: eGFR measured 4 times------------------
##------------------Inclusion/Exclusion: Total N=117537------------------
# Similar as before, only keep: total eGFR count from baseline ≥4

ids_step2_eGFR4 <- egfr_from_baseline$accountid[egfr_from_baseline$n_egfr_from_baseline >= 4]

# Filter rows from baseline1 where accountid belongs to ids_step2_eGFR4, save as baseline2_eGFR4.
baseline2_eGFR4 <- baseline1[baseline1$accountid %in% ids_step2_eGFR4, ]
length(unique(baseline2_eGFR4$accountid))

## (3) Exclude those with baseline history of CMDs (diabetes, stroke, cardiovascular disease) (n=11170)
baseline3_eGFR4 <- baseline2_eGFR4[
  !is.na(baseline2_eGFR4$DM) & baseline2_eGFR4$DM == 1 & # No diabetes
    !is.na(baseline2_eGFR4$Cerebrovascular_diseases) & baseline2_eGFR4$Cerebrovascular_diseases == 1 & # No stroke
    !is.na(baseline2_eGFR4$Cardiovascular_diseases) & baseline2_eGFR4$Cardiovascular_diseases == 1 # No cardiovascular disease
  , ]
length(unique(baseline3_eGFR4$accountid))

## (4) Exclude kidney disease (NS=2) or eGFR <60 (n=10969)
baseline4_eGFR4 <- baseline3_eGFR4[
  !is.na(baseline3_eGFR4$Kidney_diseases) & baseline3_eGFR4$Kidney_diseases == 1 & # No kidney disease
    !is.na(baseline3_eGFR4$eGFR) & baseline3_eGFR4$eGFR >= 60 # eGFR ≥60
  , ]
length(unique(baseline4_eGFR4$accountid))

# Enrolled accountids
ids_step4_eGFR4 <- baseline4_eGFR4$accountid

## Construct longitudinal follow-up data: all physical examination records from baseline
# Filter mj2 for final accountids and exdate >= baseline_date
mj3_eGFR4 <- mj2[
  mj2$accountid %in% ids_step4_eGFR4 &
    mj2$exdate >= mj2$baseline_date,
]

# Re-extract "year" directly from the date, do not use previous year variable
mj3_eGFR4$year <- as.integer(format(mj3_eGFR4$exdate, "%Y"))
mj3_eGFR4$baseline_year <- as.integer(format(mj3_eGFR4$baseline_date, "%Y"))

# Continuous time from baseline (years)
mj3_eGFR4$time <- as.numeric(
  difftime(mj3_eGFR4$exdate, mj3_eGFR4$baseline_date, units = "days")
) / 365.25

## (4) Exclude accountids with NA in the outcome CMDs during follow-up (n=10967)
# CMDs_event: 1=not occurred, 2=occurred, NA=cannot be determined
mj3_eGFR4$CMDs_event <- with(mj3_eGFR4, ifelse(
  DM == 2 | Cardiovascular_diseases == 2 | Cerebrovascular_diseases == 2, 2L, # Any disease present
  ifelse(
    DM == 1 & Cardiovascular_diseases == 1 & Cerebrovascular_diseases == 1, 1L, # All three absent
    NA_integer_ # Other cases (has NA)
  )
))

# Exclude accountids where CMDs_event ever had NA
valid_ids_eGFR4 <- unique(mj3_eGFR4$accountid[is.na(mj3_eGFR4$CMDs_event)])
final17_24_eGFR4 <- mj3_eGFR4[!mj3_eGFR4$accountid %in% valid_ids_eGFR4, ]
length(unique(final17_24_eGFR4$accountid))

###------------------Trajectory identification: LCGM------------------
# Convert accountid to numeric
final17_24_eGFR4$accountid <- as.numeric(final17_24_eGFR4$accountid)

## (1) Model fitting
# Polynomial degree (quadratic): First build a 1-class base model
set.seed(2025)
LCGM1_eGFR4 <- hlme(
  fixed = eGFR ~ 1 + time + I(time^2), # Fixed effect part of the model (main formula), set to quadratic (linear: eGFR ~ 1 + time, cubic: eGFR ~ 1 + time + I(time^2) + I(time^3))
  ng = 1, # Number of latent classes
  subject = "accountid", # Subject id for nested structure
  data = final17_24_eGFR4 # Dataset name
)

## 2-5 class models
# Store each LCGM
LCGMs_eGFR4 <- list()

# LCGM loop
for (k in 2:5) {
  LCGMs_eGFR4[[paste0("LCGM", k)]] <- hlme(
    fixed = eGFR ~ 1 + time + I(time^2),
    mixture = ~ 1 + time + I(time^2), # Mixture effect: required when number of classes > 1 (parameters may differ across latent classes)
    ng = k,
    subject = "accountid",
    data = final17_24_eGFR4,
    B = LCGM1_eGFR4 # Initial model
  )
}

### (2) Model fit evaluation: AIC, BIC, entropy, class proportions, Avepp, OCC
## ① BIC (lower absolute value), AIC (lower absolute value), entropy (close to 1), class proportion (>5%)

LCGM_result_eGFR4 <- summarytable(LCGMs_eGFR4$LCGM2, LCGMs_eGFR4$LCGM3, LCGMs_eGFR4$LCGM4, LCGMs_eGFR4$LCGM5,
                                  which = c("G", "AIC", "BIC", "SABIC", "entropy", "%class"))

# Convert LCGM_result to data frame, get the minimum class proportion into a new column
LCGM_result_eGFR4 <- LCGM_result_eGFR4 %>%
  as.data.frame() %>%
  mutate(
    `Smallest Class (%)` = pmin(`%class1`, `%class2`, `%class3`, `%class4`, `%class5`,
                                na.rm = TRUE)
  ) %>%
  select(-starts_with("%class")) %>%
  rename(Model = G, Entropy = entropy)

## ② Average posterior probability Avepp > 0.7; Odds of correct classification (OCC) > 5

# First define the list of models
LCGM_models_eGFR4 <- list(
  LCGM2 = LCGMs_eGFR4$LCGM2,
  LCGM3 = LCGMs_eGFR4$LCGM3,
  LCGM4 = LCGMs_eGFR4$LCGM4,
  LCGM5 = LCGMs_eGFR4$LCGM5
)

# Run LCTMtoolkit for each model
Avepp_OCC_results_list_eGFR4 <- lapply(names(LCGM_models_eGFR4), function(name) {
  cat("Model being processed:", name, "\n")
  mod <- LCGM_models_eGFR4[[name]]
  # Safely call, won't interrupt the loop if error occurs
  tryCatch({
    res <- LCTMtoolkit(mod)
    list(name = name, result = res)
  }, error = function(e) {
    cat("??? Model", name, "Error:", e$message, "\n")
    NULL
  })
})

# Extract Avepp results
appa_results_eGFR4 <- bind_rows(Avepp_OCC_results_list_eGFR4[[1]][["result"]][["appa"]],
                                Avepp_OCC_results_list_eGFR4[[2]][["result"]][["appa"]],
                                Avepp_OCC_results_list_eGFR4[[3]][["result"]][["appa"]],
                                Avepp_OCC_results_list_eGFR4[[4]][["result"]][["appa"]])

# Get the minimum Avepp into a new column
appa_results_eGFR4 <- appa_results_eGFR4 %>%
  mutate(
    `Smallest avepp` = pmin(Class_1, Class_2, Class_3, Class_4, Class_5, na.rm = TRUE)
  )

# Extract OCC results
occ_results_eGFR4 <- bind_rows(Avepp_OCC_results_list_eGFR4[[1]][["result"]][["occ"]],
                               Avepp_OCC_results_list_eGFR4[[2]][["result"]][["occ"]],
                               Avepp_OCC_results_list_eGFR4[[3]][["result"]][["occ"]],
                               Avepp_OCC_results_list_eGFR4[[4]][["result"]][["occ"]])

# Get the minimum OCC into a new column
occ_results_eGFR4 <- occ_results_eGFR4 %>%
  mutate(
    `Smallest OCC` = pmin(Class_1, Class_2, Class_3, Class_4, Class_5, na.rm = TRUE)
  )

# Combine all results
LCGM_results_eGFR4 <- cbind(
  LCGM_result,
  `Smallest avepp` = appa_results_eGFR4[ , "Smallest avepp"],
  `Smallest OCC` = occ_results_eGFR4[ , "Smallest OCC"]
)

# Keep two decimal places
LCGM_results_eGFR4[ , -1] <- round(LCGM_results_eGFR4[ , -1], 2)

### (3) LCGM trajectory plot
# Construct time grid
newdata_eGFR4 <- data.frame(time = seq(
  from = min(final17_24_eGFR4$time, na.rm = TRUE),
  to = max(final17_24_eGFR4$time, na.rm = TRUE),
  length.out = 100
))

# Extract model predictions
plotpred_eGFR4 <- predictY(LCGMs_eGFR4$LCGM3, newdata_eGFR4, var.time = "time", draws = TRUE, marg = TRUE)

# Create data frame
pred_dfci_eGFR4 <- data.frame(
  time = plotpred_eGFR4[["times"]]$time,
  class1 = plotpred_eGFR4[["pred"]][, "Ypred_class1"],
  class2 = plotpred_eGFR4[["pred"]][, "Ypred_class3"],
  class3 = plotpred_eGFR4[["pred"]][, "Ypred_class2"],
  # Add confidence interval data
  class1_lower = plotpred_eGFR4[["pred"]][, "lower.Ypred_class1"],
  class1_upper = plotpred_eGFR4[["pred"]][, "upper.Ypred_class1"],
  class2_lower = plotpred_eGFR4[["pred"]][, "lower.Ypred_class3"],
  class2_upper = plotpred_eGFR4[["pred"]][, "upper.Ypred_class3"],
  class3_lower = plotpred_eGFR4[["pred"]][, "lower.Ypred_class2"],
  class3_upper = plotpred_eGFR4[["pred"]][, "upper.Ypred_class2"]
)

# Directly plot 3 curves and confidence intervals
LCGM_plotci_eGFR4 <- ggplot(pred_dfci_eGFR4, aes(x = time)) +
  # Draw confidence intervals (ribbon)
  geom_ribbon(aes(ymin = class1_lower, ymax = class1_upper, fill = "Class 1"), alpha = 0.2) +
  geom_ribbon(aes(ymin = class2_lower, ymax = class2_upper, fill = "Class 2"), alpha = 0.2) +
  geom_ribbon(aes(ymin = class3_lower, ymax = class3_upper, fill = "Class 3"), alpha = 0.2) +
  # Draw predicted lines
  geom_line(aes(y = class1, color = "Class 1"), size = 0.5) +
  geom_line(aes(y = class2, color = "Class 2"), size = 0.5) +
  geom_line(aes(y = class3, color = "Class 3"), size = 0.5) +
  labs(
    x = "Years since baseline",
    y = expression(eGFR~"(ml/min/1.73"~m^2*")"),
    color = "Trajectory",
    fill = "Trajectory"
  ) +
  scale_color_manual(values = c(
    "Class 1" = "#9E9AC8",
    "Class 2" = "#FDAE6B",
    "Class 3" = "#6BAED6"
  ),
  labels = c(
    "Class 1" = "Low decline (9.15%)",
    "Class 2" = "Unstable (34.92%)",
    "Class 3" = "High-stable (55.93%)"
  )) +
  scale_fill_manual(values = c(
    "Class 1" = "#9E9AC8",
    "Class 2" = "#FDAE6B",
    "Class 3" = "#6BAED6"),
    labels = c(
      "Class 1" = "Low decline (9.15%)",
      "Class 2" = "Unstable (34.92%)",
      "Class 3" = "High-stable (55.93%)"
    )) +
  scale_y_continuous(limits = c(70, 110)) + # y-axis range
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = NA), # Overall image background, border color none
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.2), # Add border
    panel.grid.major = element_line(linewidth = 0.2), # Adjust major grid line thickness
    axis.ticks.x = element_line(color = "black"), # x-axis tick marks
    axis.ticks.y = element_line(color = "black"), # y-axis tick marks
    axis.ticks.length = unit(0.05, "cm"), # Tick mark length and placement outside
    axis.text = element_text(size = 20), # Axis tick label font size
    axis.title = element_text(size = 30), # Axis title font size
    axis.ticks = element_line(size = 0.2), # Axis tick line thickness
    legend.margin = margin(0, 0, -0.2, 0, "cm"), # Adjust margins
    legend.key.height = unit(0.2, "cm"), # Bar legend height
    legend.key.width = unit(0.4, "cm"), # Bar legend width
    legend.position = "top",
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20)
  ) +
  guides(color = guide_legend(nrow = 2))

# Export trajectory plot
ggsave(
  filename = "LCGM_trajectory_plot_eGFR4.png",
  plot = LCGM_plotci_eGFR4,
  width = 7,
  height = 7,
  units = "cm",
  dpi = 600
)

### (4) Trajectory grouping
LCGM_group_eGFR4 <- LCGMs_eGFR4$LCGM3$pprob[, c(1, 2)] # Extract accountid and class columns
names(LCGM_group_eGFR4)[names(LCGM_group_eGFR4) == "class"] <- "eGFR_group" # Rename

### (5) Merge with other data
data_traj_eGFR4 <- merge(final17_24_eGFR4, LCGM_group_eGFR4, by = "accountid")

# For each accountid, if there is a 2 in CMDs_event, code new variable CMDs as 2, otherwise 1
data_traj2_eGFR4 <- data_traj_eGFR4 %>%
  group_by(accountid) %>%
  mutate(
    CMDs = if_else(any(CMDs_event == 2, na.rm = TRUE), 2L, 1L),
    CMDs = factor(CMDs, levels = c(1, 2), labels = c("No", "Yes")) # Convert to labelled factor
  ) %>%
  arrange(exdate, .by_group = TRUE) %>% # Sort by date (early -> late) within each accountid
  dplyr::slice(1) %>% # Take the earliest row
  ungroup()

###------------------Other predictor coding------------------
data_traj2_eGFR4 <- data_traj2_eGFR4 %>%
  mutate(
    across(c(5:48, 50, 53:56, 60:63, 89:117, 128:130), ~ as.numeric(.x)), # Convert these columns from character to numeric
    # Education level Education: NA=(0), 1=Illiterate (scholar: 1), 2=Primary school (scholar: 2), 3=Middle school and above (scholar: 3-7)
    Education = case_when(
      Education == 0 ~ NA_real_,
      Education == 1 ~ 1,
      Education == 2 ~ 2,
      Education >= 3 ~ 3),
    Education = factor(
      Education,
      levels = c(1, 2, 3),
      labels = c("Illiterate", "Primary school", "Middle school and above")),
    # Occupation: NA=(0), 1=Non-agricultural work (occupation: 1-19,21-26), 2=Agricultural work (occupation: 20)
    Occupation = case_when(
      Occupation == 0 ~ NA_real_,
      Occupation == 20 ~ 2,
      Occupation %in% c(1:19, 21:26) ~ 1),
    Occupation = factor(
      Occupation,
      levels = c(1, 2),
      labels = c("Non-agricultural work", "Agricultural work")),
    # Marital status: NA=(0), 1=Other (marriage: 1,3,4), 2=Married (marriage: 2)
    Marital_status = case_when(
      Marital_status == 0 ~ NA_real_,
      Marital_status == 2 ~ 2,
      Marital_status %in% c(1, 3, 4) ~ 1),
    Marital_status = factor(
      Marital_status,
      levels = c(1, 2),
      labels = c("Other", "Married")),
    # Personal annual income: NA=(0), 1=Less than 100,000 yuan (yearincome:1-2), 2=100,000-200,000 yuan (yearincome:3-4), 3=More than 200,000 yuan (yearincome:5-6)
    Personal_annual_income = case_when(
      Personal_annual_income == 0 ~ NA_real_,
      Personal_annual_income %in% c(1, 2) ~ 1,
      Personal_annual_income %in% c(3, 4) ~ 2,
      Personal_annual_income %in% c(5, 6) ~ 3),
    Personal_annual_income = factor(
      Personal_annual_income,
      levels = c(1, 2, 3),
      labels = c(
        "Less than 100,000 yuan",
        "100,000-200,000 yuan",
        "More than 200,000 yuan")),
    # Blood type: NA=(0/5), 1=A (BloodType:1), 2=B (BloodType:2), 3=O (BloodType:3), 4=AB (BloodType:4)
    Blood_type = case_when(
      Blood_type %in% c(0, 5) ~ NA_real_,
      TRUE ~ Blood_type),
    Blood_type = factor(
      Blood_type,
      levels = c(1, 2, 3, 4),
      labels = c("A", "B", "O", "AB")),
    # Smoking status: NA=(0), 1=No (smokeornot: 1-2), 2=Yes (smokeornot: 3-5)
    Smoking = case_when(
      Smoking == 0 ~ NA_real_,
      Smoking %in% c(1, 2) ~ 1,
      Smoking %in% c(3, 4, 5) ~ 2),
    Smoking = factor(
      Smoking,
      levels = c(1, 2),
      labels = c("No", "Yes")),
    # Drinking status: NA=(0), 1=No (drinkornot: 1-2), 2=Yes (drinkornot: 3-5)
    Drinking = case_when(
      Drinking == 0 ~ NA_real_,
      Drinking %in% c(1, 2) ~ 1,
      Drinking %in% c(3, 4, 5) ~ 2),
    Drinking = factor(
      Drinking,
      levels = c(1, 2),
      labels = c("No", "Yes")),
    # Regular eating: NA=(0), 1=No (foodtime: 1), 2=Yes (foodtime: 2)
    Regular_eating = case_when(
      Regular_eating == 0 ~ NA_real_,
      TRUE ~ Regular_eating),
    Regular_eating = factor(
      Regular_eating,
      levels = c(1, 2),
      labels = c("No", "Yes")),
    # Nighttime sleep: 1= < 6h (sleeptime: 1-2), 2= 6-8h (sleeptime: 3-4), 3= > 8h (sleeptime: 5-6)
    Nighttime_sleep = case_when(
      Nighttime_sleep == 0 ~ NA_real_,
      Nighttime_sleep %in% c(1, 2) ~ 1,
      Nighttime_sleep %in% c(3, 4) ~ 2,
      Nighttime_sleep %in% c(5, 6) ~ 3),
    Nighttime_sleep = factor(
      Nighttime_sleep,
      levels = c(1, 2, 3),
      labels = c("<6 h", "6-8 h", ">8 h")),
    # Exercise frequency: NA=(0), 1=No exercise or very rarely (FirstSportFrequ:5), 2=Once per week (FirstSportFrequ:4), 3=2--3 times per week (FirstSportFrequ:3), 4=Once or more times per day (FirstSportFrequ:1-2)
    Exercise_frequency = case_when(
      Exercise_frequency == 0 ~ NA_real_,
      Exercise_frequency == 5 ~ 1,
      Exercise_frequency == 4 ~ 2,
      Exercise_frequency == 3 ~ 3,
      Exercise_frequency %in% c(1,2)~ 4),
    Exercise_frequency = factor(
      Exercise_frequency,
      levels = c(1, 2, 3, 4),
      labels = c(
        "No exercise or very rarely",
        "Once per week",
        "2-3 times per week",
        "Once or more times per day")),
    # Self-rated health: NA=(0), 1=Poor(SameAgeHealth: 4+5), 2=Fair(SameAgeHealth: 3), 3=Good(SameAgeHealth: 1+2)
    Self_rated_health = case_when(
      Self_rated_health == 0 ~ NA_real_,
      Self_rated_health %in% c(4, 5) ~ 1,
      Self_rated_health == 3 ~ 2,
      Self_rated_health %in% c(1, 2) ~ 3),
    Self_rated_health = factor(
      Self_rated_health,
      levels = c(1, 2, 3),
      labels = c("Poor", "Fair", "Good")),
    # Disease category conversion Arthritis, Asthma, nopsick, psick01:psick26: 1=No(F/FALSE), 2=Yes(T/TRUE)
    across(c(Arthritis, Asthma, nopsick, psick01:psick26),
           ~ factor(case_when(
             . %in% c("T", "TRUE", "2") ~ 2,
             . %in% c("F", "FALSE", "1") ~ 1,
             TRUE ~ NA_real_),
             levels = c(1, 2),
             labels = c("No", "Yes")
           )
    )
  )

# Select cancer variables
Cancer_vars <- c("psick01","psick02","psick03","psick04",
                 "psick05","psick06","psick07","psick08","psick25")
data_traj2_eGFR4 <- data_traj2_eGFR4 %>%
  rowwise() %>%
  mutate(
    # Cancer: 1=No (psick01-08, psick25: all 1 or nopsick == 2), 2=Yes (psick01-08, psick25: any 2)
    Cancer = case_when(
      any(c_across(all_of(Cancer_vars)) == "Yes") ~ 2,
      all(c_across(all_of(Cancer_vars)) == "No") | nopsick == "Yes" ~ 1,
      TRUE ~ NA_real_),
    Cancer = factor(
      Cancer,
      levels = c(1, 2),
      labels = c("No", "Yes")),
    # Liver diseases: 1=No (psick17-18: all 1 or nopsick == 2), 2=Yes (psick17-18 any 2)
    Liver_diseases = case_when(
      any(c_across(all_of(c("Hepatitis", "Cirrhosis"))) == "Yes") ~ 2,
      all(c_across(all_of(c("Hepatitis", "Cirrhosis"))) == "No") | nopsick == "Yes" ~ 1,
      TRUE ~ NA_real_),
    Liver_diseases = factor(
      Liver_diseases,
      levels = c(1, 2),
      labels = c("No", "Yes"))
  ) %>%
  ungroup()

data_traj2_eGFR4 <- data_traj2_eGFR4 %>%
  mutate(
    # Convert variables < 0 to NA
    across(
      c(
        ESLLS, ESRLS, LHearing, RHearing,
        SBP, DBP, Height, Weight, BMI,
        Waist_circumference, Hip_circumference, BFP,
        Urine_protein, Urine_glucose, Insulin, SpO2,
        HS_CRP, HbA1c, TC, HDL_C, LDL_C, TG,
        BUN, FBG, UA, WBC, HB, MCV, PLT, HCT
      ),
      ~ if_else(. < 0, NA_real_, .)),
    # Vision: create new variable Vision by taking the maximum of ESLLS, ESRLS
    Vision = pmax(ESLLS, ESRLS, na.rm = TRUE),
    # Hearing: create new variable Hearing by taking the maximum of LHearing, RHearing
    Hearing = pmax(LHearing, RHearing, na.rm = TRUE),
    # Recalculate BMI (kg / m^2)
    BMI = if_else(
      !is.na(Height) & !is.na(Weight) & Height > 0 & Weight > 0,
      Weight / (Height / 100)^2,
      NA_real_
    ),
    # BMI classification
    BMI = case_when(
      BMI < 18.5 ~ 1,
      BMI >= 18.5 & BMI < 24 ~ 2,
      BMI >= 24 ~ 3,
      TRUE ~ NA_real_
    ),
    BMI = factor(
      BMI,
      levels = c(1, 2, 3),
      labels = c("<18.5", "18.5-24", "≥24")),
    # Urine protein classification
    Urine_protein = case_when(
      Urine_protein == 1 ~ 1,
      Urine_protein == 2 ~ 2,
      Urine_protein %in% 3:5 ~ 3,
      TRUE ~ NA_real_
    ),
    Urine_protein = factor(
      Urine_protein,
      levels = c(1, 2, 3),
      labels = c("Negative", "Weakly positive", "Positive")),
    # Urine glucose classification
    Urine_glucose = case_when(
      Urine_glucose == 1 ~ 1,
      Urine_glucose == 2 ~ 2,
      Urine_glucose %in% 3:5 ~ 3,
      TRUE ~ NA_real_
    ),
    Urine_glucose = factor(
      Urine_glucose,
      levels = c(1, 2, 3),
      labels = c("Negative", "Weakly positive", "Positive")),
    SpO2 = case_when(
      SpO2 > 100 | SpO2 < 70 ~ NA_real_,
      TRUE ~ SpO2
    )
  )

# Extract needed variables
vars_needed <- c(
  "Age","Sex","Education","Marital_status","Smoking","Drinking",
  "Cancer","Arthritis","Asthma","Thyroid_diseases","Tuberculosis",
  "Digestive_diseases","Liver_diseases","Urolithiasis","Gout","Anemia",
  "Vision","Hearing","SBP","DBP","BMI","Waist_circumference",
  "Hip_circumference","BFP","Urine_protein","Urine_glucose",
  "HbA1c","TC","HDL_C","LDL_C","TG","BUN","FBG","UA","WBC",
  "HB","MCV","PLT","HCT","eGFR_group", "CMDs"
)

# Final analysis dataset
data_analyze_eGFR4 <- data_traj2_eGFR4 %>%
  dplyr::select(all_of(vars_needed))

# Copy dataset for variable name replacement
data_analyze1_eGFR4 <- data_analyze_eGFR4
idx2_eGFR4 <- match(names(data_analyze1_eGFR4), variable_dict$analysis_name)
colnames(data_analyze1_eGFR4)[!is.na(idx2_eGFR4)] <- variable_dict$display_name[idx2_eGFR4[!is.na(idx2_eGFR4)]]

# Delete columns with collinearity and missing >20%
data_analyze2_eGFR4 <- data_analyze_eGFR4[, -missing_columns]

###-------------------Missing value imputation: Random Forest imputation------------------
# Convert to data frame
data_analyze2_eGFR4 <- as.data.frame(data_analyze2_eGFR4)

# Set random seed for reproducibility
set.seed(2025)

# Impute missing values in data_analyze2 using missForest()
data_missresult_eGFR4 <- missForest(data_analyze2_eGFR4)

# Extract imputed data and convert to data frame
data_analyze3_eGFR4 <- as.data.frame(data_missresult_eGFR4$ximp)

###-------------------Data splitting------------------
## Trajectory group split
# Low decline and High-stable groups
data_LH_eGFR4 <- data_analyze3_eGFR4 %>%
  filter(eGFR_group %in% c(1, 2)) %>%
  select(-eGFR_group) # Delete eGFR_group column

# Unstable and High-stable groups
data_UH_eGFR4 <- data_analyze3_eGFR4 %>%
  filter(eGFR_group %in% c(2, 3)) %>%
  select(-eGFR_group) # Delete eGFR_group column

## Split training and test sets for Low decline and High-stable groups
# Set random seed for reproducibility
set.seed(1217)

# createDataPartition() automatically takes equal proportions of data from each level of y, p indicates training/test set proportion, list = F returns a vector, times = 1 means perform split only once
trainindex_LH_eGFR4 <- createDataPartition(data_LH_eGFR4$CMDs, p = 0.7, list = F, times = 1)

# Extract training set
data_LH_train_eGFR4 <- data_LH_eGFR4[trainindex_LH_eGFR4, ]

# Extract test set
data_LH_test_eGFR4 <- data_LH_eGFR4[-trainindex_LH_eGFR4, ]

## Split training and test sets for Unstable and High-stable groups
# Set random seed for reproducibility
set.seed(1217)

# createDataPartition() automatically takes equal proportions of data from each level of y, p indicates training/test set proportion, list = F returns a vector, times = 1 means perform split only once
trainindex_UH_eGFR4 <- createDataPartition(data_UH_eGFR4$CMDs, p = 0.7, list = F, times = 1)

# Extract training set
data_UH_train_eGFR4 <- data_UH_eGFR4[trainindex_UH_eGFR4, ]

# Extract test set
data_UH_test_eGFR4 <- data_UH_eGFR4[-trainindex_UH_eGFR4, ]

###-------------------Variable standardization-------------------
## Standardize variables for Low decline and High-stable groups
# Use preProcess() to standardize (center and scale) the training set, making the mean 0 and SD 1 for each column
standardized_para_LH_eGFR4 <- preProcess(data_LH_train_eGFR4, method = c("center", "scale"))

# Standardize training set
data_LH_train_std_eGFR4 <- predict(standardized_para_LH_eGFR4, newdata = data_LH_train_eGFR4)

# Standardize test set using the standardization parameters from the training set
data_LH_test_std_eGFR4 <- predict(standardized_para_LH_eGFR4, newdata = data_LH_test_eGFR4)

## Standardize variables for Unstable and High-stable groups
# Use preProcess() to standardize (center and scale) the training set, making the mean 0 and SD 1 for each column
standardized_para_UH_eGFR4 <- preProcess(data_UH_train_eGFR4, method = c("center", "scale"))

# Standardize training set
data_UH_train_std_eGFR4 <- predict(standardized_para_UH_eGFR4, newdata = data_UH_train_eGFR4)

# Standardize test set using the standardization parameters from the training set
data_UH_test_std_eGFR4 <- predict(standardized_para_UH_eGFR4, newdata = data_UH_test_eGFR4)

## XGBoost, SVM cannot handle factor variables, convert dataset to numeric, except CMDs
data_LH_train_std_num_eGFR4 <- to_num_except_CMDs(data_LH_train_std_eGFR4)
data_LH_test_std_num_eGFR4 <- to_num_except_CMDs(data_LH_test_std_eGFR4)
data_UH_train_std_num_eGFR4 <- to_num_except_CMDs(data_UH_train_std_eGFR4)
data_UH_test_std_num_eGFR4 <- to_num_except_CMDs(data_UH_test_std_eGFR4)

# Extract selected features and outcome
data_LH5_train_eGFR4 <- data_LH_train_std_num_eGFR4[, c(LH_round_outputs[[7]][["features"]], "CMDs")]
data_LH5_test_eGFR4 <- data_LH_test_std_num_eGFR4[, c(LH_round_outputs[[7]][["features"]], "CMDs")]
data_UH5_train_eGFR4 <- data_UH_train_std_num_eGFR4[, c(UH_round_outputs[[7]][["features"]], "CMDs")]
data_UH5_test_eGFR4 <- data_UH_test_std_num_eGFR4[, c(UH_round_outputs[[7]][["features"]], "CMDs")]

##------------------Model building------------------
# Create tasks
LH_train_task_eGFR4 <- as_task_classif(data_LH5_train_eGFR4, target = "CMDs")
LH_test_task_eGFR4 <- as_task_classif(data_LH5_test_eGFR4, target = "CMDs")
UH_train_task_eGFR4 <- as_task_classif(data_UH5_train_eGFR4, target = "CMDs")
UH_test_task_eGFR4 <- as_task_classif(data_UH5_test_eGFR4, target = "CMDs")

## Train and predict
# Low decline and High-stable groups: XGBoost
LH_xgb_learner_eGFR4 <- lrn("classif.xgboost", predict_type = "prob", verbose = 0)
LH_xgb_learner_eGFR4$param_set$values <- list(
  nrounds = to_tune(500, 1000), # Number of trees: controls number of boosting iterations, determines learning extent.
  max_depth = to_tune(1, 10), # Maximum tree depth: controls tree complexity to prevent overfitting.
  eta = to_tune(0.001, 0.05), # Learning rate: controls learning speed, affects convergence and stability.
  min_child_weight = to_tune(1, 10), # Minimum sum of instance weights in a child node: restricts splitting to prevent overfitting.
  subsample = to_tune(0.1, 0.5) # Subsample ratio: provides randomness, reduces overfitting risk, increases diversity.
)

set.seed(123)
LH_xgb_eGFR4 <- tune(tuner = tnr("grid_search", resolution = 5),
                     task = LH_train_task_eGFR4,
                     learner = LH_xgb_learner_eGFR4,
                     resampling = rsmp("cv", folds = 5),
                     measure = msr("classif.auc")
)
LH_xgb_learner_eGFR4$param_set$values <- LH_xgb_eGFR4$result_learner_param_vals
LH_xgb_learner_eGFR4$train(LH_train_task_eGFR4)
LH_train_xgb_pred_eGFR4 <- LH_xgb_learner_eGFR4$predict(LH_train_task_eGFR4)
LH_test_xgb_pred_eGFR4 <- LH_xgb_learner_eGFR4$predict(LH_test_task_eGFR4)

# Unstable and High-stable groups: XGBoost
UH_xgb_learner_eGFR4 <- lrn("classif.xgboost", predict_type = "prob")
UH_xgb_learner_eGFR4$param_set$values <- list(
  nrounds = to_tune(100, 500), # Number of trees: controls number of boosting iterations, determines learning extent.
  max_depth = to_tune(1, 10), # Maximum tree depth: controls tree complexity to prevent overfitting.
  eta = to_tune(0.001, 0.05), # Learning rate: controls learning speed, affects convergence and stability.
  min_child_weight = to_tune(1, 10), # Minimum sum of instance weights in a child node: restricts splitting to prevent overfitting.
  subsample = to_tune(0.5, 1) # Subsample ratio: provides randomness, reduces overfitting risk, increases diversity.
)

set.seed(123)
UH_xgb_eGFR4 <- tune(tuner = tnr("grid_search", resolution = 5),
                     task = UH_train_task_eGFR4,
                     learner = UH_xgb_learner_eGFR4,
                     resampling = rsmp("cv", folds = 5),
                     measure = msr("classif.auc")
)
UH_xgb_learner_eGFR4$param_set$values <- UH_xgb_eGFR4$result_learner_param_vals
UH_xgb_learner_eGFR4$train(UH_train_task_eGFR4)
UH_train_xgb_pred_eGFR4 <- UH_xgb_learner_eGFR4$predict(UH_train_task_eGFR4)
UH_test_xgb_pred_eGFR4 <- UH_xgb_learner_eGFR4$predict(UH_test_task_eGFR4)

###------------------Performance evaluation------------------
##------------------Low decline and High-stable groups------------------
LH_train_metrics_eGFR4 <- train_performance(LH_train_xgb_pred_eGFR4,
                                            data_LH_train_std_num_eGFR4,
                                            Dataset_name = "LH_train_eGFR4")
LH_test_metrics_eGFR4 <- test_performance(LH_test_xgb_pred_eGFR4,
                                          data_LH_test_std_num_eGFR4,
                                          train_threshold = LH_train_metrics_eGFR4$Threshold,
                                          Dataset_name = "LH_test_eGFR4")

# DeLong test
LH_test_delong_test_eGFR4 <- roc.test(LH_test_roc_auc_xgb$xgb_5, LH_test_metrics_eGFR4[["roc_test"]], method = "delong")
# Extract Z-score and p-value, rounding Z-score to 3 decimals and formatting p-value
LH_test_Z_score_eGFR4 <- round(LH_test_delong_test_eGFR4$statistic, 3)
LH_test_P_value_eGFR4 <- pvalue_format(LH_test_delong_test_eGFR4$p.value)

#------------------Unstable and High-stable groups------------------
UH_train_metrics_eGFR4 <- train_performance(UH_train_xgb_pred_eGFR4,
                                            data_UH_train_std_num_eGFR4,
                                            Dataset_name = "UH_train_eGFR4")
UH_test_metrics_eGFR4 <- test_performance(UH_test_xgb_pred_eGFR4,
                                          data_UH_test_std_num_eGFR4,
                                          train_threshold = UH_train_metrics_eGFR4$Threshold,
                                          Dataset_name = "UH_test_eGFR4")

# DeLong test
UH_test_delong_test_eGFR4 <- roc.test(UH_test_roc_auc_xgb$xgb_5, UH_test_metrics_eGFR4[["roc_test"]], method = "delong")
# Extract Z-score and p-value, rounding Z-score to 3 decimals and formatting p-value
UH_test_Z_score_eGFR4 <- round(UH_test_delong_test_eGFR4$statistic, 3)
UH_test_P_value_eGFR4 <- pvalue_format(UH_test_delong_test_eGFR4$p.value)

# Add DeLong test results to data frame and combine
test_auc_comparisons_eGFR4 <- data.frame(
  Model = "sensitivity2",
  LH_test_Z_score = LH_test_Z_score_eGFR4,
  LH_test_P_value = LH_test_P_value_eGFR4,
  UH_test_Z_score = UH_test_Z_score_eGFR4,
  UH_test_P_value = UH_test_P_value_eGFR4
)

# Combine DeLong test results from the first 3 sensitivity analyses
sensitivity123_delong_result <- rbind(sensitivity1_test_auc_comparisons,
                                      sensitivity2_test_auc_comparisons,
                                      test_auc_comparisons_eGFR4)

# Combine performance metric results from the first 3 sensitivity analyses
sensitivity123_result <- rbind(sensitivity1_LH_test_metrics[["test_metrics_result"]],
                               sensitivity2_LH_test_metrics[["test_metrics_result"]],
                               LH_test_metrics_eGFR4[["test_metrics_result"]],
                               sensitivity1_UH_test_metrics[["test_metrics_result"]],
                               sensitivity2_UH_test_metrics[["test_metrics_result"]],
                               UH_test_metrics_eGFR4[["test_metrics_result"]])

###------------------Sensitivity analysis SHAP beeswarm plots------------------
# Sensitivity analysis SHAP object function
sensitivity_shap_sv <- function(sensitivity_train_task,
                                sensitivity_test_task,
                                sensitivity_learner) {
  # Extract predictor variables for training and test sets
  sensitivity_train_x <- sensitivity_train_task$data(cols = sensitivity_train_task$feature_names)
  sensitivity_test_x <- sensitivity_test_task$data(cols = sensitivity_test_task$feature_names)
  # Randomly sample 200 rows from the training set
  set.seed(123)
  sensitivity_train_x_200 <- sensitivity_train_x %>%
    slice_sample(n = 200)
  # Calculate SHAP values: svm_learner trained model; X = test_x test dataset to explain; bg_X = train_x_200: background dataset for SHAP baseline; predict_type = "prob": specify model prediction type as probability; verbose = F to turn off detailed output.
  shap_value <- kernelshap(sensitivity_learner,
                           X = sensitivity_test_x,
                           bg_X = sensitivity_train_x_200, predict_type = "prob", verbose = T)
  # Build visualization object
  sv_xgb <- shapviz(shap_value, which_class = 2)
  # Map variable names using data dictionary
  shap_values_df <- as.data.frame(sv_xgb$S)
  colnames(shap_values_df) <- variable_dict$display_name[match(names(shap_values_df), variable_dict$analysis_name)]
  sv_xgb$S <- as.matrix(shap_values_df)
  colnames(sv_xgb$X) <- variable_dict$display_name[match(names(sv_xgb$X), variable_dict$analysis_name)]
  return(sv_xgb)
}

# Call function for each sensitivity analysis SHAP object
LH_sensitivity1_shap_sv <- sensitivity_shap_sv(sensitivity1_LH_train_task,
                                               sensitivity1_LH_test_task,
                                               sensitivity1_LH_xgb_learner)
LH_sensitivity2_shap_sv <- sensitivity_shap_sv(sensitivity2_LH_train_task,
                                               sensitivity2_LH_test_task,
                                               sensitivity2_LH_xgb_learner)
LH_shap_sv_eGFR4 <- sensitivity_shap_sv(LH_train_task_eGFR4,
                                        LH_test_task_eGFR4,
                                        LH_xgb_learner_eGFR4)
UH_sensitivity1_shap_sv <- sensitivity_shap_sv(sensitivity1_UH_train_task,
                                               sensitivity1_UH_test_task,
                                               sensitivity1_UH_xgb_learner)
UH_sensitivity2_shap_sv <- sensitivity_shap_sv(sensitivity2_UH_train_task,
                                               sensitivity2_UH_test_task,
                                               sensitivity2_UH_xgb_learner)
UH_shap_sv_eGFR4 <- sensitivity_shap_sv(UH_train_task_eGFR4,
                                        UH_test_task_eGFR4,
                                        UH_xgb_learner_eGFR4)

# SHAP beeswarm plots
LH_sensitivity1_shap_beeswarm <- sv_importance(LH_sensitivity1_shap_sv,
                                               kind = "beeswarm", # Beeswarm plot
                                               size = 0.2, # Scatter point size (beeswarm)
                                               bee_width = 0.3, # Horizontal spread width of points
                                               max_display = Inf) + # Display all features
  scale_color_gradient(low = "#6601F7", high = "#48EDFE") +
  scale_x_continuous(limits = c(-0.1, 0.5), # Limit x-axis range
                     breaks = seq(-0.1, 0.5, by = 0.1)) + # Set x-axis ticks
  theme(
    panel.background = element_rect(fill = "white"), # Panel background white
    plot.background = element_rect(fill = "white"), # Entire plot background white
    axis.title = element_text(size = 40), # Axis title font size
    axis.text = element_text(size = 30), # Axis tick label font size
    legend.title = element_text(size = 30), # Legend title font size
    legend.text = element_text(size = 20), # Legend text font size
    legend.key.height = unit(0.35, "cm"), # Bar legend height
    legend.key.width = unit(0.1, "cm"), # Bar legend width
    plot.tag = element_text(size = 70, face = "bold"), # Set tag text size
    plot.margin = margin(0, 0, 0, 0, "cm"), # Adjust margins
    axis.ticks = element_line(size = 0.25), # y-axis tick line thickness
    axis.ticks.length = unit(0.05, "cm"), # Tick mark length and placement outside
    axis.ticks.y = element_blank() # Remove y-axis tick marks
  )

LH_sensitivity2_shap_beeswarm <- sv_importance(LH_sensitivity2_shap_sv,
                                               kind = "beeswarm", # Beeswarm plot
                                               size = 0.2, # Scatter point size (beeswarm)
                                               bee_width = 0.3, # Horizontal spread width of points
                                               max_display = Inf) + # Display all features
  scale_color_gradient(low = "#6601F7", high = "#48EDFE") +
  scale_x_continuous(limits = c(-0.1, 0.7), # Limit x-axis range
                     breaks = seq(-0.1, 0.7, by = 0.1)) + # Set x-axis ticks
  theme(
    panel.background = element_rect(fill = "white"), # Panel background white
    plot.background = element_rect(fill = "white"), # Entire plot background white
    axis.title = element_text(size = 40), # Axis title font size
    axis.text = element_text(size = 30), # Axis tick label font size
    legend.title = element_text(size = 30), # Legend title font size
    legend.text = element_text(size = 20), # Legend text font size
    legend.key.height = unit(0.35, "cm"), # Bar legend height
    legend.key.width = unit(0.1, "cm"), # Bar legend width
    plot.tag = element_text(size = 70, face = "bold"), # Set tag text size
    plot.margin = margin(0, 0, 0, 0, "cm"), # Adjust margins
    axis.ticks = element_line(size = 0.25), # y-axis tick line thickness
    axis.ticks.length = unit(0.05, "cm"), # Tick mark length and placement outside
    axis.ticks.y = element_blank() # Remove y-axis tick marks
  )

UH_sensitivity1_shap_beeswarm <- sv_importance(UH_sensitivity1_shap_sv,
                                               kind = "beeswarm", # Beeswarm plot
                                               size = 0.2, # Scatter point size (beeswarm)
                                               bee_width = 0.3, # Horizontal spread width of points
                                               max_display = Inf) + # Display all features
  scale_color_gradient(low = "#6601F7", high = "#48EDFE") +
  scale_x_continuous(limits = c(-0.1, 0.5), # Limit x-axis range
                     breaks = seq(-0.1, 0.5, by = 0.1)) + # Set x-axis ticks
  theme(
    panel.background = element_rect(fill = "white"), # Panel background white
    plot.background = element_rect(fill = "white"), # Entire plot background white
    axis.title = element_text(size = 40), # Axis title font size
    axis.text = element_text(size = 30), # Axis tick label font size
    legend.title = element_text(size = 30), # Legend title font size
    legend.text = element_text(size = 20), # Legend text font size
    legend.key.height = unit(0.35, "cm"), # Bar legend height
    legend.key.width = unit(0.1, "cm"), # Bar legend width
    plot.tag = element_text(size = 70, face = "bold"), # Set tag text size
    plot.margin = margin(0, 0, 0, 0, "cm"), # Adjust margins
    axis.ticks = element_line(size = 0.25), # y-axis tick line thickness
    axis.ticks.length = unit(0.05, "cm"), # Tick mark length and placement outside
    axis.ticks.y = element_blank() # Remove y-axis tick marks
  )

UH_sensitivity2_shap_beeswarm <- sv_importance(UH_sensitivity2_shap_sv,
                                               kind = "beeswarm", # Beeswarm plot
                                               size = 0.2, # Scatter point size (beeswarm)
                                               bee_width = 0.3, # Horizontal spread width of points
                                               max_display = Inf) + # Display all features
  scale_color_gradient(low = "#6601F7", high = "#48EDFE") +
  scale_x_continuous(limits = c(-0.1, 0.5), # Limit x-axis range
                     breaks = seq(-0.1, 0.5, by = 0.1)) + # Set x-axis ticks
  theme(
    panel.background = element_rect(fill = "white"), # Panel background white
    plot.background = element_rect(fill = "white"), # Entire plot background white
    axis.title = element_text(size = 40), # Axis title font size
    axis.text = element_text(size = 30), # Axis tick label font size
    legend.title = element_text(size = 30), # Legend title font size
    legend.text = element_text(size = 20), # Legend text font size
    legend.key.height = unit(0.35, "cm"), # Bar legend height
    legend.key.width = unit(0.1, "cm"), # Bar legend width
    plot.tag = element_text(size = 70, face = "bold"), # Set tag text size
    plot.margin = margin(0, 0, 0, 0, "cm"), # Adjust margins
    axis.ticks = element_line(size = 0.25), # y-axis tick line thickness
    axis.ticks.length = unit(0.05, "cm"), # Tick mark length and placement outside
    axis.ticks.y = element_blank() # Remove y-axis tick marks
  )

UH_sensitivity3_shap_beeswarm <- sv_importance(UH_sensitivity3_shap_sv,
                                               kind = "beeswarm", # Beeswarm plot
                                               size = 0.2, # Scatter point size (beeswarm)
                                               bee_width = 0.3, # Horizontal spread width of points
                                               max_display = Inf) + # Display all features
  scale_color_gradient(low = "#6601F7", high = "#48EDFE") +
  scale_x_continuous(limits = c(-0.6, 0.6), # Limit x-axis range
                     breaks = seq(-0.6, 0.6, by = 0.3)) + # Set x-axis ticks
  theme(
    panel.background = element_rect(fill = "white"), # Panel background white
    plot.background = element_rect(fill = "white"), # Entire plot background white
    axis.title = element_text(size = 40), # Axis title font size
    axis.text = element_text(size = 30), # Axis tick label font size
    legend.title = element_text(size = 30), # Legend title font size
    legend.text = element_text(size = 20), # Legend text font size
    legend.key.height = unit(0.35, "cm"), # Bar legend height
    legend.key.width = unit(0.1, "cm"), # Bar legend width
    plot.tag = element_text(size = 70, face = "bold"), # Set tag text size
    plot.margin = margin(0, 0, 0, 0, "cm"), # Adjust margins
    axis.ticks = element_line(size = 0.25), # y-axis tick line thickness
    axis.ticks.length = unit(0.05, "cm"), # Tick mark length and placement outside
    axis.ticks.y = element_blank() # Remove y-axis tick marks
  )

# SHAP beeswarm plots
LH_shap_beeswarm_eGFR4 <- sv_importance(LH_shap_sv_eGFR4,
                                        kind = "beeswarm", # Beeswarm plot
                                        size = 0.2, # Scatter point size (beeswarm)
                                        bee_width = 0.3, # Horizontal spread width of points
                                        max_display = Inf) + # Display all features
  scale_color_gradient(low = "#6601F7", high = "#48EDFE") +
  scale_x_continuous(limits = c(-0.2, 0.8), # Limit x-axis range
                     breaks = seq(-0.2, 0.8, by = 0.2)) + # Set x-axis ticks
  theme(
    panel.background = element_rect(fill = "white"), # Panel background white
    plot.background = element_rect(fill = "white"), # Entire plot background white
    axis.title = element_text(size = 40), # Axis title font size
    axis.text = element_text(size = 30), # Axis tick label font size
    legend.title = element_text(size = 30), # Legend title font size
    legend.text = element_text(size = 20), # Legend text font size
    legend.key.height = unit(0.35, "cm"), # Bar legend height
    legend.key.width = unit(0.1, "cm"), # Bar legend width
    plot.tag = element_text(size = 70, face = "bold"), # Set tag text size
    plot.margin = margin(0, 0, 0, 0, "cm"), # Adjust margins
    axis.ticks = element_line(size = 0.25), # y-axis tick line thickness
    axis.ticks.length = unit(0.05, "cm"), # Tick mark length and placement outside
    axis.ticks.y = element_blank() # Remove y-axis tick marks
  )

UH_shap_beeswarm_eGFR4 <- sv_importance(UH_shap_sv_eGFR4,
                                        kind = "beeswarm", # Beeswarm plot
                                        size = 0.2, # Scatter point size (beeswarm)
                                        bee_width = 0.3, # Horizontal spread width of points
                                        max_display = Inf) + # Display all features
  scale_color_gradient(low = "#6601F7", high = "#48EDFE") +
  scale_x_continuous(limits = c(-0.2, 0.8), # Limit x-axis range
                     breaks = seq(-0.2, 0.8, by = 0.2)) + # Set x-axis ticks
  theme(
    panel.background = element_rect(fill = "white"), # Panel background white
    plot.background = element_rect(fill = "white"), # Entire plot background white
    axis.title = element_text(size = 40), # Axis title font size
    axis.text = element_text(size = 30), # Axis tick label font size
    legend.title = element_text(size = 30), # Legend title font size
    legend.text = element_text(size = 20), # Legend text font size
    legend.key.height = unit(0.35, "cm"), # Bar legend height
    legend.key.width = unit(0.1, "cm"), # Bar legend width
    plot.tag = element_text(size = 70, face = "bold"), # Set tag text size
    plot.margin = margin(0, 0, 0, 0, "cm"), # Adjust margins
    axis.ticks = element_line(size = 0.25), # y-axis tick line thickness
    axis.ticks.length = unit(0.05, "cm"), # Tick mark length and placement outside
    axis.ticks.y = element_blank() # Remove y-axis tick marks
  )

# Combine SHAP beeswarm plots from 3 sensitivity analyses
LH_UH_sensitivity_shap_beeswarm <- (LH_sensitivity1_shap_beeswarm + UH_sensitivity1_shap_beeswarm) /
  (LH_sensitivity2_shap_beeswarm + UH_sensitivity2_shap_beeswarm) /
  (LH_shap_beeswarm_eGFR4 + UH_shap_beeswarm_eGFR4) +
  plot_annotation(tag_levels = "A") # Automatically add labels

# Export SHAP beeswarm plots from 3 sensitivity analyses
ggsave(plot = LH_UH_sensitivity_shap_beeswarm,
       filename = "LH_UH_sensitivity_shap_beeswarm.png",
       width = 17,
       height = 10.5,
       units = "cm",
       dpi = 600)

##------------------(4) Sensitivity analysis: including baseline eGFR------------------
# Extract eGFR data
data_analyze_eGFR <- data_traj2[, "eGFR"]

# Combine with dataset
data_analyze4 <- cbind(data_analyze3, data_analyze_eGFR)

###-------------------Data splitting-------------------
## Trajectory group split
# Low decline and High-stable groups
data_LH_eGFR <- data_analyze4 %>%
  filter(eGFR_group %in% c(1, 3)) %>%
  select(-eGFR_group) # Delete eGFR_group column

# Unstable and High-stable groups
data_UH_eGFR <- data_analyze4 %>%
  filter(eGFR_group %in% c(2, 3)) %>%
  select(-eGFR_group) # Delete eGFR_group column

# Convert row names to column id
data_LH_eGFR$row_id <- rownames(data_LH_eGFR)
data_UH_eGFR$row_id <- rownames(data_UH_eGFR)

data_LH_train_std_num1 <- data_LH_train_std_num
data_LH_test_std_num1 <- data_LH_test_std_num
data_UH_train_std_num1 <- data_UH_train_std_num
data_UH_test_std_num1 <- data_UH_test_std_num

data_LH_train_std_num1$row_id <- rownames(data_LH_train_std_num1)
data_LH_test_std_num1$row_id <- rownames(data_LH_test_std_num1)
data_UH_train_std_num1$row_id <- rownames(data_UH_train_std_num1)
data_UH_test_std_num1$row_id <- rownames(data_UH_test_std_num1)

# Merge into training and test sets for each trajectory group
data_LH_train_std_num1 <- data_LH_train_std_num1 %>%
  left_join(data_LH_eGFR %>% select(row_id, eGFR), by = "row_id") %>%
  select(-row_id)
data_LH_test_std_num1 <- data_LH_test_std_num1 %>%
  left_join(data_LH_eGFR %>% select(row_id, eGFR), by = "row_id") %>%
  select(-row_id)
data_UH_train_std_num1 <- data_UH_train_std_num1 %>%
  left_join(data_UH_eGFR %>% select(row_id, eGFR), by = "row_id") %>%
  select(-row_id)
data_UH_test_std_num1 <- data_UH_test_std_num1 %>%
  left_join(data_UH_eGFR %>% select(row_id, eGFR), by = "row_id") %>%
  select(-row_id)

##------------------Model building: Low decline and High-stable groups------------------
# Create training task
LH_train_task_eGFR <- as_task_classif(data_LH_train_std_num1, target = "CMDs")

# Create test task
LH_test_task_eGFR <- as_task_classif(data_LH_test_std_num1, target = "CMDs")

## Logistic Regression LR
# Select algorithm, predict output as probability
LH_lr_learner_eGFR <- lrn("classif.log_reg", predict_type = "prob")

# Train on training set
LH_lr_learner_eGFR$train(LH_train_task_eGFR)

# Predict on training set
LH_train_lr_pred_eGFR <- LH_lr_learner_eGFR$predict(LH_train_task_eGFR)

# Predict on test set
LH_test_lr_pred_eGFR <- LH_lr_learner_eGFR$predict(LH_test_task_eGFR)

## Random Forest RF
# Select algorithm, predict output as probability
LH_rf_learner_eGFR <- lrn("classif.ranger", predict_type = "prob")

# Select hyperparameters and their ranges for tuning
LH_rf_learner_eGFR$param_set$values <- list(
  num.trees = to_tune(10, 100), # Number of trees: controls forest size, affects model stability and computational efficiency.
  max.depth = to_tune(1, 5), # Maximum tree depth: limits tree depth to prevent overfitting.
  min.node.size = to_tune(p_int(5, 10)), # Minimum node size: sets minimum samples in a node, controls tree splitting. p_int() defines integer parameter range, suitable for newer mlr3tuning
  mtry = to_tune(5, 10), # Number of random features to try at each split: controls randomness, balances diversity and accuracy.
  sample.fraction = to_tune(0.5, 1) # Proportion of samples to draw for each tree
)

# Set random seed for reproducibility
set.seed(123)

# Perform tuning
LH_rf_eGFR <- tune(tuner = tnr("grid_search", resolution = 5), # Grid search; resolution = 5 means each parameter to tune is divided into 5 equally spaced values
                   task = LH_train_task_eGFR, # Specify tuning task
                   learner = LH_rf_learner_eGFR, # Specify learner to tune
                   resampling = rsmp("cv", folds = 5), # 5-fold cross-validation
                   measure = msr("classif.auc") # Model evaluation metric AUC
)

# Apply best hyperparameters to the learner
LH_rf_learner_eGFR$param_set$values <- LH_rf_eGFR$result_learner_param_vals

# Train on training set
LH_rf_learner_eGFR$train(LH_train_task_eGFR)

# Predict on training set
LH_train_rf_pred_eGFR <- LH_rf_learner_eGFR$predict(LH_train_task_eGFR)

# Predict on test set
LH_test_rf_pred_eGFR <- LH_rf_learner_eGFR$predict(LH_test_task_eGFR)

## XGBoost
LH_xgb_learner_eGFR <- lrn("classif.xgboost", predict_type = "prob")
LH_xgb_learner_eGFR$param_set$values <- list(
  nrounds = to_tune(500, 1000), # Number of trees: controls number of boosting iterations, determines learning extent.
  max_depth = to_tune(1, 10), # Maximum tree depth: controls tree complexity to prevent overfitting.
  eta = to_tune(0.001, 0.05), # Learning rate: controls learning speed, affects convergence and stability.
  min_child_weight = to_tune(1, 10), # Minimum sum of instance weights in a child node: restricts splitting to prevent overfitting.
  subsample = to_tune(0.1, 0.5) # Subsample ratio: provides randomness, reduces overfitting risk, increases diversity.
)

set.seed(123)
LH_xgb_eGFR <- tune(tuner = tnr("grid_search", resolution = 5),
                    task = LH_train_task_eGFR,
                    learner = LH_xgb_learner_eGFR,
                    resampling = rsmp("cv", folds = 5),
                    measure = msr("classif.auc")
)
LH_xgb_learner_eGFR$param_set$values <- LH_xgb_eGFR$result_learner_param_vals
LH_xgb_learner_eGFR$train(LH_train_task_eGFR)
LH_train_xgb_pred_eGFR <- LH_xgb_learner_eGFR$predict(LH_train_task_eGFR)
LH_test_xgb_pred_eGFR <- LH_xgb_learner_eGFR$predict(LH_test_task_eGFR)

## Support Vector Machine SVM
LH_svm_learner_eGFR <- lrn("classif.svm", predict_type = "prob")
LH_svm_learner_eGFR$param_set$values <- list(
  type = "C-classification", # Classification problem
  kernel = "radial", # Radial Basis Function (RBF) kernel: handles complex nonlinear classification problems
  cost = to_tune(0.001, 1), # C or penalty parameter: penalty for misclassification
  gamma = to_tune(0.001, 1) # gamma parameter: defines width of RBF kernel, affects distribution of data mapped to high-dimensional feature space
)

set.seed(123)
LH_svm_eGFR <- tune(tuner = tnr("grid_search", resolution = 5),
                    task = LH_train_task_eGFR,
                    learner = LH_svm_learner_eGFR,
                    resampling = rsmp("cv", folds = 5),
                    measure = msr("classif.auc")
)
LH_svm_learner_eGFR$param_set$values <- LH_svm_eGFR$result_learner_param_vals
LH_svm_learner_eGFR$train(LH_train_task_eGFR)
LH_train_svm_pred_eGFR <- LH_svm_learner_eGFR$predict(LH_train_task_eGFR)
LH_test_svm_pred_eGFR <- LH_svm_learner_eGFR$predict(LH_test_task_eGFR)

## K-Nearest Neighbors KNN
LH_knn_learner_eGFR <- lrn("classif.kknn", predict_type = "prob")
LH_knn_learner_eGFR$param_set$values <- list(
  k = to_tune(500, 1000) # Number of nearest neighbors
)

set.seed(123)
LH_knn_eGFR <- tune(tuner = tnr("grid_search", resolution = 5),
                    task = LH_train_task_eGFR,
                    learner = LH_knn_learner_eGFR,
                    resampling = rsmp("cv", folds = 5),
                    measure = msr("classif.auc")
)
LH_knn_learner_eGFR$param_set$values <- LH_knn_eGFR$result_learner_param_vals
LH_knn_learner_eGFR$train(LH_train_task_eGFR)
LH_train_knn_pred_eGFR <- LH_knn_learner_eGFR$predict(LH_train_task_eGFR)
LH_test_knn_pred_eGFR <- LH_knn_learner_eGFR$predict(LH_test_task_eGFR)

## Decision Tree DT
LH_dt_learner_eGFR <- lrn("classif.rpart", predict_type = "prob")
LH_dt_learner_eGFR$param_set$values <- list(
  minsplit = to_tune(20, 50), # Minimum number of observations for a split
  minbucket = to_tune(10, 20), # Minimum number of observations in a terminal node
  cp = to_tune(0.01, 0.1), # Complexity parameter
  maxdepth = to_tune(3, 10) # Maximum tree depth
)

set.seed(123)
LH_dt_eGFR <- tune(tuner = tnr("grid_search", resolution = 5),
                   task = LH_train_task_eGFR,
                   learner = LH_dt_learner_eGFR,
                   resampling = rsmp("cv", folds = 5),
                   measure = msr("classif.auc")
)
LH_dt_learner_eGFR$param_set$values <- LH_dt_eGFR$result_learner_param_vals
LH_dt_learner_eGFR$train(LH_train_task_eGFR)
LH_train_dt_pred_eGFR <- LH_dt_learner_eGFR$predict(LH_train_task_eGFR)
LH_test_dt_pred_eGFR <- LH_dt_learner_eGFR$predict(LH_test_task_eGFR)

## Artificial Neural Network ANN
LH_ann_learner_eGFR <- lrn("classif.nnet", predict_type = "prob")
LH_ann_learner_eGFR$param_set$values <- list(
  size = to_tune(3, 10), # Number of neurons in the hidden layer, larger size -> stronger model capacity, can fit more complex nonlinear relationships
  decay = to_tune(0.01, 0.1) # Weight decay coefficient / L2 regularization strength, larger decay -> stronger penalty on weights, weights are shrunk -> less prone to overfitting, but too large can cause severe underfitting
)

set.seed(123)
LH_ann_eGFR <- tune(tuner = tnr("grid_search", resolution = 5),
                    task = LH_train_task_eGFR,
                    learner = LH_ann_learner_eGFR,
                    resampling = rsmp("cv", folds = 5),
                    measure = msr("classif.auc")
)
LH_ann_learner_eGFR$param_set$values <- LH_ann_eGFR$result_learner_param_vals
LH_ann_learner_eGFR$train(LH_train_task_eGFR)
LH_train_ann_pred_eGFR <- LH_ann_learner_eGFR$predict(LH_train_task_eGFR)
LH_test_ann_pred_eGFR <- LH_ann_learner_eGFR$predict(LH_test_task_eGFR)

##------------------Model building: Unstable and High-stable groups------------------
# Create training task
UH_train_task_eGFR <- as_task_classif(data_UH_train_std_num1, target = "CMDs")

# Create test task
UH_test_task_eGFR <- as_task_classif(data_UH_test_std_num1, target = "CMDs")

## Logistic Regression LR
# Select algorithm, predict output as probability
UH_lr_learner_eGFR <- lrn("classif.log_reg", predict_type = "prob")

# Train on training set
UH_lr_learner_eGFR$train(UH_train_task_eGFR)

# Predict on training set
UH_train_lr_pred_eGFR <- UH_lr_learner_eGFR$predict(UH_train_task_eGFR)

# Predict on test set
UH_test_lr_pred_eGFR <- UH_lr_learner_eGFR$predict(UH_test_task_eGFR)

## Random Forest RF
# Select algorithm, predict output as probability
UH_rf_learner_eGFR <- lrn("classif.ranger", predict_type = "prob")

# Select hyperparameters and their ranges for tuning
UH_rf_learner_eGFR$param_set$values <- list(
  num.trees = to_tune(10, 100), # Number of trees: controls forest size, affects model stability and computational efficiency.
  max.depth = to_tune(1, 5), # Maximum tree depth: limits tree depth to prevent overfitting.
  min.node.size = to_tune(p_int(1, 5)), # Minimum node size: sets minimum samples in a node, controls tree splitting. p_int() defines integer parameter range, suitable for newer mlr3tuning
  mtry = to_tune(1, 5), # Number of random features to try at each split: controls randomness, balances diversity and accuracy.
  sample.fraction = to_tune(0.3, 1) # Proportion of samples to draw for each tree
)

# Set random seed for reproducibility
set.seed(123)

# Perform tuning
UH_rf_eGFR <- tune(tuner = tnr("grid_search", resolution = 5), # Grid search; resolution = 5 means each parameter to tune is divided into 5 equally spaced values
                   task = UH_train_task_eGFR, # Specify tuning task
                   learner = UH_rf_learner_eGFR, # Specify learner to tune
                   resampling = rsmp("cv", folds = 5), # 5-fold cross-validation
                   measure = msr("classif.auc") # Model evaluation metric AUC
)

# Apply best hyperparameters to the learner
UH_rf_learner_eGFR$param_set$values <- UH_rf_eGFR$result_learner_param_vals

# Train on training set
UH_rf_learner_eGFR$train(UH_train_task_eGFR)

# Predict on training set
UH_train_rf_pred_eGFR <- UH_rf_learner_eGFR$predict(UH_train_task_eGFR)

# Predict on test set
UH_test_rf_pred_eGFR <- UH_rf_learner_eGFR$predict(UH_test_task_eGFR)

## XGBoost
UH_xgb_learner_eGFR <- lrn("classif.xgboost", predict_type = "prob")
UH_xgb_learner_eGFR$param_set$values <- list(
  nrounds = to_tune(100, 500), # Number of trees: controls number of boosting iterations, determines learning extent.
  max_depth = to_tune(1, 10), # Maximum tree depth: controls tree complexity to prevent overfitting.
  eta = to_tune(0.001, 0.05), # Learning rate: controls learning speed, affects convergence and stability.
  min_child_weight = to_tune(1, 10), # Minimum sum of instance weights in a child node: restricts splitting to prevent overfitting.
  subsample = to_tune(0.5, 1) # Subsample ratio: provides randomness, reduces overfitting risk, increases diversity.
)

set.seed(123)
UH_xgb_eGFR <- tune(tuner = tnr("grid_search", resolution = 5),
                    task = UH_train_task_eGFR,
                    learner = UH_xgb_learner_eGFR,
                    resampling = rsmp("cv", folds = 5),
                    measure = msr("classif.auc")
)
UH_xgb_learner_eGFR$param_set$values <- UH_xgb_eGFR$result_learner_param_vals
UH_xgb_learner_eGFR$train(UH_train_task_eGFR)
UH_train_xgb_pred_eGFR <- UH_xgb_learner_eGFR$predict(UH_train_task_eGFR)
UH_test_xgb_pred_eGFR <- UH_xgb_learner_eGFR$predict(UH_test_task_eGFR)

## Support Vector Machine SVM
UH_svm_learner_eGFR <- lrn("classif.svm", predict_type = "prob")
UH_svm_learner_eGFR$param_set$values <- list(
  type = "C-classification", # Classification problem
  kernel = "radial", # Radial Basis Function (RBF) kernel: handles complex nonlinear classification problems
  cost = to_tune(1, 10), # C or penalty parameter: penalty for misclassification
  gamma = to_tune(0.001, 1) # gamma parameter: defines width of RBF kernel, affects distribution of data mapped to high-dimensional feature space
)

set.seed(123)
UH_svm_eGFR <- tune(tuner = tnr("grid_search", resolution = 5),
                    task = UH_train_task_eGFR,
                    learner = UH_svm_learner_eGFR,
                    resampling = rsmp("cv", folds = 5),
                    measure = msr("classif.auc")
)
UH_svm_learner_eGFR$param_set$values <- UH_svm_eGFR$result_learner_param_vals
UH_svm_learner_eGFR$train(UH_train_task_eGFR)
UH_train_svm_pred_eGFR <- UH_svm_learner_eGFR$predict(UH_train_task_eGFR)
UH_test_svm_pred_eGFR <- UH_svm_learner_eGFR$predict(UH_test_task_eGFR)

## K-Nearest Neighbors KNN
UH_knn_learner_eGFR <- lrn("classif.kknn", predict_type = "prob")
UH_knn_learner_eGFR$param_set$values <- list(
  k = to_tune(100, 1000) # Number of nearest neighbors
)

set.seed(123)
UH_knn_eGFR <- tune(tuner = tnr("grid_search", resolution = 5),
                    task = UH_train_task_eGFR,
                    learner = UH_knn_learner_eGFR,
                    resampling = rsmp("cv", folds = 5),
                    measure = msr("classif.auc")
)
UH_knn_learner_eGFR$param_set$values <- UH_knn_eGFR$result_learner_param_vals
UH_knn_learner_eGFR$train(UH_train_task_eGFR)
UH_train_knn_pred_eGFR <- UH_knn_learner_eGFR$predict(UH_train_task_eGFR)
UH_test_knn_pred_eGFR <- UH_knn_learner_eGFR$predict(UH_test_task_eGFR)

## Decision Tree DT
UH_dt_learner_eGFR <- lrn("classif.rpart", predict_type = "prob")
UH_dt_learner_eGFR$param_set$values <- list(
  minsplit = to_tune(20, 50), # Minimum number of observations for a split
  minbucket = to_tune(10, 20), # Minimum number of observations in a terminal node
  cp = to_tune(0.01, 0.1), # Complexity parameter
  maxdepth = to_tune(3, 10) # Maximum tree depth
)

set.seed(123)
UH_dt_eGFR <- tune(tuner = tnr("grid_search", resolution = 5),
                   task = UH_train_task_eGFR,
                   learner = UH_dt_learner_eGFR,
                   resampling = rsmp("cv", folds = 5),
                   measure = msr("classif.auc")
)
UH_dt_learner_eGFR$param_set$values <- UH_dt_eGFR$result_learner_param_vals
UH_dt_learner_eGFR$train(UH_train_task_eGFR)
UH_train_dt_pred_eGFR <- UH_dt_learner_eGFR$predict(UH_train_task_eGFR)
UH_test_dt_pred_eGFR <- UH_dt_learner_eGFR$predict(UH_test_task_eGFR)

## Artificial Neural Network ANN
UH_ann_learner_eGFR <- lrn("classif.nnet", predict_type = "prob")
UH_ann_learner_eGFR$param_set$values <- list(
  size = to_tune(1, 10), # Number of neurons in the hidden layer, larger size -> stronger model capacity, can fit more complex nonlinear relationships
  decay = to_tune(0.001, 0.01) # Weight decay coefficient / L2 regularization strength, larger decay -> stronger penalty on weights, weights are shrunk -> less prone to overfitting, but too large can cause severe underfitting
)

set.seed(123)
UH_ann_eGFR <- tune(tuner = tnr("grid_search", resolution = 5),
                    task = UH_train_task_eGFR,
                    learner = UH_ann_learner_eGFR,
                    resampling = rsmp("cv", folds = 5),
                    measure = msr("classif.auc")
)
UH_ann_learner_eGFR$param_set$values <- UH_ann_eGFR$result_learner_param_vals
UH_ann_learner_eGFR$train(UH_train_task_eGFR)
UH_train_ann_pred_eGFR <- UH_ann_learner_eGFR$predict(UH_train_task_eGFR)
UH_test_ann_pred_eGFR <- UH_ann_learner_eGFR$predict(UH_test_task_eGFR)

###-------------------Model performance evaluation: 36 factors-------------------
##------------------Model performance evaluation: Low decline and High-stable groups------------------
# Create lists of model predictions for training and test sets
LH_train_preds_eGFR <- list(
  LR = LH_train_lr_pred_eGFR,
  RF = LH_train_rf_pred_eGFR,
  XGBoost = LH_train_xgb_pred_eGFR,
  SVM = LH_train_svm_pred_eGFR,
  KNN = LH_train_knn_pred_eGFR,
  DT = LH_train_dt_pred_eGFR,
  ANN = LH_train_ann_pred_eGFR
)

LH_test_preds_eGFR <- list(
  LR = LH_test_lr_pred_eGFR,
  RF = LH_test_rf_pred_eGFR,
  XGBoost = LH_test_xgb_pred_eGFR,
  SVM = LH_test_svm_pred_eGFR,
  KNN = LH_test_knn_pred_eGFR,
  DT = LH_test_dt_pred_eGFR,
  ANN = LH_test_ann_pred_eGFR
)

# Create empty data frames for storing performance evaluation results for training and test sets
LH_train_metrics_eGFR <- data.frame()
LH_test_metrics_eGFR <- data.frame()

# Calculate performance metrics for training set
for (model_name in names(LH_train_preds_eGFR)) {
  # Get prediction results for training set model
  pred_train <- LH_train_preds_eGFR[[model_name]]
  # Get predicted probabilities from training set model
  pred_prob_train <- pred_train$prob[, "Yes"]
  # Get actual class labels for training set
  true_class_train <- data_LH_train_std_num1$CMDs
  true_class_train <- ifelse(true_class_train == "Yes", 1, 0)
  roc_train <- roc(true_class_train, pred_prob_train)
  auc_train <- roc_train$auc
  ci_train <- ci(roc_train)
  # Combine AUC and 95%CI, keep three decimal places
  auc_with_ci_train <- sprintf("%.3f\n(%.3f-%.3f)", auc_train, ci_train[1], ci_train[3])
  
  ## Calculate best threshold for training set
  # Define threshold range
  threshold <- seq(0, 1, by = 0.001)
  # Calculate performance metrics for different thresholds
  metrics_list_train <- sapply(threshold, function(t) {
    calculate_metrics(pred_prob_train, true_class_train, t)
  }, simplify = F)
  distances <- sapply(metrics_list_train, function(metrics) {
    sqrt((1-metrics$Sensitivity)^2 + (1-metrics$Specificity)^2)
  })
  # Find best threshold for training set
  best_threshold_train <- threshold[which.min(distances)]
  # Calculate performance metrics for training set using the best threshold
  best_metrics_train <- calculate_metrics(pred_prob_train, true_class_train, best_threshold_train)
  # Calculate Brier score
  brier_score_train <- mean((pred_prob_train - true_class_train)^2)
  # Bootstrap CI
  set.seed(123)
  ci_train_metrics <- bootstrap_ci(pred_prob_train, true_class_train, best_threshold_train, B = 1000)
  Sensitivity_CI <- format_ci(best_metrics_train$Sensitivity, ci_train_metrics[, "Sensitivity"])
  Specificity_CI <- format_ci(best_metrics_train$Specificity, ci_train_metrics[, "Specificity"])
  PPV_CI <- format_ci(best_metrics_train$PPV, ci_train_metrics[, "PPV"])
  NPV_CI <- format_ci(best_metrics_train$NPV, ci_train_metrics[, "NPV"])
  CCR_CI <- format_ci(best_metrics_train$CCR, ci_train_metrics[, "CCR"])
  F1_score_CI <- format_ci(best_metrics_train$F1_score, ci_train_metrics[, "F1_score"])
  Brier_score_CI <- format_ci(brier_score_train, ci_train_metrics[, "Brier_score"])
  # Summarize training set model results
  train_metrics_result <- data.frame(
    Model = model_name,
    Dataset = "Training cohort",
    AUC_CI = auc_with_ci_train,
    Threshold = round(best_threshold_train, 3),
    Sensitivity_CI = Sensitivity_CI,
    Specificity_CI = Specificity_CI,
    PPV_CI = PPV_CI,
    NPV_CI = NPV_CI,
    CCR_CI = CCR_CI,
    F1_score_CI = F1_score_CI,
    Brier_score_CI = Brier_score_CI
  )
  # Add results for each model for training set to data frame
  LH_train_metrics_eGFR <- rbind(LH_train_metrics_eGFR, train_metrics_result)
}

# Calculate performance metrics for test set
for (model_name in names(LH_test_preds_eGFR)) {
  # Get prediction results for test set model
  pred_test <- LH_test_preds_eGFR[[model_name]]
  # Get predicted probabilities from test set model
  pred_prob_test <- pred_test$prob[, "Yes"]
  # Get actual class labels for test set
  true_class_test <- data_LH_test_std_num1$CMDs
  true_class_test <- ifelse(true_class_test == "Yes", 1, 0)
  # Calculate test set AUC and 95%CI
  roc_test <- roc(true_class_test, pred_prob_test)
  auc_test <- roc_test$auc
  ci_test <- ci(roc_test)
  # Combine AUC and 95%CI, keep three decimal places
  auc_with_ci_test <- sprintf("%.3f\n(%.3f-%.3f)", auc_test, ci_test[1], ci_test[3])
  # Get best threshold from training set
  best_threshold_train <- LH_train_metrics_eGFR[LH_train_metrics_eGFR$Model == model_name, "Threshold"]
  # Calculate test set performance using training set's best threshold
  best_metrics_test <- calculate_metrics(pred_prob_test, true_class_test, best_threshold_train)
  # Calculate test set Brier score
  brier_score_test <- mean((pred_prob_test - true_class_test)^2)
  # Bootstrap CI
  set.seed(123)
  ci_test_metrics <- bootstrap_ci(pred_prob_test, true_class_test, best_threshold_train, B = 1000)
  Sensitivity_CI <- format_ci(best_metrics_test$Sensitivity, ci_test_metrics[, "Sensitivity"])
  Specificity_CI <- format_ci(best_metrics_test$Specificity, ci_test_metrics[, "Specificity"])
  PPV_CI <- format_ci(best_metrics_test$PPV, ci_test_metrics[, "PPV"])
  NPV_CI <- format_ci(best_metrics_test$NPV, ci_test_metrics[, "NPV"])
  CCR_CI <- format_ci(best_metrics_test$CCR, ci_test_metrics[, "CCR"])
  F1_score_CI <- format_ci(best_metrics_test$F1_score, ci_test_metrics[, "F1_score"])
  Brier_score_CI <- format_ci(brier_score_test, ci_test_metrics[, "Brier_score"])
  # Summarize test set model results into data frame
  test_metrics_result <- data.frame(
    Model = model_name,
    Dataset = "Testing cohort",
    AUC_CI = auc_with_ci_test,
    Threshold = round(best_threshold_train, 3), # Use training set's best threshold
    Sensitivity_CI = Sensitivity_CI,
    Specificity_CI = Specificity_CI,
    PPV_CI = PPV_CI,
    NPV_CI = NPV_CI,
    CCR_CI = CCR_CI,
    F1_score_CI = F1_score_CI,
    Brier_score_CI = Brier_score_CI
  )
  # Combine results for each model for test set
  LH_test_metrics_eGFR <- rbind(LH_test_metrics_eGFR, test_metrics_result)
}

##------------------Model performance evaluation: Unstable and High-stable groups------------------
# Create lists of model predictions for training and test sets
UH_train_preds_eGFR <- list(
  LR = UH_train_lr_pred_eGFR,
  RF = UH_train_rf_pred_eGFR,
  XGBoost = UH_train_xgb_pred_eGFR,
  SVM = UH_train_svm_pred_eGFR,
  KNN = UH_train_knn_pred_eGFR,
  DT = UH_train_dt_pred_eGFR,
  ANN = UH_train_ann_pred_eGFR
)

UH_test_preds_eGFR <- list(
  LR = UH_test_lr_pred_eGFR,
  RF = UH_test_rf_pred_eGFR,
  XGBoost = UH_test_xgb_pred_eGFR,
  SVM = UH_test_svm_pred_eGFR,
  KNN = UH_test_knn_pred_eGFR,
  DT = UH_test_dt_pred_eGFR,
  ANN = UH_test_ann_pred_eGFR
)

# Create empty data frames for storing performance evaluation results for training and test sets
UH_train_metrics_eGFR <- data.frame()
UH_test_metrics_eGFR <- data.frame()

# Calculate performance metrics for training set
for (model_name in names(UH_train_preds_eGFR)) {
  # Get prediction results for training set model
  pred_train <- UH_train_preds_eGFR[[model_name]]
  # Get predicted probabilities from training set model
  pred_prob_train <- pred_train$prob[, "Yes"]
  # Get actual class labels for training set
  true_class_train <- pred_train$truth
  true_class_train <- ifelse(true_class_train == "Yes", 1, 0)
  roc_train <- roc(true_class_train, pred_prob_train)
  auc_train <- roc_train$auc
  ci_train <- ci(roc_train)
  # Combine AUC and 95%CI, keep three decimal places
  auc_with_ci_train <- sprintf("%.3f\n(%.3f-%.3f)", auc_train, ci_train[1], ci_train[3])
  
  ## Calculate best threshold for training set
  # Define threshold range
  threshold <- seq(0, 1, by = 0.001)
  # Calculate performance metrics for different thresholds
  metrics_list_train <- sapply(threshold, function(t) {
    calculate_metrics(pred_prob_train, true_class_train, t)
  }, simplify = F)
  distances <- sapply(metrics_list_train, function(metrics) {
    sqrt((1-metrics$Sensitivity)^2 + (1-metrics$Specificity)^2)
  })
  # Find best threshold for training set
  best_threshold_train <- threshold[which.min(distances)]
  # Calculate performance metrics for training set using the best threshold
  best_metrics_train <- calculate_metrics(pred_prob_train, true_class_train, best_threshold_train)
  # Calculate Brier score
  brier_score_train <- mean((pred_prob_train - true_class_train)^2)
  # Bootstrap CI
  set.seed(123)
  ci_train_metrics <- bootstrap_ci(pred_prob_train, true_class_train, best_threshold_train, B = 1000)
  Sensitivity_CI <- format_ci(best_metrics_train$Sensitivity, ci_train_metrics[, "Sensitivity"])
  Specificity_CI <- format_ci(best_metrics_train$Specificity, ci_train_metrics[, "Specificity"])
  PPV_CI <- format_ci(best_metrics_train$PPV, ci_train_metrics[, "PPV"])
  NPV_CI <- format_ci(best_metrics_train$NPV, ci_train_metrics[, "NPV"])
  CCR_CI <- format_ci(best_metrics_train$CCR, ci_train_metrics[, "CCR"])
  F1_score_CI <- format_ci(best_metrics_train$F1_score, ci_train_metrics[, "F1_score"])
  Brier_score_CI <- format_ci(brier_score_train, ci_train_metrics[, "Brier_score"])
  # Summarize training set model results
  train_metrics_result <- data.frame(
    Model = model_name,
    Dataset = "Training cohort",
    AUC_CI = auc_with_ci_train,
    Threshold = round(best_threshold_train, 3),
    Sensitivity_CI = Sensitivity_CI,
    Specificity_CI = Specificity_CI,
    PPV_CI = PPV_CI,
    NPV_CI = NPV_CI,
    CCR_CI = CCR_CI,
    F1_score_CI = F1_score_CI,
    Brier_score_CI = Brier_score_CI
  )
  # Add results for each model for training set to data frame
  UH_train_metrics_eGFR <- rbind(UH_train_metrics_eGFR, train_metrics_result)
}

# Calculate performance metrics for test set
for (model_name in names(UH_test_preds_eGFR)) {
  # Get prediction results for test set model
  pred_test <- UH_test_preds_eGFR[[model_name]]
  # Get predicted probabilities from test set model
  pred_prob_test <- pred_test$prob[, "Yes"]
  # Get actual class labels for test set
  true_class_test <- pred_test$truth
  true_class_test <- ifelse(true_class_test == "Yes", 1, 0)
  # Calculate test set AUC and 95%CI
  roc_test <- roc(true_class_test, pred_prob_test)
  auc_test <- roc_test$auc
  ci_test <- ci(roc_test)
  # Combine AUC and 95%CI, keep three decimal places
  auc_with_ci_test <- sprintf("%.3f\n(%.3f-%.3f)", auc_test, ci_test[1], ci_test[3])
  # Get best threshold from training set
  best_threshold_train <- UH_train_metrics_eGFR[UH_train_metrics_eGFR$Model == model_name, "Threshold"]
  # Calculate test set performance using training set's best threshold
  best_metrics_test <- calculate_metrics(pred_prob_test, true_class_test, best_threshold_train)
  # Calculate test set Brier score
  brier_score_test <- mean((pred_prob_test - true_class_test)^2)
  # Bootstrap CI
  set.seed(123)
  ci_test_metrics <- bootstrap_ci(pred_prob_test, true_class_test, best_threshold_train, B = 1000)
  Sensitivity_CI <- format_ci(best_metrics_test$Sensitivity, ci_test_metrics[, "Sensitivity"])
  Specificity_CI <- format_ci(best_metrics_test$Specificity, ci_test_metrics[, "Specificity"])
  PPV_CI <- format_ci(best_metrics_test$PPV, ci_test_metrics[, "PPV"])
  NPV_CI <- format_ci(best_metrics_test$NPV, ci_test_metrics[, "NPV"])
  CCR_CI <- format_ci(best_metrics_test$CCR, ci_test_metrics[, "CCR"])
  F1_score_CI <- format_ci(best_metrics_test$F1_score, ci_test_metrics[, "F1_score"])
  Brier_score_CI <- format_ci(brier_score_test, ci_test_metrics[, "Brier_score"])
  # Summarize test set model results into data frame
  test_metrics_result <- data.frame(
    Model = model_name,
    Dataset = "Testing cohort",
    AUC_CI = auc_with_ci_test,
    Threshold = round(best_threshold_train, 3), # Use training set's best threshold
    Sensitivity_CI = Sensitivity_CI,
    Specificity_CI = Specificity_CI,
    PPV_CI = PPV_CI,
    NPV_CI = NPV_CI,
    CCR_CI = CCR_CI,
    F1_score_CI = F1_score_CI,
    Brier_score_CI = Brier_score_CI
  )
  # Combine results for each model for test set
  UH_test_metrics_eGFR <- rbind(UH_test_metrics_eGFR, test_metrics_result)
}

# Combine model performance results for both groups
test_metrics_eGFR <- rbind(LH_test_metrics_eGFR, UH_test_metrics_eGFR)


##------------------------------Export various tables to Word------------------------------
# Create a blank Word document
doc <- read_docx()

# Convert data frame to flextable
Table <- flextable(Table)

# Add table to Word document
doc <- body_add_par(doc, value = "Table x") # Add a blank line between tables for separation
doc <- body_add_flextable(doc, value = Table)

# Export Word document
print(doc, target = "Table x.docx")
