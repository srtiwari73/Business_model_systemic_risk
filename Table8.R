rm(list = ls())
# ==============================================================================
# SCRIPT: Replicate Table 8 & Table A.4 (Updated with Model_lag & Type)
# ==============================================================================
# Description:
#   1. Prepares data: Lags controls, creates 'Model_lag', includes 'Type'.
#   2. Saves the prepared dataset.
#   3. Estimates Multinomial Logit (Step 1) including 'Type'.
#   4. Calculates Dubin-McFadden Inverse Mills Ratios (IMRs).
#   5. Estimates Systemic Risk Regressions (Step 2) using 'Model_lag' dummies.
# ==============================================================================

# 1. Setup & Package Load
# ------------------------------------------------------------------------------
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("nnet")) install.packages("nnet")       # For Multinomial Logit
if (!require("plm")) install.packages("plm")         # For Panel Data Linear Models
if (!require("lmtest")) install.packages("lmtest")   # For Robust SE
if (!require("sandwich")) install.packages("sandwich") # For Robust SE
library(tidyverse)
library(nnet)
library(plm)
library(lmtest)
library(sandwich)

# 2. Configurable Parameters & Crisis Years
# ------------------------------------------------------------------------------
INPUT_FILE <- "Indian_banks_data.csv"
OUTPUT_TABLE_8 <- "Table_8_Step2_Results_Final.csv"
OUTPUT_TABLE_A4 <- "Table_A4_Step1_Results_Final.csv"
OUTPUT_DATA_PREP <- "data_prepared_for_analysis.csv"

# Crisis Years defined from User Image
years_endogenous <- c(2015, 2016, 2017, 2018, 2019)
years_exogenous  <- c(2007, 2008, 2009, 2020)

# 3. Data Loading & Preparation
# ------------------------------------------------------------------------------
if (!file.exists(INPUT_FILE)) stop("Input file not found.")
df <- read.csv(INPUT_FILE)

# A. Basic Cleaning
df_clean <- df %>%
  mutate(
    Year = as.numeric(Year),
    bank_id = as.character(bank_id),
    Model = str_trim(as.character(Model)),
    
    # [cite_start]Flip signs for Risk Measures [cite: 512-513]
    MES_pos = -1 * as.numeric(MES),
    ACoVar_pos = -1 * as.numeric(ACoVaR),
    CoVar_pos = -1 * as.numeric(CoVaR)
  )

# B. Orthogonalize SIZE
# Including 'Type' in the orthogonalization regression as a control
size_model <- lm(Size ~ Lev + Cost_TI + NPA + DSIB + RWA_TA + WPS + ROE, 
                 data = df_clean, na.action = na.exclude)
df_clean$SIZE_ortho <- residuals(size_model)

# C. Create Lags, Crisis Dummies, and Model_lag
df_prepared <- df_clean %>%
  arrange(bank_id, Year) %>%
  group_by(bank_id) %>%
  mutate(
    # Crisis Dummies
    INSTABILITY = ifelse(Year %in% c(years_endogenous, years_exogenous), 1, 0),
    ENDOGENOUS  = ifelse(Year %in% years_endogenous, 1, 0),
    EXOGENOUS   = ifelse(Year %in% years_exogenous, 1, 0),
    
    # --- NEW: Explicit Model Lag ---
    Model_lag   = dplyr::lag(Model),
    
    # 1-Year Lags for dependent and controls
    MES_lag      = dplyr::lag(MES_pos),
    ACoVar_lag   = dplyr::lag(ACoVar_pos),
    CoVar_lag    = dplyr::lag(CoVar_pos),
    SIZE_lag     = dplyr::lag(SIZE_ortho),
    Lev_lag      = dplyr::lag(Lev),
    Cost_TI_lag  = dplyr::lag(Cost_TI),
    NPA_lag      = dplyr::lag(NPA),
    DSIB_lag     = dplyr::lag(DSIB),
    RWA_TA_lag   = dplyr::lag(RWA_TA),
    WPS_lag      = dplyr::lag(WPS),
    ROE_lag      = dplyr::lag(ROE),
    
    # --- NEW: Include Type Lag ---
    Type_lag     = dplyr::lag(Type),
    
    # Lags for Exclusion Restrictions
    Acquirer_lag = dplyr::lag(Acquirer),
    Target_lag   = dplyr::lag(Target),
    Distance_lag = dplyr::lag(Distance) 
  ) %>%
  ungroup() %>%
  filter(!is.na(MES_lag)) %>%  # Remove first year (NAs)
  filter(!is.na(Model_lag))    # Ensure we have a previous model

# D. Save Prepared Data
write.csv(df_prepared, OUTPUT_DATA_PREP, row.names = FALSE)
message("Prepared dataframe saved to: ", OUTPUT_DATA_PREP)

# 4. Heckman Step 1: Multinomial Logit (Table A.4)
# ------------------------------------------------------------------------------
message("Estimating Step 1 (Multinomial Logit)...")

# Set Reference Level (BM5)
df_prepared$Model <- as.factor(df_prepared$Model)
df_prepared$Model <- relevel(df_prepared$Model, ref = "BM4") 

# Update formula to include Type_lag
step1_formula <- Model ~ Acquirer_lag + Target_lag + Type_lag + Distance_lag + 
  SIZE_lag + Lev_lag + Cost_TI_lag + NPA_lag + 
  DSIB_lag + RWA_TA_lag + WPS_lag + ROE_lag 
   

mnl_model <- multinom(step1_formula, data = df_prepared, trace = FALSE)

# --- Calculate P-values and Save Table A.4 ---
sum_mnl <- summary(mnl_model)
coeffs  <- sum_mnl$coefficients
std_err <- sum_mnl$standard.errors

# Calculate Z-statistics and P-values (2-tailed)
z_stats <- coeffs / std_err
p_values <- (1 - pnorm(abs(z_stats), 0, 1)) * 2

# Flatten to DF
bm_levels <- rownames(coeffs)
table_a4_list <- list()

for(bm in bm_levels) {
  df_temp <- data.frame(
    Outcome_Model = bm,
    Variable = colnames(coeffs),
    Coefficient = coeffs[bm, ],
    Std_Error = std_err[bm, ],
    P_Value = p_values[bm, ]
  )
  table_a4_list[[bm]] <- df_temp
}

final_table_a4 <- bind_rows(table_a4_list)
write.csv(final_table_a4, OUTPUT_TABLE_A4, row.names = FALSE)
message("Table A.4 saved to: ", OUTPUT_TABLE_A4)


# 5. Calculate IMRs (Inverse Mills Ratios)
# ------------------------------------------------------------------------------
probs <- fitted(mnl_model)
bm_names <- colnames(probs)

for (bm in bm_names) {
  p_col <- probs[, bm]
  p_col <- pmax(p_col, 1e-6)
  p_col <- pmin(p_col, 1 - 1e-6)
  
  # Dubin-McFadden term
  imr_val <- (p_col * log(p_col)) / (1 - p_col)
  
  col_name <- paste0("IMR_", bm)
  df_prepared[[col_name]] <- imr_val
}

# 6. Heckman Step 2: Systemic Risk (Table 8)
# ------------------------------------------------------------------------------
message("Estimating Step 2 (Panel Regressions)...")

# --- NEW: Generate Dummies from Model_lag ---
df_prepared <- df_prepared %>%
  mutate(
    BM1_lag = ifelse(Model_lag == "BM1", 1, 0),
    BM2_lag = ifelse(Model_lag == "BM2", 1, 0),
    BM3_lag = ifelse(Model_lag == "BM3", 1, 0),
    # BM4_lag = ifelse(Model_lag == "BM4", 1, 0)
    # BM4 is reference
  )

write.csv(df_prepared, OUTPUT_DATA_PREP, row.names = FALSE)
message("Prepared dataframe saved to: ", OUTPUT_DATA_PREP)

# Base controls string (Added Type_lag)
base_controls <- paste(
  "BM1_lag + BM2_lag + BM3_lag",
  "SIZE_lag + Lev_lag + Cost_TI_lag + NPA_lag + DSIB_lag + RWA_TA_lag + WPS_lag + ROE_lag",
  "IMR_BM1 + IMR_BM2 + IMR_BM3", 
  sep = " + "
)

# Helper to format robust results
get_robust_res <- function(model_obj, col_prefix) {
  robust_test <- coeftest(model_obj, vcov = vcovHC(model_obj, method = "arellano", type = "HC1"))
  
  res <- data.frame(
    Term = rownames(robust_test),
    Est = robust_test[, 1],
    SE  = robust_test[, 2],
    PVal = robust_test[, 4]
  )
  
  colnames(res) <- c("Term", 
                     paste0(col_prefix, "_Coeff"), 
                     paste0(col_prefix, "_SE"), 
                     paste0(col_prefix, "_Pval"))
  return(res)
}

# --- Model 1: MES ~ Instability ---
f1 <- as.formula(paste("MES_pos ~ MES_lag + INSTABILITY +", base_controls))
mod1 <- plm(f1, data = df_prepared, index = c("bank_id", "Year"), model = "pooling")
res1 <- get_robust_res(mod1, "MES_Base")

# --- Model 2: MES ~ Split Crisis ---
f2 <- as.formula(paste("MES_pos ~ MES_lag + ENDOGENOUS + EXOGENOUS +", base_controls))
mod2 <- plm(f2, data = df_prepared, index = c("bank_id", "Year"), model = "pooling")
res2 <- get_robust_res(mod2, "MES_Split")

# --- Model 3: ACoVar ~ Instability ---
f3 <- as.formula(paste("ACoVar_pos ~ ACoVar_lag + INSTABILITY +", base_controls))
mod3 <- plm(f3, data = df_prepared, index = c("bank_id", "Year"), model = "pooling")
res3 <- get_robust_res(mod3, "ACoVar_Base")

# --- Model 4: ACoVar ~ Split Crisis ---
f4 <- as.formula(paste("ACoVar_pos ~ ACoVar_lag + ENDOGENOUS + EXOGENOUS +", base_controls))
mod4 <- plm(f4, data = df_prepared, index = c("bank_id", "Year"), model = "pooling")
res4 <- get_robust_res(mod4, "ACoVar_Split")

# --- Model 5: CoVar ~ Instability ---
f5 <- as.formula(paste("CoVar_pos ~ CoVar_lag + INSTABILITY +", base_controls))
mod5 <- plm(f5, data = df_prepared, index = c("bank_id", "Year"), model = "pooling")
res5 <- get_robust_res(mod5, "CoVar_Base")

# --- Model 6: CoVar ~ Split Crisis ---
f6 <- as.formula(paste("CoVar_pos ~ CoVar_lag + ENDOGENOUS + EXOGENOUS +", base_controls))
mod6 <- plm(f6, data = df_prepared, index = c("bank_id", "Year"), model = "pooling")
res6 <- get_robust_res(mod6, "CoVar_Split")

# 7. Merge and Save
# ------------------------------------------------------------------------------
final_table_8 <- res1 %>%
  full_join(res2, by = "Term") %>%
  full_join(res3, by = "Term") %>%
  full_join(res4, by = "Term") %>%
  full_join(res5, by = "Term") %>%
  full_join(res6, by = "Term")

# Format row names
final_table_8$Term <- gsub("_lag", " (t-1)", final_table_8$Term)

write.csv(final_table_8, OUTPUT_TABLE_8, row.names = FALSE)

message("Table 8 generation complete.")
message("Output saved to: ", OUTPUT_TABLE_8)

## Get R-sq and obs
r2_1 <- summary(mod1)$r.squared["rsq"]
obs_1 <- nobs(mod1)
r2_2 <- summary(mod2)$r.squared["rsq"]
obs_2 <- nobs(mod2)
r2_3 <- summary(mod3)$r.squared["rsq"]
obs_3 <- nobs(mod3)
r2_4 <- summary(mod4)$r.squared["rsq"]
obs_4 <- nobs(mod4)
r2_5 <- summary(mod5)$r.squared["rsq"]
obs_5 <- nobs(mod5)
r2_6 <- summary(mod6)$r.squared["rsq"]
obs_6 <- nobs(mod6)
