rm(list = ls())
# ==============================================================================
# SCRIPT: Replicate Table 10 - PSM Analysis of Migrations
# ==============================================================================
# Description:
#   1. Identifies specific migration events (BM1->BM2 and BM1->BM3).
#   2. Calculates outcome variables (Levels and Changes over time windows).
#   3. Performs Propensity Score Matching (Probit + Nearest Neighbor).
#   4. Estimates ATET (Difference in Means) for Systemic Risk (MES & ACoVar).
# ==============================================================================

# 1. Setup & Package Load
# ------------------------------------------------------------------------------
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("MatchIt")) install.packages("MatchIt")     # For PSM
if (!require("lmtest")) install.packages("lmtest")       # For Robust SE
if (!require("sandwich")) install.packages("sandwich")   # For Robust SE
library(tidyverse)
library(MatchIt)
library(lmtest)
library(sandwich)

# 2. Configurable Parameters
# ------------------------------------------------------------------------------
INPUT_DATA <- "data_prepared_for_analysis.csv" # From previous step
OUTPUT_FILE <- "Table_10_PSM_Results.csv"

# 3. Data Loading & Outcome Calculation
# ------------------------------------------------------------------------------
if (!file.exists(INPUT_DATA)) stop("Input file not found.")
df <- read.csv(INPUT_DATA)

# Ensure data is sorted for lead/lag calculation
df <- df %>%
  arrange(bank_id, Year) %>%
  group_by(bank_id) %>%
  mutate(
    # Ensure Risk Measures are present (Positive values from previous steps)
    MES = MES_pos,
    ACoVar = ACoVar_pos,
    
    # Calculate Future/Past Levels for Windows
    # t is current row
    MES_t_plus_1 = dplyr::lead(MES, 1),
    MES_t_plus_2 = dplyr::lead(MES, 2),
    MES_t_minus_1 = dplyr::lag(MES, 1),
    
    ACoVar_t_plus_1 = dplyr::lead(ACoVar, 1),
    ACoVar_t_plus_2 = dplyr::lead(ACoVar, 2),
    ACoVar_t_minus_1 = dplyr::lag(ACoVar, 1),
    
    # Calculate Changes (Deltas) for Time Windows [cite: 699-703]
    # (t; t+1)
    d_MES_t_t1 = MES_t_plus_1 - MES,
    d_ACoVar_t_t1 = ACoVar_t_plus_1 - ACoVar,
    
    # (t-1; t)
    d_MES_tm1_t = MES - MES_t_minus_1,
    d_ACoVar_tm1_t = ACoVar - ACoVar_t_minus_1,
    
    # (t-1; t+1)
    d_MES_tm1_t1 = MES_t_plus_1 - MES_t_minus_1,
    d_ACoVar_tm1_t1 = ACoVar_t_plus_1 - ACoVar_t_minus_1,
    
    # (t; t+2)
    d_MES_t_t2 = MES_t_plus_2 - MES,
    d_ACoVar_t_t2 = ACoVar_t_plus_2 - ACoVar
  ) %>%
  ungroup()

# 4. Helper Function: Run PSM and Estimate Effects
# ------------------------------------------------------------------------------
run_psm_analysis <- function(data, source_bm, target_bm, panel_name) {
  
  message(paste("Processing:", panel_name, "(", source_bm, "->", target_bm, ")"))
  
  # A. Define Treatment and Control Groups
  # Treatment: Migrated from Source to Target at time t
  # Control:   Stayed in Source (Source -> Source) at time t
  # Note: Model_lag is Model at t-1. Model is Model at t.
  
  psm_data <- data %>%
    filter(Model_lag == source_bm) %>% # Everyone starts in Source
    mutate(
      Treatment = case_when(
        Model == target_bm ~ 1, # Migrated
        Model == source_bm ~ 0, # Stayed
        TRUE ~ NA_real_         # Went somewhere else (exclude)
      )
    ) %>%
    filter(!is.na(Treatment)) %>%
    # Ensure no NAs in Probit predictors
    drop_na(SIZE_lag, Lev_lag, Cost_TI_lag, NPA_lag, DSIB_lag, RWA_TA_lag, 
            WPS_lag, ROE_lag, Type_lag, Acquirer_lag, Target_lag, Distance_lag)
  
  if (sum(psm_data$Treatment == 1) < 5) {
    warning(paste("Not enough treatment observations for", panel_name))
    return(NULL)
  }
  
  # B. Propensity Score Matching (Probit) 
  # Formula based on Eq 7 / Table A.4 variables
  psm_formula <- Treatment ~ SIZE_lag + Lev_lag + Cost_TI_lag + NPA_lag + 
    DSIB_lag + RWA_TA_lag + WPS_lag + ROE_lag + Type_lag +
    Acquirer_lag + Target_lag + Distance_lag
  
  set.seed(123) # Reproducibility
  match_out <- matchit(psm_formula, data = psm_data, 
                       method = "nearest", distance = "glm", link = "probit",
                       replace = TRUE, ratio = 1)
  
  # Extract Matched Data
  matched_data <- match.data(match_out)
  
  # C. Estimate ATET for Each Outcome Window
  # List of outcomes to test
  outcomes <- list(
    # Format: Label = Variable Name
    "At t"         = c("ACoVar", "MES"),
    "At t+1"       = c("ACoVar_t_plus_1", "MES_t_plus_1"),
    "(t; t+1)"     = c("d_ACoVar_t_t1", "d_MES_t_t1"),
    "(t-1; t)"     = c("d_ACoVar_tm1_t", "d_MES_tm1_t"),
    "(t-1; t+1)"   = c("d_ACoVar_tm1_t1", "d_MES_tm1_t1"),
    "(t; t+2)"     = c("d_ACoVar_t_t2", "d_MES_t_t2")
  )
  
  results_list <- list()
  
  for (window_label in names(outcomes)) {
    vars <- outcomes[[window_label]]
    acovar_var <- vars[1]
    mes_var <- vars[2]
    
    # Function to fit model and extract stats
    fit_outcome <- function(var_name) {
      # Remove NAs for specific time window (e.g. t+2 might be missing)
      reg_data <- matched_data %>% filter(!is.na(.data[[var_name]]))
      
      if(nrow(reg_data) < 10) return(c(NA, NA, NA, 0))
      
      # Weighted regression on matched data
      fit <- lm(as.formula(paste(var_name, "~ Treatment")), 
                data = reg_data, weights = weights)
      
      # Robust SE
      co <- coeftest(fit, vcov = vcovHC(fit, type = "HC1"))
      
      # Return Coeff, Pval, Obs (Treatment is 2nd row)
      return(c(
        Est = co[2, 1],
        SE  = co[2, 2],
        Pval = co[2, 4],
        Obs = nrow(reg_data)
      ))
    }
    
    # Fit ACoVar
    res_acovar <- fit_outcome(acovar_var)
    # Fit MES
    res_mes <- fit_outcome(mes_var)
    
    # Combine into row
    row_df <- data.frame(
      Panel = panel_name,
      Time_Window = window_label,
      
      ACoVar_Coeff = res_acovar[1],
      ACoVar_SE    = res_acovar[2],
      ACoVar_Pval  = res_acovar[3],
      ACoVar_Obs   = res_acovar[4],
      
      MES_Coeff    = res_mes[1],
      MES_SE       = res_mes[2],
      MES_Pval     = res_mes[3],
      MES_Obs      = res_mes[4]
    )
    results_list[[window_label]] <- row_df
  }
  
  return(bind_rows(results_list))
}

# 5. Execute Analysis
# ------------------------------------------------------------------------------

# Panel A: Migration BM1 -> BM2 (Focused Retail to Diversified Type 2)
res_panel_a <- run_psm_analysis(df, 
                                source_bm = "BM1", 
                                target_bm = "BM2", 
                                panel_name = "Panel A (BM1 -> BM2)")

### Not enough obs for BM1->BM4
# Panel B: Migration BM3 -> BM1 (Diversified Type 2 to Focused Retail)
res_panel_b <- run_psm_analysis(df, 
                                source_bm = "BM1", 
                                target_bm = "BM4", 
                                panel_name = "Panel B (BM1 -> BM4)")

# Panel B: Migration BM2 -> BM1 (Diversified Type 2 to Focused Retail)
res_panel_c <- run_psm_analysis(df, 
                                source_bm = "BM2", 
                                target_bm = "BM1", 
                                panel_name = "Panel B (BM2 -> BM1)")

### Not enough obs for BM2->BM3
# Panel B: Migration BM2 -> BM4 (Diversified Type 2 to Focused Retail)
res_panel_d <- run_psm_analysis(df, 
                                source_bm = "BM2", 
                                target_bm = "BM4", 
                                panel_name = "Panel B (BM2 -> BM4)")

#### Not enough obs for BM3->BM1
### Not enough obs for BM3->BM2
### Not enough obs for BM3-> BM4
# Panel B: Migration BM4 -> BM1 (Diversified Type 2 to Focused Retail)
res_panel_e <- run_psm_analysis(df, 
                                source_bm = "BM4", 
                                target_bm = "BM1", 
                                panel_name = "Panel B (BM4 -> BM1)")

# Panel B: Migration BM4 -> BM2 (Diversified Type 2 to Focused Retail)
res_panel_f <- run_psm_analysis(df, 
                                source_bm = "BM4", 
                                target_bm = "BM2", 
                                panel_name = "Panel B (BM4 -> BM2)")

### Not enough obs for BM4->BM3

# 6. Combine and Save
# ------------------------------------------------------------------------------
final_table_10 <- bind_rows(res_panel_a, res_panel_b, res_panel_c, res_panel_d, res_panel_e, res_panel_f)

# Formatting for readability (Rounding)
final_table_10 <- final_table_10 %>%
  mutate(across(where(is.numeric), ~ round(., 4)))

write.csv(final_table_10, OUTPUT_FILE, row.names = FALSE)

# Display Preview
print(final_table_10)

message("----------------------------------------------------------------")
message("Table 10 (PSM Analysis) generated successfully.")
message("Output saved to: ", OUTPUT_FILE)
message("----------------------------------------------------------------")
