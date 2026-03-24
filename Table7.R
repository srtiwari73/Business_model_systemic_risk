rm(list = ls())
# ==============================================================================
# SCRIPT: Replicate Table 7 - Bank Systemic Risk and Migrations
# ==============================================================================
# Description:
#   This script classifies banks into "Migrating" (changed model at least once)
#   and "Non-Migrating" (never changed model).
#   It calculates descriptive statistics (Obs, Mean) for both groups and performs
#   a Wilcoxon-Mann-Whitney test to compare them.
# ==============================================================================

# 1. Setup & Package Load
# ------------------------------------------------------------------------------
if (!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)

# 2. Configurable Parameters
# ------------------------------------------------------------------------------
INPUT_FILE <- "Indian_banks_data.csv"
OUTPUT_FILE <- "Table_7_Migrations_Comparison.csv"

# 3. Data Loading
# ------------------------------------------------------------------------------
if (!file.exists(INPUT_FILE)) stop("Input file not found.")
df <- read.csv(INPUT_FILE)

# 4. Data Preparation & Classification
# ------------------------------------------------------------------------------
# A. Rename variables to match Table 7
df_proc <- df %>%
  rename(
    ACoVar       = ACoVaR,
    CoVar       = CoVaR,
    SIZE         = Size,
    LEVERAGE     = Lev,
    COST_INCOME  = Cost_TI,
    RWA_TA       = RWA_TA,
    WPS          = WPS,
    MES          = MES
  ) %>%
  mutate(
    Model = str_trim(as.character(Model))
  )

# B. Check for 'MBV'
if (!"MBV" %in% names(df_proc)) {
  warning("Variable 'MBV' not found. It will be excluded from Table 7.")
}

# C. Classify Banks as Migrating vs Non-Migrating [cite: 542]
#    "Data refers to banks that change their business model at least once...
#     and banks that never change their business model."
bank_classification <- df_proc %>%
  group_by(bank_id) %>%
  summarise(
    unique_models = n_distinct(Model),
    .groups = "drop"
  ) %>%
  mutate(
    # If unique_models > 1, the bank changed models at least once
    Group = ifelse(unique_models > 1, "Migrating", "Non-Migrating")
  )

# Merge classification back to main data
df_classified <- df_proc %>%
  left_join(bank_classification, by = "bank_id")

# 5. Define Variables for Table 7
# ------------------------------------------------------------------------------
# List based on Table 7 [cite: 540]
target_vars <- c("MES", "ACoVar", "CoVar", "SIZE", "LEVERAGE", "MBV", "RWA_TA", "WPS", "COST_INCOME")
existing_vars <- target_vars[target_vars %in% names(df_classified)]

# 6. Comparison Logic (Stats & Wilcoxon Test)
# ------------------------------------------------------------------------------
# Helper for Significance Stars
get_stars <- function(p_val) {
  if (is.na(p_val)) return("")
  if (p_val < 0.01) return("***")
  if (p_val < 0.05) return("**")
  if (p_val < 0.1)  return("*")
  return("")
}

results_list <- list()

for (var in existing_vars) {
  
  # A. Extract Vectors
  vec_migrating <- df_classified %>% 
    filter(Group == "Migrating") %>% 
    pull(var)
  
  vec_non_migrating <- df_classified %>% 
    filter(Group == "Non-Migrating") %>% 
    pull(var)
  
  # B. Calculate Statistics
  stats_mig <- c(
    Obs = sum(!is.na(vec_migrating)),
    Mean = mean(vec_migrating, na.rm = TRUE)
  )
  
  stats_non <- c(
    Obs = sum(!is.na(vec_non_migrating)),
    Mean = mean(vec_non_migrating, na.rm = TRUE)
  )
  
  # C. Wilcoxon-Mann-Whitney Test
  #    Tests if the distribution of Migrating differs from Non-Migrating
  w_test <- tryCatch({
    wilcox.test(vec_non_migrating, vec_migrating)
  }, error = function(e) NULL)
  
  p_val <- if (!is.null(w_test)) w_test$p.value else NA
  
  # D. Assemble Row
  row_df <- data.frame(
    Variable = var,
    
    # Non-Migrating Stats
    NonMig_Obs = stats_non["Obs"],
    NonMig_Mean = sprintf("%.3f", stats_non["Mean"]),
    
    # Migrating Stats
    Mig_Obs = stats_mig["Obs"],
    Mig_Mean = sprintf("%.3f", stats_mig["Mean"]),
    
    # Test Results
    Wilcox_P_Value = sprintf("%.3f", p_val),
    Significance = get_stars(p_val)
  )
  
  results_list[[var]] <- row_df
}

# 7. Final Output Assembly
# ------------------------------------------------------------------------------
final_table <- bind_rows(results_list)

# Print Preview
print(final_table)

# Save
write.csv(final_table, OUTPUT_FILE, row.names = FALSE)
message("Table 7 generated and saved to: ", OUTPUT_FILE)
message("Comparison: Non-Migrating vs Migrating Banks.")
message("Significance: *** p<0.01, ** p<0.05, * p<0.1")
