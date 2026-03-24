rm(list = ls())
# ==============================================================================
# SCRIPT: Replicate Table 5 - Descriptive Statistics & ANOVA by Business Model
# ==============================================================================
# Description:
#   This script calculates the Mean and Observations (Obs) for key variables
#   across each Business Model (BM1-BM5).
#   It also performs a One-Way ANOVA test to check for statistical differences
#   between the models and reports significance stars.
# ==============================================================================

# 1. Setup & Package Load
# ------------------------------------------------------------------------------
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("broom")) install.packages("broom") # For tidy ANOVA results
library(tidyverse)
library(broom)

# 2. Configurable Parameters
# ------------------------------------------------------------------------------
INPUT_FILE <- "Indian_banks_data.csv"
OUTPUT_FILE <- "Table_5_Stats_by_Model_ANOVA.csv"

# 3. Data Loading
# ------------------------------------------------------------------------------
if (!file.exists(INPUT_FILE)) stop("Input file not found.")
df <- read.csv(INPUT_FILE)

# 4. Data Preparation & Mapping
# ------------------------------------------------------------------------------
# Rename variables to match Paper/Table 4 terminology
df_proc <- df %>%
  rename(
    ACoVar       = ACoVaR,
    CoVar       = CoVaR,
    SIZE         = Size,
    LEVERAGE     = Lev,
    COST_INCOME  = Cost_TI,
    NPA          = NPA,
    ROE          = ROE,
    RWA_TA       = RWA_TA,
    WPS          = WPS,
    MES          = MES
  ) %>%
  mutate(
    # Clean Model names (ensure no spaces)
    Model = str_trim(as.character(Model))
  )

# Check for MBV
if (!"MBV" %in% names(df_proc)) {
  warning("Variable 'MBV' not found. It will be excluded.")
}

# Define list of variables to analyze
target_vars <- c(
  "MES", "CoVar", "ACoVar", "SIZE", "LEVERAGE", "MBV", 
  "RWA_TA", "WPS", "COST_INCOME", "NPA", "ROE"
)
existing_vars <- target_vars[target_vars %in% names(df_proc)]

# 5. Helper Function: Significance Stars
# ------------------------------------------------------------------------------
# Based on paper footnotes: *** p<0.01, ** p<0.05, * p<0.1 
get_stars <- function(p_val) {
  if (is.na(p_val)) return("")
  if (p_val < 0.01) return("***")
  if (p_val < 0.05) return("**")
  if (p_val < 0.1)  return("*")
  return("ns") # Not significant
}

# 6. Core Calculation: Stats per Model + ANOVA
# ------------------------------------------------------------------------------
results_list <- list()

for (var in existing_vars) {
  
  # A. Calculate Descriptive Stats (Mean, Obs) by Model
  # -----------------------------------------------------
  stats <- df_proc %>%
    group_by(Model) %>%
    summarise(
      Obs = sum(!is.na(.data[[var]])),
      Mean = mean(.data[[var]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # Format numbers
    mutate(Mean = sprintf("%.3f", Mean)) %>%
    # Pivot to Wide format: e.g., BM1_Obs, BM1_Mean...
    pivot_wider(
      names_from = Model, 
      values_from = c(Obs, Mean),
      names_glue = "{Model}_{.value}" # Creates BM1_Obs, BM1_Mean
    )
  
  # B. Perform ANOVA (One-Way Analysis of Variance)
  # -----------------------------------------------------
  # Formula: Variable ~ Model
  anova_fit <- aov(as.formula(paste(var, "~ Model")), data = df_proc)
  anova_res <- tidy(anova_fit)
  
  # Extract P-value (term == "Model")
  p_val <- anova_res %>% filter(term == "Model") %>% pull(p.value)
  
  # Get Significance Sign
  anova_sign <- get_stars(p_val)
  
  # C. Combine into a single row
  # -----------------------------------------------------
  # Add Variable Name
  row_data <- stats %>%
    mutate(
      Variable = var, 
      ANOVA_Sign = anova_sign
    ) %>%
    select(Variable, everything()) # Move Variable to first col
  
  results_list[[var]] <- row_data
}

# 7. Assemble Final Table
# ------------------------------------------------------------------------------
# Bind all rows together
final_table <- bind_rows(results_list)

# Reorder columns to group Obs and Mean for each BM side-by-side (optional but cleaner)
# Get model names present in data
models <- sort(unique(df_proc$Model)) # e.g., BM1, BM2...

# Construct desired column order: Variable, BM1_Obs, BM1_Mean, BM2_Obs... ANOVA
desired_order <- c("Variable")
for (m in models) {
  desired_order <- c(desired_order, paste0(m, "_Obs"), paste0(m, "_Mean"))
}
desired_order <- c(desired_order, "ANOVA_Sign")

# Reorder columns
final_table <- final_table %>% select(any_of(desired_order))

# Print preview
print(final_table)

# 8. Save Output
# ------------------------------------------------------------------------------
write.csv(final_table, OUTPUT_FILE, row.names = FALSE)
message("Table 5 generated and saved to: ", OUTPUT_FILE)
