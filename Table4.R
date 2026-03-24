rm(list = ls())
# ==============================================================================
# SCRIPT: Replicate Table 4 (Updated) - Descriptive Statistics
# ==============================================================================
# Description:
#   Generates descriptive statistics (Obs, Mean, SD, Min, Max) for key variables.
#   Includes specific risk measures (MES, CoVaR), bank metrics (NPA, ROE), and
#   Business Model dummies (BM1-BM5).
# ==============================================================================

# 1. Setup & Package Load
# ------------------------------------------------------------------------------
if (!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)

# 2. Configurable Parameters
# ------------------------------------------------------------------------------
INPUT_FILE <- "Indian_banks_data.csv"
OUTPUT_FILE <- "Table_4_Descriptive_Statistics_Updated.csv"

# 3. Data Loading
# ------------------------------------------------------------------------------
if (!file.exists(INPUT_FILE)) stop("Input file not found.")
df <- read.csv(INPUT_FILE)

# 4. Data Preparation & Mapping
# ------------------------------------------------------------------------------
# Create a processed dataframe with standardized names
# Mapping: New_Name = Existing_Column_Name_In_CSV
df_proc <- df %>%
  rename(
    CoVar       = CoVaR,       # Paper uses ACoVar
    ACoVar       = ACoVaR,       # Paper uses ACoVar
    SIZE         = Size,        # Paper uses SIZE
    LEVERAGE     = Lev,         # Paper uses LEVERAGE
    COST_INCOME  = Cost_TI,     # Paper uses COST_INCOME
    # Variables requested to be included directly:
    NPA          = NPA,
    ROE          = ROE,
    # Standard variables that usually match:
    RWA_TA       = RWA_TA,
    WPS          = WPS,
    MES          = MES
  )

# Check for 'MBV' (Market to Book Value) - warning if missing
if (!"MBV" %in% names(df_proc)) {
  warning("Variable 'MBV' not found in data. It will be excluded from the table.")
}

# Create Business Model Dummies (BM1 - BM5)
# We calculate these as 0/1. The 'Mean' of these variables = % of sample.
df_proc <- df_proc %>%
  mutate(
    Model = str_trim(as.character(Model)),
    
    BM1 = ifelse(Model == "BM1", 1, 0),
    BM2 = ifelse(Model == "BM2", 1, 0),
    BM3 = ifelse(Model == "BM3", 1, 0),
    BM4 = ifelse(Model == "BM4", 1, 0)
  )

# 5. Select Variables for the Table
# ------------------------------------------------------------------------------
# Define the order of variables for the final table
target_vars <- c(
  "MES", "CoVar", "ACoVar", 
  "SIZE", "LEVERAGE", "MBV", "RWA_TA", "WPS", "COST_INCOME",
  "NPA", "ROE",            # Added as requested
  "BM1", "BM2", "BM3", "BM4"  # Kept as BM names as requested
)

# Filter list to only include variables that actually exist in the dataframe
existing_vars <- target_vars[target_vars %in% names(df_proc)]

# 6. Calculate Descriptive Statistics
# ------------------------------------------------------------------------------
table_stats <- df_proc %>%
  select(all_of(existing_vars)) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
  group_by(Variable) %>%
  summarise(
    Obs.      = sum(!is.na(Value)),         # Count valid observations
    Mean      = mean(Value, na.rm = TRUE),
    Std_Dev   = sd(Value, na.rm = TRUE),
    Min       = min(Value, na.rm = TRUE),
    Max       = max(Value, na.rm = TRUE)
  ) %>%
  ungroup()

# 7. Formatting & Sorting
# ------------------------------------------------------------------------------
table_stats <- table_stats %>%
  # Sort by the order defined in 'target_vars'
  mutate(Variable = factor(Variable, levels = target_vars)) %>%
  arrange(Variable) %>%
  mutate(
    # Format numbers to 3 decimal places for readability
    Mean    = sprintf("%.3f", Mean),
    Std_Dev = sprintf("%.3f", Std_Dev),
    Min     = sprintf("%.3f", Min),
    Max     = sprintf("%.3f", Max)
  )

# Display result in console
print(table_stats)

# 8. Save Output
# ------------------------------------------------------------------------------
write.csv(table_stats, OUTPUT_FILE, row.names = FALSE)
message("Updated Table 4 generated and saved to: ", OUTPUT_FILE)
