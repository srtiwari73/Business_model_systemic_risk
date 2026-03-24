rm(list = ls())
# ==============================================================================
# SCRIPT: Replicate Table 6 - Stats & Tests across Crisis Periods
# ==============================================================================
# Description:
#   This script compares systemic risk (MES, ACoVar) across three periods:
#   No Crisis, Endogenous Crisis, and Exogenous Crisis.
#   It produces a table containing:
#     - Obs & Means for Total and each Period.
#     - Wilcoxon-Mann-Whitney Test (Endogenous vs Exogenous).
#     - ANOVA Test (across all 3 periods).
#     - Breakdowns for each Business Model (BM1-BM5).
# ==============================================================================

# 1. Setup & Package Load
# ------------------------------------------------------------------------------
if (!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)

# 2. Configurable Parameters
# ------------------------------------------------------------------------------
INPUT_FILE <- "Indian_banks_data.csv"
OUTPUT_FILE <- "Table_6_Crisis_Analysis.csv"

# Define Years for Periods 
years_endogenous <- c(2015, 2016, 2017, 2018, 2019)
years_exogenous  <- c(2007, 2008, 2009, 2020)
years_no_crisis  <- c(2005, 2006, 2010, 2011, 2012, 2013, 2014, 2021, 2022, 2023, 2024)

# 3. Data Loading & Preparation
# ------------------------------------------------------------------------------
if (!file.exists(INPUT_FILE)) stop("Input file not found.")
df <- read.csv(INPUT_FILE)

# Process Data: Mapping and Period Creation
df_proc <- df %>%
  rename(CoVar = CoVaR, ACoVar = ACoVaR, MES = MES) %>% # Ensure correct names
  mutate(
    Model = str_trim(as.character(Model)),
    
    # Create Period Column
    Period = case_when(
      Year %in% years_endogenous ~ "Endogenous",
      Year %in% years_exogenous  ~ "Exogenous",
      Year %in% years_no_crisis  ~ "No_Crisis",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Period)) # Remove data not matching defined years

# 4. Helper Function: Significance Stars
# ------------------------------------------------------------------------------
get_stars <- function(p_val) {
  if (is.na(p_val)) return("")
  if (p_val < 0.01) return("***")
  if (p_val < 0.05) return("**")
  if (p_val < 0.1)  return("*")
  return("ns")
}

# 5. Core Function: Calculate Row Statistics
# ------------------------------------------------------------------------------
# This function takes the dataframe, a variable name (e.g., 'MES'), 
# and an optional model filter (e.g., 'BM1').
# It returns a single row dataframe matching Table 6 columns.

calc_row_stats <- function(full_data, variable_name, model_filter = NULL) {
  
  # A. Filter Data (if specific model requested)
  data_subset <- full_data
  row_label <- variable_name
  
  if (!is.null(model_filter)) {
    data_subset <- full_data %>% filter(Model == model_filter)
    row_label <- paste(variable_name, model_filter)
  }
  
  # B. Calculate Obs & Means for each Period
  # ----------------------------------------
  # Helper to get specific stat
  get_stat <- function(period_name, stat_type) {
    d <- data_subset 
    if(period_name != "Total") d <- d %>% filter(Period == period_name)
    
    vals <- d[[variable_name]]
    if (stat_type == "Obs") return(sum(!is.na(vals)))
    if (stat_type == "Mean") return(mean(vals, na.rm = TRUE))
    return(NA)
  }
  
  # Calculate Total Stats
  tot_obs  <- get_stat("Total", "Obs")
  tot_mean <- get_stat("Total", "Mean")
  
  # Calculate Period Stats
  no_obs   <- get_stat("No_Crisis", "Obs")
  no_mean  <- get_stat("No_Crisis", "Mean")
  
  endo_obs  <- get_stat("Endogenous", "Obs")
  endo_mean <- get_stat("Endogenous", "Mean")
  
  exo_obs   <- get_stat("Exogenous", "Obs")
  exo_mean  <- get_stat("Exogenous", "Mean")
  
  # C. Statistical Tests
  # ----------------------------------------
  # 1. Wilcoxon-Mann-Whitney (Endogenous vs Exogenous)
  #    Extract vectors for the test
  vec_endo <- data_subset %>% filter(Period == "Endogenous") %>% pull(variable_name)
  vec_exo  <- data_subset %>% filter(Period == "Exogenous") %>% pull(variable_name)
  
  wilcox_res <- tryCatch({
    wilcox.test(vec_endo, vec_exo)
  }, error = function(e) return(NULL))
  
  wilcox_p <- if(!is.null(wilcox_res)) wilcox_res$p.value else NA
  
  # 2. ANOVA (Across No_Crisis, Endogenous, Exogenous)
  #    Formula: Variable ~ Period
  anova_res <- tryCatch({
    aov(as.formula(paste(variable_name, "~ Period")), data = data_subset)
  }, error = function(e) return(NULL))
  
  anova_p <- NA
  if(!is.null(anova_res)) {
    # Extract p-value safely
    tidy_res <- tryCatch(summary(anova_res)[[1]], error=function(e) NULL)
    if(!is.null(tidy_res)) anova_p <- tidy_res["Period", "Pr(>F)"]
  }
  
  # D. Assemble Row
  # ----------------------------------------
  data.frame(
    Variable = row_label,
    
    Total_Obs = tot_obs,
    Total_Mean = sprintf("%.3f", tot_mean),
    
    NoCrisis_Obs = no_obs,
    NoCrisis_Mean = sprintf("%.3f", no_mean),
    
    Endo_Obs = endo_obs,
    Endo_Mean = sprintf("%.3f", endo_mean),
    
    Exo_Obs = exo_obs,
    Exo_Mean = sprintf("%.3f", exo_mean),
    
    Wilcox_P = sprintf("%.3f", wilcox_p),
    Wilcox_Sig = get_stars(wilcox_p),
    
    ANOVA_Sign = get_stars(anova_p)
  )
}

# 6. Generate All Rows
# ------------------------------------------------------------------------------
row_list <- list()

# 1. Main Rows (All Models)
row_list[[1]] <- calc_row_stats(df_proc, "MES")
row_list[[2]] <- calc_row_stats(df_proc, "ACoVar")
row_list[[3]] <- calc_row_stats(df_proc, "CoVar")
# 2. Interaction Rows (Specific Business Models)
#    We loop through BM1 to BM4 for both variables
models <- paste0("BM", 1:4) # BM1, BM2, BM3, BM4, BM4

for (bm in models) {
  row_list[[length(row_list) + 1]] <- calc_row_stats(df_proc, "ACoVar", bm)
}

for (bm in models) {
  row_list[[length(row_list) + 1]] <- calc_row_stats(df_proc, "MES", bm)
}

for (bm in models) {
  row_list[[length(row_list) + 1]] <- calc_row_stats(df_proc, "CoVar", bm)
}

# 7. Final Output Assembly
# ------------------------------------------------------------------------------
final_table <- bind_rows(row_list)

# Display for verification
print(head(final_table))

# Save
write.csv(final_table, OUTPUT_FILE, row.names = FALSE)
message("Table 6 generated and saved to: ", OUTPUT_FILE)
message("Note: 'Endo' = Endogenous Crisis, 'Exo' = Exogenous Crisis.")
message("Significance: *** p<0.01, ** p<0.05, * p<0.1")
