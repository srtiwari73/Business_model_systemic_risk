rm(list = ls())
# ==============================================================================
# SCRIPT: Replicate Table 3 - Bank Business Model Migrations Matrix
# ==============================================================================
# Description:
#   This script calculates the transition matrix (percentages) of bank business
#   models. It generates four panels:
#     - Panel A: Total Sample (All transitions)
#     - Panel B: No Crisis Periods
#     - Panel C: Endogenous Crisis Periods
#     - Panel D: Exogenous Crisis Periods
#   The output is saved as a formatted CSV file.
# ==============================================================================

# 1. Setup & Package Load
# ------------------------------------------------------------------------------
if (!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)

# 2. Configurable Parameters
# ------------------------------------------------------------------------------
INPUT_FILE <- "Indian_banks_data.csv"
OUTPUT_FILE <- "Table_3_Migrations_Matrix.csv"

# Define Periods based on the paper [cite: 339-340]
# Note: The transition year 't' determines the classification.
years_endogenous <- c(2015, 2016, 2017, 2018, 2019)
years_exogenous  <- c(2007, 2008, 2009, 2020)
years_no_crisis  <- c(2005, 2006, 2010, 2011, 2012, 2013, 2014, 2021, 2022, 2023, 2024)

# 3. Data Loading & Preprocessing
# ------------------------------------------------------------------------------
if (!file.exists(INPUT_FILE)) stop("Input file not found.")

df <- read.csv(INPUT_FILE)

# Clean and Sort Data
df_clean <- df %>%
  mutate(
    bank_id = as.character(bank_id),
    Model = str_trim(as.character(Model)), # Remove extra spaces
    Year = as.integer(Year)
  ) %>%
  arrange(bank_id, Year) # Sort is crucial for lag calculation

# 4. Prepare Transition Data
# ------------------------------------------------------------------------------
# We create a dataset of pairs: (Model at t-1) -> (Model at t)
transitions <- df_clean %>%
  group_by(bank_id) %>%
  mutate(
    prev_Model = dplyr::lag(Model),
    prev_Year = dplyr::lag(Year),
    is_consecutive = (Year == prev_Year + 1)
  ) %>%
  filter(is_consecutive & !is.na(prev_Model)) %>%
  ungroup() %>%
  select(bank_id, Year, prev_Model, Model)

# Assign 'Period' to each transition based on the destination Year 't'
transitions <- transitions %>%
  mutate(
    Period = case_when(
      Year %in% years_endogenous ~ "Endogenous Crisis",
      Year %in% years_exogenous  ~ "Exogenous Crisis",
      Year %in% years_no_crisis  ~ "No Crisis",
      TRUE ~ "Other" # Should not happen given the years in paper, but for safety
    )
  )

# 5. Helper Function to Calculate Transition Matrix
# ------------------------------------------------------------------------------
calculate_matrix <- function(data_subset, panel_label) {
  
  # Check if data exists for this subset
  if(nrow(data_subset) == 0) {
    warning(paste("No data for", panel_label))
    return(NULL)
  }
  
  # 1. Count transitions (From prev_Model to Model)
  counts <- data_subset %>%
    group_by(prev_Model, Model) %>%
    tally(name = "n") %>%
    ungroup()
  
  # 2. Calculate Row Totals (Total banks starting in prev_Model)
  row_totals <- counts %>%
    group_by(prev_Model) %>%
    summarise(total = sum(n), .groups = "drop")
  
  # 3. Calculate Percentages and Pivot to Matrix Format
  matrix_df <- counts %>%
    left_join(row_totals, by = "prev_Model") %>%
    mutate(perc = (n / total) * 100) %>%
    select(prev_Model, Model, perc) %>%
    # Pivot to make it a matrix: Rows = prev_Model, Cols = Model
    pivot_wider(names_from = Model, values_from = perc, values_fill = 0) %>%
    # Format numbers to 2 decimal places
    mutate(across(-prev_Model, ~ sprintf("%.2f%%", .)))
  
  # 4. Add Panel Label
  # Create a header row dataframe
  header <- data.frame(prev_Model = panel_label)
  
  # Bind label to the matrix
  # We convert all columns to character to allow binding the header
  matrix_df <- matrix_df %>% mutate(across(everything(), as.character))
  
  return(bind_rows(header, matrix_df))
}

# 6. Generate Panels (A, B, C, D)
# ------------------------------------------------------------------------------

# Panel A: Total Sample (All Years)
panel_a <- calculate_matrix(transitions, "Panel A: Total Sample")

# Panel B: No Crisis
panel_b <- calculate_matrix(transitions %>% filter(Period == "No Crisis"), 
                            "Panel B: No Crisis")

# Panel C: Endogenous Crisis
panel_c <- calculate_matrix(transitions %>% filter(Period == "Endogenous Crisis"), 
                            "Panel C: Endogenous Crisis")

# Panel D: Exogenous Crisis
panel_d <- calculate_matrix(transitions %>% filter(Period == "Exogenous Crisis"), 
                            "Panel D: Exogenous Crisis")

# 7. Combine and Save
# ------------------------------------------------------------------------------
# Stack all panels with some spacing (empty rows for readability)
empty_row <- function(cols) {
  as.data.frame(matrix("", ncol = length(cols), dimnames = list(NULL, cols)))
}

# Get column names from Panel A (Assuming BM1, BM2... exist in all)
# Ensure columns align. We use bind_rows which matches by name.
final_table <- bind_rows(
  panel_a,
  empty_row(names(panel_a)),
  panel_b,
  empty_row(names(panel_a)),
  panel_c,
  empty_row(names(panel_a)),
  panel_d
)

# Rename the first column to "Time t-1 \ Time t" for clarity
colnames(final_table)[1] <- "From (t-1) / To (t)"

# Save to CSV
write.csv(final_table, OUTPUT_FILE, row.names = FALSE, na = "")

message("Table 3 generated successfully.")
message("Output saved to: ", OUTPUT_FILE)
message("Note: Rows represent the business model at t-1. Columns represent the business model at t.")
