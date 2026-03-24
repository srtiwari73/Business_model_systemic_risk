rm(list = ls())
# ==============================================================================
# CORRECTED SCRIPT 1: Replicate Figure 1 - Bank Business Model Migrations
# ==============================================================================
# Purpose: Detects year-over-year changes in 'Model' for each bank and plots
#          the total number of migrations per year against crisis backgrounds.
# ==============================================================================

# 1. Setup & Package Load
# ------------------------------------------------------------------------------
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("ggplot2")) install.packages("ggplot2")
library(tidyverse)
library(ggplot2)

# 2. Configurable Parameters
# ------------------------------------------------------------------------------
INPUT_FILE <- "Indian_banks_data.csv"
OUTPUT_FILE <- "Figure_1_Migrations.png"

# Crisis Bands (Approximate based on paper description)
# Global Financial Crisis (2007-2009), Sovereign Debt (2010-2012)
# Political Instability (2016), COVID-19 (2020)
crisis_bands <- data.frame(
  start = c(2007, 2015, 2020),
  end   = c(2009, 2019, 2020),
  fill_color = c("#ffe5e5", "#e5ffe5", "#e5e5ff") # Light distinct colors
)

# 3. Data Loading & Cleaning
# ------------------------------------------------------------------------------
if (!file.exists(INPUT_FILE)) stop("Input file not found.")

df <- read.csv(INPUT_FILE)

# Ensure strict data types for detection logic
df <- df %>%
  mutate(
    bank_id = as.character(bank_id),
    Model = str_trim(as.character(Model)), # Remove potential whitespace "BM1 " -> "BM1"
    Year = as.integer(Year)
  )

# 4. Core Logic: Robust Migration Detection
# ------------------------------------------------------------------------------
# We detect a migration if:
# 1. It is the same bank.
# 2. The Current Year is exactly Previous Year + 1 (Consecutive).
# 3. The Current Model is NOT equal to Previous Model.

migration_analysis <- df %>%
  arrange(bank_id, Year) %>%               # Critical: Sort ensures lag works correctly
  group_by(bank_id) %>%
  mutate(
    prev_Model = dplyr::lag(Model),        # Look at row above
    prev_Year = dplyr::lag(Year),          # Look at year above
    
    # Check if years are consecutive (e.g., 2010 then 2011)
    is_consecutive = (Year == prev_Year + 1),
    
    # Check if Model changed
    has_changed_model = (Model != prev_Model)
  ) %>%
  ungroup() %>%
  # Filter: Must be consecutive years AND have a model change
  filter(is_consecutive & has_changed_model)

# 5. Aggregation
# ------------------------------------------------------------------------------
# Count migrations per year. 
# We use complete() to ensure years with 0 migrations still appear in the data.
migration_counts <- migration_analysis %>%
  count(Year, name = "Migration_Count") %>%
  complete(Year = min(df$Year):max(df$Year), fill = list(Migration_Count = 0))

# Sanity Check: Print the first few detected migrations to console
print("Sanity Check - First 5 Migrations Detected:")
print(head(migration_analysis %>% select(bank_id, Year, prev_Model, Model), 5))
print("Summary of Counts:")
print(migration_counts)

# 6. Visualization
# ------------------------------------------------------------------------------
p1 <- ggplot() +
  # A. Add Background Crisis Bands
  # We use xmin/xmax manipulation to ensure bands align with discrete bars
  geom_rect(data = crisis_bands,
            aes(xmin = start - 0.5, xmax = end + 0.5, ymin = -Inf, ymax = Inf),
            fill = c("mistyrose", "honeydew", "aliceblue"), 
            alpha = 0.7) +
  
  # B. Add Migration Bars
  geom_bar(data = migration_counts, 
           aes(x = Year, y = Migration_Count), 
           stat = "identity", 
           fill = "#2b6a99", # Dark blue color similar to paper
           width = 0.6) +
  
  # C. Formatting
  scale_x_continuous(breaks = seq(min(df$Year), max(df$Year), 1)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) + # Remove gap at bottom
  labs(
    #title = "Figure 1: Bank Business Model Migrations (Corrected)",
    #subtitle = paste("Total Migrations Detected:", sum(migration_counts$Migration_Count)),
    x = "Year",
    y = "Number of Migrations"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.line.x = element_line(color = "black")
  )

print(p1)

# 7. Save Output
# ------------------------------------------------------------------------------
ggsave(OUTPUT_FILE, plot = p1, width = 10, height = 6, dpi = 800)
message("Script completed. Plot saved to: ", OUTPUT_FILE)
