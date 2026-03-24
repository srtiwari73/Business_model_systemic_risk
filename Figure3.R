rm(list = ls())
# ==============================================================================
# SCRIPT 3: Replicate Figure 3 - Systemic Risk by Business Model (Panels a & b)
# ==============================================================================
# Description:
#   This script breaks down the average CoVaR (Panel A) and MES (Panel B)
#   by Year AND Business Model (Model).
# ==============================================================================

# 1. Setup
# ------------------------------------------------------------------------------
if (!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)
library(ggplot2)

INPUT_FILE <- "Indian_banks_data.csv"
OUTPUT_FILE_A <- "Figure_3_Panel_A_ACoVaR.png"
OUTPUT_FILE_B <- "Figure_3_Panel_B_MES.png"

# Crisis Bands
crisis_bands <- data.frame(
  start = c(2007, 2015, 2020),
  end   = c(2009, 2019, 2020),
  fill_color = c("#ffe5e5", "#e5ffe5", "#e5e5ff") # Light distinct colors
)

# 2. Data Processing
# ------------------------------------------------------------------------------
if (!file.exists(INPUT_FILE)) stop("Input file not found.")
df <- read.csv(INPUT_FILE)

# Aggregate by Year and Model
bm_risk_data <- df %>%
  group_by(Year, Model) %>%
  summarise(
    Mean_CoVaR = mean(CoVaR, na.rm = TRUE),
    Mean_MES = mean(MES, na.rm = TRUE),
    .groups = "drop"
  )

# 3. Helper Function for Plotting
# ------------------------------------------------------------------------------
# Since Panel A and B are identical in structure but differ in metric, 
# we create a function to avoid code duplication.

create_panel_plot <- function(data, y_col, title_text, y_label) {
  ggplot() +
    # Background bands
    geom_rect(data = crisis_bands,
              aes(xmin = start - 0.5, xmax = end + 0.5, ymin = -Inf, ymax = Inf),
              fill = c("mistyrose", "honeydew", "aliceblue"),
              alpha = 0.8) +
    # Lines for each Business Model
    geom_line(data = data, aes(x = Year, y = .data[[y_col]], color = Model), size = 0.8) +
    
    scale_x_continuous(breaks = seq(min(data$Year), max(data$Year), 2)) +
    labs(
      #title = title_text,
      x = "Year",
      y = y_label,
      color = "Business Model"
    ) +
    theme_minimal() +
    theme(
      legend.position = "right",
      plot.title = element_text(face = "bold", hjust = 0.5)
    )
}

# 4. Generate Panels
# ------------------------------------------------------------------------------

# Panel A: ACoVaR (using CoVaR column)
p_a <- create_panel_plot(bm_risk_data, "Mean_CoVaR", 
                         "Figure 3 (Panel a): Delta-CoVar by Business Model", 
                         "Average Delta-CoVar")

# Panel B: MES
p_b <- create_panel_plot(bm_risk_data, "Mean_MES", 
                         "Figure 3 (Panel b): MES by Business Model", 
                         "Average MES")

print(p_a)
print(p_b)

# 5. Save Output
# ------------------------------------------------------------------------------
ggsave(OUTPUT_FILE_A, plot = p_a, width = 10, height = 6, dpi = 800)
ggsave(OUTPUT_FILE_B, plot = p_b, width = 10, height = 6, dpi = 800)
message("Figure 3 Panels generated and saved.")
