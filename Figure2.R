rm(list = ls())
# ==============================================================================
# SCRIPT 2: Replicate Figure 2 - Average ACoVar and Average MES
# ==============================================================================
# Description:
#   This script calculates the yearly average of MES and CoVaR across all banks
#   and plots them as line charts over time.
# ==============================================================================

# 1. Setup & Package Load
# ------------------------------------------------------------------------------
if (!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)
library(ggplot2)

# 2. Configurable Parameters
# ------------------------------------------------------------------------------
INPUT_FILE <- "Indian_banks_data.csv" 
OUTPUT_FILE <- "Figure_2_Average_Risk.png"

# Crisis Bands (Same as Script 1)
crisis_bands <- data.frame(
  start = c(2007, 2010, 2016, 2020),
  end   = c(2009, 2012, 2019, 2020),
  name  = c("GFC", "Sov Debt", "NPA-Crisis", "COVID")
)

# 3. Data Processing
# ------------------------------------------------------------------------------
if (!file.exists(INPUT_FILE)) stop("Input file not found.")
df <- read.csv(INPUT_FILE)

# Ensure numeric types for risk measures
df$MES <- as.numeric(df$MES)
df$CoVaR <- as.numeric(df$CoVaR)

# Aggregation: Calculate mean MES and CoVaR per Year
# The paper creates two lines. We need to pivot to "long" format for ggplot legend.
plot_data <- df %>%
  group_by(Year) %>%
  summarise(
    Average_MES = mean(MES, na.rm = TRUE),
    Average_CoVaR = mean(CoVaR, na.rm = TRUE) # Using CoVaR column for ACoVaR
  ) %>%
  pivot_longer(cols = c("Average_MES", "Average_CoVaR"), 
               names_to = "Metric", 
               values_to = "Value")

# 4. Visualization (Figure 2 Replication)
# ------------------------------------------------------------------------------
p2 <- ggplot() +
  # Background Bands
  geom_rect(data = crisis_bands,
            aes(xmin = start - 0.5, xmax = end + 0.5, ymin = -Inf, ymax = Inf),
            fill = c("mistyrose", "honeydew", "aliceblue", "lightyellow"),
            alpha = 0.8) +
  # Line Plot
  geom_line(data = plot_data, aes(x = Year, y = Value, color = Metric), size = 1) +
  
  # Manual coloring to match the paper (Red/Pink for MES, Blue for CoVaR usually)
  scale_color_manual(values = c("Average_CoVaR" = "#4682B4", "Average_MES" = "#CD5C5C"),
                     labels = c("Average ACoVaR", "Average MES")) +
  
  scale_x_continuous(breaks = seq(min(df$Year), max(df$Year), 2)) +
  labs(
    title = "Figure 2: Average ACoVar and Average MES (2005-2020)",
    x = "Year",
    y = "Risk Measure",
    color = NULL # Remove legend title
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

print(p2)

# 5. Save Output
# ------------------------------------------------------------------------------
ggsave(OUTPUT_FILE, plot = p2, width = 10, height = 6, dpi = 800)
message("Figure 2 generated and saved to: ", OUTPUT_FILE)
