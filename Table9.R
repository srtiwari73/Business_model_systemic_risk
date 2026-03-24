rm(list = ls())
# ==============================================================================
# SCRIPT: Replicate Table 9 - Heckman Step 2 with Interactions (Explicit)
# ==============================================================================
# Description:
#   1. Loads prepared data (from Table 8 step).
#   2. Estimates 4 separate Panel Regressions (Pooling) with Interactions:
#      - Model 1: MES ~ Instability * BMs
#      - Model 2: MES ~ Split Crisis (Endo/Exo) * BMs
#      - Model 3: ACoVar ~ Instability * BMs
#      - Model 4: ACoVar ~ Split Crisis (Endo/Exo) * BMs
#   3. Outputs Coefficients, Robust SE, P-values, R-squared, and Observations.
# ==============================================================================

# 1. Setup & Package Load
# ------------------------------------------------------------------------------
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("plm")) install.packages("plm")         # Panel Data
if (!require("lmtest")) install.packages("lmtest")   # Robust SE
if (!require("sandwich")) install.packages("sandwich") # Robust SE
library(tidyverse)
library(plm)
library(lmtest)
library(sandwich)

# 2. Configurable Parameters
# ------------------------------------------------------------------------------
INPUT_DATA <- "data_prepared_for_analysis.csv"
OUTPUT_TABLE_9 <- "Table_9_Step2_Interactions_Final.csv"

# 3. Data Loading
# ------------------------------------------------------------------------------
if (!file.exists(INPUT_DATA)) stop("Input data file not found. Run Table 8 script first.")
df_prepared <- read.csv(INPUT_DATA)

# 4. Define Common Control Strings
# ------------------------------------------------------------------------------
# We define these strings just to keep the formula readable, but we will
# construct the full formula explicitly for each model.

# Bank Controls & IMRs
controls <- paste(
  "SIZE_lag + Lev_lag + Cost_TI_lag + NPA_lag + DSIB_lag + RWA_TA_lag + WPS_lag + ROE_lag",
  "IMR_BM1 + IMR_BM2 + IMR_BM3",
  sep = " + "
)

# Business Model Main Effects (Reference BM4 omitted)
bms <- "BM1_lag + BM2_lag + BM3_lag"

# ------------------------------------------------------------------------------
# MODEL 1: MES ~ Instability * Business Models
# ------------------------------------------------------------------------------
message("Estimating Model 1: MES with Instability Interactions...")

# Formula: MES ~ Lag + Instability + BMs + Controls + (Instability * BMs)
f1_str <- paste(
  "MES_pos ~ MES_lag + INSTABILITY",
  "+", bms,
  "+", controls,
  # Interactions
  "+ INSTABILITY:BM1_lag + INSTABILITY:BM2_lag + INSTABILITY:BM3_lag"
)

mod1 <- plm(as.formula(f1_str), data = df_prepared, index = c("bank_id", "Year"), model = "pooling")

# Statistics
r2_1 <- summary(mod1)$r.squared["rsq"]
obs_1 <- nobs(mod1)

# Robust SE
robust_1 <- coeftest(mod1, vcov = vcovHC(mod1, method = "arellano", type = "HC1"))

# Create Result Dataframe
res1 <- data.frame(
  Term = rownames(robust_1),
  MES_Instab_Coeff = robust_1[, 1],
  MES_Instab_SE    = robust_1[, 2],
  MES_Instab_Pval  = robust_1[, 4]
)

# ------------------------------------------------------------------------------
# MODEL 2: MES ~ Split Crisis (Endogenous/Exogenous) * Business Models
# ------------------------------------------------------------------------------
message("Estimating Model 2: MES with Split Crisis Interactions...")

f2_str <- paste(
  "MES_pos ~ MES_lag + ENDOGENOUS + EXOGENOUS",
  "+", bms,
  "+", controls,
  # Endogenous Interactions
  "+ ENDOGENOUS:BM1_lag + ENDOGENOUS:BM2_lag + ENDOGENOUS:BM3_lag",
  # Exogenous Interactions
  "+ EXOGENOUS:BM1_lag + EXOGENOUS:BM2_lag + EXOGENOUS:BM3_lag"
)

mod2 <- plm(as.formula(f2_str), data = df_prepared, index = c("bank_id", "Year"), model = "pooling")

r2_2 <- summary(mod2)$r.squared["rsq"]
obs_2 <- nobs(mod2)
robust_2 <- coeftest(mod2, vcov = vcovHC(mod2, method = "arellano", type = "HC1"))

res2 <- data.frame(
  Term = rownames(robust_2),
  MES_Split_Coeff = robust_2[, 1],
  MES_Split_SE    = robust_2[, 2],
  MES_Split_Pval  = robust_2[, 4]
)

# ------------------------------------------------------------------------------
# MODEL 3: ACoVar ~ Instability * Business Models
# ------------------------------------------------------------------------------
message("Estimating Model 3: ACoVar with Instability Interactions...")

f3_str <- paste(
  "ACoVar_pos ~ ACoVar_lag + INSTABILITY",
  "+", bms,
  "+", controls,
  # Interactions
  "+ INSTABILITY:BM1_lag + INSTABILITY:BM2_lag + INSTABILITY:BM3_lag"
)

mod3 <- plm(as.formula(f3_str), data = df_prepared, index = c("bank_id", "Year"), model = "pooling")

r2_3 <- summary(mod3)$r.squared["rsq"]
obs_3 <- nobs(mod3)
robust_3 <- coeftest(mod3, vcov = vcovHC(mod3, method = "arellano", type = "HC1"))

res3 <- data.frame(
  Term = rownames(robust_3),
  ACoVar_Instab_Coeff = robust_3[, 1],
  ACoVar_Instab_SE    = robust_3[, 2],
  ACoVar_Instab_Pval  = robust_3[, 4]
)

# ------------------------------------------------------------------------------
# MODEL 4: ACoVar ~ Split Crisis (Endogenous/Exogenous) * Business Models
# ------------------------------------------------------------------------------
message("Estimating Model 4: ACoVar with Split Crisis Interactions...")

f4_str <- paste(
  "ACoVar_pos ~ ACoVar_lag + ENDOGENOUS + EXOGENOUS",
  "+", bms,
  "+", controls,
  # Endogenous Interactions
  "+ ENDOGENOUS:BM1_lag + ENDOGENOUS:BM2_lag + ENDOGENOUS:BM3_lag",
  # Exogenous Interactions
  "+ EXOGENOUS:BM1_lag + EXOGENOUS:BM2_lag + EXOGENOUS:BM3_lag"
)

mod4 <- plm(as.formula(f4_str), data = df_prepared, index = c("bank_id", "Year"), model = "pooling")

r2_4 <- summary(mod4)$r.squared["rsq"]
obs_4 <- nobs(mod4)
robust_4 <- coeftest(mod4, vcov = vcovHC(mod4, method = "arellano", type = "HC1"))

res4 <- data.frame(
  Term = rownames(robust_4),
  ACoVar_Split_Coeff = robust_4[, 1],
  ACoVar_Split_SE    = robust_4[, 2],
  ACoVar_Split_Pval  = robust_4[, 4]
)

# ------------------------------------------------------------------------------
# MODEL 5: CoVar ~ Instability * Business Models
# ------------------------------------------------------------------------------
message("Estimating Model 3: CoVar with Instability Interactions...")

f5_str <- paste(
  "CoVar_pos ~ CoVar_lag + INSTABILITY",
  "+", bms,
  "+", controls,
  # Interactions
  "+ INSTABILITY:BM1_lag + INSTABILITY:BM2_lag + INSTABILITY:BM3_lag"
)

mod5 <- plm(as.formula(f5_str), data = df_prepared, index = c("bank_id", "Year"), model = "pooling")

r2_5 <- summary(mod5)$r.squared["rsq"]
obs_5 <- nobs(mod5)
robust_5 <- coeftest(mod5, vcov = vcovHC(mod5, method = "arellano", type = "HC1"))

res5 <- data.frame(
  Term = rownames(robust_5),
  CoVar_Instab_Coeff = robust_5[, 1],
  CoVar_Instab_SE    = robust_5[, 2],
  CoVar_Instab_Pval  = robust_5[, 4]
)

# ------------------------------------------------------------------------------
# MODEL 6: CoVar ~ Split Crisis (Endogenous/Exogenous) * Business Models
# ------------------------------------------------------------------------------
message("Estimating Model 4: ACoVar with Split Crisis Interactions...")

f6_str <- paste(
  "CoVar_pos ~ CoVar_lag + ENDOGENOUS + EXOGENOUS",
  "+", bms,
  "+", controls,
  # Endogenous Interactions
  "+ ENDOGENOUS:BM1_lag + ENDOGENOUS:BM2_lag + ENDOGENOUS:BM3_lag",
  # Exogenous Interactions
  "+ EXOGENOUS:BM1_lag + EXOGENOUS:BM2_lag + EXOGENOUS:BM3_lag"
)

mod6 <- plm(as.formula(f6_str), data = df_prepared, index = c("bank_id", "Year"), model = "pooling")

r2_6 <- summary(mod6)$r.squared["rsq"]
obs_6 <- nobs(mod6)
robust_6 <- coeftest(mod6, vcov = vcovHC(mod6, method = "arellano", type = "HC1"))

res6 <- data.frame(
  Term = rownames(robust_6),
  CoVar_Split_Coeff = robust_6[, 1],
  CoVar_Split_SE    = robust_6[, 2],
  CoVar_Split_Pval  = robust_6[, 4]
)

# 5. Merge Results
# ------------------------------------------------------------------------------
message("Merging results...")

final_table_9 <- res1 %>%
  full_join(res2, by = "Term") %>%
  full_join(res3, by = "Term") %>%
  full_join(res4, by = "Term") %>%
  full_join(res5, by = "Term") %>%
  full_join(res6, by = "Term")

# Clean up Term names
final_table_9$Term <- final_table_9$Term %>%
  gsub("_lag", " (t-1)", .) %>%
  gsub(":", " x ", .)

# 6. Append R-squared and Observations rows
# ------------------------------------------------------------------------------
# Helper to create a row for the bottom of the table
create_summary_row <- function(label, v1, v2, v3, v4, v5, v6) {
  row_df <- data.frame(
    Term = label,
    # Assign values to the first column of each model section
    MES_Instab_Coeff = v1, MES_Instab_SE = NA, MES_Instab_Pval = NA,
    MES_Split_Coeff  = v2, MES_Split_SE  = NA, MES_Split_Pval  = NA,
    ACoVar_Instab_Coeff = v3, ACoVar_Instab_SE = NA, ACoVar_Instab_Pval = NA,
    ACoVar_Split_Coeff  = v4, ACoVar_Split_SE  = NA, ACoVar_Split_Pval  = NA,
    CoVar_Instab_Coeff = v5, CoVar_Instab_SE = NA, CoVar_Instab_Pval = NA,
    CoVar_Split_Coeff  = v6, CoVar_Split_SE  = NA, CoVar_Split_Pval  = NA
  )
  return(row_df)
}

row_r2 <- create_summary_row("R-squared", r2_1, r2_2, r2_3, r2_4, r2_5, r2_6)
row_obs <- create_summary_row("Observations", obs_1, obs_2, obs_3, obs_4, obs_5, obs_6)

# Bind to the bottom
final_table_9 <- bind_rows(final_table_9, row_r2, row_obs)

# 7. Save
# ------------------------------------------------------------------------------
write.csv(final_table_9, OUTPUT_TABLE_9, row.names = FALSE)

message("----------------------------------------------------------------")
message("Table 9 Generation Complete.")
message(paste("Model 1 R2:", round(r2_1, 3), "Obs:", obs_1))
message(paste("Model 2 R2:", round(r2_2, 3), "Obs:", obs_2))
message(paste("Model 3 R2:", round(r2_3, 3), "Obs:", obs_3))
message(paste("Model 4 R2:", round(r2_4, 3), "Obs:", obs_4))
message("Output saved to: ", OUTPUT_TABLE_9)
message("----------------------------------------------------------------")
