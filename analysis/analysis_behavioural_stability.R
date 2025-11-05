library("tidyverse")
library("dplyr")

data <- read.table("data/data_full_groups.csv", header=TRUE, sep=",", quote='"')

# Create an indicator variable
data <- data %>% 
  mutate(global_only_indicator = if_else(condition == "GlobalOnly", 1, 0))

data <- data %>% 
  mutate(global_boost_indicator = if_else(condition == "GlobalBoost", 1, 0))

#################
# Data Wrangling

library(ordinal)

# The ordinal fitting library requires columns in this format:
# group_id, subgroup_id, round, n_global, n_defect, n_local
# In order to pivot the data into a usable format
# We first need to pivot long to get cols with each "game_variable" and "value"
cols_n_global <- sprintf("game.%s.player.global_group_contributions", 1:20)
cols_n_local <- sprintf("game.%s.player.local_group_contributions", 1:20)
cols_n_defect <- sprintf("game.%s.player.kept_group_contributions", 1:20)

cols <- c(cols_n_global, cols_n_local, cols_n_defect)

pivoted_data_long_ord <- pivot_longer(data, 
                                      cols,
                                      names_to = c("round", "game_variable"),
                                      names_pattern = "game.(.*).player.(.*)",
                                      values_transform = list(value = as.character)
)

# Remove unneeded columns
cols_of_interest <- c("participant.code",
                      "group_id",
                      "subgroup_id",
                      "round",
                      "game_variable",
                      "value",
                      "global_only_indicator",
                      "global_boost_indicator")
pivoted_data_long_ord <- pivoted_data_long_ord[cols_of_interest]

pivoted_data_ord <- pivot_wider(pivoted_data_long_ord,
                                names_from="game_variable",
                                values_from="value"
)

# Convert to format that the orindal library can use
pivoted_data_ord$group_id <- as.factor(pivoted_data_ord$group_id)
pivoted_data_ord$subgroup_id <- as.factor(pivoted_data_ord$subgroup_id)
pivoted_data_ord$global_group_contributions <- as.factor(pivoted_data_ord$global_group_contributions)
pivoted_data_ord$round <- as.numeric(pivoted_data_ord$round)
pivoted_data_ord$kept_group_contributions <- as.factor(pivoted_data_ord$kept_group_contributions)
pivoted_data_ord$local_group_contributions <- as.factor(pivoted_data_ord$local_group_contributions)

# Run with group as the random effect
# Remove duplicates on group_id AND round_number
pivoted_data_unique_group_round <- pivoted_data_ord %>% distinct(group_id, round, .keep_all = TRUE)

show_CI_clmm2 <- function(clmm2_model) {
  est <- coef(clmm2_model)
  sdhat <- sqrt(diag(vcov(clmm2_model)))
  CI <- est + tcrossprod(1.96 * sdhat, c(-1, 1))
  rownames(CI) <- names(est)
  colnames(CI) <- c("lower", "upper")
  CI <- CI[seq_len(length(est)-1), ]
  CI
}

model_global_more <- clmm2(global_group_contributions ~ round + global_only_indicator + global_boost_indicator + global_only_indicator*round + global_boost_indicator*round, 
                           random=group_id, data = pivoted_data_unique_group_round, Hess=TRUE)
summary(model_global_more)
show_CI_clmm2(model_global_more)


model_defect_more <- clmm2(kept_group_contributions ~ round + global_only_indicator + global_boost_indicator + round*global_only_indicator + round*global_boost_indicator, random=group_id, data = pivoted_data_unique_group_round, Hess=TRUE)
summary(model_defect_more)
show_CI_clmm2(model_defect_more)

# Local
# Need to group data by subgroup
pivoted_data_unique_subgroup_round <- pivoted_data_ord %>% distinct(subgroup_id, round, .keep_all = TRUE)

# Remove GlobalOnly condition
pivoted_data_unique_subgroup_round <- pivoted_data_unique_subgroup_round %>%
  filter(global_only_indicator == 0)

model_local_more <- clmm2(local_group_contributions ~ round + global_boost_indicator + round*global_boost_indicator, 
                          random=subgroup_id, data = pivoted_data_unique_subgroup_round, Hess=TRUE)

summary(model_local_more)
show_CI_clmm2(model_local_more)



########################
# Stability

library(ordinal)


pivoted_data_unique_group_round <- pivoted_data_unique_group_round %>%
  mutate(period = case_when(
    round <= 10 ~ "rounds_1_10",
    round <= 20 ~ "rounds_11_20"
  ))

pivoted_data_unique_group_round$period <- factor(
  pivoted_data_unique_group_round$period,
  levels = c("rounds_11_20", "rounds_1_10")
)

###########################################
# GLOBAL COOPERATION - Stability Analysis
###########################################

# Global Only
data_global_only <- pivoted_data_unique_group_round %>% filter(global_only_indicator == 1)
model_global_globalonly <- clmm2(
  global_group_contributions ~ round*period,
  random = group_id,
  data = data_global_only,
  Hess = TRUE
)
cat("\n=== GLOBAL COOPERATION - GlobalOnly ===\n")
print(summary(model_global_globalonly))

# Balanced
data_balanced <- pivoted_data_unique_group_round %>% 
  filter(global_only_indicator == 0 & global_boost_indicator == 0)
model_global_balanced <- clmm2(
  global_group_contributions ~ round*period,
  random = group_id,
  data = data_balanced,
  Hess = TRUE
)
cat("\n=== GLOBAL COOPERATION - Balanced ===\n")
print(summary(model_global_balanced))

# Global Boost
data_globalboost <- pivoted_data_unique_group_round %>% filter(global_boost_indicator == 1)
model_global_globalboost <- clmm2(
  global_group_contributions ~ round * period,
  random = group_id,
  data = data_globalboost,
  Hess = TRUE,
  nAGQ = 10,  # Increase adaptive Gauss-Hermite quadrature points
  control = clmm2.control(maxIter = 1000, gradTol = 1e-4)
)
cat("\n=== GLOBAL COOPERATION - GlobalBoost ===\n")
print(summary(model_global_globalboost))

###########################################
# DEFECTION - Stability Analysis
###########################################

# Global Only
model_defect_globalonly <- clmm2(
  kept_group_contributions ~ round*period,
  random = group_id,
  data = data_global_only,
  Hess = TRUE
)
cat("\n=== DEFECTION - GlobalOnly ===\n")
print(summary(model_defect_globalonly))

# Balanced
model_defect_balanced <- clmm2(
  kept_group_contributions ~ round*period,
  random = group_id,
  data = data_balanced,
  Hess = TRUE
)
cat("\n=== DEFECTION - Balanced ===\n")
print(summary(model_defect_balanced))

# Global Boost
model_defect_globalboost <- clmm2(
  kept_group_contributions ~ round*period,
  random = group_id,
  data = data_globalboost,
  Hess = TRUE
)
cat("\n=== DEFECTION - GlobalBoost ===\n")
print(summary(model_defect_globalboost))
print(show_CI_clmm2(model_defect_globalboost))

###########################################
# LOCAL COOPERATION - Stability Analysis
###########################################

# Need subgroup-level data
pivoted_data_unique_subgroup_round <- pivoted_data_ord %>% 
  distinct(subgroup_id, round, .keep_all = TRUE)

pivoted_data_unique_subgroup_round <- pivoted_data_unique_subgroup_round %>%
  mutate(period = case_when(
    round <= 10 ~ "rounds_1_10",
    round <= 20 ~ "rounds_11_20"
  ))

pivoted_data_unique_subgroup_round$period <- factor(
  pivoted_data_unique_subgroup_round$period,
  levels = c("rounds_11_20", "rounds_1_10")
)

# Remove GlobalOnly condition
pivoted_data_unique_subgroup_round <- pivoted_data_unique_subgroup_round %>%
  filter(global_only_indicator == 0)
# Balanced
data_balanced_subgroup <- pivoted_data_unique_subgroup_round %>% 
  filter(global_boost_indicator == 0)
model_local_balanced <- clmm2(
  local_group_contributions ~ round*period,
  random = subgroup_id,
  data = data_balanced_subgroup,
  Hess = TRUE
)
cat("\n=== LOCAL COOPERATION - Balanced ===\n")
print(summary(model_local_balanced))
print(show_CI_clmm2(model_local_balanced))

# Global Boost
data_globalboost_subgroup <- pivoted_data_unique_subgroup_round %>% 
  filter(global_boost_indicator == 1)
model_local_globalboost <- clmm2(
  local_group_contributions ~ round*period,
  random = subgroup_id,
  data = data_globalboost_subgroup,
  Hess = TRUE
)
cat("\n=== LOCAL COOPERATION - GlobalBoost ===\n")
print(summary(model_local_globalboost))
print(show_CI_clmm2(model_local_globalboost))


# Function to extract model results with proper formatting
extract_model_results <- function(model, model_name) {
  # Get summary
  summ <- summary(model)
  
  # Get coefficients (excluding threshold parameters)
  coef_table <- coef(summ)
  
  # Get confidence intervals
  CI <- show_CI_clmm2(model)
  
  # Filter to only fixed effects (exclude threshold parameters which start with numbers)
  # and exclude the random effect stDev row
  fixed_effect_rows <- !grepl("^[0-9]", rownames(coef_table)) & 
    rownames(coef_table) != "stDev"
  
  coef_table_filtered <- coef_table[fixed_effect_rows, , drop = FALSE]
  coef_names <- rownames(coef_table_filtered)
  
  # Match CI rows to coefficient names
  CI_matched <- CI[match(coef_names, rownames(CI)), , drop = FALSE]
  
  results <- data.frame(
    Coefficient = coef_names,
    Model = model_name,
    Estimate = sprintf("%.3g", coef_table_filtered[, "Estimate"]),
    CI = sprintf("(%.3g, %.3g)", CI_matched[, "lower"], CI_matched[, "upper"]),
    P_value = ifelse(coef_table_filtered[, "Pr(>|z|)"] < 0.001, 
                     "p<0.001", 
                     sprintf("p = %.3g", coef_table_filtered[, "Pr(>|z|)"]))
  )
  
  return(results)
}

# Function to reshape results for wide format
create_wide_table <- function(model_list, condition_names) {
  all_results <- list()
  
  for (i in seq_along(model_list)) {
    results <- extract_model_results(model_list[[i]], condition_names[i])
    all_results[[i]] <- results
  }
  
  # Combine all results
  combined <- bind_rows(all_results)
  
  # Get unique coefficients in order
  unique_coefs <- unique(combined$Coefficient)
  
  # Create wide format
  wide_table <- data.frame(Coefficient = character(), stringsAsFactors = FALSE)
  
  for (coef in unique_coefs) {
    # Create 3 rows for this coefficient
    coef_data <- combined %>% filter(Coefficient == coef)
    
    # Row 1: Estimates
    row1 <- data.frame(Coefficient = coef, stringsAsFactors = FALSE)
    for (cond in condition_names) {
      val <- coef_data %>% filter(Model == cond) %>% pull(Estimate)
      row1[[cond]] <- ifelse(length(val) > 0, val, "")
    }
    
    # Row 2: CIs
    row2 <- data.frame(Coefficient = "", stringsAsFactors = FALSE)
    for (cond in condition_names) {
      val <- coef_data %>% filter(Model == cond) %>% pull(CI)
      row2[[cond]] <- ifelse(length(val) > 0, val, "")
    }
    
    # Row 3: P-values
    row3 <- data.frame(Coefficient = "", stringsAsFactors = FALSE)
    for (cond in condition_names) {
      val <- coef_data %>% filter(Model == cond) %>% pull(P_value)
      row3[[cond]] <- ifelse(length(val) > 0, val, "")
    }
    
    wide_table <- bind_rows(wide_table, row1, row2, row3)
  }
  
  return(wide_table)
}

###########################################
# Create CSV outputs
###########################################

# GLOBAL COOPERATION
global_models <- list(model_global_globalonly, model_global_balanced, model_global_globalboost)
global_conditions <- c("GlobalOnly", "Balanced", "GlobalBoost")
global_table <- create_wide_table(global_models, global_conditions)
write.csv(global_table, "results_global_cooperation_stability.csv", row.names = FALSE)
cat("\nGlobal cooperation table saved to: results_global_cooperation_stability.csv\n")

# DEFECTION
defect_models <- list(model_defect_globalonly, model_defect_balanced, model_defect_globalboost)
defect_conditions <- c("GlobalOnly", "Balanced", "GlobalBoost")
defect_table <- create_wide_table(defect_models, defect_conditions)
write.csv(defect_table, "results_defection_stability.csv", row.names = FALSE)
cat("\nDefection table saved to: results_defection_stability.csv\n")

# LOCAL COOPERATION (no GlobalOnly)
local_models <- list(model_local_balanced, model_local_globalboost)
local_conditions <- c("Balanced", "GlobalBoost")
local_table <- create_wide_table(local_models, local_conditions)
write.csv(local_table, "results_local_cooperation_stability.csv", row.names = FALSE)
cat("\nLocal cooperation table saved to: results_local_cooperation_stability.csv\n")


  summarise(variance = var(coop_numeric, na.rm = TRUE))