library("tidyverse")
library("dplyr")

data <- read.table("data/data_full_groups.csv", header=TRUE, sep=",", quote='"')

# Create an indicator variable
data <- data %>% 
  mutate(global_only_indicator = if_else(condition == "GlobalOnly", 1, 0),
         global_boost_indicator = if_else(condition == "GlobalBoost", 1, 0))

#################
# Get individual actions
cols_player_actions <- sprintf("game.%s.player.contribution", 1:20)

individual_data <- pivot_longer(data, 
                                cols_player_actions,
                                names_to = "round",
                                names_pattern = "game.(.*).player.contribution",
                                values_to = "action") %>%
  filter(action != "") %>%
  select(participant.code, group_id, condition, round, action)

individual_data$round <- as.numeric(individual_data$round)

#################
# Get group-level data for BR calculation
cols_n_global <- sprintf("game.%s.player.global_group_contributions", 1:20)
cols_n_local <- sprintf("game.%s.player.local_group_contributions", 1:20)
cols_n_defect <- sprintf("game.%s.player.kept_group_contributions", 1:20)

pivoted_data_long <- pivot_longer(data, 
                                  c(cols_n_global, cols_n_local, cols_n_defect),
                                  names_to = c("round", "game_variable"),
                                  names_pattern = "game.(.*).player.(.*)",
                                  values_transform = list(value = as.character))

pivoted_data <- pivot_wider(pivoted_data_long,
                            names_from = "game_variable",
                            values_from = "value") %>%
  select(group_id, round, condition, 
         global_group_contributions, 
         local_group_contributions,
         kept_group_contributions) %>%
  distinct(group_id, round, .keep_all = TRUE)

pivoted_data$round <- as.numeric(pivoted_data$round)
pivoted_data$global_group_contributions <- as.numeric(pivoted_data$global_group_contributions)
pivoted_data$local_group_contributions <- as.numeric(pivoted_data$local_group_contributions)
pivoted_data$kept_group_contributions <- as.numeric(pivoted_data$kept_group_contributions)

# Get condition indicators
condition_data <- data %>% 
  select(group_id, global_only_indicator, global_boost_indicator) %>%
  distinct()

pivoted_data <- pivoted_data %>%
  left_join(condition_data, by = "group_id")

#################
# Join and calculate BR
individual_data_br <- individual_data %>%
  left_join(pivoted_data, by = c("group_id", "round", "condition"))


was_best_response_hardcoded <- function(action,
                                        global_group_contributions,
                                        local_group_contributions,
                                        global_only_indicator,
                                        global_boost_indicator) {
  action <- if (action == "self") "kept" else action
  
  # derive condition from indicators
  if (isTRUE(global_boost_indicator == 1)) {
    condition <- "GlobalBoost"
  } else if (isTRUE(global_only_indicator == 1)) {
    condition <- "GlobalOnly"
  } else {
    condition <- "Balanced"
  }
  
  if (action == "global") {
    return(global_group_contributions == 4)
  }
  
  if (action == "local") {
    if (condition == "Balanced")    return(local_group_contributions == 2)
    if (condition == "GlobalBoost") return(local_group_contributions == 2 &&
                                             global_group_contributions != 3)
    return(FALSE) # GlobalOnly
  }
  
  if (action == "kept") {
    return(global_group_contributions != 3 && local_group_contributions != 1)
  }
  
  if (action == "other") {
    return(FALSE)
  }
  
  stop("Unknown action: ", action)
}



individual_data_br <- individual_data_br %>%
  rowwise() %>%
  mutate(
    is_best = was_best_response_hardcoded(
      action,
      global_group_contributions,
      local_group_contributions,
      global_only_indicator,
      global_boost_indicator
    )
  ) %>%
  ungroup()

#################
# Classify participants
participant_classification <- individual_data_br %>%
  group_by(participant.code, condition) %>%
  summarise(
    n_rounds = n(),
    n_global = sum(action == "global"),
    n_local = sum(action == "local"),
    n_defect = sum(action == "self"),
    n_br = sum(is_best),
    pct_global = n_global / n_rounds * 100,
    pct_local = n_local / n_rounds * 100,
    pct_defect = n_defect / n_rounds * 100,
    br_rate = n_br / n_rounds * 100,
    .groups = "drop"
  ) %>%
  mutate(
    strategy_type = case_when(
      pct_global >= 67 ~ "Global-oriented",
      pct_local >= 67 ~ "Local-oriented",
      pct_defect >= 67 ~ "Free-rider",
      TRUE ~ "Mixed"
    ),
    br_category = if_else(br_rate >= 67, "High BR", "Low BR")
  )

#################
# Report results

cat("\n=== STRATEGY TYPE DISTRIBUTION BY CONDITION ===\n")
strategy_dist <- participant_classification %>%
  group_by(condition, strategy_type) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(condition) %>%
  mutate(percentage = round(n / sum(n) * 100, 1))
print(strategy_dist)


cat("\n=== BEST RESPONSE DISTRIBUTION BY CONDITION ===\n")
br_dist <- participant_classification %>%
  group_by(condition, br_category) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(condition) %>%
  mutate(percentage = round(n / sum(n) * 100, 1))
print(br_dist)

cat("\n=== CROSS-CLASSIFICATION: STRATEGY TYPE × BEST RESPONSE ===\n")
cross_class <- participant_classification %>%
  group_by(condition, strategy_type, br_category) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(condition, strategy_type) %>%
  mutate(percentage = round(n / sum(n) * 100, 1))
print(cross_class)

cat("\n=== CROSS-TABULATION TABLES BY CONDITION ===\n")
for (cond in c("GlobalOnly", "Balanced", "GlobalBoost")) {
  cat("\n", cond, ":\n")
  cond_data <- participant_classification %>% filter(condition == cond)
  tab <- table(cond_data$strategy_type, cond_data$br_category)
  print(tab)
  print(prop.table(tab, margin = 1) * 100)  # Row percentages
}

combined_plot_data <- participant_classification %>%
  mutate(
    strategy_type = factor(
      strategy_type,
      levels = c("Global-oriented", "Local-oriented", "Free-rider", "Mixed")
    ),
    br_category = factor(br_category, levels = c("High BR", "Low BR"))
  ) %>%
  count(condition, strategy_type, br_category, name = "n") %>%
  group_by(condition) %>%
  mutate(proportion = n / sum(n)) %>%
  ungroup()

# 2) (Optional) color palettes; replace with yours if you already have them
strategy_colors <- c(
  "Global-oriented" = "#8e44ad",
  "Local-oriented"  =  "#e67e22",
  "Free-rider"      = "#404040",
  "Mixed"           = "#9E9E9E"
)


br_fill_colors <- c("High BR" = "#27ae60", "Low BR" = "#a9dfbf")



combined_plot_data$br_category <- factor(
  combined_plot_data$br_category,
  levels = c("Low BR", "High BR")
)

combined_plot_data$condition <- factor(
  combined_plot_data$condition,
  levels = c("GlobalOnly", "Balanced", "GlobalBoost")
)

# Relabel conditions for the facet strips (keeps order)
lvl_in  <- c("GlobalOnly", "Balanced", "GlobalBoost")
lbl_out <- c("Global Only", "Balanced", "Global Boost")

combined_plot_data <- combined_plot_data %>%
  dplyr::mutate(
    condition = factor(condition, levels = lvl_in, labels = lbl_out)
  )


annotation_df <- data.frame(
  condition = factor(c("Global Only", "Balanced", "Global Boost"), 
                     levels = c("Global Only", "Balanced", "Global Boost")),
  x = 1,  # Position at first strategy type
  y = 1,  # Top of plot
  label = c("a", "b", "c")
)



library(ggplot2)

# 3) Build the plot (compatible with your previous style)
p_combined <- ggplot(
  combined_plot_data,
  aes(x = strategy_type, y = proportion, fill = br_category)
) +
  geom_bar(stat = "identity", position = "stack",
           aes(color = strategy_type), size = 2) +
  scale_fill_manual(values = br_fill_colors, name = "Best Response") +
  scale_color_manual(values = strategy_colors, guide = "none") +
  theme_minimal() +
  ylab("Proportion of Participants") +
  xlab("Strategy Type") +
  ylim(0, 1) +
  facet_wrap(~ condition, nrow = 1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        strip.text = element_text(face = "bold", size = 12),
        panel.spacing = grid::unit(2, "lines"),
        legend.position = "bottom") +
  geom_text(data = annotation_df,
            aes(x = x, y = y, label = c("a", "b", "c")),
            color = "#404040", fontface = "bold", size = 5,
            vjust = 1.5, hjust = 1.7, inherit.aes = FALSE)

p_combined

ggsave("strategy_br_combined_distribution.png", p_combined, width = 10, dpi=600)


levels_strat <- c("Global-oriented", "Local-oriented", "Free-rider", "Mixed")

prop_high_by_strategy <- participant_classification %>%
  count(strategy_type, br_category, name = "n") %>%
  group_by(strategy_type) %>%
  summarise(
    n_total = sum(n),
    n_high  = sum(n[br_category == "High BR"]),
    prop_high_br = ifelse(n_total > 0, n_high / n_total, NA_real_)
  ) %>%
  tidyr::complete(strategy_type = levels_strat,
                  fill = list(n_total = 0, n_high = 0, prop_high_br = NA_real_)) %>%
  mutate(strategy_type = factor(strategy_type, levels = levels_strat)) %>%
  arrange(strategy_type)

prop_high_by_strategy

## Overall association
tab_overall <- xtabs(~ strategy_type + br_category, data = participant_classification)
chisq_overall <- chisq.test(tab_overall, correct = FALSE)
chisq_overall

# Test: Differences in action-oriented (non-Mixed) proportions across conditions

# Create action-oriented indicator
participant_classification <- participant_classification %>%
  mutate(action_oriented = strategy_type != "Mixed")

# Contingency table: condition × action-oriented
tab_action_oriented <- table(
  participant_classification$condition,
  participant_classification$action_oriented
)

cat("\n=== ACTION-ORIENTED vs MIXED BY CONDITION ===\n")
print(tab_action_oriented)
print(prop.table(tab_action_oriented, margin = 1) * 100)  # Row percentages

(54.6 + 66.1 + 72.5) / 3

# Chi-square test
chisq_action_oriented <- chisq.test(tab_action_oriented, correct = FALSE)
cat("\nChi-square test for action-oriented vs mixed across conditions:\n")
print(chisq_action_oriented)




## Test B: Differences in High BR proportions across conditions

# Contingency table: condition × BR category
tab_br <- table(
  participant_classification$condition,
  participant_classification$br_category
)
 

cat("\n\n=== HIGH BR vs LOW BR BY CONDITION ===\n")
print(tab_br)
print(prop.table(tab_br, margin = 1) * 100)  # Row percentages

# Chi-square test
chisq_br <- chisq.test(tab_br, correct = FALSE)
cat("\nChi-square test for BR category across conditions:\n")
print(chisq_br)

(14.3 + 9.9 + 15.5)/3
