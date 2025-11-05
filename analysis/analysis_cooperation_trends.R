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

# Local cooperation propogate round coefficient in GlobalBoost condition
# Get variance-covariance matrix
coef <- coef(model_local_more)
v <- vcov(model_local_more)

# Get combined estimate
combined_estimate <- coef["round"] + coef["round:global_boost_indicator"]

# Get combined SE
combined_se <- sqrt(v["round", "round"] + 
                      v["round:global_boost_indicator", "round:global_boost_indicator"] + 
                      2*v["round", "round:global_boost_indicator"])

# Calculate z and p value
z_value <- combined_estimate/combined_se
p_value <- 2 * pnorm(-abs(z_value))

# Print results
print(paste("Estimate:", round(combined_estimate, 4)))
print(paste("p-value:", format.pval(p_value, digits = 3)))


<<<<<<< HEAD

=======
>>>>>>> d46444cc10ad9e1d424b7a8fdf90c990114708ce
###############
# Plots
allshire_color <- "#8e44ad";
westville_color <- "#e67e22";
eastburgh_color <- "#01c8ee";
kept_color <- "#404040";

##########################
# Plot action counts

cols_player_actions <- sprintf("game.%s.player.contribution", 1:20)

pivoted_data_long <- pivot_longer(data, 
                                  cols_player_actions,
                                  names_to = c("round"),
                                  names_pattern = "game.(.*).player.contribution"
)





pivoted_data_long <- pivoted_data_long %>%
  filter(value != "")

action_sums <- pivoted_data_long %>% 
  group_by(round, condition, value) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::mutate(freq = n / sum(n))

action_sums <- action_sums %>%
  mutate(value = ifelse(value == '', "missing", value))

action_sums$round <- as.numeric(action_sums$round)
action_sums$value <- factor(as.character(action_sums$value), levels <- c("global", "local", "self",  "other", "missing"))

n_participants <- nrow(data)
n_groups <- length(unique(data$group_id))

action_sums$freq_se <- sqrt((action_sums$freq * (1 - action_sums$freq))/action_sums$n)

# Fill local and otehr as 0s if they are not in the data
complete_data <- expand_grid(
  round = unique(action_sums$round),
  condition = unique(action_sums$condition),
  value = unique(action_sums$value)
)

action_sums <- complete_data %>%
  left_join(action_sums, by = c("round", "condition", "value")) %>%
  replace_na(list(freq = 0, freq_se = 0))

# Remove zero counts for local in GLobalOnly
action_sums <- action_sums %>%
  filter(!((value == "local") & (condition == "GlobalOnly"))) %>%
  filter(!((value == "other") & (condition == "GlobalOnly")))


###################
# Plot Threhsolds
cols_global_threshold <- sprintf("game.%s.player.global_threshold_met", 1:20)
cols_local_threshold <- sprintf("game.%s.player.local_threshold_met", 1:20)
cols_other_threshold <- sprintf("game.%s.player.other_threshold_met", 1:20)

cols <- c(cols_global_threshold, cols_local_threshold, cols_other_threshold)

pivoted_data_long_ord <- pivot_longer(data, 
                                      cols,
                                      names_to = c("round", "game_variable"),
                                      names_pattern = "game.(.*).player.(.*)",
                                      values_transform = list(value = as.character)
)

# Remove unneeded columns
cols_of_interest <- c("participant.code",
                      "group_id",
                      "round",
                      "game_variable",
                      "value",
                      "players_in_group",
                      "players_in_group_non_exclusions",
                      "condition")

pivoted_data_ord <- pivot_wider(pivoted_data_long_ord,
                                names_from="game_variable",
                                values_from="value"
)

pivoted_data_unique_group_round <- pivoted_data_ord %>% distinct(group_id,round, .keep_all = TRUE)

pivoted_data_unique_group_round$global_threshold_met <- as.numeric(pivoted_data_unique_group_round$global_threshold_met)
pivoted_data_unique_group_round$local_threshold_met <- as.numeric(pivoted_data_unique_group_round$local_threshold_met)
pivoted_data_unique_group_round$other_threshold_met <- as.numeric(pivoted_data_unique_group_round$other_threshold_met)

pivoted_data_unique_group_round$totallocal_threshold_met <- pivoted_data_unique_group_round$local_threshold_met + pivoted_data_unique_group_round$other_threshold_met 

pivoted_data_unique_group_round$normedlocal_threshold_met <- pivoted_data_unique_group_round$totallocal_threshold_met/2

pivoted_data_unique_group_round$total_threshold_met <- pivoted_data_unique_group_round$global_threshold_met + pivoted_data_unique_group_round$normedlocal_threshold_met

cols_threshold_counts_normed <- c("global_threshold_met",
                                  "normedlocal_threshold_met")

pivoted_data_long <- pivot_longer(pivoted_data_unique_group_round, 
                                  cols_threshold_counts_normed,
                                  names_to = c("threshold_count"),
                                  names_pattern = "(.*)_threshold_met"
)

agg_threshold_counts_in_groups <- pivoted_data_long %>%
  group_by(round, condition, threshold_count) %>%
  dplyr::summarise(n = n(), mean=mean(value)) %>%
  dplyr::mutate(n = if_else(threshold_count == "normedlocal", 2*n, n))

agg_threshold_counts_in_groups$round <- as.numeric(agg_threshold_counts_in_groups$round)

# Remove zero counts for local in GLobalOnly
agg_threshold_counts_in_groups <- agg_threshold_counts_in_groups %>%
  filter(!((threshold_count == "normedlocal") & (condition == "GlobalOnly")))

# Replace totallocal with local
agg_threshold_counts_in_groups <- agg_threshold_counts_in_groups %>% 
  mutate(threshold_count = ifelse(threshold_count == "normedlocal", "local (normalised)", threshold_count))


agg_threshold_counts_in_groups <- agg_threshold_counts_in_groups %>% 
  mutate(threshold_count = ifelse(threshold_count == "normed", "total", threshold_count))

agg_threshold_counts_in_groups$mean_se <- sqrt((agg_threshold_counts_in_groups$mean * (1 - agg_threshold_counts_in_groups$mean))/agg_threshold_counts_in_groups$n)

agg_threshold_counts_in_groups <- agg_threshold_counts_in_groups %>%
  filter(!((condition == "GlobalOnly") & (threshold_count == "total")))

##########
# PLOTS

library(patchwork)


annotation_df <- data.frame(
  condition = c("GlobalOnly", "Balanced", "GlobalBoost"), # Match these to your actual group names
  x = 1, # Position on the x-axis; Inf means far right
  y = 1, # Position on the y-axis; adjust this based on your y-axis scale
  value="global",
  threshold_count = "global"
)
annotation_df$condition <- factor(annotation_df$condition, levels=c("GlobalOnly", "Balanced", "GlobalBoost"))

action_sums$condition <- factor(action_sums$condition, levels=c("GlobalOnly", "Balanced", "GlobalBoost"))



p1 <- ggplot(action_sums, aes(x=round, y=freq, color=value, fill=value, shape=value)) +
  geom_line(size=1) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin=freq - freq_se, ymax=freq + freq_se), width=0.2, alpha = 0.5) +
  scale_color_manual(values=c(allshire_color, westville_color, kept_color, eastburgh_color, "#cccccc")) +
  scale_fill_manual(values=c(allshire_color, westville_color, kept_color, eastburgh_color, "#cccccc")) +
  scale_shape_manual(values=c(21, 24, 22, 23)) +
  theme_minimal() +
  ylab("Action Proportion") +
  xlab("Round") +
  ylim(0,1.05) +
  xlim(0,20) +
  facet_wrap(~ condition) + 
  theme(legend.position="top") +
  theme(legend.title = element_blank()) +
  theme(axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())  +
  geom_text(data = annotation_df, aes(x = 1, y = 1, label = c("a", "b", "c")),
            color="#404040", fontface="bold", size=5, vjust = -0.4, hjust=0.4)


agg_threshold_counts_in_groups$condition <- factor(agg_threshold_counts_in_groups$condition, levels=c("GlobalOnly", "Balanced", "GlobalBoost"))




p2 <- ggplot(agg_threshold_counts_in_groups, aes(x=round, y=mean, group=threshold_count, color=threshold_count, fill=threshold_count, shape=threshold_count)) +
  geom_line(size=1) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin=mean - mean_se, ymax=mean + mean_se), width=0.2, alpha = 0.5) +
  scale_color_manual(values=c(allshire_color, westville_color, "#cccccc")) +
  scale_fill_manual(values=c(allshire_color, westville_color, "gray")) +
  scale_shape_manual(values=c(21, 24, 22, 23)) +
  theme_minimal() + 
  theme(legend.position="none") +
  ylab("Thresholds Met Proportion") +
  xlab("Round") +
  ylim(0,1.05) +
  xlim(0,20) +
  facet_wrap(~ condition) + 
  theme(strip.text = element_blank())  +
  geom_text(data = annotation_df, aes(x = 1, y = 1, label = c("d", "e", "f")),
            color="#404040", fontface="bold", size=5, vjust = -0.4, hjust=0.4)

#install.packages("cowplot")  
library(cowplot)

dummy_data_for_legend <- data.frame(
  round <- c(1,2,3, 4),
  color_types <- c("global", "local", "other", "defect"),
  values <- c(1,2,3,2)
)

dummy_data_for_legend$color_types <- factor(dummy_data_for_legend$color_types, levels=c("global", "local", "other", "defect"))

p0 <- ggplot(dummy_data_for_legend, aes(x=round, y=values, color=color_types)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values=c(allshire_color, westville_color, eastburgh_color, kept_color, "gray")) +
  theme_minimal() + 
  theme(legend.position="top") +
  theme(legend.title = element_blank()) +
  theme(axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())

legend <- cowplot::get_legend(p0)
# Remove legend from p1
p1 <- p1 + theme(legend.position = "none")

# Combine plots and place the legend at the bottom
final_plot <- (p1 / p2) / legend

final_plot + plot_layout(heights = c(1, 1, 0.1))

save_file <- "images/fig_actions_and_threhsolds_trends.png"
ggsave(save_file, dpi=600, bg='#ffffff', width = 8, height = 6)


<<<<<<< HEAD

#################################################
# Plot GLobalBoost local cooperation rates

# Extract GlobalBoost data for local cooperation
globalboost_local <- pivoted_data_unique_subgroup_round %>%
  filter(global_boost_indicator == 1)

# Convert local_group_contributions to numeric for centering
globalboost_local <- globalboost_local %>%
  mutate(local_numeric = as.numeric(as.character(local_group_contributions)))

# Center each subgroup on their round 1 value
globalboost_local_centered <- globalboost_local %>%
  group_by(subgroup_id) %>%
  mutate(local_centered = (local_numeric - local_numeric[round == 1]) * 6) %>%  # Multiply by 6
  ungroup()

# Calculate mean centered values by round
centered_means <- globalboost_local_centered %>%
  group_by(round) %>%
  summarise(
    mean_centered = mean(local_centered, na.rm = TRUE),
    se = sd(local_centered, na.rm = TRUE) / sqrt(n())
  )

# Create model prediction line (starting at 0 for round 1)
model_line <- data.frame(
  round = 1:20,
  predicted = -0.0248 * (1:20 - 1)  # Multiply by 6 for counts
)

# Plot
ggplot(centered_means, aes(x = round, y = mean_centered)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_line(data = model_line, aes(x = round, y = predicted, linetype = "Model prediction"), 
            color = "black", size = 1) +
  geom_point(aes(color = "Observed data"), size = 2) +
  geom_line(aes(color = "Observed data")) +
  geom_errorbar(aes(ymin = mean_centered - se, ymax = mean_centered + se, color = "Observed data"), 
                width = 0.2, alpha = 0.5) +
  scale_color_manual(values = c("Observed data" = westville_color)) +
  scale_linetype_manual(values = c("Model prediction" = "dashed")) +
  theme_minimal() +
  labs(
    x = "Round",
    y = "Change in Local Cooperation Count (from Round 1)",
    color = NULL,
    linetype = NULL
  ) +
  theme(legend.position = "bottom")

save_file <- "images/fig_global_boost_local_actions_centered.png"
ggsave(save_file, dpi=600, bg='#ffffff', width = 8, height = 6)


=======
>>>>>>> d46444cc10ad9e1d424b7a8fdf90c990114708ce
