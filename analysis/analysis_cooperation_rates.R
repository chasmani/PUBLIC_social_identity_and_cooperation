library("tidyverse")
library("dplyr")

data <- read.table("data/data_full_groups.csv", header=TRUE, sep=",", quote='"')

nrow(data)

###################
# 

cols_n_global <- sprintf("game.%s.player.global_group_contributions", 1:20)
cols_n_local <- sprintf("game.%s.player.local_group_contributions", 1:20)
cols_n_other <- sprintf("game.%s.player.other_group_contributions", 1:20)
cols_n_defect <- sprintf("game.%s.player.kept_group_contributions", 1:20)

cols <- c(cols_n_global, cols_n_local, cols_n_other, cols_n_defect)

pivoted_data_long <- pivot_longer(data, 
                                  cols,
                                  names_to = c("round", "action_type"),
                                  names_pattern = "game.(.*).player.(.*)",
                                  values_transform = list(value = as.character)
)

# Remove unneeded columns
cols_of_interest <- c("group_id",
                      "subgroup_id",
                      "round",
                      "action_type",
                      "value",
                      "condition")
pivoted_data_long <- pivoted_data_long[cols_of_interest]

# Global group contributions
# Remove repeated data from memebers fo each group
pivoted_data_unique <- pivoted_data_long %>% distinct(group_id,round,action_type, .keep_all = TRUE)

pivoted_data_unique$value <- as.integer(pivoted_data_unique$value)
pivoted_data_unique$id <- pivoted_data_unique$group_id

#########
# GLobal

# Get just global contributions
data_global_counts <- pivoted_data_unique %>%
  filter(action_type=="global_group_contributions") %>%
  group_by(id, condition) %>%
  dplyr::summarise(total = sum(value)/120)

data_global_counts$count_type <- "Actions"
data_global_counts$level <- "Global"


########
# Local

pivoted_data_unique_subgroup <- pivoted_data_long %>% distinct(subgroup_id,round,action_type, .keep_all = TRUE)

pivoted_data_unique_subgroup$value <- as.integer(pivoted_data_unique_subgroup$value)
pivoted_data_unique_subgroup$id <- pivoted_data_unique_subgroup$subgroup_id


data_local_counts <- pivoted_data_unique_subgroup %>%
  filter(action_type=="local_group_contributions") %>%
  group_by(id, condition) %>%
  dplyr::summarise(total = sum(value)/60)

data_local_counts$count_type <- "Actions"
data_local_counts$level <- "Local"

#########
# Defect

# Get just global contributions
data_kept_counts <- pivoted_data_unique %>%
  filter(action_type=="kept_group_contributions") %>%
  group_by(id, condition) %>%
  dplyr::summarise(total = sum(value)/120)

data_kept_counts$count_type <- "Actions"
data_kept_counts$level <- "Self"

#########
# Global Thresholds

cols_n_global_threshold <- sprintf("game.%s.player.global_threshold_met", 1:20)

pivoted_data_global_threshold <- pivot_longer(data, 
                                              cols_n_global_threshold,
                                              names_to = c("round"),
                                              names_pattern = "game.(.*).player.global_threshold_met",
                                              values_transform = list(value = as.character)
)

# Remove unneeded columns
cols_of_interest <- c("group_id",
                      "round",
                      "value",
                      "condition")
pivoted_data_global_threshold <- pivoted_data_global_threshold[cols_of_interest]

# Global group contributions
# Remove repeated data from memebers fo each group
global_threshold_unique <- pivoted_data_global_threshold %>% distinct(group_id,round, .keep_all = TRUE)

global_threshold_unique$value <- as.integer(global_threshold_unique$value)
global_threshold_unique$id <- global_threshold_unique$group_id

data_global_thresholds <- global_threshold_unique %>%
  group_by(id, condition) %>%
  dplyr::summarise(total = sum(value)/20)

data_global_thresholds$count_type <- "Thresholds"
data_global_thresholds$level <- "Global"



#########
# Local Thresholds

cols_n_local_threshold <- sprintf("game.%s.player.local_threshold_met", 1:20)

pivoted_data_local_threshold <- pivot_longer(data, 
                                              cols_n_local_threshold,
                                              names_to = c("round"),
                                              names_pattern = "game.(.*).player.local_threshold_met",
                                              values_transform = list(value = as.character)
)

# Remove unneeded columns
cols_of_interest <- c("subgroup_id",
                      "round",
                      "value",
                      "condition")
pivoted_data_local_threshold <- pivoted_data_local_threshold[cols_of_interest]

# Global group contributions
# Remove repeated data from memebers fo each group
local_threshold_unique <- pivoted_data_local_threshold %>% distinct(subgroup_id,round, .keep_all = TRUE)

local_threshold_unique$value <- as.integer(local_threshold_unique$value)
local_threshold_unique$id <- local_threshold_unique$subgroup_id

data_local_thresholds <- local_threshold_unique %>%
  group_by(id, condition) %>%
  dplyr::summarise(total = sum(value)/20)

data_local_thresholds$count_type <- "Thresholds"
data_local_thresholds$level <- "Local"

df <- dplyr::bind_rows(data_global_counts, data_local_counts, data_kept_counts, 
                       data_global_thresholds, data_local_thresholds)

# Plots
df$condition <- factor(df$condition, levels = c("GlobalOnly", "Balanced", "GlobalBoost"))

df$level <- if_else(df$level == "Self", "Defect", df$level)

df <- df %>%
  filter(!(condition == "GlobalOnly" & level == "Local"))

df <- df %>%
  mutate(grouping = interaction(level, count_type, sep=" "))

df$grouping <- factor(df$grouping, levels = c("Global Actions", "Local Actions", "Defect Actions", "Global Thresholds", "Local Thresholds"))


ggplot(df, aes(x = condition, y = total, color = condition)) +
  geom_boxplot() +  # Removed position_dodge, adjust if needed
  geom_jitter(width=0.2, alpha=0.2) +  # Adjust 'width' to ensure alignment
  facet_wrap(~grouping, nrow=1) +
  labs(y = "Proportion") +
  theme_minimal() +
  theme(legend.position="bottom") +
  theme(axis.text.x = element_blank())

#install.packages("ggsignif")
library(ggsignif)

install.packages("ggpubr")
library(ggpubr)


annotation_df <- data.frame(
  grouping = c("Global Actions", "Local Actions", "Defect Actions", "Global Thresholds", "Local Thresholds"), # Match these to your actual group names
  x = 1, # Position on the x-axis; Inf means far right
  y = 1, # Position on the y-axis; adjust this based on your y-axis scale
  condition="Global Only"
)
annotation_df$grouping <- factor(annotation_df$grouping, levels=c("Global Actions", "Local Actions", "Defect Actions", "Global Thresholds", "Local Thresholds"))


ggplot(df, aes(x = condition, y = total, color = condition)) +
  geom_boxplot(outlier.shape=NA) +  # Removed position_dodge, adjust if needed
  geom_jitter(width=0.2, alpha=0.2) +  # Adjust 'width' to ensure alignment
  facet_wrap(~grouping, nrow=1, strip.position="bottom") +
  geom_signif(comparisons = list(c("GlobalOnly", "Balanced"), c("Balanced", "GlobalBoost")), 
              map_signif_level=TRUE, y_position = c(1.1, 1.2), color="#718093") +
  geom_signif(comparisons = list(c("GlobalOnly", "GlobalBoost")), 
              annotations="NS.", y_position = c(1.3), color="#718093") +
  stat_summary(fun.y = mean, geom = "point", shape = "x", size = 5) +
  labs(y = "Proportion") +
  theme_minimal() +
  theme(legend.position="bottom",
        legend.title=element_blank()) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  geom_text(data = annotation_df, aes(x = x, y = y, label = c("a", "b", "c", "d", "e")),
            color="#404040", fontface="bold", size=5, vjust=-8.7, hjust=1.6)

ggsave("images/outcomes_boxplots.png", dpi=600, width = 8, height = 6, bg='#ffffff')

################################
# Some stats
# Proprtions of global, local, other, defection rates across all conidtions. 


# 1. Total global, defection rates across all conditions
result_actions <- data %>%
  select(matches("game\\.\\d+\\.player\\.contribution")) %>%
  pivot_longer(cols = everything(), names_to = "round", values_to = "decision") %>%
  group_by(decision) %>%
  dplyr::summarise(total = n()) %>%
  dplyr::mutate(proportion = total/sum(total))

# 2. Total thresholds across conditions
result_global_thresholds <- data %>%
  select(matches("game\\.\\d+\\.player\\.global_threshold_met")) %>%
  pivot_longer(cols = everything(), names_to = "round", values_to = "decision") %>%
  group_by(decision) %>%
  dplyr::summarise(total = n()) %>%
  dplyr::mutate(proportion = total/sum(total))  

# 3. Total local, other actions rates across GlobalBoost and Balanced conditions
result_actions_local <- data %>%
  filter(condition != "GlobalOnly") %>%
  select(matches("game\\.\\d+\\.player\\.contribution")) %>%
  pivot_longer(cols = everything(), names_to = "round", values_to = "decision") %>%
  group_by(decision) %>%
  dplyr::summarise(total = n()) %>%
  dplyr::mutate(proportion = total/sum(total))

result_thresholds_local <- data %>%
  filter(condition != "GlobalOnly") %>%
  select(matches("game\\.\\d+\\.player\\.local_threshold_met")) %>%
  pivot_longer(cols = everything(), names_to = "round", values_to = "decision") %>%
  group_by(decision) %>%
  dplyr::summarise(total = n())  %>%
  dplyr::mutate(proportion = total/sum(total))


compare_pairwise_conditions <- function(df, conditions, this_grouping) {
  
  this_df <- df %>%
    filter(condition %in% conditions) %>%
    filter(grouping == this_grouping)

  result <- wilcox.test(total ~ condition, data=this_df)  
  print(result)
  
  this_counts <- this_df %>%
    group_by(condition) %>%
    dplyr::summarise(total = mean(total))
  print(this_counts)
}


compare_pairwise_conditions(df, c("GlobalOnly", "Balanced"), "Global Actions")
compare_pairwise_conditions(df, c("GlobalBoost", "Balanced"), "Global Actions")
compare_pairwise_conditions(df, c("GlobalBoost", "GlobalOnly"), "Global Actions")

compare_pairwise_conditions(df, c("GlobalBoost", "Balanced"), "Local Actions")

compare_pairwise_conditions(df, c("GlobalOnly", "Balanced"), "Defect Actions")
compare_pairwise_conditions(df, c("GlobalBoost", "Balanced"), "Defect Actions")
compare_pairwise_conditions(df, c("GlobalBoost", "GlobalOnly"), "Defect Actions")

compare_pairwise_conditions(df, c("GlobalOnly", "Balanced"), "Global Thresholds")
compare_pairwise_conditions(df, c("GlobalBoost", "Balanced"), "Global Thresholds")
compare_pairwise_conditions(df, c("GlobalBoost", "GlobalOnly"), "Global Thresholds")

compare_pairwise_conditions(df, c("GlobalBoost", "Balanced"), "Local Thresholds")

