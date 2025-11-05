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
                      "condition",
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

###########################################
# Group-Level Cooperation Patterns
# Jaccard Similarity for Successful Groups
###########################################



# Get group-level data for rounds 11-20
final_rounds_data <- pivoted_data_unique_group_round %>%
  filter(round >= 11 & round <= 20)


GLOBAL_THRESHOLD <- 4

# Calculate global threshold success for each group in rounds 11-20
group_success <- final_rounds_data %>%
  mutate(
    global_threshold_met = as.numeric(as.character(global_group_contributions)) >= GLOBAL_THRESHOLD
  ) %>%
  group_by(group_id, condition) %>%
  summarise(
    n_success = sum(global_threshold_met),
    .groups = "drop"
  )

# Filter for successful groups (at least 4 out of 5 rounds)
successful_groups <- group_success %>%
  filter(n_success >= 8)

cat("\n=== Number of Successful Groups by Condition ===\n")
print(successful_groups %>% 
        group_by(condition) %>% 
        summarise(n_groups = n()))

#################
# Get individual actions
cols_player_actions <- sprintf("game.%s.player.contribution", 1:20)

individual_data_br <- pivot_longer(data, 
                                   cols_player_actions,
                                   names_to = "round",
                                   names_pattern = "game.(.*).player.contribution",
                                   values_to = "action") %>%
  filter(action != "") %>%
  select(participant.code, group_id, subgroup_id, condition, round, action)

individual_data_br$round <- as.numeric(individual_data_br$round)

LOCAL_THRESHOLD <- 2

# Get individual actions for rounds 11-20
individual_final_rounds <- individual_data_br %>%
  filter(round >= 11 & round <= 20) %>%
  filter(group_id %in% successful_groups$group_id)

# Calculate Jaccard similarity for each group
jaccard_results <- list()

for (gid in successful_groups$group_id) {
  group_data <- individual_final_rounds %>%
    filter(group_id == gid) %>%
    arrange(round)
  
  # Get condition
  cond <- group_data$condition[1]
  
  # Calculate Jaccard for consecutive rounds
  jaccard_values <- c()
  
  for (r in 11:19) {
    # Get contributors in round r
    contributors_r <- group_data %>%
      filter(round == r, action == "global") %>%
      pull(participant.code)
    
    # Get contributors in round r+1
    contributors_r1 <- group_data %>%
      filter(round == r+1, action == "global") %>%
      pull(participant.code)
    
    # Calculate Jaccard similarity
    if (length(contributors_r) == 0 & length(contributors_r1) == 0) {
      jaccard <- 1  # Both empty sets
    } else {
      intersection <- length(intersect(contributors_r, contributors_r1))
      union <- length(union(contributors_r, contributors_r1))
      jaccard <- intersection / union
    }
    
    jaccard_values <- c(jaccard_values, jaccard)
  }
  
  # Calculate mean Jaccard for this group
  mean_jaccard <- mean(jaccard_values)
  
  jaccard_results[[length(jaccard_results) + 1]] <- data.frame(
    group_id = gid,
    condition = cond,
    mean_jaccard = mean_jaccard
  )
}

# Combine results
jaccard_df <- bind_rows(jaccard_results)

# Summary statistics by condition
cat("\n=== Mean Jaccard Similarity by Condition ===\n")
jaccard_summary <- jaccard_df %>%
  group_by(condition) %>%
  summarise(
    n_groups = n(),
    mean_jaccard = mean(mean_jaccard),
    sd_jaccard = sd(mean_jaccard),
    median_jaccard = median(mean_jaccard),
    .groups = "drop"
  )

print(jaccard_summary)



# Calculate mean number of global contributors for successful groups
mean_contributors <- individual_final_rounds %>%
  filter(action == "global") %>%
  group_by(group_id, condition, round) %>%
  summarise(n_contributors = n(), .groups = "drop") %>%
  group_by(group_id, condition) %>%
  summarise(mean_contributors = mean(n_contributors), .groups = "drop")

# Summary by condition
cat("\n=== Mean Number of Global Contributors by Condition (Successful Groups) ===\n")
contributors_summary <- mean_contributors %>%
  group_by(condition) %>%
  summarise(
    n_groups = n(),
    mean_contributors = mean(mean_contributors),
    sd_contributors = sd(mean_contributors),
    median_contributors = median(mean_contributors),
    .groups = "drop"
  )

print(contributors_summary)







###########################################
# Local Cooperation Patterns
# Jaccard Similarity for Successful Local Groups
###########################################

# Get subgroup-level data for rounds 11-20
# Need to work at subgroup level for local cooperation
final_rounds_data_subgroup <- pivoted_data_ord %>%
  filter(round >= 11 & round <= 20) %>%
  distinct(subgroup_id, round, .keep_all = TRUE)

# Calculate local threshold success for each subgroup in rounds 11-20
subgroup_success <- final_rounds_data_subgroup %>%
  filter(global_only_indicator == 0) %>%  # Exclude GlobalOnly
  mutate(
    local_threshold_met = as.numeric(as.character(local_group_contributions)) >= LOCAL_THRESHOLD
  ) %>%
  group_by(subgroup_id, condition) %>%
  summarise(
    n_success = sum(local_threshold_met),
    .groups = "drop"
  )

# Filter for successful subgroups (at least 4 out of 5 rounds)
successful_subgroups <- subgroup_success %>%
  filter(n_success >= 8)

cat("\n=== Number of Successful Local Subgroups by Condition ===\n")
print(successful_subgroups %>% 
        group_by(condition) %>% 
        summarise(n_subgroups = n()))

# Get individual actions for rounds 11-20 for successful subgroups
individual_final_rounds_local <- individual_data_br %>%
  filter(round >= 11 & round <= 20) %>%
  filter(subgroup_id %in% successful_subgroups$subgroup_id)

# Calculate Jaccard similarity for each subgroup
jaccard_results_local <- list()

for (sid in successful_subgroups$subgroup_id) {
  subgroup_data <- individual_final_rounds_local %>%
    filter(subgroup_id == sid) %>%
    arrange(round)
  
  # Get condition
  cond <- subgroup_data$condition[1]
  
  # Calculate Jaccard for consecutive rounds
  jaccard_values <- c()
  
  for (r in 11:19) {
    # Get local contributors in round r
    contributors_r <- subgroup_data %>%
      filter(round == r, action == "local") %>%
      pull(participant.code)
    
    # Get local contributors in round r+1
    contributors_r1 <- subgroup_data %>%
      filter(round == r+1, action == "local") %>%
      pull(participant.code)
    
    # Calculate Jaccard similarity
    if (length(contributors_r) == 0 & length(contributors_r1) == 0) {
      jaccard <- 1  # Both empty sets
    } else {
      intersection <- length(intersect(contributors_r, contributors_r1))
      union <- length(union(contributors_r, contributors_r1))
      jaccard <- intersection / union
    }
    
    jaccard_values <- c(jaccard_values, jaccard)
  }
  
  # Calculate mean Jaccard for this subgroup
  mean_jaccard <- mean(jaccard_values)
  
  jaccard_results_local[[length(jaccard_results_local) + 1]] <- data.frame(
    subgroup_id = sid,
    condition = cond,
    mean_jaccard = mean_jaccard
  )
}

# Combine results
jaccard_df_local <- bind_rows(jaccard_results_local)

# Summary statistics by condition
cat("\n=== Mean Jaccard Similarity for Local Cooperation by Condition ===\n")
jaccard_summary_local <- jaccard_df_local %>%
  group_by(condition) %>%
  summarise(
    n_subgroups = n(),
    mean_jaccard = mean(mean_jaccard),
    sd_jaccard = sd(mean_jaccard),
    median_jaccard = median(mean_jaccard),
    .groups = "drop"
  )

print(jaccard_summary_local)

# Calculate mean number of local contributors for successful subgroups
mean_contributors_local <- individual_final_rounds_local %>%
  filter(action == "local") %>%
  group_by(subgroup_id, condition, round) %>%
  summarise(n_contributors = n(), .groups = "drop") %>%
  group_by(subgroup_id, condition) %>%
  summarise(mean_contributors = mean(n_contributors), .groups = "drop")

# Summary by condition
cat("\n=== Mean Number of Local Contributors by Condition (Successful Subgroups) ===\n")
contributors_summary_local <- mean_contributors_local %>%
  group_by(condition) %>%
  summarise(
    n_subgroups = n(),
    mean_contributors = mean(mean_contributors),
    sd_contributors = sd(mean_contributors),
    median_contributors = median(mean_contributors),
    .groups = "drop"
  )

print(contributors_summary_local)
