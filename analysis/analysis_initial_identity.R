library("tidyverse")
library("dplyr")
#  install.packages('coin')

data <- read.table("data/data_participants.csv", header=TRUE, sep=",", quote='"')

# Model
convert_DIFI <- function(DIFI_value) {
  # Convert the DIFI range to an integer from 0-100.
  # DIFI Range is [-175.69, 125]
  return ((DIFI_value + 175.7)*100/(175.7+125))
}

data$global_initial_group_identity <- convert_DIFI(data$survey.1.player.DIFI_big_group_distance)
data$local_initial_group_identity <- convert_DIFI(data$survey.1.player.DIFI_my_group_distance)
data$other_initial_group_identity <- convert_DIFI(data$survey.1.player.DIFI_other_group_distance)

nrow(data)

table(data$condition)


balanced_data <- data %>%
  filter(condition == "Balanced")

shapiro.test(balanced_data$global_initial_group_identity)
shapiro.test(balanced_data$local_initial_group_identity)
shapiro.test(balanced_data$other_initial_group_identity)



# Group Identity
cols_to_compare <- c("global_initial_group_identity", 
                     "local_initial_group_identity",
                     "other_initial_group_identity")

pivoted_balanced <- pivot_longer(balanced_data, cols=cols_to_compare, 
                             names_to="initial_identity",
                             values_to="value"
)



#install.packages("rstatix")
#install.packages("coin")
library(rstatix)

mean(balanced_data$local_initial_group_identity)
sd(balanced_data$local_initial_group_identity)
mean(balanced_data$global_initial_group_identity)
sd(balanced_data$global_initial_group_identity)
mean(balanced_data$other_initial_group_identity)
sd(balanced_data$other_initial_group_identity)


pivoted_balanced %>% 
  pairwise_wilcox_test(value ~ initial_identity,, paired = TRUE)

pivoted_balanced %>% 
  wilcox_effsize(value ~ initial_identity,, paired = TRUE)


global_only_data <- data %>%
  filter(condition == "GlobalOnly")

shapiro.test(global_only_data$global_initial_group_identity)
shapiro.test(global_only_data$local_initial_group_identity)
shapiro.test(global_only_data$other_initial_group_identity)

# Group Identity
cols_to_compare <- c("global_initial_group_identity", 
                     "local_initial_group_identity",
                     "other_initial_group_identity")

pivoted_go <- pivot_longer(global_only_data, cols=cols_to_compare, 
                                 names_to="initial_identity",
                                 values_to="value"
)

#install.packages("rstatix")
#install.packages("coin")
library(rstatix)



mean(global_only_data$local_initial_group_identity)
sd(global_only_data$local_initial_group_identity)
mean(global_only_data$global_initial_group_identity)
sd(global_only_data$global_initial_group_identity)
mean(global_only_data$other_initial_group_identity)
sd(global_only_data$other_initial_group_identity)

pivoted_go %>% 
  pairwise_wilcox_test(value ~ initial_identity, paired = TRUE, p.adjust.method = "none") %>%
  mutate(p = format.pval(p, digits = 10))

# Do the same thing more manually to get more significant figures 
unique_pairs <- combn(unique(pivoted_go$initial_identity), 2, simplify = FALSE)

# Perform Wilcoxon signed-rank test for each pair and capture precise p-values
pairwise_results <- lapply(unique_pairs, function(pair) {
  subset_data <- pivoted_go %>% filter(initial_identity %in% pair)
  test_result <- wilcox.test(value ~ initial_identity, data = subset_data, paired = TRUE, exact = TRUE)
  data.frame(
    group1 = pair[1],
    group2 = pair[2],
    p_value = formatC(test_result$p.value, format = "e", digits = 10)
  )
})

pairwise_results_df <- do.call(rbind, pairwise_results)

# Display the results
pairwise_results_df

global_boost_data <- data %>%
  filter(condition == "GlobalBoost")

shapiro.test(global_boost_data$global_initial_group_identity)
shapiro.test(global_boost_data$local_initial_group_identity)
shapiro.test(global_boost_data$other_initial_group_identity)

# Group Identity
cols_to_compare <- c("global_initial_group_identity", 
                     "local_initial_group_identity",
                     "other_initial_group_identity")

pivoted_gb <- pivot_longer(global_boost_data, cols=cols_to_compare, 
                           names_to="initial_identity",
                           values_to="value"
)

# install.packages("rstatix")
library(rstatix)

mean(global_boost_data$local_initial_group_identity)
sd(global_boost_data$local_initial_group_identity)
mean(global_boost_data$global_initial_group_identity)
sd(global_boost_data$global_initial_group_identity)
mean(global_boost_data$other_initial_group_identity)
sd(global_boost_data$other_initial_group_identity)

pivoted_gb %>% 
  pairwise_wilcox_test(value ~ initial_identity,, paired = TRUE)


# Comparing GlobalBoost and GLobalOnly
data %>% 
  pairwise_wilcox_test(global_initial_group_identity ~ condition, paired = FALSE)

data %>% 
  pairwise_wilcox_test(local_initial_group_identity ~ condition, paired = FALSE)

data %>% 
  pairwise_wilcox_test(other_initial_group_identity ~ condition, paired = FALSE)


