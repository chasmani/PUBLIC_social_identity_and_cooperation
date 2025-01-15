
library("tidyverse")

balanced_data_source <- "data/data_balanced.csv"
balanced_data <- read.table(balanced_data_source, header=TRUE, sep=",", quote='"')

data <- balanced_data

#####################
# Exclusions

# Unique group id
data$group_id <- paste(data$session.code, data$game.1.group.id_in_subsession, sep="-")
# Subgroup
data <- data %>% 
  mutate(subgroup = if_else(participant.custom_id_in_group <= 3, 1, 2))
data$subgroup_id <- paste(data$group_id, data$subgroup, sep="-")


# Dropouts
data <- data %>% 
  filter((participant._current_app_name == "payment"))

data <- data %>% 
  filter(!(game.20.player.contribution == ""))

# Comprehension checks
data <- data %>%
  filter(quiz_exclusion == 0)

# Attention checks
data <- data %>%
  filter(survey.1.player.attention_check_1 == 2) %>%
  filter(survey_post.1.player.attention_check_1 == 4)

####################
# Variables

nrow(data)

# Perception scales
data$global_initial_mutual_dependence <- data$survey.1.player.survey_global_sis_1 + (5 - data$survey.1.player.survey_global_sis_3)
data$local_initial_mutual_dependence <- data$survey.1.player.survey_local_sis_1 + (5 - data$survey.1.player.survey_local_sis_3)
data$other_initial_mutual_dependence <- data$survey.1.player.survey_other_sis_1 + (5 - data$survey.1.player.survey_other_sis_3)

data$global_initial_conflict <- data$survey.1.player.survey_global_sis_4 + (5 - data$survey.1.player.survey_global_sis_2)
data$local_initial_conflict <- data$survey.1.player.survey_local_sis_4 + (5 - data$survey.1.player.survey_local_sis_2)
data$other_initial_conflict <- data$survey.1.player.survey_other_sis_4 + (5 - data$survey.1.player.survey_other_sis_2)

convert_DIFI <- function(DIFI_value) {
  # Convert the DIFI range to an integer from 0-100.
  # DIFI Range is [-175.69, 125]
  return ((DIFI_value + 175.7)*100/(175.7+125))
}

data$global_initial_group_identity <- convert_DIFI(data$survey.1.player.DIFI_big_group_distance)
data$local_initial_group_identity <- convert_DIFI(data$survey.1.player.DIFI_my_group_distance)
data$other_initial_group_identity <- convert_DIFI(data$survey.1.player.DIFI_other_group_distance)


# Get the varibles for final survey measures
data$global_final_mutual_dependence <- data$survey_post.1.player.survey_global_sis_1 + (5 - data$survey_post.1.player.survey_global_sis_3)
data$local_final_mutual_dependence <- data$survey_post.1.player.survey_local_sis_1 + (5 - data$survey_post.1.player.survey_local_sis_3)
data$other_final_mutual_dependence <- data$survey_post.1.player.survey_other_sis_1 + (5 - data$survey_post.1.player.survey_other_sis_3)

data$global_final_conflict <- data$survey_post.1.player.survey_global_sis_4 + (5 - data$survey_post.1.player.survey_global_sis_2)
data$local_final_conflict <- data$survey_post.1.player.survey_local_sis_4 + (5 - data$survey_post.1.player.survey_local_sis_2)
data$other_final_conflict <- data$survey_post.1.player.survey_other_sis_4 + (5 - data$survey_post.1.player.survey_other_sis_2)

data$global_final_group_identity <- convert_DIFI(data$game.20.player.DIFI_big_group_distance)
data$local_final_group_identity <- convert_DIFI(data$game.20.player.DIFI_my_group_distance)
data$other_final_group_identity <- convert_DIFI(data$game.20.player.DIFI_other_group_distance)


################################################################################
# Hypothesis 1 
# Differences in initial perceptions of mutual dependence, conflict and group identity. 
################################################################################

# Group Identity
cols_to_compare <- c("global_initial_group_identity", 
                     "local_initial_group_identity",
                     "other_initial_group_identity")

pivoted_data <- pivot_longer(data, cols=cols_to_compare, 
                             names_to="initial_identity",
                             values_to="value"
)

#install.packages("rstatix")
#install.packages("coin")
library(rstatix)

mean(data$global_initial_group_identity)
sd(data$global_initial_group_identity)
mean(data$local_initial_group_identity)
sd(data$local_initial_group_identity)
mean(data$other_initial_group_identity)
sd(data$other_initial_group_identity)

shapiro.test(data$global_initial_group_identity)
shapiro.test(data$local_initial_group_identity)
shapiro.test(data$other_initial_group_identity)

pivoted_data %>% 
  pairwise_wilcox_test(value ~ initial_identity, paired = TRUE)


# Mutual Dependence
cols_to_compare <- c("global_initial_mutual_dependence",
                     "local_initial_mutual_dependence",
                     "other_initial_mutual_dependence")

pivoted_data <- pivot_longer(data, cols=cols_to_compare, 
                             names_to="initial_perception",
                             values_to="value"
)

mean(data$global_initial_mutual_dependence)
sd(data$global_initial_mutual_dependence)
mean(data$local_initial_mutual_dependence)
sd(data$local_initial_mutual_dependence)
mean(data$other_initial_mutual_dependence)
sd(data$other_initial_mutual_dependence)

shapiro.test(data$global_initial_mutual_dependence)
shapiro.test(data$local_initial_mutual_dependence)
shapiro.test(data$other_initial_mutual_dependence)

pivoted_data %>% 
  pairwise_wilcox_test(value ~ initial_perception, paired = TRUE)



# Conflict
cols_to_compare <- c("global_initial_conflict",
                     "local_initial_conflict",
                     "other_initial_conflict")

pivoted_data <- pivot_longer(data, cols=cols_to_compare, 
                             names_to="initial_perception",
                             values_to="value"
)


mean(data$global_initial_conflict)
sd(data$global_initial_conflict)
mean(data$local_initial_conflict)
sd(data$local_initial_conflict)
mean(data$other_initial_conflict)
sd(data$other_initial_conflict)


shapiro.test(data$global_initial_conflict)
shapiro.test(data$local_initial_conflict)
shapiro.test(data$other_initial_conflict)

pivoted_data %>% 
  pairwise_wilcox_test(value ~ initial_perception, paired = TRUE)

# Note - Adjust p-values for one/two-sided tests. 

################################################################################
# Hypothesis 2 & 3
# Initial perceptions of mutual dependence/conflict at local and global scales
# will be correlated with perceptions of social identity. 
################################################################################

get_CI_spearman_rank <- function(cor_result, n) {
  # Spearman's rho to Fisher Z transformation
  z <- atanh(res$estimate)
  # Standard error
  se <- 1 / sqrt(n - 3)
  # Confidence interval
  z.ci <- qnorm(c(0.025, 0.975)) * se
  # Transform back to correlation
  ci <- tanh(z + z.ci)
  # Print results
  cat("Spearman's rho:", res$estimate, "\n",
      "95% CI:", ci, "\n")
}

# Local mutual dependence ~ local social identity
res <- cor.test(data$local_initial_mutual_dependence, 
                data$local_initial_group_identity, 
                method = "spearman", exact = FALSE)
print(res)
get_CI_spearman_rank(res, nrow(data))

# Global group identity ~ global md
res <- cor.test(data$global_initial_group_identity, 
         data$global_initial_mutual_dependence, method = "spearman",
         exact=FALSE)

print(res)
get_CI_spearman_rank(res, nrow(data))

# Local group identity ~ local conflict
res <- cor.test(data$local_initial_group_identity, 
         data$local_initial_conflict, method = "spearman",
         exact=FALSE)

print(res)
get_CI_spearman_rank(res, nrow(data))

# Global group identity ~ global conflict
res <- cor.test(data$global_initial_group_identity, 
         data$global_initial_conflict, method = "spearman",
         exact=FALSE)

print(res)
get_CI_spearman_rank(res, nrow(data))

### EXTRA
# Look at all observations
group_types <- c("global", "local", "other")
time_points <- c("initial", "final")
perceptions <- c("mutual_dependence", "conflict", "group_identity")


# Generate all combinations
combinations <- expand.grid(group_type = group_types, 
                            time = time_points, 
                            perception = perceptions)

# Create column names
column_names <- apply(combinations, 1, function(x) paste(x, collapse = "_"))

# Pivot data to get perception emasures in all group_type and initial and final
df_long <- pivot_longer(data, 
                        cols = all_of(column_names),
                        names_to = c("group_type", "time", "perception"),
                        names_pattern = "^([a-z]+)_(initial|final)_(.+)$",
                        values_to = "perception_value"
) 

df_wide <- pivot_wider(df_long, 
    names_from = perception,
    values_from = perception_value,
    names_prefix = "",
  )

# all group identity ~ md
res <- cor.test(df_wide$group_identity, 
         df_wide$mutual_dependence, method = "spearman",
         exact=FALSE)
print(res)
get_CI_spearman_rank(res, nrow(df_wide))

# all group identity ~ conflict
res <- cor.test(df_wide$group_identity, 
         df_wide$conflict, method = "spearman",
         exact=FALSE)
print(res)
get_CI_spearman_rank(res, nrow(df_wide))


################################################################################
# Hypothesis 4
# Changes in perceptions of local & global interdependence, conflict, and social 
# identity between the start and the end of the game will be associated with
# rates of successful local and global cooperation during the game. 
################################################################################

# Get variables of changes
data$global_change_mutual_dependence = data$global_final_mutual_dependence - data$global_initial_mutual_dependence
data$local_change_mutual_dependence = data$local_final_mutual_dependence - data$local_initial_mutual_dependence
data$global_change_conflict = data$global_final_conflict - data$global_initial_conflict
data$local_change_conflict = data$local_final_conflict - data$local_initial_conflict
data$global_change_group_identity = data$global_final_group_identity - data$global_initial_group_identity
data$local_change_group_identity = data$local_final_group_identity - data$local_initial_group_identity


# Get the IV - number of rounds local/global thresholds were met
local_threshold_colnames <- paste0("game.", 1:20, ".player.local_threshold_met")
data$number_of_rounds_local_threshold_met <- rowSums(data[local_threshold_colnames])

global_threshold_colnames <- paste0("game.", 1:20, ".player.global_threshold_met")
data$number_of_rounds_global_threshold_met <- rowSums(data[global_threshold_colnames])

table(data$subgroup_id)

library(afex)

model <- mixed(local_change_group_identity ~ local_initial_group_identity + number_of_rounds_local_threshold_met +
                 (1|subgroup_id),
               data = data)
summary(model)
confint(model$full_model)


# Global group identity
model <- mixed(global_change_group_identity ~ global_initial_group_identity + number_of_rounds_global_threshold_met +
                 (1|group_id),
               data = data)
summary(model)
confint(model$full_model)

# Global MD
model <- mixed(global_change_mutual_dependence ~ global_initial_mutual_dependence + number_of_rounds_global_threshold_met +
                 (1|group_id),
               data = data)
summary(model)
confint(model$full_model)


model <- mixed(local_change_mutual_dependence ~ local_initial_mutual_dependence + number_of_rounds_local_threshold_met +
                (1|subgroup_id),
            data = data)
summary(model)
confint(model$full_model)


model <- lm(global_change_conflict ~ global_initial_conflict + number_of_rounds_global_threshold_met,
               data = data)
summary(model)
confint(model)


model <- mixed(local_change_conflict ~ local_initial_conflict + number_of_rounds_local_threshold_met +
                 (local_initial_conflict||group_id) +
                 (local_initial_conflict + number_of_rounds_local_threshold_met||subgroup_id),
               data = data)
summary(model)
confint(model$full_model, method="Wald")




################################################################################
# Hypothesis 5
# Perceptions of local and global social identity will decline over the course of the game. 
################################################################################


# Global group identity
# Pivot the data to get the global DIFI values on different rows. 
pivoted_data_global <- data %>% pivot_longer(cols=c('survey.1.player.DIFI_big_group_distance', 
                                                                    'game.5.player.DIFI_big_group_distance', 
                                                                    'game.10.player.DIFI_big_group_distance', 
                                                                    'game.15.player.DIFI_big_group_distance', 
                                                                    'game.20.player.DIFI_big_group_distance'),
                                                             names_to='global_DIFI',
                                                             values_to='DIFI_value')


library(stringr)
pivoted_data_global$round_number <- as.numeric(str_extract(pivoted_data_global$global_DIFI, "[[:digit:]]+"))
pivoted_data_global$round_number[pivoted_data_global$round_number == 1] <- 0
pivoted_data_global$global_group_identity <- convert_DIFI(pivoted_data_global$DIFI_value)


model <- mixed(global_group_identity ~ round_number + 
                 (1|group_id) +
                 (1|participant.code),
               data=pivoted_data_global)
summary(model)
confint(model$full_model)

nrow(data)

# Local group identity
pivoted_data_local <- data %>% pivot_longer(cols=c('survey.1.player.DIFI_my_group_distance', 
                                                                   'game.5.player.DIFI_my_group_distance', 
                                                                   'game.10.player.DIFI_my_group_distance', 
                                                                   'game.15.player.DIFI_my_group_distance', 
                                                                   'game.20.player.DIFI_my_group_distance'),
                                                            names_to='local_DIFI',
                                                            values_to='DIFI_value')

pivoted_data_local$round_number <- as.numeric(str_extract(pivoted_data_local$local_DIFI, "[[:digit:]]+"))
pivoted_data_local$round_number[pivoted_data_local$round_number == 1] <- 0
pivoted_data_local$local_group_identity <- convert_DIFI(pivoted_data_local$DIFI_value)

# Reduced model

model <- mixed(local_group_identity ~ round_number + 
                 (round_number||subgroup_id) + 
                 (round_number||participant.code),
               data=pivoted_data_local)
summary(model)
confint(model$full_model)

################################################################################
# Hypothesis 6
# People who perceive more positive interdependence, and a stronger identity, 
# with the local (compared to the global) group will be initially more likely to
# contribute to the local (than the global) public good (and vice versa).
################################################################################

data_filtered <- data %>%
  filter(game.1.player.contribution %in% c("global", "local"))

data_filtered <- data_filtered %>% 
  mutate(action_1_g = as.numeric(game.1.player.contribution == "global"))

# Model with differences between global/local
data_filtered$initial_difference_global_local_mutual_dependence <- data_filtered$global_initial_mutual_dependence - data_filtered$local_initial_mutual_dependence
data_filtered$initial_difference_global_local_conflict <- data_filtered$global_initial_conflict - data_filtered$local_initial_conflict
data_filtered$initial_difference_global_local_group_identity <- data_filtered$global_initial_group_identity - data_filtered$local_initial_group_identity

model <- glm(action_1_g ~ initial_difference_global_local_mutual_dependence +
               initial_difference_global_local_conflict +
               initial_difference_global_local_group_identity, 
             data = data_filtered, family = binomial)
summary(model)
confint(model)

nrow(data_filtered)

library(car)
# Variance Inflation Function
vif(model)
# In the pilot case no VIF > 5, so we keep the model as it is. 
# See pre-reg for analysis plan if VIF > 5 

# Sense Check - Model with absolute values (not differences)
model <- glm(action_1_g ~ local_initial_mutual_dependence + local_initial_conflict + local_initial_group_identity +
               global_initial_mutual_dependence + global_initial_conflict + global_initial_group_identity, 
             data = data_filtered, family = binomial)
summary(model)


################################################################################
# Hypothesis 6a
# People who perceive less positive interdependence/more conflict/less social 
# identity will be initially more likely to defect than to contribute to the 
# local or global public good. 
################################################################################

data_filtered <- data %>%
  filter(game.1.player.contribution %in% c("global", "local", "self"))

table(data_filtered$game.1.player.contribution)

data_filtered <- data_filtered %>% 
  mutate(action_1_d = as.numeric(game.1.player.contribution == "self"))

# Model with differences between global/local
model <- glm(action_1_d ~ local_initial_mutual_dependence + local_initial_conflict + local_initial_group_identity +
               global_initial_mutual_dependence + global_initial_conflict + global_initial_group_identity, 
             data = data_filtered, family = binomial)
summary(model)

nrow(data_filtered)

vif(model)

confint(model)




################################################################################
# Hypothesis 7
# The outcome of the game in each round will predict participants’ next actions
################################################################################

# Pivot Data to a format with each round for each player as a row 
# We also want to get the next round's action of each player in the previous round's row

# First create new columns of next_round_action for each round number in {1,2,...,19}
for (x in 1:19) {
  existing_col_name_next_action = paste0("game.", x+1, ".player.contribution")
  new_col_name_next_action = paste0("game.", x, ".player.next_round_action")
  data[new_col_name_next_action] = data[existing_col_name_next_action]
}

data <- data %>%
  dplyr::mutate(participant.code = row_number())

# In order to pivot the data into a usable format
# We first need to pivot long to get cols with each "game_variable" and "value"
cols_actions <- sprintf("game.%s.player.contribution", 1:19)
cols_n_global <- sprintf("game.%s.player.global_group_contributions", 1:19)
cols_n_local <- sprintf("game.%s.player.local_group_contributions", 1:19)
cols_n_defect <- sprintf("game.%s.player.kept_group_contributions", 1:19)
cols_next_actions <- sprintf("game.%s.player.next_round_action", 1:19)

cols <- c(cols_actions, cols_n_global, cols_n_local, cols_n_defect, cols_next_actions)

pivoted_data_long <- pivot_longer(data, 
                                  cols,
                                  names_to = c("round", "game_variable"),
                                  names_pattern = "game.(.*).player.(.*)",
                                  values_transform = list(value = as.character)
)

# Remove unneeded columns
cols_of_interest <- c("participant.code",
                      "subgroup_id",
                      "group_id",
                      "round",
                      "game_variable",
                      "value")
pivoted_data_long <- pivoted_data_long[cols_of_interest]

pivoted_data_long <- pivoted_data_long[pivoted_data_long$value!="",]

# Pivot wide to expand the game variables to rows
pivoted_data <- pivot_wider(pivoted_data_long,
                            names_from="game_variable",
                            values_from="value"
)

table(pivoted_data$contribution)
60/(nrow(pivoted_data))

pivoted_data <- pivoted_data[!is.na(pivoted_data$contribution),]
nrow(pivoted_data)

# Convert data rows to correct dtypes
pivoted_data$round <- as.numeric(pivoted_data$round)
pivoted_data$global_group_contributions <- as.numeric(pivoted_data$global_group_contributions)
pivoted_data$local_group_contributions <- as.numeric(pivoted_data$local_group_contributions)
pivoted_data$kept_group_contributions <- as.numeric(pivoted_data$kept_group_contributions)
str(pivoted_data)

# Remove participant's own action from the counts
pivoted_data <- pivoted_data %>%
  mutate(global_group_contributions = ifelse(contribution == 'global', global_group_contributions - 1, global_group_contributions))

pivoted_data <- pivoted_data %>%
  mutate(local_group_contributions = ifelse(contribution == 'local', local_group_contributions - 1, local_group_contributions))

pivoted_data <- pivoted_data %>%
  mutate(kept_group_contributions = ifelse(contribution == 'self', kept_group_contributions - 1, kept_group_contributions))

pivoted_data$n_global = pivoted_data$global_group_contributions 
pivoted_data$n_local = pivoted_data$local_group_contributions
pivoted_data$n_defect = pivoted_data$kept_group_contributions

# Generate squared variables
pivoted_data$n_global_squared = pivoted_data$n_global^2
pivoted_data$n_local_squared = pivoted_data$n_local^2
pivoted_data$n_defect_squared = pivoted_data$n_defect^2

table(pivoted_data$next_round_action)

pivoted_data = na.omit(pivoted_data)

# Nested Dichotomies - First G/not G
pivoted_data <- pivoted_data %>% 
  mutate(next_action_g = if_else(next_round_action == "global", 1, 0))


# Get dummy variable for contrubutions of global/local
pivoted_data <- pivoted_data %>% 
  mutate(action_g = if_else(contribution == "global", 1, 0))

pivoted_data <- pivoted_data %>% 
  mutate(action_l = if_else(contribution == "local", 1, 0))


library(afex)

nrow(data)
nrow(pivoted_data)

model_global <- glm(next_action_g ~ round + action_g  + action_l +
                        n_global + n_local + n_defect +
                        n_global_squared + n_local_squared + n_defect_squared, 
                      family = binomial, data = pivoted_data)

summary(model_global)
confint(model_global)

# # Nested Dichotomies - Remove Global and just look at Local/Defect
pivoted_data_not_G <- pivoted_data %>% 
  filter(!(next_round_action == "global")) %>% 
  mutate(next_action_l = as.numeric(next_round_action == "local"))

nrow(pivoted_data_not_G)

# Reduced model
model_local <- glm(next_action_l ~ round + action_g + action_l + 
                       n_global + n_local + n_defect +
                       n_global_squared + n_local_squared + n_defect_squared, 
                     family = binomial, data = pivoted_data_not_G)
summary(model_local)
confint(model_local)
