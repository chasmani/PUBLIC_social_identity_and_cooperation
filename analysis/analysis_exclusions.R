library("tidyverse")

balanced_data_source <- "data/data_balanced.csv"
balanced_data <- read.table(balanced_data_source, header=TRUE, sep=",", quote='"')

global_boost_data_source <- "data/data_global_boost.csv"
global_boost_data <- read.table(global_boost_data_source, header=TRUE, sep=",", quote='"')

global_only_data_source <- "data/data_global_only.csv"
global_only_data <- read.table(global_only_data_source, header=TRUE, sep=",", quote='"')

balanced_data$condition <- "Balanced"
global_boost_data$condition <- "GlobalBoost"
global_only_data$condition <- "GlobalOnly"

library("plyr")
data <- rbind.fill(global_boost_data, balanced_data)
data <- rbind.fill(data, global_only_data)

# Total data entries
print("Total data rows:")
nrow(data)

# Total participants recruited
print("Total participants recruited:")
data <- data %>% 
  filter(participant._current_app_name != "") %>%
  filter(participant._current_app_name != "consent")
nrow(data)

57*3

# Total players finished
data <- data %>% 
  filter(participant._current_app_name == "payment")

print("Total players finished study:")
nrow(data)

data$group_id <- paste(data$session.code, data$game.1.group.id_in_subsession, sep="-")

# Unique subgroup id
data <- data %>% 
  mutate(subgroup = if_else(participant.custom_id_in_group <= 3, 1, 2))
data$subgroup_id <- paste(data$group_id, data$subgroup, sep="-")

# Unique participant id
library(dplyr)
data <- data %>%
  dplyr::mutate(participant.code = row_number())


###################
# Group exclusions
# Unique group id

full_group_data <- data  %>% 
  filter(!(game.20.player.contribution == "")) %>%
  filter(participant.is_dropout == 0)

full_group_data <- add_count(full_group_data, group_id, name="players_in_group")

full_group_data <- full_group_data %>% 
  filter(players_in_group == 6)
nrow(full_group_data)/6/3
# Export to data
write.csv(full_group_data, file = "data/data_full_groups.csv", row.names = FALSE)


####################
# Individual Exclusions

data <- rbind.fill(global_boost_data, balanced_data)
data <- rbind.fill(data, global_only_data)

# Total participants started game
data <- data %>% 
  filter(participant._current_app_name != "consent") %>%
  filter(participant._current_app_name != "")
print("Total players began study:")
nrow(data)

# Total participants to get stuck on questions
count_comprehension_stuck <- sum(data$participant._current_app_name == "instructions")
print("Total players stuck on comprehensions:")
print(count_comprehension_stuck)


data <- data %>%
  filter(participant._current_app_name == "payment")


print(nrow(data))


data$group_id <- paste(data$session.code, data$game.1.group.id_in_subsession, sep="-")

# Unique subgroup id
data <- data %>% 
  mutate(subgroup = if_else(participant.custom_id_in_group <= 3, 1, 2))
data$subgroup_id <- paste(data$group_id, data$subgroup, sep="-")


data <- data %>%
  dplyr::mutate(participant.code = row_number())

table(data$quiz_exclusion)

print("Total players exlcuded on comprehension:")
print(sum(data$quiz_exclusion == 1))

data_participants <- data %>%
  filter(!(game.20.player.contribution == "")) %>%
  filter(participant.is_dropout == 0)

print("Total players failing pre-game attention")
print(sum(data_participants$survey.1.player.attention_check_1 != 2))
print("Total players failing post-game attention")
print(sum(data_participants$survey_post.1.player.attention_check_1 != 4))


# Exclusions
data_participants <- 
  data_participants %>%
  filter(quiz_exclusion == 0)

# Attention checks
data_participants <- 
  data_participants %>%
  filter(survey.1.player.attention_check_1 == 2) %>%
  filter(survey_post.1.player.attention_check_1 == 4)

print("Number of participants left after attention checks")
print(nrow(data_participants))


# Export to data
write.csv(data_participants, file = "data/data_participants.csv", row.names = FALSE)




