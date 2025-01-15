library("tidyverse")
library("dplyr")

data <- read.table("data/data_participants.csv", header=TRUE, sep=",", quote='"')

##########################
# Colors

allshire_color <- "#8e44ad";
westville_color <- "#e67e22";
eastburgh_color <- "#01c8ee";
kept_color <- "#404040";

######################
# Model


convert_DIFI <- function(DIFI_value) {
  # Convert the DIFI range to an integer from 0-100.
  # DIFI Range is [-175.69, 125]
  return ((DIFI_value + 175.7)*100/(175.7+125))
}

data$initial_group_identity_global <- convert_DIFI(data$survey.1.player.DIFI_big_group_distance)
data$initial_group_identity_local <- convert_DIFI(data$survey.1.player.DIFI_my_group_distance)
data$initial_group_identity_other <- convert_DIFI(data$survey.1.player.DIFI_other_group_distance)

data$final_group_identity_global <- convert_DIFI(data$game.20.player.DIFI_big_group_distance)
data$final_group_identity_local <- convert_DIFI(data$game.20.player.DIFI_my_group_distance)
data$final_group_identity_other <- convert_DIFI(data$game.20.player.DIFI_other_group_distance)

data$change_group_identity_global = data$final_group_identity_global - data$initial_group_identity_global
data$change_group_identity_local = data$final_group_identity_local - data$initial_group_identity_local
data$change_group_identity_other = data$final_group_identity_other - data$initial_group_identity_other

local_threshold_colnames <- paste0("game.", 1:20, ".player.local_threshold_met")
data$number_of_thresholds_met_local <- rowSums(data[local_threshold_colnames])

global_threshold_colnames <- paste0("game.", 1:20, ".player.global_threshold_met")
data$number_of_thresholds_met_global <- rowSums(data[global_threshold_colnames])

data <- data %>%
  mutate(GlobalOnly = ifelse(condition=="GlobalOnly", 1, 0)) %>%
  mutate(GlobalBoost = ifelse(condition=="GlobalBoost", 1, 0))



#################
# Models

library(afex)
model_global_global <- mixed(change_group_identity_global ~ initial_group_identity_global +
                               number_of_thresholds_met_global +
                               GlobalBoost +
                               GlobalOnly + 
                               (1|group_id),
                             data = data)

summary(model_global_global)
confint(model_global_global$full_model)

data_local <- data %>%
  filter(condition != "GlobalOnly")

nrow(data_local)


model_local_local <- mixed(change_group_identity_local ~ initial_group_identity_local +
                             number_of_thresholds_met_local +
                             GlobalBoost +
                             (1|subgroup_id) +
                             (1|group_id),
                           data = data_local)
summary(model_local_local)

confint(model_local_local$full_model)


data_long <- data %>%
  # Select necessary columns and the condition column
  dplyr::select(condition, change_group_identity_global, change_group_identity_local,
                number_of_thresholds_met_global, number_of_thresholds_met_local) %>%
  # Reshape change in group identity to long format
  pivot_longer(cols = c(change_group_identity_global, change_group_identity_local),
               names_to = "identity_type",
               values_to = "change_group_identity",
               names_prefix = "change_group_identity_") %>%
  # Reshape number of thresholds met to long format
  pivot_longer(cols = c(number_of_thresholds_met_global,number_of_thresholds_met_local),
               names_to = "thresholds_met_type",
               values_to = "number_of_thresholds_met",
               names_prefix = "number_of_thresholds_met_")

data_long <- data_long %>% 
  filter(identity_type == thresholds_met_type)

data_long <- data_long %>% 
  filter(!(condition == "GlobalOnly" & thresholds_met_type == "local"))

data_long$condition <- factor(data_long$condition, levels = c("GlobalOnly", "Balanced", "GlobalBoost"))

data_long$identity_type <- tools::toTitleCase(data_long$identity_type)
data_long$thresholds_met_type <- tools::toTitleCase(data_long$thresholds_met_type)



annotation_df <- data.frame(
  thresholds_met_type = c("Global", "Local"),
  identity_type = c("Global", "Local"),
  # Match these to your actual group names
  x = 1, # Position on the x-axis; Inf means far right
  y = 1, # Position on the y-axis; adjust this based on your y-axis scale
  condition="Balanced"
)

annotation_df$thresholds_met_type <- factor(annotation_df$thresholds_met_type, levels=c("Global", "Local"))
annotation_df$identity_type <- factor(annotation_df$identity_type, levels=c("Global", "Local", "Other"))



data_long <- data_long %>%
  filter(!((condition == "GlobalOnly") & (thresholds_met_type == "Local")))


ggplot(data_long, aes(x=number_of_thresholds_met, y=change_group_identity, color=condition, linetype=condition)) +
  geom_point(alpha=0.1, size=0.5)  +
  geom_smooth(method="lm", alpha=0.2) +
  facet_grid(~identity_type) +
  theme_minimal() +
  ylim(-100,110) +
  labs(x = "Number of Group Thresholds Met", y = "Change in Group Identity") +
  theme(legend.position="bottom",
        legend.title=element_blank()) +
  geom_text(data = annotation_df, aes(x = x, y = y, label = c("a", "b")),
            color="#404040", fontface="bold", size=5, vjust=-14, hjust=0.5)

ggsave("images/performance_cohesion_simple.png", dpi=600, bg="#ffffff")

###################
# First round actions

data$global_initial_group_identity <- convert_DIFI(data$survey.1.player.DIFI_big_group_distance)
data$local_initial_group_identity <- convert_DIFI(data$survey.1.player.DIFI_my_group_distance)


data_filtered <- data %>%
  filter(game.1.player.contribution %in% c("global", "local"))

data_filtered <- data_filtered %>% 
  mutate(action_1_g = as.numeric(game.1.player.contribution == "global"))

# Model with differences between global/local
data_filtered$initial_difference_global_local_group_identity <- data_filtered$global_initial_group_identity - data_filtered$local_initial_group_identity

data_filtered <- data_filtered %>%
  filter(condition != "GlobalOnly")

?glm

nrow(data_filtered)

model <- glm(action_1_g ~ initial_difference_global_local_group_identity +
               GlobalBoost +
               GlobalBoost*initial_difference_global_local_group_identity, 
             data = data_filtered, family = binomial)
summary(model)
confint(model)

library(car)
# Variance Inflation Function
vif(model)


