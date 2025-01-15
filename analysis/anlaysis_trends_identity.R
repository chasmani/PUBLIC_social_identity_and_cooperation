library("tidyverse")
library("dplyr")

data <- read.table("data/data_participants.csv", header=TRUE, sep=",", quote='"')




##########################
# Colors

allshire_color <- "#8e44ad"
westville_color <- "#e67e22"
eastburgh_color <- "#01c8ee"
kept_color <- "#404040"


nrow(data)

convert_DIFI <- function(DIFI_value) {
  # Convert the DIFI range to an integer from 0-100.
  # DIFI Range is [-175.69, 125]
  return ((DIFI_value + 175.7)*100/(175.7+125))
}

data$measure_global_0_group_identity <- convert_DIFI(data$survey.1.player.DIFI_big_group_distance)
data$measure_local_0_group_identity <- convert_DIFI(data$survey.1.player.DIFI_my_group_distance)
data$measure_other_0_group_identity <- convert_DIFI(data$survey.1.player.DIFI_other_group_distance)

data$measure_global_5_group_identity <- convert_DIFI(data$game.5.player.DIFI_big_group_distance)
data$measure_local_5_group_identity <- convert_DIFI(data$game.5.player.DIFI_my_group_distance)
data$measure_other_5_group_identity <- convert_DIFI(data$game.5.player.DIFI_other_group_distance)

data$measure_global_10_group_identity <- convert_DIFI(data$game.10.player.DIFI_big_group_distance)
data$measure_local_10_group_identity <- convert_DIFI(data$game.10.player.DIFI_my_group_distance)
data$measure_other_10_group_identity <- convert_DIFI(data$game.10.player.DIFI_other_group_distance)

data$measure_global_15_group_identity <- convert_DIFI(data$game.15.player.DIFI_big_group_distance)
data$measure_local_15_group_identity <- convert_DIFI(data$game.15.player.DIFI_my_group_distance)
data$measure_other_15_group_identity <- convert_DIFI(data$game.15.player.DIFI_other_group_distance)

data$measure_global_20_group_identity <- convert_DIFI(data$game.20.player.DIFI_big_group_distance)
data$measure_local_20_group_identity <- convert_DIFI(data$game.20.player.DIFI_my_group_distance)
data$measure_other_20_group_identity <- convert_DIFI(data$game.20.player.DIFI_other_group_distance)

cols_id <- c("measure_global_0_group_identity",
             "measure_local_0_group_identity",
             "measure_other_0_group_identity",
             "measure_global_5_group_identity",
             "measure_local_5_group_identity",
             "measure_other_5_group_identity",
             "measure_global_10_group_identity",
             "measure_local_10_group_identity",
             "measure_other_10_group_identity",
             "measure_global_15_group_identity",
             "measure_local_15_group_identity",
             "measure_other_15_group_identity",
             "measure_global_20_group_identity",
             "measure_local_20_group_identity",
             "measure_other_20_group_identity")

data_long <- pivot_longer(data, 
                          cols_id,
                          names_to = c("group_type", "round", "measure"),
                          names_pattern = "measure_([^_]*)_([^_]*)_(.*)"
)


#############################
# Fit model

data_model <- data_long[c("round", "participant.code", "subgroup_id", "group_id", "condition", "group_type", "value")]

data_model <- data_model %>%
  mutate(GlobalOnly = ifelse(condition=="GlobalOnly", 1, 0)) %>%
  mutate(GlobalBoost = ifelse(condition=="GlobalBoost", 1, 0))

data_model$round <- as.integer(data_model$round)

data_global <- data_model %>%
  filter(group_type == "global")

nrow(data_global)


#install.packages("afex")
library(afex)
model_global <- mixed(value ~ round + GlobalOnly + GlobalBoost + round*GlobalOnly + round*GlobalBoost +
                        (1|participant.code) +
                        (1|group_id),
                      data=data_global, method="S")
summary(model_global)

confint(model_global$full_model, method="Wald")

propagate_additive_coefficients(model_global, "round", "round:GlobalBoost")


data_local <- data_model %>%
  filter(group_type == "local")

model_local <- mixed(value ~ round + GlobalOnly + GlobalBoost + round*GlobalOnly + round*GlobalBoost +
                       (1|participant.code) +
                       (1|subgroup_id) +
                       (1|group_id),
                     data=data_local, method="S")
summary(model_local)

confint(model_local$full_model, method="Wald")


data_other <- data_model %>%
  filter(group_type == "other")

model_other <- mixed(value ~ round + GlobalOnly + GlobalBoost + round*GlobalOnly + round*GlobalBoost +
                       (round||participant.code) +
                       (1|subgroup_id),
                     data=data_other, method="S")
summary(model_other)

confint(model_other$full_model, method="Wald")


propagate_additive_coefficients <- function(afex_model, var1, var2) {
  
  coefficients <- summary(afex_model)$coefficients
  print(coefficients["round", "Estimate"])
  print(coefficients)
  propagated_coef <- coefficients[var1, "Estimate"] +  coefficients[var2, "Estimate"]
  
  vcov_matrix <- vcov(afex_model$full_model)
  var_combined_effect <- vcov_matrix[var1, var1] +
    vcov_matrix[var2, var2] +
    2 * vcov_matrix[var1, var2]
  
  se_combined_effect <- sqrt(var_combined_effect)
  print("Propagated coefficient: ")
  print(propagated_coef)
  
  z_score <-propagated_coef / se_combined_effect
  
  # Calculate the two-tailed p-value
  p_value <- 2 * (1 - pnorm(abs(z_score)))
  
  print("Propagated p-value: ")  
  print(p_value)
}

propagate_additive_coefficients(model_other, "round", "round:GlobalOnly")



##############
# PLOTS

data_plot <- data_model %>%
  group_by(round, group_type, condition) %>%
  dplyr::summarise(mean = mean(value), sd = sd(value), n=n())

data_plot$mean_se = data_plot$sd/sqrt(data_plot$n)

data_plot <- data_plot[order(data_plot$group_type, decreasing=TRUE),]  # Reorder data based on some variable

data_plot$condition <- factor(data_plot$condition, levels=c("GlobalOnly", "Balanced", "GlobalBoost"))


annotation_df <- data.frame(
  condition = c("GlobalOnly", "Balanced", "GlobalBoost"), # Match these to your actual group names
  x = 1, # Position on the x-axis; Inf means far right
  y = 1, # Position on the y-axis; adjust this based on your y-axis scale
  group_type="global"
)
annotation_df$condition <- factor(annotation_df$condition, levels=c("GlobalOnly", "Balanced", "GlobalBoost"))


ggplot(data_plot, aes(x=round, y=mean, color=group_type, fill=group_type, linestyle=group_type, alpha=0.5)) +
  geom_smooth(method="lm", alpha=0.3) +
  geom_point(size=2, alpha=1, shape=3) + 
  geom_errorbar(aes(ymin=mean - mean_se, ymax=mean + mean_se), width=1, alpha =1) +
  ylim(0,105) +
  scale_color_manual(values=c(allshire_color, westville_color, eastburgh_color)) +
  scale_fill_manual(values=c(allshire_color, westville_color, eastburgh_color)) +
  facet_wrap(~condition) + 
  theme_minimal() +
  theme(legend.position="bottom") +
  ylab("Group Identity") +
  xlab("Round") + 
  theme(legend.title = element_blank()) + 
  theme(panel.grid.minor = element_blank()) +
  geom_text(data = annotation_df, aes(x = x, y = y, label = c("a", "b", "c")),
            color="#404040", alpha=1, fontface="bold", size=5, vjust=-22.8, hjust=0.4)

ggsave("images/trends_in_identity.png", width = 7, height=5, dpi=600,  bg='#ffffff')



