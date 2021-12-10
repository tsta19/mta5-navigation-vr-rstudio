# Analysis and Data vis

#------- libraries ------------
library(tidyverse)
library(magrittr)

#------- Load Data ------------
load("logged_data_Clean.rda")
load("Small_data_Clean.rda")

#------- Travel Length --------

dfS %>%
  filter(TrialID == 3) %>%
  ggplot(aes(x = FreqTempo, y = TravelDistance, color = DirectionDistance)) +
  geom_boxplot() +
  theme_classic() +
  ylab("Teavel Distance") +
  xlab("") +
  theme(legend.position="bottom", 
        axis.text.x = element_text(size = 14), 
        axis.text.y = element_text(size = 14), 
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) +
  scale_color_discrete("") +
  scale_shape_discrete("")

#------- Travel Time ~ Maze Compleation Time --------

dfS %>%
  filter(TrialID == 3) %>%
  ggplot(aes(x = FreqTempo, y = MazeTime, color = DirectionDistance)) +
  geom_boxplot() +
  theme_classic() +
  ylab("Compleation time") +
  xlab("") +
  theme(legend.position="bottom", 
        axis.text.x = element_text(size = 14), 
        axis.text.y = element_text(size = 14), 
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) +
  scale_color_discrete("") +
  scale_shape_discrete("")

#------- Travel Time ~ Test Compleation Time --------

dfS %>%
  filter(TrialID == 3) %>%
  ggplot(aes(x = FreqTempo, y = TestTime, color = DirectionDistance)) +
  geom_boxplot() +
  theme_classic() +
  ylab("Compleation time") +
  xlab("") +
  theme(legend.position="bottom", 
        axis.text.x = element_text(size = 14), 
        axis.text.y = element_text(size = 14), 
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) +
  scale_color_discrete("") +
  scale_shape_discrete("")

#------- Travel Time ~ Maze Compareson --------

dfS %>%
  ggplot(aes(x = MazeID, y = MazeTime, color = DirectionDistance)) +
  geom_boxplot() +
  theme_classic() +
  ylab("Compleation time") +
  xlab("") +
  theme(legend.position="bottom", 
        axis.text.x = element_text(size = 14), 
        axis.text.y = element_text(size = 14), 
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) +
  scale_color_discrete("") +
  scale_shape_discrete("")


#------- Travel Time ~ Trial Order --------

dfS %>%
  ggplot(aes(x = TrialID, y = MazeTime, color = DirectionDistance)) +
  geom_boxplot() +
  theme_classic() +
  ylab("Compleation time") +
  xlab("") +
  theme(legend.position="bottom", 
        axis.text.x = element_text(size = 14), 
        axis.text.y = element_text(size = 14), 
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) +
  scale_color_discrete("") +
  scale_shape_discrete("")

#------- Travel Time ~ Trial Order Improvements --------

# Make functions
lower_ci <- function(mean, se, n, conf_level = 0.95){
  lower_ci <- mean - qt(1 - ((1 - conf_level) / 2), n - 1) * se
}
upper_ci <- function(mean, se, n, conf_level = 0.95){
  upper_ci <- mean + qt(1 - ((1 - conf_level) / 2), n - 1) * se
}

# Plot 
dfTest <- dfS %>%
  dplyr::group_by(DirectionDistance, FreqTempo, TrialID) %>%
  dplyr::summarise(meanMazeTime = mean(MazeTime),
                   smean = mean(MazeTime, na.rm = TRUE),
                   ssd = sd(MazeTime, na.rm = TRUE),
                   count = n()) %>%
  dplyr::mutate(
    se = ssd / sqrt(count),
    lowerci = lower_ci(smean, se, count),
    upperci = upper_ci(smean, se, count))

dfTest %>%
  ggplot(aes(x = TrialID,
             y = meanMazeTime,
             group = FreqTempo,
             color = FreqTempo,
             shape = FreqTempo)) +
  facet_grid(cols = vars(DirectionDistance)) +
  geom_point(position = position_dodge(0.1), alpha=1, size = 5) +
  geom_line(position = position_dodge(0.1),
            alpha = 1,
            size = 1) +
  geom_errorbar(aes(ymin = lowerci,
                    ymax = upperci),
                width = 0.2,
                color = "Black",
                position = position_dodge(0.1)) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  scale_color_discrete("") +
  scale_shape_discrete("")



