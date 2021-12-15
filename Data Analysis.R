# Analysis and Data vis

#------- libraries ------------
library(tidyverse)
library(magrittr)
library(dplyr)
library(car)
library(dunn.test)
library(FSA)
#------- Load Data ------------
load("logged_data_Clean.rda")
load("Small_data_Clean.rda")

#------- Travel Length --------

meanLength <- dfS %>%
  group_by(FreqTempo, DirectionDistance) %>%
  summarise(MeanLength = mean(TravelDistance), 
            medianLength = median(TravelDistance))

meanLength


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

#---------- Travel Lenght, but corrected ------------------------
# Kan godt være man skal holde ctrl ned og klikke enter et par gange.

#Only trial 1 
dfTrial1 <- dfS %>% data.frame()
dfTrial1 <- filter(dfTrial1, TrialID == 1) 

#Only trial 2
dfTrial2 <- dfS %>% data.frame()
dfTrial2 <- filter(dfTrial2, TrialID == 2) 
dfTrial2$TravelDistance <- (dfTrial2$TravelDistance - dfTrial1$TravelDistance)

#Only trial 3
dfTrial3 <- dfS %>% data.frame()
dfTrial3 <- filter(dfTrial3, TrialID == 3) 
dfTrial3$TravelDistance <- (dfTrial3$TravelDistance - dfTrial2$TravelDistance - dfTrial1$TravelDistance)



#-------- Grouped summarized datasets.-----------------  

dfTrial1Gp <- dfTrial1 %>% 
  group_by(FreqTempo, TravelDistance, DirectionDistance) %>%
  summarise(MeanLenght = mean(TravelDistance), 
            medianLength = median(TravelDistance))
dfTrial1Gp <- aggregate(. ~ FreqTempo, dfTrial1Gp, sum)  


dfTrial2Gp <- dfTrial2 %>% 
  group_by(FreqTempo, TravelDistance, DirectionDistance) %>% 
  summarise(MeanLenght = mean(TravelDistance), 
            medianLength = median(TravelDistance))
dfTrial2Gp <- aggregate(. ~ FreqTempo, dfTrial2Gp, sum)


dfTrial3Gp <- dfTrial3 %>%
  group_by(FreqTempo, TravelDistance, DirectionDistance) %>%
  summarise(MeanLenght = mean(TravelDistance), 
            medianLength = median(TravelDistance))
dfTrial3Gp <- aggregate(. ~ FreqTempo, dfTrial3Gp, sum)


#Skift ud med en af de andre trials, hvis man ønsker.)

dfTrial3 %>%
  ggplot(aes(x = FreqTempo, y = TravelDistance, color = DirectionDistance)) +
  geom_boxplot() +
  theme_classic() +
  ggtitle("Maze 3") +
  ylab("Teavel Distance") +
  xlab("Independent Variable") +
  theme(legend.position="bottom", 
        axis.text.x = element_text(size = 14), 
        axis.text.y = element_text(size = 14), 
        axis.title = element_text(size = 16),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) +
  scale_color_discrete("") +
  scale_shape_discrete("")


#----------------- PLOT POINT VISES I PLOTS -------------

newPlotdata <- cbind(mazePathEffeciencyMAZE3,mazeTimeEffeciencyMAZE3)

ggplot() + 
  geom_point(data = newPlotdata, aes(x=mazePathEffeciency, y=mazeTimeEffeciency, color=DirectionDistance...2, shape= FreqTempo...1, size = 3)) +  
  geom_smooth(method=lm) + 
  theme_classic() +
  ggtitle("") +
  ylab("Time Effeciency") +
  xlab("Path Effeciency") +
  guides(color = guide_legend(order = 1), shape = guide_legend(order = 2),
        size = guide_legend(order = 3)) +
  theme(axis.text.x = element_text(size = 14), 
        axis.text.y = element_text(size = 14), 
        axis.title = element_text(size = 16),
        aspect.ratio = 1,
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 15),
        #legend.guide = element_rect(size = 20),
        panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) +
  scale_color_discrete("") +
  scale_shape_discrete("")

# Remove confidence intervals
# Extend the regression lines


#------- Travel Time ~ Maze Compleation Time --------

meanMazeTime <- dfS %>%
  group_by(FreqTempo, DirectionDistance) %>%
  summarise(meanMazeTime = mean(MazeTime), 
            medianMazeTime = median(MazeTime),
            sdMazeTime = sd(MazeTime))

meanMazeTime

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




#------- normality test
STD <- shapiro.test(dfS$TravelDistance)
qqPlot(dfS$TravelDistance)
qqPlot(dfS$TestTime)


#------- Homogeneity of variance
model2 <- lm(log(TravelDistance) ~ DirectionDistance, data = dfS)
plot(model2, 3)


?bptest
BPTD <- lmtest::bptest(model2)
summary(BPTD)

model3 <- lm(TestTime ~ FreqTempo, data = dfS)
plot(model2, 3)

BPTT <- lmtest::bptest(model2)
summary(BPTT)


#------- Two Way Anova

#two way anova
dfS %>%
  filter(TrialID == 3)
  anova2TD <- aov(TravelDistance ~ FreqTempo + DirectionDistance, data = dfS)
summary(anova2TD)

dfS %>%
  filter(TrialID == 3)
  anova2TT <- aov(TestTime ~ FreqTempo + DirectionDistance, data = dfS)
summary(anova2TT)

#------- One Way Anova
#one way anova
FreqTempoAnovaTD <- aov(TravelDistance ~ FreqTempo, data = dfS)
summary(FreqTempoAnovaTD)

DirDisAnovaTD <- aov(TravelDistance ~ DirectionDistance, data = dfS)
summary(DirDisAnovaTD)


FreqTempoAnovaTT <- aov(TestTime ~ FreqTempo, data = dfS)
summary(FreqTempoAnovaTT)

DirDisAnovaTT <- aov(TestTime ~ DirectionDistance, data = dfS)
summary(DirDisAnovaTT)

#------- Two Way Manova
manRes <- manova(cbind(dfS$DirectionDistance, dfS$FreqTempo) ~ dfS$TravelDistance, data = dfS)
summary(manRes)
summary.aov(manRes)

#------- Kruskal-Wallis test
KWTD <- kruskal.test(TravelDistance ~ FreqTempo, data = dfS)
summary(KWTD)

DTTD <- dunnTest(dfS$TravelDistance, dfS$DirectionDistance, method = "bonferroni")
DTTD

KWTT <- kruskal.test(TestTime ~ FreqTempo, data = dfS)
summary(KWTT)

DTTT <- dunnTest(dfS$TestTime, dfS$FreqTempo, method = "bonferroni")
DTTT

#------- Time effeciency ------

mazeTimeEffeciencyMAZE1 <- dfS %>%
  group_by(FreqTempo, DirectionDistance, MazeID==0) %>%
  summarise(mazeTimeEffeciency = median((24.20/MazeTime)*100))


mazeTimeEffeciencyMAZE2 <- dfS %>%
  group_by(FreqTempo, DirectionDistance, MazeID==1) %>%
  summarise(mazeTimeEffeciency = median((72.65/MazeTime)*100))

mazeTimeEffeciencyMAZE3 <- dfS %>%
  group_by(FreqTempo, DirectionDistance, MazeID==2) %>%
  summarise(mazeTimeEffeciency = median((51.14/MazeTime)*100))

mazeTimeEffeciencyMAZE1
mazeTimeEffeciencyMAZE2
mazeTimeEffeciencyMAZE3


#------- Path effeciency ------
mazePathEffeciencyMAZE1 <- dfTrial1 %>%
  group_by(FreqTempo, DirectionDistance, MazeID==0) %>%
  summarise(mazePathEffeciency = median((75.10/TravelDistance)*100))

mazePathEffeciencyMAZE2 <- dfTrial2 %>%
  group_by(FreqTempo, DirectionDistance, MazeID==1) %>%
  summarise(mazePathEffeciency = median((213.38/TravelDistance)*100))

mazePathEffeciencyMAZE3 <- dfTrial3 %>%
  group_by(FreqTempo, DirectionDistance, MazeID==2) %>%
  summarise(mazePathEffeciency = median((149.48/TravelDistance)*100))

mazePathEffeciencyMAZE1
mazePathEffeciencyMAZE2
mazePathEffeciencyMAZE3


dfS %>%
  group_by(FreqTempo, DirectionDistance) %>%
  ggplot(aes(x = mazePathEffeciencyMAZE1, y = 0:100, color = DirectionDistance)) +
  geom_boxplot() +
  theme_classic() +
  ylab("Effeciency %") +
  xlab("Participants") +
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


