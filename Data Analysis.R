# Analysis and Data vis

#------- libraries ------------
library(tidyverse)
library(magrittr)
library(dplyr)
library(car)
library(dunn.test)
library(FSA)
#library(lsmeans)
#library(multcompView)
library(MASS)
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
  xlab("Condition groups") +
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
#<<<<<<< Updated upstream
dfTrial3 <- filter(dfTrial3, TrialID == 3) 
dfTrial3$TravelDistance <- (dfTrial3$TravelDistance - dfTrial2$TravelDistance - dfTrial1$TravelDistance)


#Kruskal-wallis test for maze 0
KWTDT1 <- kruskal.test(MazeTime ~ FreqTempo, data = dfTrial1)
KWTDT1

#Kruskal-wallis test for maze 1
KWTDT2 <- kruskal.test(MazeTime ~ DirectionDistance, data = dfTrial2)
KWTDT2

#Kruskal-wallis test for maze 2
KWTDT3 <- kruskal.test(MazeTime ~ FreqTempo, data = dfTrial3)
KWTDT3

DTTDT3 <- dunnTest(dfTrial3$MazeTime, dfTrial3$DirectionDistance, method = "bonferroni")
DTTDT3

#ANOVA test for maze 0
FreqTempoAnovaTDT1 <- aov(TravelDistance ~ DirectionDistance * FreqTempo, data = dfTrial1)
summary(FreqTempoAnovaTDT1)

FreqTempoAnovaTTT1 <- aov(MazeTime ~ DirectionDistance * FreqTempo, data = dfTrial1)
summary(FreqTempoAnovaTTT1)

#ANOVA test for maze 1
FreqTempoAnovaTDT2 <- aov(TravelDistance ~ DirectionDistance * FreqTempo, data = dfTrial2)
summary(FreqTempoAnovaTDT2)
#>>>>>>> Stashed changes

FreqTempoAnovaTTT2 <- aov(MazeTime ~ DirectionDistance * FreqTempo, data = dfTrial2)
summary(FreqTempoAnovaTTT2)

#ANOVA test for maze 2
FreqTempoAnovaTDT3 <- aov(TravelDistance ~ DirectionDistance * FreqTempo, data = dfTrial3)
summary(FreqTempoAnovaTDT3)

FreqTempoAnovaTTT3 <- aov(MazeTime ~ DirectionDistance * FreqTempo, data = dfTrial3)
summary(FreqTempoAnovaTTT3)

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

#---------------------------------------------------------
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

#newPlotdataMaze1 <- cbind(mazePathEffeciencyMAZE1,mazeTimeEffeciencyMAZE1)
#newPlotdataMaze2 <- cbind(mazePathEffeciencyMAZE2,mazeTimeEffeciencyMAZE2)
#newPlotdataMaze3 <- cbind(mazePathEffeciencyMAZE3,mazeTimeEffeciencyMAZE3)

mergedStuffMaze1 <- merge(mazePathEffeciencyMAZE1, mazeTimeEffeciencyMAZE1)
mergedStuffMaze2 <- merge(mazePathEffeciencyMAZE2, mazeTimeEffeciencyMAZE2)
mergedStuffMaze3 <- merge(mazePathEffeciencyMAZE3, mazeTimeEffeciencyMAZE3)

MergedDoubleBatch <- rbind(mergedStuffMaze1,mergedStuffMaze2,mergedStuffMaze3)

trial123 <- rbind(dfTrial1,dfTrial2,dfTrial3)

GROUPINGgroups <- MergedDoubleBatch %>%
  unite("Gp", FreqTempo:DirectionDistance, remove=FALSE)

ScatPlotMazes <- ggplot(data = GROUPINGgroups, aes(x=MTrialID, y=mazeTimeEffeciency, colour = FreqTempo, shape = DirectionDistance, size = 2, group = Gp))+
  geom_line(size = 1) +
  geom_point() 
#position = position_dodge(0.1), alpha=1) +
  
#geom_line(aes(group = position = position_dodge(0.1),
            #alpha = 1,
            #size = 1)

ScatPlotMazes + guides(
      colour = guide_legend(order = 1, "Audio", override.aes = list(size=6)),
      shape = guide_legend(order = 2, "Device", override.aes = list(size=5)),
      size = guide_none(size)
      ) +  
  ylab("Time Effeciency") + 
  xlab("Trial ID") + 
  ggtitle("Trial Time Effeciency") +
  #geom_line(aes(linetype=1))+
  theme_classic() + 
  theme(axis.text = element_text(size = 14), axis.text.x = element_text(size=13))


#------------------------------- PLOT POINT VISES I PLOTS ---------------------

  

ggplot() + 
  geom_point(data = MergedDoubleBatch, aes(x=MazeID, y=mazePathEffeciency, color=DirectionDistance, shape= FreqTempo)) +  
  geom_smooth(method=lm) + 
  theme_classic() +
  ggtitle("") +
  ylab("Path Effeciency") +
  xlab("Maze ID") +
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
  xlab("Condition groups") +
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
  xlab("MazeID") +
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
  xlab("TrialID") +
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
             group = DirectionDistance,
             color = DirectionDistance,
             shape = DirectionDistance)) +
  facet_grid(cols = vars(FreqTempo)) +
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


#------- Two Way Anova

#two way anova
modelTD <- lm(TravelDistance ~ FreqTempo * DirectionDistance, data = dfS)

dfS %>%
  filter(TrialID == 3)
  anova2TD <- aov(TravelDistance ~ FreqTempo * DirectionDistance, data = dfS)
summary(anova2TD)

dfS %>%
  filter(TrialID == 3)
  anova2TT <- aov(TestTime ~ FreqTempo * DirectionDistance, data = dfS)
summary(anova2TT)

shapiro.test(residuals(object = modelTD))
leveneTest(TravelDistance ~ FreqTempo * DirectionDistance, data = dfS)

TUKEY <- TukeyHSD(x=anova2TD, dfS$DirectionDistance, conf.level=0.95)

plot(TUKEY, las=1, col="brown")


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
dep_vars <- cbind(dfS$TravelDistance, dfS$TestTime)
manRes <- manova(dep_vars ~ DirectionDistance * FreqTempo, data = dfS)
summary(manRes)

posthoc2WM <- lda(dfS$DirectionDistance ~ dep_vars, CV=F)


#------- Kruskal-Wallis test
dfS %>%
  filter(TrialID == 3)
  KWTD <- kruskal.test(TravelDistance ~ FreqTempo, data = dfS)
  KWTD

DTTD <- dunnTest(dfS$TravelDistance, dfS$DirectionDistance, method = "bonferroni")
DTTD

KWTT <- kruskal.test(TestTime ~ FreqTempo, data = dfS)
summary(KWTT)

DTTT <- dunnTest(dfS$TestTime, dfS$FreqTempo, method = "bonferroni")
DTTT

#------- Time effeciency ------

mazeTimeEffeciencyMAZE1 <- trial123 %>%
  filter(TrialID==1) %>%
  group_by(FreqTempo, DirectionDistance, TrialID) %>%
  summarise(mazeTimeEffeciency = mean((24.20/MazeTime)*100, na.rm = TRUE),
            mazeTimeESTD = sd((24.20/MazeTime)*100, na.rm = TRUE))

mazeTimeSet1 <- trial123 %>%
  filter(TrialID==1) %>%
  summarise(mazeTimeEfficiency = (24.20/MazeTime)*100)



mazeTimeEffeciencyMAZE2 <- trial123 %>%
  filter(TrialID==2) %>%
  group_by(FreqTempo, DirectionDistance, TrialID) %>%
  summarise(mazeTimeEffeciency = mean((72.65/MazeTime)*100, na.rm = TRUE),
            mazeTimeESTD = sd((72.65/MazeTime)*100, na.rm = TRUE))

mazeTimeSet2 <- trial123 %>%
  filter(TrialID==2) %>%
  summarise(mazeTimeEfficiency = (72.65/MazeTime)*100)



mazeTimeEffeciencyMAZE3 <- trial123 %>%
  filter(TrialID==3) %>%
  group_by(FreqTempo, DirectionDistance, TrialID) %>%
  summarise(mazeTimeEffeciency = mean((51.14/MazeTime)*100, na.rm = TRUE),
            mazeTimeESTD = sd((51.14/MazeTime)*100, na.rm = TRUE))

mazeTimeSet3 <- trial123 %>%
  filter(TrialID==3) %>%
  summarise(mazeTimeEfficiency = (51.14/MazeTime)*100)


mazeTimeEffeciencyMAZE1
mazeTimeEffeciencyMAZE2
mazeTimeEffeciencyMAZE3


#------- Path effeciency ------
mazePathEffeciencyMAZE1 <- trial123 %>%
  filter(TrialID==1) %>%
  group_by(FreqTempo, DirectionDistance, TrialID) %>%
  summarise(mazePathEffeciency = mean((75.10/TravelDistance)*100, na.rm = TRUE),
            mazePathESTD = sd((75.10/TravelDistance)*100, na.rm = TRUE))

mazePathSet1 <- trial123 %>%
  filter(TrialID==1) %>%
  summarise(mazePathEfficiency = (75.10/TravelDistance)*100)


mazePathEffeciencyMAZE2 <- trial123 %>%
  filter(TrialID==2) %>%
  group_by(FreqTempo, DirectionDistance, TrialID) %>%
  summarise(mazePathEffeciency = mean((213.38/TravelDistance)*100, na.rm = TRUE),
            mazePathESTD = sd((213.38/TravelDistance)*100, na.rm = TRUE))

mazePathSet2 <- trial123 %>%
  filter(TrialID==2) %>%
  summarise(mazePathEfficiency = (213.38/TravelDistance)*100)



mazePathEffeciencyMAZE3 <- trial123 %>%
  filter(TrialID==3) %>%
  group_by(FreqTempo, DirectionDistance, TrialID) %>%
  summarise(mazePathEffeciency = mean((149.48/TravelDistance)*100, na.rm = TRUE),
            mazePathESTD = sd((149.48/TravelDistance)*100, na.rm = TRUE))

mazePathSet3 <- trial123 %>%
  filter(TrialID==3) %>%
  summarise(mazePathEfficiency = (149.48/TravelDistance)*100)



mazePathEffeciencyMAZE1
mazePathEffeciencyMAZE2
mazePathEffeciencyMAZE3

timeEfficiencySet <- bind_rows(mazeTimeSet1, mazeTimeSet2, mazeTimeSet3)
pathEfficiencySet <- bind_rows(mazePathSet1, mazePathSet2, mazePathSet3)
trial123 %<>%
  mutate(pathEfficiencySet) %>%
  mutate(timeEfficiencySet)


#------- Shapiro + Kruskal-Wallis test for Efficiencies

SPE <- shapiro.test(trial123$mazePathEfficiency)
STE <- shapiro.test(trial123$mazeTimeEfficiency)
SPE
STE

KWE <- kruskal.test(trial123$mazePathEfficiency ~ trial123$mazeTimeEfficiency)
KWE

DTTD <- dunnTest(trial123$mazePathEfficiency, trial123$mazeTimeEfficiency, method = "bonferroni")
DTTD

KWTT <- kruskal.test(TestTime ~ FreqTempo, data = dfS)
summary(KWTT)

DTTT <- dunnTest(dfS$TestTime, dfS$FreqTempo, method = "bonferroni")
DTTT


#------- Time effeciency MAZEID------

mazeTimeEffeciencyMAZE11 <- trial123 %>%
  filter(MazeID==0) %>%
  group_by(FreqTempo, DirectionDistance, MazeID) %>%
  summarise(mazeTimeEffeciency = mean((24.20/MazeTime)*100, na.rm = TRUE),
            mazeTimeESTD = sd((24.20/MazeTime)*100, na.rm = TRUE))

mazeTimeSet11 <- trial123 %>%
  filter(MazeID==0) %>%
  summarise(mazeTimeEfficiency = (24.20/MazeTime)*100)



mazeTimeEffeciencyMAZE22 <- trial123 %>%
  filter(MazeID==1) %>%
  group_by(FreqTempo, DirectionDistance, MazeID) %>%
  summarise(mazeTimeEffeciency = mean((72.65/MazeTime)*100, na.rm = TRUE),
            mazeTimeESTD = sd((72.65/MazeTime)*100, na.rm = TRUE))

mazeTimeSet22 <- trial123 %>%
  filter(MazeID==1) %>%
  summarise(mazeTimeEfficiency = (72.65/MazeTime)*100)



mazeTimeEffeciencyMAZE33 <- trial123 %>%
  filter(MazeID==2) %>%
  group_by(FreqTempo, DirectionDistance, MazeID) %>%
  summarise(mazeTimeEffeciency = mean((51.14/MazeTime)*100, na.rm = TRUE),
            mazeTimeESTD = sd((51.14/MazeTime)*100, na.rm = TRUE))

mazeTimeSet33 <- trial123 %>%
  filter(MazeID==2) %>%
  summarise(mazeTimeEfficiency = (51.14/MazeTime)*100)


mazeTimeEffeciencyMAZE11
mazeTimeEffeciencyMAZE22
mazeTimeEffeciencyMAZE33


#------- Path effeciency MAZEID ------
mazePathEffeciencyMAZE11 <- trial123 %>%
  filter(MazeID==0) %>%
  group_by(FreqTempo, DirectionDistance, MazeID) %>%
  summarise(mazePathEffeciency = mean((75.10/TravelDistance)*100, na.rm = TRUE),
            mazePathESTD = sd((75.10/TravelDistance)*100, na.rm = TRUE))

mazePathSet11 <- trial123 %>%
  filter(MazeID==0) %>%
  summarise(mazePathEfficiency = (75.10/TravelDistance)*100)


mazePathEffeciencyMAZE22 <- trial123 %>%
  filter(MazeID==1) %>%
  group_by(FreqTempo, DirectionDistance, MazeID) %>%
  summarise(mazePathEffeciency = mean((213.38/TravelDistance)*100, na.rm = TRUE),
            mazePathESTD = sd((213.38/TravelDistance)*100, na.rm = TRUE))

mazePathSet22 <- trial123 %>%
  filter(MazeID==1) %>%
  summarise(mazePathEfficiency = (213.38/TravelDistance)*100)



mazePathEffeciencyMAZE33 <- trial123 %>%
  filter(MazeID==2) %>%
  group_by(FreqTempo, DirectionDistance, MazeID) %>%
  summarise(mazePathEffeciency = mean((149.48/TravelDistance)*100, na.rm = TRUE),
            mazePathESTD = sd((149.48/TravelDistance)*100, na.rm = TRUE))

mazePathSet33 <- trial123 %>%
  filter(MazeID==2) %>%
  summarise(mazePathEfficiency = (149.48/TravelDistance)*100)



mazePathEffeciencyMAZE1
mazePathEffeciencyMAZE2
mazePathEffeciencyMAZE3


trial123 %<>% 
mutate(FreqDisPathE = ifelse(DirectionDistance == "Distance" & FreqTempo == "Frequency", mazePathEfficiency, NA),
       FreqDirePathE = ifelse(DirectionDistance == "Direction" & FreqTempo == "Frequency", mazePathEfficiency, NA),
       TempDisPathE = ifelse(DirectionDistance == "Distance" & FreqTempo == "Tempo", mazePathEfficiency, NA),
       TempDirePathE = ifelse(DirectionDistance == "Direction" & FreqTempo == "Tempo", mazePathEfficiency, NA)) %>%
  

  

#------- Shapiro + Kruskal-Wallis test for Efficiencies

SPE <- shapiro.test(trial123$mazePathEfficiency)
STE <- shapiro.test(trial123$mazeTimeEfficiency)
SPE
STE

KWE <- kruskal.test(mazePathSet11 ~ mazePathSet22)
KWE

DTTD <- dunnTest(trial123$mazePathEfficiency, trial123$mazeTimeEfficiency, method = "bonferroni")
DTTD

KWTT <- kruskal.test(TestTime ~ FreqTempo, data = dfS)
summary(KWTT)

DTTT <- dunnTest(dfS$TestTime, dfS$FreqTempo, method = "bonferroni")
DTTT

#dplyr::filter(df, !grepl("RTB",TrackingPixel))
#testest <- filter()



KWE <- kruskal.test(trial123$FreqTempo, trial123$mazeTimeEfficiency)
KWE


#--------- Device button click ------

meanFPS <- mean((1/dfP$TimeSinceLastFrame))
meanFPS

dfS %<>% 
  mutate(NewState1 = ifelse(DirectionDistance == "Distance" & DeviceButtonClickStart == "0", MazeTime, DeviceButtonClickStart/meanFPS)) %>%
  unite("DeviceOnTime", NewState1, remove = TRUE, na.rm = TRUE) %>%
  mutate(DeviceOnDistance = as.numeric(DeviceOnDistance))


dfS %>%
  ggplot(aes(x = MazeID, y = DeviceOnTime, color = DirectionDistance)) +
  geom_boxplot() +
  theme_classic() +
  ylab("Seconds of Device ON") +
  xlab("MazeID") +
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



dfS %>%
  ggplot(aes(x = MazeID, y = DeviceOnTime, color = DirectionDistance)) +
  geom_boxplot() +
  theme_classic() +
  ylab("Seconds of Device ON") +
  xlab("MazeID") +
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


dfS %>%
  ggplot(aes(x = MazeID, y = DeviceOnDistance, color = FreqTempo)) +
  geom_boxplot() +
  theme_classic() +
  ylab("Seconds of Device ON") +
  xlab("MazeID") +
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



# %>%
  #filter(str_detect(trial123, 'Toyota|Mazda'))



