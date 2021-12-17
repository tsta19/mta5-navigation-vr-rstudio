# Import, Clean and Save Data

#------- Libraries -------
library(tidyverse)
library(readbulk)
library(magrittr)

options("digits.secs" = 6)

#------- LOGGED DATA --------

#------- Import and save raw Logged data --------
# Combine all logged data files into one data frame
dfP = readbulk::read_bulk('Testparticipents', sep=';', na.strings = 'none', stringsAsFactors=FALSE)

# In R there are two main ways to get a quick overview of the data
str(dfP)

summary(dfP)

#Save the imported files
save(dfP, file='logged_data_Raw.rda', compress=TRUE)

#------- Load Raw Data From rda --------

#Load
load("logged_data_Raw.rda")

#------- Clean ~ Remove Participant 1 --------

dfP %<>% 
  filter(SessionID != "6d8c455c86d59f819d30865bbaa6f60b")


dfP$TestID[dfP$SessionID == "03c52e1f1283df72ae6c22a1beb65dd5"] <- 39
dfP$VariableCondition[dfP$SessionID == "48fc76648ded5f3266ec7acf99b6a7be"] <- 0
dfP$Freq.Tempo[dfP$SessionID == "685eea408ccd31196a3fa3cc0102bfe7"] <- "ctrl"
dfP$Direction.Distance[dfP$SessionID == "685eea408ccd31196a3fa3cc0102bfe7"] <- "ctrl"
dfP$VariableCondition[dfP$SessionID == "062a7813923c79c60590a858a62f8bc1"] <- 0
dfP$VariableCondition[dfP$SessionID == "faf4bc05969d0562c97caa49300623cc"] <- 0
dfP$Freq.Tempo[dfP$SessionID == "0b7efeddfda6e83fa30b9a978b28abd7"] <- "Temp"
dfP$VariableCondition[dfP$SessionID == "a9bc20799af465863031b7229d0e6221"] <- 2
dfP$Age[dfP$SessionID == "fb523d076f2f67cf04a15db80d2e10c8"] <- 25
dfP$VariableCondition[dfP$SessionID == "f5b90eb6c1ce95b4bf6b083a3e449094"] <- 1
dfP$Age[dfP$SessionID == "29f0865c073707cdddd6a21c74886c92"] <- 22
dfP$Sex[dfP$SessionID == "29f0865c073707cdddd6a21c74886c92"] <- F
dfP$Age[dfP$SessionID == "9f4065d5c23eac4272771074b5231c04"] <- 23
dfP$VariableCondition[dfP$SessionID == "9f4065d5c23eac4272771074b5231c04"] <- 2
dfP$VariableCondition[dfP$SessionID == "8c572d924904270febcf9edd1fcb28fd"] <- 0
dfP$Age[dfP$SessionID == "b57ec5e6e3bf26d0f6fbeb83f98b36f5"] <- 27


#------- Clean ~ Row # --------

# order data correctly by creating a row number to keep track of things
dfP <- dfP %>%
  arrange(TestID, Timestamp) %>%
  mutate(rowNum = 1:n()) %>%
  relocate(rowNum)

#------- Clean ~ names --------

# Rename Columns
dfP %<>% rename(FreqTempo = Freq.Tempo,
                DirectionDistance = Direction.Distance
                ) 

#------- Clean ~ factors --------

# Make variables into factor(s)
dfP %<>%  mutate(FreqTempo = as.factor(FreqTempo),
                 DirectionDistance = as.factor(DirectionDistance),
                 #TestID = as.factor(TestID),
                 TrialID = as.factor(TrialID),
                 Sex = as.factor(Sex),
                 MazeID = as.factor(MazeID)
                 )

# Rename the different Variables into something more telling
dfP$Sex <- recode_factor(dfP$Sex,
                               "M" = "Male",
                               "FALSE" = "Female"
)

# Rename the different Variables into something more telling
dfP$FreqTempo <- recode_factor(dfP$FreqTempo,
                                "ctrl" = "Control",
                                "Freq" = "Frequency",
                                "freq" = "Frequency",
                                "Temp" = "Tempo"
                               )

# Make sure all graphs and analysis keeps the same order of pams - just for concistancy
dfP$FreqTempo <- factor(dfP$FreqTempo, 
                        levels=c("Control", 
                                 "Frequency",
                                 "Tempo"))

# Rename the different Variables into something more telling
dfP$DirectionDistance <- recode_factor(dfP$DirectionDistance,
                                       "ctrl" = "Control",
                                       "dire" = "Direction",
                                       "dis" = "Distance",
                                       "Direc" = "Direction"
                                       )

# Make sure all graphs and analysis keeps the same order of pams - just for concistancy
dfP$DirectionDistance <- factor(dfP$DirectionDistance, 
                        levels=c("Control", 
                                 "Direction",
                                 "Distance"))

#------- Clean ~ Between Trials --------

dfP$newTestStarts <- ifelse(dfP$TestID > lag(dfP$TestID, default = 0), 1, 0)

dfP %<>% 
  mutate(NewState0 = ifelse(newTestStarts == "1", 1, NA),
         NewState1 = ifelse(MazeID == "2" & 
                           lag(MazeID) == "0", 1, NA),
         NewState2 = ifelse(MazeID == "1" & 
                              lag(MazeID) == "0", 1, NA),
         NewState3 = ifelse(MazeID == "1" & 
                              lag(MazeID) == "2", 1, NA),
         NewState4 = ifelse(MazeID == "2" & 
                              lag(MazeID) == "1", 1, NA)) %>%
  unite("NewMaze", NewState0:NewState4, remove = TRUE, na.rm = TRUE) %>%
  mutate(NewMaze = as.numeric(NewMaze))


dfP %<>% 
  mutate(NewState1 = ifelse(TrialID == "1" & 
                              lag(TrialID) == "0", 2, NA),
         NewState2 = ifelse(TrialID == "2" & 
                              lag(TrialID) == "1", 2, NA),
         NewState3 = ifelse(TrialID == "3" & 
                              lag(TrialID) == "2", 2, NA)) %>%
  unite("NewTrial", NewState1:NewState3, remove = TRUE, na.rm = TRUE) %>%
  mutate(NewTrial = as.numeric(NewTrial))


dfP %<>%
  group_by(TestID) %>%
  mutate(flag  = +(row_number() %in% unlist(map2(which(NewMaze == "1"), which(NewTrial == "2"), seq))))

dfP %<>% 
  filter(flag != 1)

#------- Clean ~ Time --------
dfP %<>%
  group_by(TestID) %>%
  mutate(testTime = cumsum(TimeSinceLastFrame)) %>%
  group_by(TestID, TrialID) %>%
  mutate(trialTime = cumsum(TimeSinceLastFrame))

#------- Save ~ Clean --------

save(dfP, file='logged_data_Clean.rda', compress=TRUE)

#------- Smaller df --------

# Here I make a Data frame that is a bit easier to work with when we are looking at simple graphs and statistic
dfS <- dfP %>%
  group_by(TestID, TrialID, Sex, Age, MazeID, FreqTempo, DirectionDistance) %>%
  #filter(TrialID != 0) %>%
  summarise(TravelDistance = max(TravelDistance, na.rm=TRUE),
            MazeTime = max(trialTime, na.rm=TRUE),
            TestTime = max(testTime, na.rm=TRUE),
            DeviceButtonClickStart = sum(DeviceButtonClickStart, na.rm=TRUE),
            DeviceButtonClickStop = sum(DeviceButtonClickStop, na.rm=TRUE),
            ToggleOnID = max(ToggleOnID, na.rm=TRUE),
            ToggleOffID = max(ToggleOffID, na.rm=TRUE)) %>%
  ungroup()

summary(dfS)

#------- Save Smaller df --------

save(dfS, file='Small_data_Clean.rda', compress=TRUE)

#------- Plot player coordinates

subdataCC <- dfP[dfP$DirectionDistance == "Control", c("MazeID", "PlayerPositionX", "PlayerPositionZ")]
plot(subdataCC$PlayerPositionX, subdataCC$PlayerPositionZ, 
     lwd=0.05, 
     main = "Control group path",
     xlab = "Player X position",
     ylab = "Player Y position")

subdataDire <- dfP[dfP$DirectionDistance == "Direction", c("PlayerPositionX", "PlayerPositionZ")]
plot(subdataDire$PlayerPositionX, subdataDire$PlayerPositionZ, 
     lwd=0.05, 
     main = "Direction group path",
     xlab = "Player X position",
     ylab = "Player Y position")

subdataDis <- dfP[dfP$DirectionDistance == "Distance", c("PlayerPositionX", "PlayerPositionZ")]
plot(subdataDis$PlayerPositionX, subdataDis$PlayerPositionZ, 
     lwd=0.05, 
     main = "Distance group path",
     xlab = "Player X position",
     ylab = "Player Y position")

subdataFreq <- dfP[dfP$FreqTempo == "Frequency", c("PlayerPositionX", "PlayerPositionZ")]
plot(subdataDis$PlayerPositionX, subdataDis$PlayerPositionZ, 
     lwd=0.05, 
     main = "Frequency group path",
     xlab = "Player X position",
     ylab = "Player Y position")

subdataTempo <- dfP[dfP$FreqTempo == "Tempo", c("PlayerPositionX", "PlayerPositionZ")]
plot(subdataDis$PlayerPositionX, subdataDis$PlayerPositionZ, 
     lwd=0.05, 
     main = "Tempo group path",
     xlab = "Player X position",
     ylab = "Player Y position")
