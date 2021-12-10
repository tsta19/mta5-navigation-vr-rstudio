library(rio)
library(utils)
library(ggplot2)
library(Hmisc)
library(tidyr)
library(dplyr)

# Datasets
data = read.table("C:/Users/thoma/Desktop/RStudio Backup Files/08-12-2021 ; 13.56/Combined/TP2.csv", header = TRUE, sep=";")
comb_data = import("C:/Users/thoma/Desktop/RStudio Backup Files/09-12-2021 ; 19.43/combined-csv-files.csv")

View(data)

#Script til at merge csv
library(data.table)
setwd("C:/Users/thoma/Desktop/RStudio Backup Files/09-12-2021 ; 19.43")
files <- list.files(pattern = ".csv")
temp <- lapply(files, fread, sep=";")
data <- rbindlist(temp, fill = TRUE)
write.csv(data, file="combined-csv-files.csv", row.names = FALSE)

bar_colors <- c("lightblue", "mistyrose", "lightcyan", 
               "lavender", "cornsilk")

barPlot <- data[1:5, "TravelDistance"]
barplot(barPlot, names.arg = c("0", "1", "2", "3", "4"), 
        col = bar_colors,
        legend = rownames(barPlot), beside = TRUE, 
        main = "Age Comparison in Groups", xlab = "Condition", ylab = "Age")

plot <- data.frame(group = c("Maze1", "Maze2", "Maze3"),
                 condition0 = c(0,0,0),
                 condition1 = c(4,10,5),
                 condition2 = c(5,8,10),
                 condition3 = c(7,2,4),
                 condition4 = c(7,2,4))
plotTall <- plot %>% gather(key = Group, value = Time_Used_On_Device, condition0:condition4)
ggplot(plotTall, aes(Group, Time_Used_On_Device, fill = group)) + geom_col(width = 0.5, position = position_dodge(0.7))

#Laver plots i par af alt dataen i dataframet
plot(rio_csv)

by_trial_device <- comb_data %>% group_by(TestID, DeviceButtonClickTimerSpent)
group_device <- comb_data %>% group_by(DeviceButtonClickTimerSpent)
by_id_age <- comb_data %>% group_by(TestID, Age) %>% dplyr::summarize(Mean = mean(DeviceButtonClickTimerSpent, na.rm=TRUE))
by_id_age
summarise(group_device)
summarise(by_trial_device)

View(data)
View(by_participant_timedevice)
View(comb_data)
str(rio_csv)
summary(rio_csv)
