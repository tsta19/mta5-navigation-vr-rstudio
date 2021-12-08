library(rio)
library(utils)

install.packages("tidyverse")

rio_csv = import("C:/Users/thoma/Desktop/RStudio Backup Files/08-12-2021 ; 13.56/Combined/combined-csv-files.csv")
head(rio_csv)
View(rio_csv)

#Script til at merge csv
library(data.table)
setwd("C:/Users/thoma/Desktop/RStudio Backup Files/08-12-2021 ; 13.56/Combined")
files <- list.files(pattern = ".csv")
temp <- lapply(files, fread, sep=";")
data <- rbindlist(temp, fill = TRUE)
write.csv(data, file="combined-csv-files.csv", row.names = FALSE)


#Laver plots i par af alt dataen i dataframet
plot(rio_csv)

str(rio_csv)
summary(rio_csv)


