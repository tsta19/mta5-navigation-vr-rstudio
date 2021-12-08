library(rio)
library(utils)

install.packages("dplyr")

rio_csv = import("/Users/tromborg/Desktop/Testparticipents - Kopi")
head(rio_csv)
View(rio_csv)

#Script til at merge csv
library(data.table)
setwd("/Users/tromborg/Desktop/Mergemap")
files <- list.files(pattern = ".csv")
temp <- lapply(files, fread, sep=";")
data <- rbindlist( temp )
write.csv(data, file="mergecsv.csv", row.names = FALSE)


#Laver plots i par af alt dataen i dataframet
plot(rio_csv)

str(rio_csv)
summary(rio_csv)


