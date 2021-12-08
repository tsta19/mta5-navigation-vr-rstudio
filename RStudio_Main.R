library(rio)
library(utils)

install.packages("dplyr")

rio_csv = import("C:/Users/thoma/Desktop/Github Repositories/mta5-navigation-vr-rstudio/Testparticipents/TP2.csv")
head(rio_csv)
View(rio_csv)

#Laver plots i par af alt dataen i dataframet
plot(rio_csv)

str(rio_csv)
summary(rio_csv)

