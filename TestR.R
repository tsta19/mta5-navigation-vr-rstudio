library(rio)
library(utils)

#Loader en csv fil
rio_csv = import("C:/Users/kspar/OneDrive/Dokumenter/GitHub/mta5-navigation-vr-rstudio/DataLogging/HopefullyLast.csv")
head(rio_csv)
View(rio_csv)

#Laver plots i par af alt dataen i dataframet
plot(rio_csv)

str(rio_csv)
summary(rio_csv)
