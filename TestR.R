library(rio)
library(utils)

#Loader en csv fil
rio_csv = import("C:/Users/kspar/OneDrive/Dokumenter/GitHub/mta5-navigation-vr-rstudio/test2.csv")
head(rio_csv)
View(rio_csv)

#Laver plots i par af alt dataen i dataframet
plot(rio_csv)

str(rio_csv)
summary(rio_csv)

#Ændre data i dataframe
rio_csv$TestID = 1

write.csv(rio_csv,"C:/Users/kspar/OneDrive/Dokumenter/GitHub/mta5-navigation-vr-rstudio/test2Updata.csv", row.names = FALSE)
