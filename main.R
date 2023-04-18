library("readxl")
library("dplyr")

# What we need to do:
# Separate out the average wind speeds per month per station

April22 <- read_excel("Wind Speed Data - April 2022 through March 2023.xlsx",sheet = "April 2022")
April22Avgs <- April22 %>%
  group_by(City) %>%
  summarise(avg=mean(DailyWindMax))