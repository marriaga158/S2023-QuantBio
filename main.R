library("readxl")
library("dplyr")

# What we need to do:
# Separate out the average wind speeds per month per station

generate_avgs <- function(month_data){
  month_data %>%
    group_by(City) %>%
    summarise(avg=mean(DailyWindSpeedAvg))
}

path <- "Wind Speed Data - April 2022 through March 2023.xlsx"
sheetnames <- excel_sheets(path)
monthSheets <- lapply(excel_sheets(path), read_excel, path = path)

names(monthSheets) <- sheetnames
# issue with name here
April22 <- monthSheets['April 2022'][1]
April22Avg <- generate_avgs(monthSheets['April 2022'])