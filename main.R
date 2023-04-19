library("readxl")
library("dplyr")
library("ggplot2")

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
April22Avg <- 
  generate_avgs(do.call(rbind.data.frame, monthSheets["April 2022"]))
May22Avg <- 
  generate_avgs(do.call(rbind.data.frame, monthSheets["May 2022"]))
June22Avg <- 
  generate_avgs(do.call(rbind.data.frame, monthSheets["June 2022"]))
July22Avg <- 
  generate_avgs(do.call(rbind.data.frame, monthSheets["July 2022"]))
August22Avg <- 
  generate_avgs(do.call(rbind.data.frame, monthSheets["August 2022"]))
September22Avg <- 
  generate_avgs(do.call(rbind.data.frame, monthSheets["September 2022"]))
October22Avg <- 
  generate_avgs(do.call(rbind.data.frame, monthSheets["October 2022"]))
November22Avg <- 
  generate_avgs(do.call(rbind.data.frame, monthSheets["November 2022"]))
December22Avg <- 
  generate_avgs(do.call(rbind.data.frame, monthSheets["December 2022"]))
January23Avg <- 
  generate_avgs(do.call(rbind.data.frame, monthSheets["January 2023"]))
February23Avg <- 
  generate_avgs(do.call(rbind.data.frame, monthSheets["February 2023"]))
March23Avg <- 
  generate_avgs(do.call(rbind.data.frame, monthSheets["March 2023"]))

# this doesn't really do anything useful, I just wanted to see if I could plot
# the data
ggplot(April22Avg, aes(x=City, y=avg)) + geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle = 90))
