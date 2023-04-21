library("readxl")
library("dplyr")
library("ggplot2")

# What we need to do:
# Separate out the average wind speeds per month per station

generate_avgs <- function(month_data){
  temp <- month_data %>%
    group_by(City) %>%
    summarise(num_measurements=n(),
              avg=mean(DailyWindSpeedAvg),
              standard_dev=sd(DailyWindSpeedAvg, na.rm=TRUE))
  temp$k <- with(temp, (standard_dev/avg) ^ -1.086)
  #temp <- temp %>%
  #  rowwise() %>%
  #  mutate(a = gamma(k))
  # temp$a <- with(temp, integrate(function(t) {(t^((1+(1/k))-1)) * (exp(1)^t)}, lower = -Inf, upper = Inf))
  
  # I am not entirely confident that this is the correct way to do this
  # because plugging this into online calculators gives nonreal answers
  temp$a <- with(temp, avg/gamma(1+(1/k)) )
  temp$avg_monthly_weibull_wind_speed <- with(temp, a*gamma(1+(1/k)) )
  # !!! THIS ASSUMES A 95% AIR DENSITY !!!
  # i couldn't get air density calculations so this is
  # what we got
  temp$avg_monthly_weibull_power_wind_potential <-
    with(temp, num_measurements * (0.5 * 0.95 * (avg^3) * gamma(1+3/k) ))
  temp
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