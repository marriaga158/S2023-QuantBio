library("readxl")
library("dplyr")
library("ggplot2")

library("knitr")
library("kableExtra")
library("magrittr")

# What we need to do:
# Separate out the average wind speeds per month per station

generate_avgs <- function(month_data){
  temp <- month_data %>%
    group_by(City) %>%
    summarise(n=n(),
              avg=mean(DailyWindSpeedAvg),
              sdev=sd(DailyWindSpeedAvg, na.rm=TRUE))
  temp$k <- with(temp, (sdev/avg) ^ -1.086)
  #temp <- temp %>%
  #  rowwise() %>%
  #  mutate(a = gamma(k))
  # temp$a <- with(temp, integrate(function(t) {(t^((1+(1/k))-1)) * (exp(1)^t)}, lower = -Inf, upper = Inf))
  
  # I am not entirely confident that this is the correct way to do this
  # because plugging this into online calculators gives nonreal answers
  temp$a <- with(temp, avg/gamma(1+(1/k)) )
  temp$avgWindSpeed <- with(temp, a*gamma(1+(1/k)) )
  # !!! THIS ASSUMES A 95% AIR DENSITY !!!
  # i couldn't get air density calculations so this is
  # what we got
  temp$avgPowWindPoten <-
    with(temp, n * (0.5 * 0.95 * (avg^3) * gamma(1+3/k) ))
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

# All of these below output the LaTeX for the tables in the appendix of the report
#April22
April22Avg %>%
  # mutate(across(where(is.numeric), round, digits = 1)) %>% ## this is deprecated, use
  mutate(across(where(is.numeric), \(x) round(x, digits = 1))) %>%
  kable(format = 'latex', caption = "April 2022", booktabs = TRUE)

#May22
May22Avg %>%
  mutate(across(where(is.numeric), \(x) round(x, digits = 1))) %>%
  kable(format = 'latex', caption = "May 2022", booktabs = TRUE)

#June22
June22Avg %>%
  mutate(across(where(is.numeric), \(x) round(x, digits = 1))) %>%
  kable(format = 'latex', caption = "June 2022", booktabs = TRUE)

#July22
July22Avg %>%
  mutate(across(where(is.numeric), \(x) round(x, digits = 1))) %>%
  kable(format = 'latex', caption = "July 2022", booktabs = TRUE)

#August22
August22Avg %>%
  mutate(across(where(is.numeric), \(x) round(x, digits = 1))) %>%
  kable(format = 'latex', caption = "August 2022", booktabs = TRUE)

#September22
September22Avg %>%
  mutate(across(where(is.numeric), \(x) round(x, digits = 1))) %>%
  kable(format = 'latex', caption = "September 2022", booktabs = TRUE)

#October22
October22Avg %>%
  mutate(across(where(is.numeric), \(x) round(x, digits = 1))) %>%
  kable(format = 'latex', caption = "October 2022", booktabs = TRUE)

#November22
November22Avg %>%
  mutate(across(where(is.numeric), \(x) round(x, digits = 1))) %>%
  kable(format = 'latex', caption = "November 2022", booktabs = TRUE)

#December22
December22Avg %>%
  mutate(across(where(is.numeric), \(x) round(x, digits = 1))) %>%
  kable(format = 'latex', caption = "December 2022", booktabs = TRUE)

#January23
January23Avg %>%
  mutate(across(where(is.numeric), \(x) round(x, digits = 1))) %>%
  kable(format = 'latex', caption = "January 2023", booktabs = TRUE)

#February23
February23Avg %>%
  mutate(across(where(is.numeric), \(x) round(x, digits = 1))) %>%
  kable(format = 'latex', caption = "February 2023", booktabs = TRUE)

#March23
March23Avg %>%
  mutate(across(where(is.numeric), \(x) round(x, digits = 1))) %>%
  kable(format = 'latex', caption = "March 2023", booktabs = TRUE)