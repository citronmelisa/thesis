# load libraries ----
rm(list = ls())
Sys.setlocale("LC_ALL", "latvian_Latvia.1257")
library(tidyr)
library(dplyr)
library(ggplot2)
library(xts)
library(reshape2)

# funcs for data reading and tidying ----
get_modal_ts <- function(data) {
  ts <- xts(x = data$value, order.by = data$datetime)
  return(ts)
}

reshape_data <- function(data) {
  data <- data %>% drop_na()
  x <- pivot_longer(data, names_to = "time", starts_with("0") | starts_with("1") | starts_with("2")) %>% 
    rename(date = "Datums \\ Laiks") %>% 
    mutate(datetime = as.POSIXct(paste(date, time), format = "%d.%m.%Y %H:%M")) %>% 
    select(datetime, value)
  
  return(x)
}


# load data ----
freq_raw <- readxl::read_excel(path = "data_2703/Frekvences_V1.xlsx")
temp_raw <- readxl::read_excel(path = "data_2003/Gaisa temp_I-VI.xls")
humid_raw <- readxl::read_excel(path = "data_2003/relativais mitrums_I_VI.xls")
prec_raw <- readxl::read_excel(path = "data_2003/Nokrisni_I-VI.xls")
snow_raw <- readxl::read_excel(path = "data_2003/sniega sega_I_VI.xls")
n <- length(freq_raw$f1)
erms_all_data <- readxl::read_excel(path="all_erms.xlsx")


# get data ready for preparing----
#freq <- freq_raw %>% 
#  mutate(datetime = posixct(Laiks, format = "'10-Nov-2021 14:06:09'	"))
#'10-Nov-2021 14:06:09'
Sys.setlocale("LC_ALL","English")

freq <- freq_raw %>% 
  mutate(datetime = strptime(Laiks, format = "%d-%b-%Y %H:%M:%S")) %>% 
  select(datetime = Laiks, f1, f2, f3, f4, f5, f6)
#sapply(prec, freq)

#all_modal <- c(reshape_data(humid_raw), reshape_data(prec_raw), reshape_data(snow_raw), reshape_data(temp_raw))
humid_0 <- reshape_data(temp_raw)
humid_0[is.na(humid_0)] <- as.POSIXct(paste("20.01.2022"), format = "%d.%m.%Y %H:%M")
get_modal_ts(reshape_data(temp_raw))
humid <- get_modal_ts(humid_0)
#humid <- get_modal_ts(reshape_data(humid_raw))
prec <- get_modal_ts(reshape_data(prec_raw))
snow <- get_modal_ts(reshape_data(snow_raw))
temp <- get_modal_ts(reshape_data(temp_raw))

# delete missing data ----
freq_no_na <- na.omit(freq)
time <- c(1:length(freq_no_na$f1))
freq_no_na <- freq_no_na %>% 
  mutate(t = time) %>% 
  select(f1, f2, f3, f4, f5, f6)

install.packages("EL")
library(EL)

temp1 <- c(1:8)
temp2 <- c(3:19)

EL.means(temp1, temp2)
