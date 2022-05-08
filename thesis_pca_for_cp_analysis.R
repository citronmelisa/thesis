
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

# # plot modal
# par(mfrow = c(4, 1))
# plot(humid, main = "Relatīvais mitrums")
# plot(prec, main = "Nokrišņi, cm")
# plot(temp, main = "Temperatūra, grādi Celsija")
# plot(snow, main = "Sniega sega, cm")
# 
# # plot frequencies
# Sys.setlocale("LC_ALL", "latvian_Latvia.1257")
# par(mfrow = c(2, 1))
# plot(freq$datetime, freq$f1, col = "red2", ylim = c(3.2,5.8), pch = 20, ylab = "Frekvence, HZ",xlab = "datums", main = "Frekvenču dati (F1-F3)")
# points(freq$datetime, freq$f2,col = "indianred", pch = 20)
# points(freq$datetime, freq$f3,col = "darkorange1", pch = 20)
# legend('top',legend = c("F1", "F2", "F3"), col = c("red2","indianred", "darkorange1"), 
#        lwd = 5, xpd = TRUE, horiz = TRUE, cex = 1, seg.len = 1, bty = 'n')
# plot(freq$datetime, freq$f4, col = "maroon", pch = 20,ylim = c(11,15.5), ylab = "Frekvence, HZ", xlab = "datums", main = "Frekvenču dati (F4-F6)")
# points(freq$datetime, freq$f5,col = "dodgerblue", pch = 20)
# points(freq$datetime, freq$f6,col = "purple4", pch = 20)
# legend('top',legend = c("F4", "F5", "F6"), col = c("maroon","dodgerblue", "purple4"), 
#        lwd = 5, xpd = TRUE, horiz = TRUE, cex = 1, seg.len = 1, bty = 'n')

# delete missing data ----
freq_no_na <- na.omit(freq)
time <- c(1:length(freq_no_na$f1))
freq_no_na <- freq_no_na %>% 
  mutate(t = time) %>% 
  select(f1, f2, f3, f4, f5, f6)

# prepare data (list of analyzable data frames) ----

cubic_damage <- function(len, coef, offset = 4200) {
  ans = 1:len
  for (idx in 1:len) {
    if (idx >= offset) {
      ans[idx] = coef * (idx - offset) ^ (1 / 3)
    } else{
      ans[idx] = 0
    }
  }
  return(ans)
}

linear_damage <- function(len, coef, offset = 4200) {
  ans = 1:len
  for (idx in 1:len) {
    if (idx >= offset) {
      ans[idx] = coef
    } else{
      ans[idx] = 0
    }
  }
  return(ans)
}

add_noise <- function(ts, noise_level, offset = 4200){
  len <- length(ts) - offset
  noise <- c(rep(0,(length(ts) - len)), rnorm(len, mean = 0, sd = noise_level))
  ans <- ts + noise
  return(ans)
}

add_damage <- function(ts, damage_coef, damage_type, offset) {
  len <- length(ts)
  
  if (damage_type == 'cubic') {
    damage <- cubic_damage(len, damage_coef, offset)
    ans <- ts + damage
    return(ans)
  } else if (damage_type == 'linear') {
    damage <- linear_damage(len, damage_coef, offset)
    ans <- ts + damage
    return(ans)
  }
}

get_ts_with_damage <-
  function(damage_type, damage_coef, ts_df, offset = 4200) {
    len <- nrow(ts_df)
    ans <- data.frame(matrix(ncol = 0, nrow = nrow(ts_df)))
    colname <- c()
    
    if (damage_type == 'cubic') {
      for (row in 1:length(ts_df)) {
        #Looping thru f1-f6
        for (col in 1:ncol(ts_df)) {
          #looping thru dam coefs
          ts <- ts_df[[col]]
          coef <- damage_coef[[col]][row]
          ts_damaged <- add_damage(ts, coef, 'cubic', offset)
          ans <- cbind(ans, ts_damaged)
          colname <-
            c(colname, paste("f", col, "_dam_", row, "_cubic"))
          
        }
      }
      colnames(ans) <- colname
    } else if (damage_type == 'linear') {
      for (row in 1:length(ts_df)) {
        #Looping thru f1-f6
        for (col in 1:ncol(ts_df)) {
          #looping thru dam coefs
          ts <- ts_df[[col]]
          coef <- damage_coef[[col]][row]
          ts_damaged <- add_damage(ts, coef, 'linear', offset)
          ans <- cbind(ans, ts_damaged)
          colname <-
            c(colname, paste("f", col, "_dam_", row, "_linear"))
        }
      }
      colnames(ans) <- colname
    }
    return(ans)
  }

get_ts_with_noise <- function(ts_df, noise_level){
  ans <- data.frame(matrix(ncol = 0, nrow = nrow(ts_df)))  
  colname <- c()
  for (sd in 1:length(noise_level)){
    for (col in 1:ncol(ts_df)){
      ts <- ts_df[[col]]
      ts_name <- names(ts_df[col]) 
      noise <- noise_level[sd]
      ts_noisy <- add_noise(ts, noise)
      ans <- cbind(ans, ts_noisy)
      colname <- c(colname, paste(ts_name,"_noise_", sd))
    }
  }
  colnames(ans) <- colname
  return(ans)
}

noise_sd_level <- c(0, 10^-4, 10^-3, 10^-2, 10^-1)
prc_damage_coef <- c(0.001, 0.005, 0.01, 0.02, 0.05)
data_mean <- c(
  mean(freq_no_na$f1),
  mean(freq_no_na$f2),
  mean(freq_no_na$f3),
  mean(freq_no_na$f4),
  mean(freq_no_na$f5),
  mean(freq_no_na$f6))
damage_coef_f1 <- prc_damage_coef * data_mean[1]
damage_coef_f2 <- prc_damage_coef * data_mean[2]
damage_coef_f3 <- prc_damage_coef * data_mean[3]
damage_coef_f4 <- prc_damage_coef * data_mean[4]
damage_coef_f5 <- prc_damage_coef * data_mean[5]
damage_coef_f6 <- prc_damage_coef * data_mean[6]
damage_coef <- as.data.frame(cbind(damage_coef_f1,
                                   damage_coef_f2,
                                   damage_coef_f3, 
                                   damage_coef_f4,
                                   damage_coef_f5,
                                   damage_coef_f6))

all_damage_ts <- cbind(get_ts_with_damage('linear', damage_coef, freq_no_na), 
                       get_ts_with_damage('cubic', damage_coef, freq_no_na))
ts.plot(all_damage_noise_ts[3])

all_damage_noise_ts <- get_ts_with_noise(all_damage_ts, noise_sd_level)

data_list <- split.default(all_damage_noise_ts, rep(1:60, each = 6))
# Do PCA on all data to get ERMS ----
split_df_test_train <- function(df, prc_train, prc_test, prc_other = 0) {
  f_all <- df
  ans <- list()
  
  if (prc_other == 0){
    train_end <- ceiling((prc_train/100) * nrow(f_all))
    f_train <- f_all[1:train_end,]
    f_test <- f_all[(train_end+1):nrow(f_all),]
  }
  ans[[1]] <- f_all
  ans[[2]] <- f_train
  ans[[3]] <- f_test
  
  return(ans)
}

# get each frequency mean
df_col_means <- function(df) {
  n <- nrow(df)
  ans <- c()
  
  for (col in 1:ncol(df)){
    ts <- df[[col]]
    mean <- mean(ts)
    ans <- c(ans, mean)
  }
  return(ans)
}

mean_center_df <- function(df) {
  means <- df_col_means(df)
  ts_centered <- c()
  colname <- colnames(df)
  ans <- c()
  
  for(col in 1:ncol(df)){
    for (datapoint in 1:nrow(df)){
      ts_centered[datapoint] <- df[datapoint, col] - means[col]
    }
    ans <- cbind(ans, ts_centered)
  }
  colnames(ans) <- colname
  return(as.data.frame(ans))
}

get_cov_matrix <- function(df_train) {
  covd_train <- cov(df_train)
  ans <- covd_train
  
  return(ans)
}

get_eigens <- function(covd_matrix){
  ans <- eigen(covd_matrix)
  
  return(ans)
}

get_total_variance_by_cols <- function(matrix){
  Y <- as.data.frame(matrix)
  col_sd <- c()
  ans <- c()
  
  for(col in 1:ncol(Y)){
    col_sd[col] <- (sd(Y[,col]))^2
  }
  ans <- sum(col_sd)
  return(ans)
}

get_variance_by_cols <- function(matrix){
  Y <- as.data.frame(matrix)
  col_sd <- c()
  ans <- c()
  
  for(col in 1:ncol(Y)){
    ans[col] <- (sd(Y[,col]))^2
  }
  
  return(ans)
}

#returns centered reconstructed pca data (USE MAX 5 comps!!!)
do_pca_reconstr <- function(df_train, df_all, n_comps){
  centered_train <- mean_center_df(f_train)
  centered_all <- mean_center_df(f_all)
  covd_train <- get_cov_matrix(centered_train)
  eigen_train <- get_eigens(covd_train)
  eigen_train_vecs <- eigen_train$vectors[, 1:n_comps]
  
  Y <- t(eigen_train_vecs) %*% t(centered_all) 
  pca_reconstr <- eigen_train_vecs %*% Y
  ans <- as.data.frame(t(pca_reconstr))
  colnames(ans) <- colnames(freq_no_na)
  
  return(ans)
}

really_stupid_conversion_function <- function(df){
  ans <- data.frame(matrix(ncol=0, nrow = nrow(df)))
  
  for (col in 1:ncol(df)){
    temp_col <- df[col]
    temp_col <- unlist(temp_col)
    temp_col <- as.data.frame(temp_col)
    
    ans <- cbind(ans, temp_col)
  }
  colnames(ans) <- c("f1", "f2", "f3", "f4", "f5", "f6")
  return(ans)
}

get_ERMS <- function(f_all, n_comps, train_prc = 70) {
  df_split <- split_df_test_train(f_all, prc_train = 70, prc_test = (100 - train_prc))
  df_all <- df_split[[1]]
  df_train <- df_split[[2]]
  
  centered_original_df <- mean_center_df(df_all)
  centered_original_df <- really_stupid_conversion_function(centered_original_df)
  centered_reconstr_pca_df <- do_pca_reconstr(df_train = df_train, df_all = df_all, n_comps = n_comps)
  res <- centered_reconstr_pca_df - centered_original_df
  res_2 <- res^2
  
  
  
  ERMS <- (rowSums(res_2) / ncol(f_all))^(1/2)
  return(ERMS)
}
ts.plot(get_ERMS(data_list[[45]], 2, 70))


f_all_train_test <- split_df_test_train(freq_no_na, 70, 30)
f_all <- as.data.frame(f_all_train_test[[1]])
f_train <- as.data.frame(f_all_train_test[[2]])
f_test <- as.data.frame(f_all_train_test[[3]])

freq_means <- df_col_means(f_all)
freq_means_train <- df_col_means(f_train)
freq_means_test <- df_col_means(f_test)

f_all_centered <- mean_center_df(f_all)
f_train_centered <- mean_center_df(f_train)
f_test_centered <- mean_center_df(f_test)

# temp <- get_ERMS(freq_no_na, 4, 70)
# ts.plot(temp)

comps_considered <- c(2, 3, 4, 5)

remotes::install_github("walkerke/tidycensus")
library(tidyverse)
library(tidycensus)
table_names <- melt(colnames(all_damage_noise_ts))
table_names <- table_names %>% 
  mutate(df_group = gsub(" ", "", substr(value, 5, length(value)), fixed = T)) %>% 
  group_by(df_group) %>% 
  slice(1) %>% 
  select(df_group)

table_names <- pull(table_names, df_group)
names(data_list) <- table_names

test_list <- data_list[1:4]

get_all_ERMS <- function(f_all_df_list, n_comps_vec, train_prc){
  start_time <- Sys.time()
  ans <- as.data.frame(matrix(nrow = nrow(f_all_df_list[[1]]), ncol = 0))
  
  for (df in 1:length(f_all_df_list)){
    data <- f_all_df_list[[df]]
    
    for(comp in n_comps_vec){
      erms <- get_ERMS(data, comp, train_prc)
      name <- paste("erms" ,names(f_all_df_list[df]), "_comp", comp)
      #names(erms) <- name
      ans <- cbind(ans, erms)
      names(ans)[names(ans) == 'erms'] <- name
    }
  }
  end_time <- Sys.time()
  print(end_time - start_time)
  return(ans)
}

#temp <- get_all_ERMS(test_list, 1, 70)
all_erms_df <- get_all_ERMS(data_list, comps_considered, 70)
ts.plot(all_erms_df[4])

ts.plot(all_erms_df[])
test <- get_all_ERMS(data_list, 6, 70)
ts.plot(test[,])
# 70% train ---> 3278
#3278+1 - 4199 ņem sd 
#cp 4200

# find change points from PCA ----
#treshold_data_start <- nrow(f_train) + 1
#treshold_data_end <- treshold_data_start  + (nrow(f_all) - 4200)
#data_for_threshold <- all_erms_df[treshold_data_start:treshold_data_end, ]

data_for_threshold <- all_erms_df[1:4200, ]

get_tresholds <- function(treshold_data_df){
  ans <- as.data.frame(matrix(nrow = 1, ncol = 0))
  for (col in 1:ncol(treshold_data_df)){
    sd <- sd(treshold_data_df[[col]])
    ans <- cbind(ans, sd)
  }
return(ans)
}

temps <- get_tresholds(data_for_threshold)
ts.plot(all_erms_df[104])
abline(h = 3*temps[104])

