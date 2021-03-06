
# load libraries ----
rm(list = ls())
Sys.setlocale("LC_ALL", "latvian_Latvia.1257")
library(tidyr)
library(dplyr)
library(ggplot2)
library(xts)
library(reshape2)
library(tidyverse)
library(tidycensus)

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

test_ts <- rep(0, 4683)
par(mfrow=c(1,1))
ts.plot(add_damage(test_ts, 0.1* mean(freq_no_na$f1), 'linear', 4200),
        xlab = 'laiks', ylab = 'frekvence, Hz')
test_ts2 <- add_damage(test_ts, 0.05* mean(freq_no_na$f3), 'cubic', 4200)
ts.plot(test_ts2)
test_ts3 <- add_noise(test_ts2, 0.1, offset = 4200)
ts.plot(test_ts3, xlab = 'laiks', ylab = 'frekvence, Hz')

get_ts_with_damage <-
  function(damage_type, damage_coef, ts_df, offset = 4200) {
    len <- nrow(ts_df)
    ans <- data.frame(matrix(ncol = 0, nrow = nrow(ts_df)))
    colname <- c()
    
    if (damage_type == 'cubic') {
      for (row in 1:nrow(damage_coef)) {
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
      for (row in 1:nrow(damage_coef)) {
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

# temp <- as.data.frame(matrix(ncol = 0, nrow = 4683))
# temp <- cbind(temp, rep(0, 4683))
# temp <- get_ts_with_damage("cubic", damage_coef, freq_no_na, offset = 4200)
# ts.plot(get_ts_with_damage("linear", 0.1, temp, offset = 4200))
# temp2 <- get_ts_with_damage("linear", 0.1, temp, offset = 4200)


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
# temp <- as.data.frame(matrix(ncol = 0, nrow = 4683))
# temp <- cbind(temp, rep(0, 4683))
# temp2 <- (get_ts_with_noise(temp, 0.01))

noise_sd_level <- c(0, 10^-4, 10^-3, 10^-2, 10^-1)
prc_damage_coef <- c(0, 0.001, 0.005, 0.01, 0.02, 0.05)


data_mean <- c(
  mean(freq_no_na$f1),
  mean(freq_no_na$f2),
  mean(freq_no_na$f3),
  mean(freq_no_na$f4),
  mean(freq_no_na$f5),
  mean(freq_no_na$f6))
damage_coef_f1 <- prc_damage_coef * data_mean[1]
damage_coef_f2 <- rep(0, length(prc_damage_coef))
damage_coef_f3 <- prc_damage_coef * data_mean[3]
damage_coef_f4 <- rep(0, length(prc_damage_coef))
damage_coef_f5 <- rep(0, length(prc_damage_coef))
damage_coef_f6 <- rep(0, length(prc_damage_coef))
damage_coef <- as.data.frame(cbind(damage_coef_f1,
                                   damage_coef_f2,
                                   damage_coef_f3, 
                                   damage_coef_f4,
                                   damage_coef_f5,
                                   damage_coef_f6))

all_damage_ts <- cbind(get_ts_with_damage('linear', damage_coef, freq_no_na), 
                       get_ts_with_damage('cubic', damage_coef, freq_no_na))
# ts.plot(all_damage_ts[31])
# ts.plot(all_damage_noise_ts[3])

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
# ts.plot(get_ERMS(test_list[[1]], 2, 70))
# temp <- get_ERMS(test_list[[1]], 2, 70)
# temp2 <- get_ERMS(test_list[[2]], 2, 70)



# temp <- get_ERMS(freq_no_na, 4, 70)
# ts.plot(temp)

comps_considered <- c(2, 3, 4, 5)


table_names <- melt(colnames(all_damage_noise_ts))
table_names <- table_names %>% 
  mutate(df_group = gsub(" ", "", substr(value, 5, length(value)), fixed = T)) %>% 
  group_by(df_group) %>% 
  filter(row_number()==1) %>% 
  select(df_group)
table_names <- table_names$df_group

names(data_list) <- table_names

test_list <- data_list[1:8]

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

#temp <- data_list[1:2]
#temp2 <- get_all_ERMS(temp, 1, 70)
#all_erms_test <- get_all_ERMS(test_list, comps_considered, 70)
all_erms_df <- get_all_ERMS(data_list, comps_considered, 70)
#Hmisc::describe(all_erms_df)
#ts.plot(all_erms_df[4])


#test <- get_all_ERMS(data_list, 6, 70)
#ts.plot(test[,])

# 70% train ---> 3278
#3278+1 - 4199 ņem sd 
#cp 4200

# find change points from PCA ----
treshold_data_start <- nrow(f_train) + 1
treshold_data_end <- treshold_data_start  + (nrow(f_all) - 4200)
#data_for_threshold <- all_erms_df[treshold_data_start:treshold_data_end, ]

data_for_threshold <- all_erms_df[treshold_data_start:treshold_data_end, ] #all_erms_df[1:3000,]

get_tresholds <- function(treshold_data_df){
  ans <- as.data.frame(matrix(nrow = 1, ncol = 0))
  for (col in 1:ncol(treshold_data_df)){
    sd <- 3 * sd(treshold_data_df[[col]])
    mean <- mean(treshold_data_df[[col]])
    tres <- sd + mean
    ans <- cbind(ans, tres)
  }
return(ans)
}

#ts.plot(all_damage_noise_ts[4])
#abline(v = 4200, col = 'red')


tresholds <- get_tresholds(data_for_threshold)
#ts.plot(all_erms_df[104])
#abline(h = 3*tresholds[104])

get_erms_metadata <- function(erms_df_column){
  metadata_raw <- colnames(erms_df_column)
  metadata <- gsub(" ", "", metadata_raw)
  
  ans_raw <- unlist(str_split(metadata, "_", 7))
  
  return(ans_raw)
}

metadata <- as.data.frame(matrix(nrow = length(all_erms_df), ncol = 0))
metadata <- cbind(1:length(all_erms_df),names(all_erms_df))
colnames(metadata) <- c("df_num","meta")
metadata <- as.data.frame(metadata)

metadata <- metadata %>% 
  mutate(damage = substr(meta, 11, 11),
         damage_type = ifelse(grepl("cubic", meta, fixed = T), substr(meta, 13, 17), substr(meta, 13, 18)),
         noise = ifelse(grepl("cubic", meta, fixed = T), substr(meta, 25, 25), substr(meta, 26, 26)),
         n_comp = ifelse(grepl("cubic", meta, fixed = T), substr(meta, 33, 33), substr(meta, 34, 34)))


# write data to excel ----
#openxlsx::write.xlsx(all_erms_df, "all_erms_df.xlsx")
#openxlsx::write.xlsx(freq_no_na, "original_data_no_missing.xlsx")
#openxlsx::write.xlsx(all_damage_noise_ts, "all_data_w_damage_noise")

# plot ERMS for all data by number of components ----
#par(mfrow=c(4, 1))
Sys.setlocale("LC_ALL", "latvian_Latvia.1257")
for (erms in 1:length(all_erms_df)){
  jpeg(filename = paste(erms, "_damage"  ,
                        metadata[erms,]$damage,
                        metadata[erms,]$damage_type,
                        "noise", metadata[erms,]$noise,
                        "n_comps", metadata[erms,]$n_comp, ".jpeg",
                        sep=""))
  plot_name <- paste("bojājuma tips ", metadata[erms,]$damage_type, ": ", metadata[erms,]$damage,
                     ", trokšņa tips: ", metadata[erms,]$noise,
                     ", ņemot vērā " , metadata[erms,]$n_comp, " komponentes.",
                     sep = "")
  ts.plot(all_erms_df[[erms]], main = plot_name, ylab = "Hz", xlab = "laiks")
  abline(h = tresholds[erms], col='red')
  dev.off()
}



# graphic playground ----
# # Damage 1-6 on frequency f1 point 4200 linear
# ts.plot(all_damage_noise_ts[1])
# abline(v=4200)
# ts.plot(all_damage_noise_ts[7])
# abline(v=4200)
# ts.plot(all_damage_noise_ts[13])
# abline(v=4200)
# ts.plot(all_damage_noise_ts[19])
# abline(v=4200)
# ts.plot(all_damage_noise_ts[25])
# abline(v=4200)
# ts.plot(all_damage_noise_ts[31])
# abline(v=4200)
# 
# # Damage 1-6 on frequency f1 point 4200 cubic
# ts.plot(all_damage_noise_ts$`f 1 _dam_ 1 _cubic _noise_ 1`)
# abline(v=4200)
# ts.plot(all_damage_noise_ts$`f 1 _dam_ 2 _cubic _noise_ 1`)
# abline(v=4200)
# ts.plot(all_damage_noise_ts$`f 1 _dam_ 3 _cubic _noise_ 1`)
# abline(v=4200)
# ts.plot(all_damage_noise_ts$`f 1 _dam_ 4 _cubic _noise_ 1`)
# abline(v=4200)
# ts.plot(all_damage_noise_ts$`f 1 _dam_ 5 _cubic _noise_ 1`)
# abline(v=4200)
# ts.plot(all_damage_noise_ts$`f 1 _dam_ 6 _cubic _noise_ 1`)
# abline(v=4200)
# 
# 
# 
# str(all_erms_test)
# all_erms_test <- cbind(all_erms_df, time = c(1:nrow(all_erms_df)))
# temp <- cbind(all_erms_test[,1:4], time =c(1:nrow(all_erms_df)))
# df_melt = melt(temp, id.vars = 'time')
# 
# 
# ggplot(df_melt, aes(x = time, y = value)) + 
#   geom_line() + 
#   facet_wrap(~ variable, scales = 'free_y', ncol = 1) +
#   geom_hline(yintercept = 0.1)
# 
# 
# all_damage_noise_test <- all_damage_noise_ts [,7:12]
# all_damage_noise_test <- cbind(all_damage_noise_test, time =c(1:nrow(all_damage_noise_test)))
# df_melt = melt(all_damage_noise_test, id.vars = 'time')
# ggplot(df_melt, aes(x = time, y = value)) + 
#   geom_line() + 
#   facet_wrap(~ variable, nrow = 3, scales = "free_y") +
#   geom_hline(yintercept = c(0.1))
# 
# dput(df_melt)
# 
# df_1 <- all_damage_noise_test[1:7,]
# df_2 <- df_melt[1:7,]                               
# dput(df_1)
# 
# str(df_1)
# str(df_2)


# split erms ----

split_erms_results <- function(all_erms_df, tresholds, train_end, test_end) {
  names <- c()
  ans <- matrix(nrow = 0, ncol = 7)
  
  for (col in 1:ncol(all_erms_df)){
  
  name <- names(all_erms_df[col])
  train <-  all_erms_df[1:train_end, col]
  test <- all_erms_df[(train_end+1):test_end, col]
  remaining <- all_erms_df[(test_end+1):nrow(all_erms_df), col]
  treshold <- tresholds[col]
  
  under <- 0
  over <- 0
  
    for (val in train) {
      if (val < treshold) {
        under <- under + 1
      } else {
        over <- over + 1
      }
    }
  train_under <- under
  train_over <- over
  
  under <- 0
  over <- 0
  for (val in test) {
    if (val < treshold) {
      under <- under + 1
    } else {
      over <- over + 1
    }
  }
  test_under <- under
  test_over <- over

  under <- 0
  over <- 0
  for (val in remaining) {
    if (val < treshold) {
      under <- under + 1
    } else {
      over <- over + 1
    }
  }
  remaining_under <- under
  remaining_over <- over
  
  temp_ans <- c(name, train_under, train_over, test_under, test_over, remaining_under, remaining_over)
  ans <- rbind(ans, temp_ans)

  } 
  ans <- as.data.frame(ans)
  colnames(ans) <- c("data", "train_under", "train_over", "test_under", "test_over", "dam_under", "dam_over")
  return(as.data.frame(ans))
}

erms_raw_stats <- split_erms_results(all_erms_df, tresholds, floor(0.7*nrow(all_erms_df)), 4200)
## TRANSPOSING TO NUMBER FUCKS IT 

erms_raw_stats$train_under <- as.numeric(as.character(erms_raw_stats$train_under))
erms_raw_stats$train_over <- as.numeric(as.character(erms_raw_stats$train_over))
erms_raw_stats$test_under <- as.numeric(as.character(erms_raw_stats$test_under))
erms_raw_stats$test_over <- as.numeric(as.character(erms_raw_stats$test_over))
erms_raw_stats$dam_under <- as.numeric(as.character(erms_raw_stats$dam_under))
erms_raw_stats$dam_over <- as.numeric(as.character(erms_raw_stats$dam_over))

erms_stats_all <- droplevels(erms_raw_stats) %>% 
  mutate(train_total = train_under + train_over, 
         test_total = test_under + test_over,
         dam_total = dam_under + dam_over,
         train_over_prct = train_over/train_total,
         test_over_prct = test_over/test_total,
         dam_over_pct = dam_over/dam_total,
         detected = ifelse(dam_over_pct >= 0.95, 1, 0))

erms_stats_detected <- erms_stats_all %>% 
  select(data, dam_over_pct, detected)

erms_stats <- left_join(metadata, erms_stats_detected, by = c("meta" = "data"))

erms_stats <- erms_stats %>% 
  mutate(dam_exists = ifelse(damage > 1, 1, 0 ),
         detected_correct = ifelse(detected == dam_exists, 1, 0))

erms_stats_1 <- erms_stats %>% 
  summarize(all_found = sum(detected_correct)/n())

erms_stats_2 <- erms_stats %>% 
  group_by(damage_type, n_comp) %>% 
  summarise(all_found = sum(detected_correct)/n())

erms_stats_3 <- erms_stats %>% 
  group_by(damage_type, noise, n_comp) %>% 
  summarise(all_found = sum(detected_correct)/n())

erms_stats_4 <- erms_stats %>% 
  group_by(damage_type, damage, n_comp) %>% 
  summarise(all_found = sum(detected_correct)/n())


erms_comps_by_damage <- erms_stats %>% 
  group_by(n_comp, damage) %>% 
  summarise(all_found = sum(detected_correct)/n())
erms_comps_by_damage$damage[erms_comps_by_damage$damage == 1] <- "0%"
erms_comps_by_damage$damage[erms_comps_by_damage$damage == 2] <- "0.1%"
erms_comps_by_damage$damage[erms_comps_by_damage$damage == 3] <- "0.5%"
erms_comps_by_damage$damage[erms_comps_by_damage$damage == 4] <- "1%"
erms_comps_by_damage$damage[erms_comps_by_damage$damage == 5] <- "2%"
erms_comps_by_damage$damage[erms_comps_by_damage$damage == 6] <- "5%"


erms_comps_by_type <- erms_stats %>% 
  group_by(n_comp, damage_type) %>% 
  summarise(all_found = sum(detected_correct)/n())
erms_comps_by_type$damage_type[erms_comps_by_type$damage_type == 'linear'] <- 'bojājuma leciens'
erms_comps_by_type$damage_type[erms_comps_by_type$damage_type == 'cubic'] <- 'kubsaknes veida bojājums'

erms_comps_by_noise <- erms_stats %>% 
  group_by(n_comp, noise) %>% 
  summarise(all_found = sum(detected_correct)/n())
erms_comps_by_noise$noise[erms_comps_by_noise$noise == 1] <- 0
erms_comps_by_noise$noise[erms_comps_by_noise$noise == 2] <- 0.1
erms_comps_by_noise$noise[erms_comps_by_noise$noise == 3] <- 0.01
erms_comps_by_noise$noise[erms_comps_by_noise$noise == 4] <- 0.001
erms_comps_by_noise$noise[erms_comps_by_noise$noise == 5] <- 0.0001

library("RColorBrewer")
library("wesanderson")

Sys.setlocale("LC_ALL", "latvian_Latvia.1257")
erms_comps_by_damage %>% 
  ggplot(aes(x = damage, y = all_found*100, fill = n_comp)) +
  geom_bar(stat = "identity", position = 'dodge') +
  scale_fill_manual(values = wes_palette("BottleRocket1")) +
  xlab("Bojājuma lielums kā % no vidējās vērtības") + ylab("% gadījumu, kas noteikti") + 
  ggtitle("Pareizi noteikts maiņas punkts % atkarībā no bojājuma pēc komponenšu skaita")+
  labs(fill = "Komponenšu skaits") +
  theme_bw() +
  geom_text(aes(label = 100*round(all_found,4))) +
  annotate("text", x = 4, y = 100, label = "n = 240")

erms_comps_by_type %>% 
  ggplot(aes(x = damage_type, y = all_found*100, fill = n_comp)) +
  geom_bar(stat = "identity", position = 'dodge') +
  scale_fill_manual(values = wes_palette("BottleRocket1")) +
  xlab("Bojājuma veids") + ylab("% gadījumu, kas noteikti") + 
  ggtitle("Pareizi noteikts maiņas punkts % atkarībā no bojājuma veida")+
  labs(fill = "Komponenšu skaits") +
  theme_bw() +
  geom_text(aes(label = 100*round(all_found,4))) +
  annotate("text", x = 2, y = 100, label = "n = 240")

erms_comps_by_noise %>% 
  ggplot(aes(x = noise, y = all_found*100, fill = n_comp)) +
  geom_bar(stat = "identity", position = 'dodge') +
  scale_fill_manual(values = wes_palette("BottleRocket1")) +
  xlab("Trokšņa amplitūda") + ylab("% gadījumu, kas noteikti") + 
  ggtitle("Pareizi noteikts maiņas punkts % atkarībā no bojājuma trokšņa amplitūdas")+
  labs(fill = "Komponenšu skaits")+
  theme_bw() +
  geom_text(aes(label = 100*round(all_found,4))) +
  annotate("text", x = 2, y = 100, label = "n = 240")

erms_damage_by_comps <- erms_stats %>% 
  group_by(n_comp, damage) %>% 
  summarise(all_found = sum(detected_correct)/n())

erms_damage_by_type <- erms_stats %>% 
  group_by(damage_type, damage) %>% 
  summarise(all_found = sum(detected_correct)/n())

erms_damage_by_noise <- erms_stats %>% 
  group_by(damage, noise) %>% 
  summarise(all_found = sum(detected_correct)/n())

erms_damage_by_comps %>%  ggplot(aes(x = n_comp, y = all_found*100, fill = damage)) +
  geom_bar(stat = "identity", position = 'dodge') +
  scale_fill_manual(values = wes_palette("BottleRocket1")) +
  xlab("Komponenšu skaits") + ylab("% noteikts") + 
  ggtitle("Pareizi noteikts maiņas punkts % atkarībā no komponenšu skaita")+
  labs(fill = "Bojājuma tips")+
  theme_bw()

erms_damage_by_type %>% ggplot(aes(x = damage_type, y = all_found, fill = damage)) +
  geom_bar(stat = "identity", position = 'dodge') +
  scale_fill_brewer(palette="Dark2") +
  xlab("Bojājuma tips") + ylab("% noteikts") + 
  ggtitle("Pareizi noteikts maiņas punkts % atkarībā no bojājuma tipa")+
  labs(fill = "Bojājuma tips")+
  theme_bw()

erms_damage_by_noise %>% ggplot(aes(x = noise, y = all_found, fill = damage)) +
  geom_bar(stat = "identity", position = 'dodge') +
  scale_fill_brewer(palette="Dark2") +
  xlab("Trokšņa tips") + ylab("% noteikts") + 
  ggtitle("Pareizi noteikts maiņas punkts % atkarībā no trokšņa tipa")+
  labs(fill = "Bojājuma tips")+
  theme_bw()

erms_type_by_comps <- erms_stats %>% 
  group_by(damage_type, n_comp) %>% 
  summarise(all_found = sum(detected_correct)/n())

erms_type_by_damage <- erms_stats %>% 
  group_by(damage_type, damage) %>% 
  summarise(all_found = sum(detected_correct)/n())

erms_type_by_noise <- erms_stats %>% 
  group_by(damage_type, noise) %>% 
  summarise(all_found = sum(detected_correct)/n())

erms_type_by_comps %>%  ggplot(aes(x = n_comp, y = all_found, fill = damage_type)) +
  geom_bar(stat = "identity", position = 'dodge') +
  scale_fill_brewer(palette="Dark2") +
  xlab("Komponenšu skaits") + ylab("% noteikts") + 
  ggtitle("Pareizi noteikts maiņas punkts % pēc komponenšu skaita")+
  labs(fill = "Bojājuma veids")+
  theme_bw()

erms_type_by_damage %>%  ggplot(aes(x = damage, y = all_found, fill = damage_type)) +
  geom_bar(stat = "identity", position = 'dodge') +
  scale_fill_brewer(palette="Dark2") +
  xlab("Bojājuma tips") + ylab("% noteikts") + 
  ggtitle("Pareizi noteikts maiņas punkts % pēc Bojājuma tipa")+
  labs(fill = "Bojājuma veids")+
  theme_bw()

erms_type_by_noise %>%  ggplot(aes(x = noise, y = all_found, fill = damage_type)) +
  geom_bar(stat = "identity", position = 'dodge') +
  scale_fill_brewer(palette="Dark2") +
  xlab("Trokšņa tips") + ylab("% noteikts") + 
  ggtitle("Pareizi noteikts maiņas punkts % pēc Trokšņa tipa")+
  labs(fill = "Bojājuma veids")+
  theme_bw()

erms_noise_by_comps <- erms_stats %>% 
  group_by(noise, n_comp) %>% 
  summarise(all_found = sum(detected_correct)/n())

erms_noise_by_damage <- erms_stats %>% 
  group_by(noise, damage) %>% 
  summarise(all_found = sum(detected_correct)/n())

erms_noise_by_type <- erms_stats %>% 
  group_by(noise, damage_type) %>% 
  summarise(all_found = sum(detected_correct)/n())

erms_noise_by_comps %>% ggplot(aes(x = n_comp, y = all_found*100, fill = noise)) +
  geom_bar(stat = "identity", position = 'dodge') +
  scale_fill_brewer(palette="Dark2") +
  xlab("Komponenšu skaits") + ylab("% noteikts") + 
  ggtitle("Pareizi noteikts maiņas punkts % pēc Trokšņa tipa")+
  labs(fill = "Trokšņa tips") +
  geom_text(aes(label = 100*round(all_found,4)))

erms_noise_by_damage %>% ggplot(aes(x = damage, y = all_found*100, fill = noise)) +
  geom_bar(stat = "identity", position = 'dodge') +
  scale_fill_brewer(palette="Dark2") +
  xlab("Bojājuma tips") + ylab("% noteikts") + 
  ggtitle("Pareizi noteikts maiņas punkts % pēc Trokšņa tipa")+
  labs(fill = "Trokšņa tips") +
  geom_text(aes(label = 100*round(all_found,4)))

erms_noise_by_type %>% ggplot(aes(x = damage_type, y = all_found*100, fill = noise)) +
  geom_bar(stat = "identity", position = 'dodge') +
  scale_fill_brewer(palette="Dark2") +
  xlab("Bojājuma veids") + ylab("% noteikts") + 
  ggtitle("Pareizi noteikts maiņas punkts % pēc Trokšņa tipa")+
  labs(fill = "Trokšņa tips") +
  geom_text(aes(label = 100*round(all_found,4)))

erms_5comp_stats <- erms_stats %>% 
  filter(n_comp == 5)  

erms_5comps_dam1 <- erms_5comp_stats %>% 
  filter(damage == 1) %>% 
  group_by(damage_type, noise) %>% 
  summarise(all_found = sum(detected_correct)/n())
erms_5comps_dam1$noise[erms_5comps_dam1$noise==1] <- 0
erms_5comps_dam1$noise[erms_5comps_dam1$noise==2] <- 10^-4
erms_5comps_dam1$noise[erms_5comps_dam1$noise==3] <- 10^-3
erms_5comps_dam1$noise[erms_5comps_dam1$noise==4] <- 10^-2
erms_5comps_dam1$noise[erms_5comps_dam1$noise==5] <- 10^-1
erms_5comps_dam1$damage_type[erms_5comps_dam1$damage_type=='linear'] <- 'bojājuma leciens'
erms_5comps_dam1$damage_type[erms_5comps_dam1$damage_type=='cubic'] <- 'kubsaknes veida bojājums'


erms_5comps_dam2 <- erms_5comp_stats %>% 
  filter(damage == 2) %>% 
  group_by(damage_type, noise) %>% 
  summarise(all_found = sum(detected_correct)/n())
erms_5comps_dam2$noise[erms_5comps_dam2$noise==1] <- 0
erms_5comps_dam2$noise[erms_5comps_dam2$noise==2] <- 10^-4
erms_5comps_dam2$noise[erms_5comps_dam2$noise==3] <- 10^-3
erms_5comps_dam2$noise[erms_5comps_dam2$noise==4] <- 10^-2
erms_5comps_dam2$noise[erms_5comps_dam2$noise==5] <- 10^-1
erms_5comps_dam2$damage_type[erms_5comps_dam2$damage_type=='linear'] <- 'bojājuma leciens'
erms_5comps_dam2$damage_type[erms_5comps_dam2$damage_type=='cubic'] <- 'kubsaknes veida bojājums'


erms_5comps_dam3<- erms_5comp_stats %>% 
  filter(damage == 3)%>% 
  group_by(damage_type, noise) %>% 
  summarise(all_found = sum(detected_correct)/n())
erms_5comps_dam3$noise[erms_5comps_dam3$noise==1] <- 0
erms_5comps_dam3$noise[erms_5comps_dam3$noise==2] <- 10^-4
erms_5comps_dam3$noise[erms_5comps_dam3$noise==3] <- 10^-3
erms_5comps_dam3$noise[erms_5comps_dam3$noise==4] <- 10^-2
erms_5comps_dam3$noise[erms_5comps_dam3$noise==5] <- 10^-1
erms_5comps_dam3$damage_type[erms_5comps_dam3$damage_type=='linear'] <- 'bojājuma leciens'
erms_5comps_dam3$damage_type[erms_5comps_dam3$damage_type=='cubic'] <- 'kubsaknes veida bojājums'


erms_5comps_dam4<- erms_5comp_stats %>% 
  filter(damage == 4)%>% 
  group_by(damage_type, noise) %>% 
  summarise(all_found = sum(detected_correct)/n())
erms_5comps_dam4$noise[erms_5comps_dam4$noise==1] <- 0
erms_5comps_dam4$noise[erms_5comps_dam4$noise==2] <- 10^-4
erms_5comps_dam4$noise[erms_5comps_dam4$noise==3] <- 10^-3
erms_5comps_dam4$noise[erms_5comps_dam4$noise==4] <- 10^-2
erms_5comps_dam4$noise[erms_5comps_dam4$noise==5] <- 10^-1
erms_5comps_dam4$damage_type[erms_5comps_dam4$damage_type=='linear'] <- 'bojājuma leciens'
erms_5comps_dam4$damage_type[erms_5comps_dam4$damage_type=='cubic'] <- 'kubsaknes veida bojājums'


erms_5comps_dam5<- erms_5comp_stats %>% 
  filter(damage == 5)%>% 
  group_by(damage_type, noise) %>% 
  summarise(all_found = sum(detected_correct)/n())
erms_5comps_dam5$noise[erms_5comps_dam5$noise==1] <- 0
erms_5comps_dam5$noise[erms_5comps_dam5$noise==2] <- 10^-4
erms_5comps_dam5$noise[erms_5comps_dam5$noise==3] <- 10^-3
erms_5comps_dam5$noise[erms_5comps_dam5$noise==4] <- 10^-2
erms_5comps_dam5$noise[erms_5comps_dam5$noise==5] <- 10^-1
erms_5comps_dam5$damage_type[erms_5comps_dam5$damage_type=='linear'] <- 'bojājuma leciens'
erms_5comps_dam5$damage_type[erms_5comps_dam5$damage_type=='cubic'] <- 'kubsaknes veida bojājums'


erms_5comps_dam6<- erms_5comp_stats %>% 
  filter(damage == 6)%>% 
  group_by(damage_type, noise) %>% 
  summarise(all_found = sum(detected_correct)/n())
erms_5comps_dam6$noise[erms_5comps_dam6$noise==1] <- 0
erms_5comps_dam6$noise[erms_5comps_dam6$noise==2] <- 10^-4
erms_5comps_dam6$noise[erms_5comps_dam6$noise==3] <- 10^-3
erms_5comps_dam6$noise[erms_5comps_dam6$noise==4] <- 10^-2
erms_5comps_dam6$noise[erms_5comps_dam6$noise==5] <- 10^-1
erms_5comps_dam6$damage_type[erms_5comps_dam6$damage_type=='linear'] <- 'bojājuma leciens'
erms_5comps_dam6$damage_type[erms_5comps_dam6$damage_type=='cubic'] <- 'kubsaknes veida bojājums'

par(mfrow = c(2, 2))

erms_5comps_dam1 %>% ggplot(aes(x = factor(noise, level = level_order), y = all_found*100, fill = damage_type))+
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = wes_palette("BottleRocket1")) +
  xlab("Trokšņa amplitūda") + ylab("% gadījumu, kas noteikti") + 
  ggtitle("Pareizi noteikts maiņas punkts % pēc trokšņa amplitūdas (Bojājums = 0%) ")+
  labs(fill = "Bojājuma veids") +
  theme_bw()

erms_5comps_dam2 %>% ggplot(aes(x = factor(noise, level = level_order), y = all_found*100, fill = damage_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = wes_palette("BottleRocket1")) +
  xlab("Trokšņa amplitūda") + ylab("% gadījumu, kas noteikti") + 
  ggtitle("Pareizi noteikts maiņas punkts % pēc trokšņa amplitūdas (Bojājums = 0.1%) ")+
  labs(fill = "Bojājuma veids") +
  theme_bw()

erms_5comps_dam3 %>%  ggplot(aes(x = factor(noise, level = level_order), y = all_found*100, fill = damage_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = wes_palette("BottleRocket1")) +
  xlab("Trokšņa amplitūda") + ylab("% gadījumu, kas noteikti") + 
  ggtitle("Pareizi noteikts maiņas punkts % pēc trokšņa amplitūdas (Bojājums = 0.5%) ")+
  labs(fill = "Bojājuma veids") +
  theme_bw()

erms_5comps_dam4 %>%  ggplot(aes(x = factor(noise, level = level_order), y = all_found*100, fill = damage_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = wes_palette("BottleRocket1")) +
  xlab("Trokšņa amplitūda") + ylab("% gadījumu, kas noteikti") + 
  ggtitle("Pareizi noteikts maiņas punkts % pēc trokšņa amplitūdas (Bojājums = 1%) ")+
  labs(fill = "Bojājuma veids") +
  theme_bw()

erms_5comps_dam5 %>%  ggplot(aes(x = factor(noise, level = level_order), y = all_found*100, fill = damage_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = wes_palette("BottleRocket1")) +
  xlab("Trokšņa amplitūda") + ylab("% gadījumu, kas noteikti") + 
  ggtitle("Pareizi noteikts maiņas punkts % pēc trokšņa amplitūdas (Bojājums = 2%) ")+
  labs(fill = "Bojājuma veids") +
  theme_bw()

erms_5comps_dam6 %>%  ggplot(aes(x = factor(noise, level = level_order), y = all_found*100, fill = damage_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = wes_palette("BottleRocket1")) +
  xlab("Trokšņa amplitūda") + ylab("% gadījumu, kas noteikti") + 
  ggtitle("Pareizi noteikts maiņas punkts % pēc trokšņa amplitūdas (Bojājums = 5%) ")+
  labs(fill = "Bojājuma veids") +
  theme_bw()

# ---- sample all data with damage 


Sys.setlocale("LC_ALL", "latvian_Latvia.1257")

dam1noise2lin <- data_list[[13]]
freq_dam1noise2lin <- dam1noise2lin %>% 
  mutate(t = 1:nrow(dam1noise2lin)) 

par(mfrow = c(2, 1))
plot(freq_dam1noise2lin$t, freq_dam1noise2lin[,1], col = "red2",  ylim = c(3.2,5.8), pch = 20,
     ylab = "Frekvence, HZ",xlab = "laiks",
     main = "Frekvenču dati (F1-F3), bez bojājuma")
points(freq_dam1noise2lin$t, freq_dam1noise2lin[,2],col = "indianred", pch = 20)
points(freq_dam1noise2lin$t, freq_dam1noise2lin[,3],col = "darkorange1", pch = 20)
abline(v = 4200, lwd = 2, lty = 'dashed')
legend('top',legend = c("F1", "F2", "F3"), col = c("red2","indianred", "darkorange1"),
        lwd = 5, xpd = TRUE, horiz = TRUE, cex = 1, seg.len = 1, bty = 'n')
#text("t=4200, vidējās vērtības izmaiņas nav")


dam5noise1lin <- data_list[[5]]
freq_dam5noise1lin <- dam5noise1lin %>% 
  mutate(t = 1:nrow(dam1noise2lin)) 

plot(freq_dam5noise1lin$t, freq_dam5noise1lin[,1], col = "red2",  ylim = c(3.2,5.8), pch = 20,
     ylab = "Frekvence, HZ",xlab = "laiks", main = "Frekvenču dati (F1-F3), f1 un f3 2% leciena bojājums pie t=4200")
points(freq_dam5noise1lin$t, freq_dam5noise1lin[,2],col = "indianred", pch = 20)
points(freq_dam5noise1lin$t, freq_dam5noise1lin[,3],col = "darkorange1", pch = 20)
abline(v = 4200, lwd = 2, lty = 'dashed')
legend('top',legend = c("F1", "F2", "F3"), col = c("red2","indianred", "darkorange1"),
       lwd = 5, xpd = TRUE, horiz = TRUE, cex = 1, seg.len = 1, bty = 'n')
#text("t=4200, vidējās vērtības izmaiņa f1 un f3: 2%, bojājuma leciens")

dam2noise2cub <- data_list[[20]]
freq_dam2noise2cub <- dam2noise2cub %>% 
  mutate(t = 1:nrow(dam1noise2lin)) 

plot(freq_dam2noise2cub$t, freq_dam2noise2cub[,1], col = "red2",  ylim = c(3.2,5.8), pch = 20,
     ylab = "Frekvence, HZ",xlab = "laiks", 
     main = "Frekvenču dati (F1-F3), f1 un f3 0.1% kubsaknes bojājums pie t=4200")
points(freq_dam2noise2cub$t, freq_dam2noise2cub[,2],col = "indianred", pch = 20)
points(freq_dam2noise2cub$t, freq_dam2noise2cub[,3],col = "darkorange1", pch = 20)
abline(v = 4200, lwd = 2, lty = 'dashed')
legend('top',legend = c("F1", "F2", "F3"), col = c("red2","indianred", "darkorange1"),
       lwd = 5, xpd = TRUE, horiz = TRUE, cex = 1, seg.len = 1, bty = 'n')

dam5noise2cub <- data_list[[22]]
freq_dam5noise2cub <- dam5noise2cub %>% 
  mutate(t = 1:nrow(dam1noise2lin))

plot(freq_dam5noise2cub$t, freq_dam5noise2cub[,1], col = "red2",  ylim = c(3.2,5.8), pch = 20,
     ylab = "Frekvence, HZ",xlab = "laiks", 
     main = "Frekvenču dati (F1-F3), f1 un f3 1% kubsaknes bojājums pie t=4200")
points(freq_dam5noise2cub$t, freq_dam5noise2cub[,2],col = "indianred", pch = 20)
points(freq_dam5noise2cub$t, freq_dam5noise2cub[,3],col = "darkorange1", pch = 20)
abline(v = 4200, lwd = 2, lty = 'dashed')
legend('top',legend = c("F1", "F2", "F3"), col = c("red2","indianred", "darkorange1"),
       lwd = 5, xpd = TRUE, horiz = TRUE, cex = 1, seg.len = 1, bty = 'n')


