library(ggplot2)
library(tidyr)
library(dplyr)
library(cusumcharter)

all_erms_df <- readxl::read_excel(path = "all_erms_df.xlsx")
tresholds <- readxl::read_excel(path = "tresholds.xlsx")
all_damage_noise_ts <- readxl::read_excel(path = "all_damage_noise_ts.xlsx")

metadata <- as.data.frame(matrix(nrow = length(all_erms_df), ncol = 0))
metadata <- cbind(1:length(all_erms_df),names(all_erms_df))
colnames(metadata) <- c("df_num","meta")
metadata <- as.data.frame(metadata)

metadata <- metadata %>% 
  mutate(damage = substr(meta, 11, 11),
         damage_type = ifelse(grepl("cubic", meta, fixed = T), substr(meta, 13, 17), substr(meta, 13, 18)),
         noise = ifelse(grepl("cubic", meta, fixed = T), substr(meta, 25, 25), substr(meta, 26, 26)),
         n_comp = ifelse(grepl("cubic", meta, fixed = T), substr(meta, 33, 33), substr(meta, 34, 34)))

 # controls <- cusum_control(all_erms_df$`erms _dam_1_cubic_noise_1 _comp 2`, target = tresholds[[1]])
 # cusum_control_plot(controls, xvar = obs) +
 #   geom_vline(xintercept = 4200)

for (erms in 1:ncol(all_erms_df)){
  file_name = paste(erms, "_damage"  ,
                        metadata[erms,]$damage,
                        metadata[erms,]$damage_type,
                        "noise", metadata[erms,]$noise,
                        "n_comps", metadata[erms,]$n_comp, ".jpeg",
                        sep="")
  plot_name <- paste("bojājuma tips ", metadata[erms,]$damage_type, ": ", metadata[erms,]$damage,
                     ", trokšņa tips: ", metadata[erms,]$noise,
                     ", ņemot vērā " , metadata[erms,]$n_comp, " komponentes.",
                     sep = "")
  
  controls <- cusum_control(all_erms_df[[erms]], target = tresholds[[erms]])
  cusum_control_plot(controls, xvar = obs, title_text = plot_name) +
    geom_vline(xintercept = 4200)
  
  ggsave(file_name)
}


sample <- all_erms_df$`erms _dam_3_linear_noise_1 _comp 4`
ts.plot(sample)

controls <- cusum_control(sample, target = tresholds[[39]])
cusum_control_plot(controls, xvar = obs) +
  geom_vline(xintercept = 4200)

ts.plot(controls$cplus, ylim = c(0, 30))
abline(h = controls$target, col = 'blue')
abline(h = controls$ucl)
abline(v = 4200, lty = 'dashed')

thesis_cusum_test_original_f1 <- as.data.frame(cbind(all_damage_noise_ts$`f 1 _dam_ 1 _linear _noise_ 2`,
                                       all_damage_noise_ts$`f 1 _dam_ 5 _linear _noise_ 1`,
                                       all_damage_noise_ts$`f 1 _dam_ 2 _cubic _noise_ 2`,
                                       all_damage_noise_ts$`f 1 _dam_ 4 _cubic _noise_ 2`))

colnames(thesis_cusum_test_original_f1) <- c('f 1 _dam_ 1 _linear _noise_ 2',
                                             'f 1 _dam_ 5 _linear _noise_ 1',
                                             'f 1 _dam_ 2 _cubic _noise_ 2',
                                             'f 1 _dam_ 4 _cubic _noise_ 2')


Sys.setlocale("LC_ALL", "latvian_Latvia.1257")
controls <- cusum_control(thesis_cusum_test_original_f1[[1]], target = mean(thesis_cusum_test_original_f1[[1]]))
cusum_control_plot(controls, xvar = obs) +
  geom_vline(xintercept = 4200)

ts.plot(controls$cplus, main = "CUSUM algoritma rezultāti f1, datiem bez bojājuma un ar troksni 0.0001 pie t= 4200",
        ylab = "kvalitātes pazīme", 
        xlab = "laiks, t", 
        col = ifelse(controls$cplus> controls$target, 'red', 'black'))
abline(h = controls$target, col = 'blue')
abline(h = controls$ucl)
abline(v = 4200, lty = 'dashed')
text("slieksnis", x = 4500, y = 25, col = 'blue')

controls <- cusum_control(thesis_cusum_test_original_f1[[2]], target = mean(thesis_cusum_test_original_f1[[2]]))
cusum_control_plot(controls, xvar = obs) +
  geom_vline(xintercept = 4200)

ts.plot(controls$cplus, main = "CUSUM rezultāti f1, datiem ar 2% bojājuma lēcienu un bez trokšņa pie t= 4200",
        ylab = "kvalitātes pazīme", 
        xlab = "laiks, t", 
        col = ifelse(controls$cplus> controls$target, 'red', 'black'))
abline(h = controls$target, col = 'blue')
abline(h = controls$ucl)
abline(v = 4200, lty = 'dashed')
text("slieksnis", x = 4500, y = 25, col = 'blue')

controls <- cusum_control(thesis_cusum_test_original_f1[[3]], target = mean(thesis_cusum_test_original_f1[[3]]))
cusum_control_plot(controls, xvar = obs) +
  geom_vline(xintercept = 4200)

ts.plot(controls$cplus, main = "CUSUM rezultāti f1, datiem ar 0.1% kubsaknes bojājumu, troksni 0.0001 pie t= 4200",
        ylab = "kvalitātes pazīme", 
        xlab = "laiks, t", 
        col = ifelse(controls$cplus> controls$target, 'red', 'black'))
abline(h = controls$target, col = 'blue')
abline(h = controls$ucl)
abline(v = 4200, lty = 'dashed')
text("slieksnis", x = 4500, y = 25, col = 'blue')


controls <- cusum_control(thesis_cusum_test_original_f1[[4]], target =  mean(thesis_cusum_test_original_f1[[4]]))
cusum_control_plot(controls, xvar = obs) +
  geom_vline(xintercept = 4200)

ts.plot(controls$cplus, main = "CUSUM rezultāti f1, datiem ar 1% kubsaknes bojājumu un troksni 0.0001 pie t= 4200",
        ylab = "kvalitātes pazīme", 
        xlab = "laiks, t", 
        col = ifelse(controls$cplus> controls$target, 'red', 'black'))
abline(h = controls$target, col = 'blue')
abline(h = controls$ucl)
abline(v = 4200, lty = 'dashed')
text("slieksnis", x = 4500, y = 25, col = 'blue')




thesis_cusum_test_erms_5comps <- as.data.frame(cbind(all_erms_df$`erms _dam_1_linear_noise_2 _comp 5`,
                                                     all_erms_df$`erms _dam_5_linear_noise_1 _comp 5`,
                                                     all_erms_df$`erms _dam_2_cubic_noise_2 _comp 5`,
                                                     all_erms_df$`erms _dam_4_cubic_noise_2 _comp 5`))

colnames(thesis_cusum_test_erms_5comps) <- c("erms _dam_1_linear_noise_2 _comp 5",
                                             "erms _dam_5_linear_noise_1 _comp 5",
                                             "erms _dam_2_cubic_noise_2 _comp 5",
                                             "erms _dam_4_cubic_noise_2 _comp 5")

Sys.setlocale("LC_ALL", "latvian_Latvia.1257")
controls <- cusum_control(thesis_cusum_test_erms_5comps[[1]], target = mean(thesis_cusum_test_erms_5comps[[1]]))
cusum_control_plot(controls, xvar = obs) +
  geom_vline(xintercept = 4200)

ts.plot(controls$cplus, main = "CUSUM algoritma rezultāti ERMS 5 komponentes \n datiem bez bojājuma un ar troksni 0.0001 pie t= 4200",
        ylab = "kvalitātes pazīme", 
        xlab = "laiks, t", 
        col = ifelse(controls$cplus> controls$target, 'red', 'black'))
abline(h = controls$target, col = 'blue')
abline(h = controls$ucl)
abline(v = 4200, lty = 'dashed')
text("slieksnis", x = 4500, y = 0.2, col = 'blue')

controls <- cusum_control(thesis_cusum_test_erms_5comps[[2]], target = mean(thesis_cusum_test_erms_5comps[[2]]))
#cusum_control_plot(controls, xvar = obs) +
#  geom_vline(xintercept = 4200)

ts.plot(controls$cplus, main = "CUSUM algoritma rezultāti ERMS 5 komponentes \n datiem ar 2% bojājuma lēcienu un bez trokšņa pie t= 4200",
        ylab = "kvalitātes pazīme", 
        xlab = "laiks, t", 
        col = ifelse(controls$cplus> controls$target, 'red', 'black'))
abline(h = controls$target)
abline(h = controls$ucl, col = 'blue')
abline(v = 4200, lty = 'dashed')
text("slieksnis", x = 4500, y = 0.3, col = 'blue')

controls <- cusum_control(thesis_cusum_test_erms_5comps[[3]], target = mean(thesis_cusum_test_erms_5comps[[3]]))
#cusum_control_plot(controls, xvar = obs) +
#  geom_vline(xintercept = 4200)

ts.plot(controls$cplus, main = "CUSUM algoritma rezultāti ERMS 5 komponentes \n datiem ar 0.1% kubsaknes bojājumu, troksni 0.0001 pie t= 4200",
        ylab = "kvalitātes pazīme", 
        xlab = "laiks, t", 
        col = ifelse(controls$cplus> controls$target, 'red', 'black'))
abline(h = controls$target)
abline(h = controls$ucl, col = 'blue')
abline(v = 4200, lty = 'dashed')
text("slieksnis", x = 4500, y = 0.3, col = 'blue')


controls <- cusum_control(thesis_cusum_test_erms_5comps[[4]], target = mean(thesis_cusum_test_erms_5comps[[4]]))
#cusum_control_plot(controls, xvar = obs) +
#  geom_vline(xintercept = 4200)

ts.plot(controls$cplus, main = "CUSUM algoritma rezultāti ERMS 5 komponentes \n datiem ar 1% kubsaknes bojājumu un troksni 0.0001 pie t= 4200",
        ylab = "kvalitātes pazīme", 
        xlab = "laiks, t", 
        col = ifelse(controls$cplus> controls$target, 'red', 'black'))
abline(h = controls$target)
abline(h = controls$ucl, col = 'blue')
abline(v = 4200, lty = 'dashed')
text("slieksnis", x = 4500, y = 0.3, col = 'blue')