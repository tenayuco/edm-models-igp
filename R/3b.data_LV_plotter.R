



  

##------------now plotting the parameters---------------

###intento loco
process_list <- function(data_list){
df_total <- data.frame()
for (i in 1:num_rep){
  df_rt_temp <- as.data.frame(data_list[[i]])
  df_rt_temp$replicate <- i
  df_rt_temp$time <- seq(1, dim(df_rt_temp)[1])
  df_total <-  rbind(df_total, df_rt_temp)
}
return(df_total)
}

#---here I all as data frames
DF_RT <- process_list(data_list = r_hat_list)
DF_RT_SE <- process_list(data_list = r_se_list)
DF_ALPHA <- process_list(data_list = alpha_hat_list)
DF_ALPHA_SE <- process_list(data_list = alpha_se_list)

##how the parameters change in time
RT_TIME_PLOT <- par_time_plotter(DF_RT, num_col =S)
ALPHA_TIME_PLOT <- par_time_plotter(DF_ALPHA, num_col =S)





ggsave(RT_TIME_PLOT, filename = paste0(fig_folder, fig_subfolder, "rt_time_", "num_rep_", num_rep, "_seed_", num_seed, ".png"),
   height = 4,
    width = 12,
    create.dir = T
  )


ggsave(ALPHA_TIME_PLOT, filename = paste0(fig_folder, fig_subfolder, "alpha_time_", "num_rep_", num_rep, "_seed_", num_seed, ".png"),
   height = 10,
    width = 12,
    create.dir = T
  )


###########3












#now the mean and sd 


LONG_FULL_RT<-long_par_formatter(df_par = DF_RT, df_par_se = DF_RT_SE)
LONG_FULL_ALPHA <- long_par_formatter(df_par=DF_ALPHA, df_par_se = DF_ALPHA_SE)

RT_MEAN_SD_PLOT <- par_mean_sd_plotter(df_par_se_long = LONG_FULL_RT , df_par_eq= NA, trueParameters = FALSE,  num_col=S)
ALPHA_MEAN_SD_PLOT <- par_mean_sd_plotter(df_par_se_long = LONG_FULL_ALPHA , df_par_eq= NA, trueParameters= FALSE, num_col=S)




ggsave(RT_MEAN_SD_PLOT, filename = paste0(fig_folder, fig_subfolder, "rt_mean_", "num_rep_", num_rep, "_seed_", num_seed, ".png"),
   height = 4,
    width = 12,
    create.dir = T
  )

ggsave(ALPHA_MEAN_SD_PLOT, filename = paste0(fig_folder, fig_subfolder, "alpha_mean_", "num_rep_", num_rep, "_seed_", num_seed,  ".png"),
   height = 10,
    width = 12,
    create.dir = T
  )

LONG_FULL_RT$type <- "r"
LONG_FULL_ALPHA$type <- "a"


LONG_FULL <- rbind(LONG_FULL_RT, LONG_FULL_ALPHA)

LONG_FULL$enem <-  chosen_enemies
LONG_FULL$numRep <-  num_rep
LONG_FULL$numSeed <-  num_seed


# Set up folders for saving figures - specifies where output plots will be saved
out_folder <- "./outputs/microcosmos/" ### Directory path for the main figure folder
out_subfolder <- paste0("R_", rpresent,  "/") ## Creates a subfolder name based on time length and noise level chosen

dir.create(paste0(out_folder, out_subfolder), recursive = TRUE)

write.csv(LONG_FULL, file= paste0(out_folder, out_subfolder, "DF_parameters_", chosen_enemies, "_rep", num_rep, "_seed_", num_seed, ".csv"))
  
  
  
