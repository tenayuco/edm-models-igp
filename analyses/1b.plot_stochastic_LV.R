## estimated parameters extractor

##The first thing to do, is download the respective data and due all the internal plots
## then per I'll do a general plot for all the treaments 

##esto es para hacerlo para todas las seeds

for (i in 1:length(list.files(used_path))){
  list_treatment_used <- readRDS(paste0(used_path, list.files(used_path)[[i]]))
}

###primero volvemos en data frames 

#for r
DF_RT_EQ <- as.double(list_treatment_used$r_eq)
DF_RT_EQ <- as.data.frame(DF_RT_EQ)
names(DF_RT_EQ) <- "par_eq"
DF_RT_EQ$varName <- c("R","N","P")

#for alpha
DF_ALPHA_EQ <- as.double(list_treatment_used$alpha_eq)
DF_ALPHA_EQ <- as.data.frame(DF_ALPHA_EQ)
names(DF_ALPHA_EQ) <- "par_eq"
DF_ALPHA_EQ$varName <- c("R.R", "N.R", "P.R", "R.N", "N.N", "P.N", "R.P", "N.P", "P.P")


############3


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
DF_RT <- process_list(data_list = list_treatment_used$r_hat_list)
DF_RT_SE <- process_list(data_list = list_treatment_used$r_se_list)
DF_ALPHA <- process_list(data_list = list_treatment_used$alpha_hat_list)
DF_ALPHA_SE <- process_list(data_list = list_treatment_used$alpha_se_list)


###########I save all data frame in the corresponding 



#### the first plot you wanna make are the time series plot (ok?)

### the plotss

#plot the phase plot and time series

#just for the plot, i put the time steps, as 1, 2, 3.. 




##how the parameters change in time
RT_TIME_PLOT <- par_time_plotter(DF_RT, num_col =S)
ALPHA_TIME_PLOT <- par_time_plotter(DF_ALPHA, num_col =S)


ggsave(RT_TIME_PLOT, filename = paste0(fig_folder, fig_subfolder, "rt_time_",  "reso_",reso, "grow_", grow_function, "_seed_", num_seed,  ".png"),
   height = 4,
    width = 12,
    create.dir = T
  )


ggsave(ALPHA_TIME_PLOT, filename = paste0(fig_folder, fig_subfolder, "alpha_time_", "reso_",reso, "grow_", grow_function, "_seed_", num_seed,  ".png"),
   height = 10,
    width = 12,
    create.dir = T
  )





LONG_FULL_RT<-long_par_formatter(df_par = DF_RT, df_par_se = DF_RT_SE)
LONG_FULL_ALPHA <- long_par_formatter(df_par=DF_ALPHA, df_par_se = DF_ALPHA_SE)

#now the mean and sd 

RT_MEAN_SD_PLOT <- par_mean_sd_plotter(df_par_se_long =  LONG_FULL_RT, df_par_eq= DF_R_EQ, num_col=S, trueParameters = 
TRUE)
ALPHA_MEAN_SD_PLOT <- par_mean_sd_plotter(df_par_se_long =  LONG_FULL_ALPHA , df_par_eq= DF_ALPHA_EQ, num_col=S, trueParameters = TRUE)


ggsave(RT_MEAN_SD_PLOT, filename = paste0(fig_folder, fig_subfolder, "rt_mean_",  "reso_",reso, "grow_", grow_function, "_seed_", num_seed, ".png"),
   height = 4,
    width = 12,
    create.dir = T
  )

ggsave(ALPHA_MEAN_SD_PLOT, filename = paste0(fig_folder, fig_subfolder, "alpha_mean_", "reso_",reso, "grow_", grow_function, "_seed_", num_seed, ".png"),
   height = 10,
    width = 12,
    create.dir = T
  )


LONG_FULL_RT$type <- "r"
LONG_FULL_ALPHA$type <- "a"


LONG_FULL <- rbind(LONG_FULL_RT, LONG_FULL_ALPHA)


LONG_FULL$numRep <- num_rep
LONG_FULL$numSeed <- num_seed
LONG_FULL$rpresent <- rpresent


# Set up folders for saving figures - specifies where output plots will be saved
out_folder <- "./outputs/simulation/demoStoc_lvmap/" ### Directory path for the main figure folder
out_subfolder <- paste0("len_", len_chosen,"/", "noise_", noise_chosen, "/", "numrep_", num_rep, "/", "R_", rpresent, "/") ## Creates a subfolder name based on time length and noise level chosen



dir.create(paste0(out_folder, out_subfolder), recursive = TRUE)

write.csv(LONG_FULL, file= paste0(out_folder, out_subfolder, "DF_parameters_", "numrep", num_rep,  "_seed_", num_seed, ".csv"))







##now check if it makes sense against the TRUE VALUES 
#averages #does ot work YET
ALPHA_EST <- av_comp_plotter_v2(df_par_se_long = LONG_FULL_ALPHA, df_par_eq = DF_ALPHA_EQ)
RT_EST <- av_comp_plotter_v2(df_par_se_long = LONG_FULL_RT, df_par_eq = DF_R_EQ)

ggsave(RT_EST, filename = paste0(fig_folder, fig_subfolder,"rt_acc_", "reso_",reso, "grow_", grow_function, "_seed_", num_seed,  ".png"),
   height = 10,
    width = 12,
    create.dir = T
  )

ggsave(ALPHA_EST, filename = paste0(fig_folder, fig_subfolder, "alpha_acc_",  "reso_",reso, "grow_", grow_function, "_seed_", num_seed,  ".png"),
   height = 10,
    width = 12,
    create.dir = T
  )

# --------------------------------------------------- ----------------------------------------
##now with the re


##########this is probabcly for another code






FULL_DF_PARAMETERS <-  data.frame()


for (i in 1:length(list.files(paste0(out_folder, out_subfolder), recursive = TRUE))){
  df_temp <- readr::read_csv(paste0(out_folder, out_subfolder, list.files(paste0(out_folder,out_subfolder),recursive=TRUE)[[i]]))
  FULL_DF_PARAMETERS <- rbind(FULL_DF_PARAMETERS, df_temp)
}

FULL_DF_PARAMETERS$...1 <- NULL

FULL_DF_PARAMETERS_M <- FULL_DF_PARAMETERS |> 
  dplyr::select(!replicate)|> 
  dplyr::group_by(varName, type, numSeed, rpresent, numRep)|> 
  dplyr::summarise_all(mean)

PLOT_PAR_SIM_RT <-  parameter_seed_sim_plotter(df_full = FULL_DF_PARAMETERS_M, par_type = "r")
PLOT_PAR_SIM_ALPHA <-  parameter_seed_sim_plotter(df_full = FULL_DF_PARAMETERS_M, par_type = "a")

fig_folder <- "./figures/simulation/demoStoc_lvmap/" ### Directory path for the main figure folder
fig_subfolder <- paste0("len_", len_chosen,"/", "noise_", noise_chosen, "/") ## Creates a subfolder name based on time length and noise level chosen


ggsave(PLOT_PAR_SIM_RT, filename = paste0(fig_folder, fig_subfolder, "rt_allrep_allr_allseed_", ".png"),
   height = 10,
    width = 13,
    create.dir = T
  )


ggsave(PLOT_PAR_SIM_ALPHA, filename = paste0(fig_folder, fig_subfolder, "alpha_allrep_allr_allseed_", ".png"),
   height = 10,
    width = 13,
    create.dir = T
  )


