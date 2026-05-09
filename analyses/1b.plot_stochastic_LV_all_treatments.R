### ok now im gonna run all over the lists, not so much the DF maybe similar.. 
process_list <- function(data_list){
df_total <- data.frame()
for (i in 1:length(data_list)){
  df_rt_temp <- as.data.frame(data_list[[i]])
  df_rt_temp$replicate <- i
  df_rt_temp$time <- seq(1, dim(df_rt_temp)[1])
  df_total <-  rbind(df_total, df_rt_temp)
}
return(df_total)
}



FULL_DF_PARAMETERS <-  data.frame()


for (i in 1:length(list.files(paste0(out_folder), recursive = TRUE, pattern = ".rds$"))){
  list_treatment_used <- readRDS(paste0(out_folder, list.files(paste0(out_folder), recursive = TRUE, pattern = ".rds$")[[i]]))

###intento loco

#---here I all as data frames
DF_RT <- process_list(data_list = list_treatment_used$r_hat_list)
DF_RT_SE <- process_list(data_list = list_treatment_used$r_se_list)
DF_ALPHA <- process_list(data_list = list_treatment_used$alpha_hat_list)
DF_ALPHA_SE <- process_list(data_list = list_treatment_used$alpha_se_list)

LONG_FULL_RT<-long_par_formatter(df_par = DF_RT, df_par_se = DF_RT_SE)
LONG_FULL_ALPHA <- long_par_formatter(df_par=DF_ALPHA, df_par_se = DF_ALPHA_SE)

  

LONG_FULL_RT$type <- "r"
LONG_FULL_ALPHA$type <- "a"

LONG_FULL <- rbind(LONG_FULL_RT, LONG_FULL_ALPHA)

LONG_FULL$numRep <- num_rep
LONG_FULL$numSeed <- num_seed
LONG_FULL$rpresent <- rpresent

FULL_DF_PARAMETERS <- rbind(FULL_DF_PARAMETERS, LONG_FULL)

}










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

