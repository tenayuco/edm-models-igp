
########function to plot per treatment

plot_per_treatment <- function(out_folder){

for (treatment in list.dirs(list.dirs(out_folder, recursive=FALSE),recursive=FALSE)){
  for (i in seq(1:length(list.files(treatment)))){
  #print(1:length(list.files(treatment)))
  num_seed <- i
  print(num_seed)
  list_used <- readRDS(paste0(treatment,"/" , list.files(treatment)[i]))
  fig_path <- paste0(fig_folder, stringr::str_remove(treatment, out_folder), "/")
  plotter_lv_map_treatment(list_used, fig_path = fig_path, num_seed = num_seed)
}
}
}


#######now a function to stract the general plot of all treatments#####


extract_par_all_treatment <- function(out_folder){

full_df<-  data.frame()

for (treatment in list.dirs(list.dirs(out_folder, recursive=FALSE),recursive=FALSE)){
  for (i in seq(1:length(list.files(treatment)))){
  #print(1:length(list.files(treatment)))
  list_used <- readRDS(paste0(treatment,"/" , list.files(treatment)[i]))
  LONG_FULL <- extracter_data_frame(list_used)
  
  full_df <- rbind(full_df, LONG_FULL)
  }
}

print(head(full_df))
return(full_df)

}


##==========================================
##for ake r



plotter_full_parameters <- function(df_full, fig_folder){


FULL_DF_PARAMETERS_M <- df_full|> 
  dplyr::select(!replicate)|> 
  dplyr::group_by(varName, type, numSeed, rpresent, numRep)|> 
  dplyr::summarise_all(mean)

PLOT_PAR_SIM_RT <-  parameter_seed_sim_plotter(df_full = FULL_DF_PARAMETERS_M, par_type = "r")
PLOT_PAR_SIM_ALPHA <-  parameter_seed_sim_plotter(df_full = FULL_DF_PARAMETERS_M, par_type = "a")


ggsave(PLOT_PAR_SIM_RT, filename = paste0(fig_folder, "rt_allrep_allr_allseed_", ".png"),
   height = 10,
    width = 13,
    create.dir = T
  )


ggsave(PLOT_PAR_SIM_ALPHA, filename = paste0(fig_folder, "alpha_allrep_allr_allseed_", ".png"),
   height = 10,
    width = 13,
    create.dir = T
  )

}








  