

########function to plot per treatment

plot_per_treatment_microcosmos <- function(out_folder){

for (treatment in list.dirs(list.dirs(out_folder, recursive=FALSE),recursive=FALSE)){
  for (i in seq(1:length(list.files(treatment, pattern = ".rds")))){
  #print(1:length(list.files(treatment)))
  num_seed <- i
  print(num_seed)
  print(list.files(treatment, pattern = ".rds")[i])
  list_used <- readRDS(paste0(treatment,"/" , list.files(treatment, pattern = ".rds")[i]))
  
  fig_path <- paste0(fig_folder, stringr::str_remove(treatment, out_folder), "/")
  plotter_lv_map_treatment(list_used, fig_path = fig_path, num_seed = num_seed, true_values = FALSE)
  }
}
}



extract_par_all_treatment_microcosmos <- function(out_folder){

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
write.csv(full_df, file= paste0(out_folder, "FULL_DF_parameters.csv"))

plotter_full_parameters_microcosmos(df_full = full_df, fig_folder = fig_folder)
##

}






plotter_full_parameters_microcosmos <- function(df_full, fig_folder){

#just if you have several treatments 
FULL_DF_PARAMETERS_M <- df_full|> 
  dplyr::select(!replicate)|> 
  dplyr::group_by(varName, type, numSeed, rpresent, numRep, enem)|> 
  dplyr::summarise_all(mean)


PLOT_PAR_RT <-  parameter_seed_plotter(df_full = FULL_DF_PARAMETERS_M, par_type = "r")
PLOT_PAR_ALPHA <-  parameter_seed_plotter(df_full = FULL_DF_PARAMETERS_M, par_type = "a")

ggsave(PLOT_PAR_RT, filename = paste0(fig_folder, "rt_allrep_allr_allseed_", ".png"),
   height = 10,
    width = 13,
    create.dir = T
  )


ggsave(PLOT_PAR_ALPHA, filename = paste0(fig_folder, "alpha_allrep_allr_allseed_", ".png"),
   height = 10,
    width = 13,
    create.dir = T
  )

}



