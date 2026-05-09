
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
write.csv(full_df, file= paste0(out_folder, "FULL_DF_parameters.csv"))

plotter_full_parameters(df_full = full_df, fig_folder = fig_folder)
##

}


##==========================================
##for ake r









  