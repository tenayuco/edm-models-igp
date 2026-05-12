

########function to plot per treatment





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




