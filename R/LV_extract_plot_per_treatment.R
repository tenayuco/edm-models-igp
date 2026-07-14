
########function to plot per treatment

plot_per_treatment <- function(out_folder, true_values){

##new method..
  # One-liner
all_dirs <- list.dirs(out_folder, recursive = TRUE)[-1]  # -1 removes the first element (root)

vec_treatments <- setdiff(all_dirs, dirname(all_dirs))

for (treatment in vec_treatments){
  for (i in seq(1:length(list.files(treatment,  pattern = ".rds")))){
  num_seed <- i
  print(list.files(treatment,  pattern = ".rds")[i])
    
  list_used <- readRDS(paste0(treatment,"/" , list.files(treatment,  pattern = ".rds")[i]))
  fig_path <- paste0(fig_folder, stringr::str_remove(treatment, out_folder), "/")
  plotter_lv_map_treatment(list_used, fig_path = fig_path, num_seed = num_seed, true_values = true_values)
  }
}
}


#######now a function to stract the general plot of all treatments#####


extract_par_all_treatment <- function(out_folder, coex_cal = TRUE){

full_df<-  data.frame()
  
##new method..
  # One-liner
all_dirs <- list.dirs(out_folder, recursive = TRUE)[-1]  # -1 removes the first element (root)

vec_treatments <- setdiff(all_dirs, dirname(all_dirs))  

for (treatment in vec_treatments){
  for (i in seq(1:length(list.files(treatment)))){
  #print(1:length(list.files(treatment)))
  list_used <- readRDS(paste0(treatment,"/" , list.files(treatment)[i]))

  LONG_FULL <- extracter_data_frame(list_used, coex_cal = coex_cal)
  
  full_df <- rbind(full_df, LONG_FULL)
  }
}

simulaciones <-  length(unique(full_df$numSeed))
print(head(full_df))
write.csv(full_df, file= paste0(out_folder, "FULL_DF_parameters_", "numseed_", simulaciones, ".csv"))

return(full_df)

}






merger_data_frame_treatment <- function(out_folder, coex_cal = TRUE){



  
}
  








  