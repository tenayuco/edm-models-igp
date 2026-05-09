
###I HAVE NOW TO SAVE IT IN A LIST with
## NUM_REP OR IN A FOLDERT

## this is the looper
## v_num_rep, v_rpresent and v_num seed are defined in the make R
## i decided not to put in a function, but more like a protocol
## in generates all the data bases per treatment

lv_looper_lists <- function(data_used, v_num_rep, v_rpresent, v_num_seed){


  for (num_rep in v_num_rep) {
    for (rpresent in v_rpresent) {
        num_rep <- num_rep
        rpresent <- rpresent
        out_subfolder <- paste0("numrep_", num_rep, "/", "R_", rpresent, "/") ## Cn
        fig_subfolder <- out_subfolder
        if (file.exists(paste0(out_folder, out_subfolder))) {print(paste0(out_folder, out_subfolder, 
    " exists already. Verifiy before or erase before running any simulation"
  ))
} else {
      dir.create(paste0(out_folder, out_subfolder), recursive = TRUE)
      for (num_seed in v_num_seed) {
        num_seed <- num_seed

        tryCatch(
          {
            print(paste0("rep", num_rep,  "_R", rpresent, "_seed", num_seed))

            list_treatment <- lv_map_per_treatment(data_used)
            #source("./analyses/1a.simulation_LV_map.R")
            saveRDS(list_treatment, paste0(out_folder, out_subfolder, "listTreatment_", 
            "noise_", noise_chosen, "_seed_", num_seed, ".rds"))
            
          },
          error = function(e) {
            cat("ERROR :", conditionMessage(e), "/n")
          }
        )
        }
        }
    }
}

}






lv_looper_plot_v2 <- function(v_num_rep, v_rpresent, v_num_seed){

  for (num_rep in v_num_rep) {
    for (rpresent in v_rpresent) {
        num_rep <- num_rep
        rpresent <- rpresent
        out_subfolder <- paste0("numrep_", num_rep, "/", "R_", rpresent, "/") ## Cn
        out_path <- paste0(out_folder, out_subfolder)
        fig_path <-  paste(fig_folder, out_subfolder)
        if (file.exists(fig_path)) {print(paste0(fig_path, 
    " exists already. Verifiy before or erase before running any simulation"
  ))
} else {
          
       tryCatch(
          {    
          
      for (i in 1:length(list.files(out_path))){
        num_seed <- i ##this is only to plot the prider (same order as the seeds)
        list_used <- readRDS(paste0(out_path, list.files(out_path)[[i]]))
        plotter_lv_map_treatment(list_used, fig_path = fig_path, num_seed = num_seed)}
      }, 
      error = function(e) {
            cat("ERROR :", conditionMessage(e), "/n")
          }
        )
            
          }
    }
    }
}
