
###I HAVE NOW TO SAVE IT IN A LIST with
## NUM_REP OR IN A FOLDERT

looper_lv_map <- function(data_used, v_num_rep, v_rpresent, v_num_seed){

  for (num_rep in v_num_rep) {
    for (rpresent in v_rpresent) {
        num_rep <- num_rep
        rpresent <- rpresent
        out_subfolder <- paste0("numrep_", num_rep, "/", "R_", rpresent, "/") ## Cn
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