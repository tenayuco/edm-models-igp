
lv_looper_lists_general <- function(data_used, v_num_rep, v_rpresent, v_num_seed, enemigo){ 
  for (num_rep in v_num_rep) {
    for (rpresent in v_rpresent) {
  
     num_rep <- num_rep
      rpresent <- rpresent

      out_subfolder <- paste0("enem_", enemigo, "/", "numrep_", num_rep, "/", "R_", rpresent, "/") ## Creates a subfolder name based on time length and noise level chosen

     # out_subfolder <- paste0("numrep_", num_rep, "/", "R_", rpresent, "/") ## Cn
      fig_subfolder <- out_subfolder
  
  
    if (file.exists(paste0(out_folder, out_subfolder))) {
      print("files exists already. Verifiy before or erase before running any simulation")
    } else {
      dir.create(paste0(out_folder, out_subfolder), recursive = TRUE)
      
      #htne i loop over the seeds (the reshuffling)
      for (num_seed in v_num_seed) {
          num_seed <- num_seed
        
        tryCatch(
          {

            

            list_treatment <- lv_map_general(df_used= data_used, rpresent, num_seed, num_rep, kernel_chosen)

            list_treatment$treatment["enem"] <- enemigo
            list_treatment$treatment["num_rep"] <- num_rep
            list_treatment$treatment["rpresent"] <- rpresent
            list_treatment$treatment["num_seed"] <- num_seed

            saveRDS(list_treatment,
              paste0(
                out_folder,
                out_subfolder,
                "listTreatment_",
                "enem_",
                enemigo,
                "_seed_",
                num_seed,
                ".rds"
              )
            )
          },
          error = function(e) {
            cat("ERROR :", conditionMessage(e), "/n")
          }
        )

        # source("./analyses/3a.data_LV_map.R")
      }
    }
  }
}
}