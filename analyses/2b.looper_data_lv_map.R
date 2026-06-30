
#document the function
## but basically this check each natural enemies and run the loop wtih catch errors



lv_looper_lists_microcosmos <- function(raw_data,v_enemigos,v_num_seed, num_rep,rpresent, kernel_chosen, surrogater=FALSE){
  for (enemigo in v_enemigos) {
    out_subfolder <- paste0("enem_", enemigo, "/", "R_", rpresent, "/") ## Creates a subfolder name based on time length and noise level chosen
    #fig_subfolder <- out_subfolder ## Creates a subfolder name based on time length and noise level chosen

    if (file.exists(paste0(out_folder, out_subfolder))) {
      print("files exists already. Verifiy before or erase before running any simulation")
    } else {
      dir.create(paste0(out_folder, out_subfolder), recursive = TRUE)
      for (num_seed in v_num_seed) {
        num_rep <- num_rep

        #function modifie and extract per ennemies
        tryCatch(
          {
            data_used <- df_modifier_lv(raw_data = raw_data,chosen_enemies = enemigo)

            if(surrogater == TRUE){
            data_used <- surrogater_df_twin(data_used = data_used)

            
            }

            list_treatment <- lv_map_microcosmos(df_used = data_used, rpresent = rpresent,num_rep = num_rep,num_seed = num_seed, kernel_chosen= kernel_chosen)

            list_treatment$treatment["enem"] <- enemigo
            list_treatment$treatment["num_rep"] <- num_rep
            list_treatment$treatment["rpresent"] <- rpresent
            list_treatment$treatment["num_seed"] <- num_seed

            path_data <- paste0(
                out_folder,
                out_subfolder,
                "listTreatment_",
                "enem_",
                enemigo,
                "_seed_",
                num_seed,
                ".rds"
              )




            saveRDS(list_treatment,
              path_data
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