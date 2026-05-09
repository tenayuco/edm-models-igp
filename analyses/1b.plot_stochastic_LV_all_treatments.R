

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

