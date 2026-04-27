
# Set up folders for saving figures - specifies where output plots will be saved
out_folder <- "./outputs/microcosmos/" ### Directory path for the main figure folder
out_subfolder <- paste0("R_", rpresent,  "/") ## Creates a subfolder name based on time length and noise level chosen


FULL_DF_PARAMETERS <-  data.frame()

for (i in 1:length(list.files(paste0(out_folder, out_subfolder)))){
  df_temp <- readr::read_csv(paste0(out_folder, out_subfolder, list.files(paste0(out_folder, out_subfolder))[[i]]))
  FULL_DF_PARAMETERS <- rbind(FULL_DF_PARAMETERS, df_temp)
}

FULL_DF_PARAMETERS$...1 <- NULL


PLOT_PAR_RT <-  parameter_seed_plotter(df_full = FULL_DF_PARAMETERS, par_type = "r")
PLOT_PAR_ALPHA <-  parameter_seed_plotter(df_full = FULL_DF_PARAMETERS, par_type = "a")

fig_folder <- "./figures/microcosmos/" ### Directory path for the main figure folder


ggsave(PLOT_PAR_RT, filename = paste0(fig_folder, "rt_all_enem_all_seed_", "num_rep_", num_rep, ".png"),
   height = 10,
    width = 12,
    create.dir = T
  )


ggsave(PLOT_PAR_ALPHA, filename = paste0(fig_folder, "alpha_all_enem_all_seed_", "num_rep_", num_rep, ".png"),
   height = 10,
    width = 12,
    create.dir = T
  )