

##analyzed data specify the conditions

type_data <- "real.data"
norm_data <- "not_normalized"

out_folder <- paste0("./outputs/LV_MAP/", type_data, "/", norm_data, "/")
fig_folder <- paste0("./figures/LV_MAP/", type_data, "/", norm_data, "/")

#now specific number of seed and all res

seeds <- 30 

FULL_SUM <- readr::read_csv(paste0(out_folder, "FULL_SUM_VARIANCE_numseed_", seeds,".csv"))



FULL_SUM <- xy_to_np_transformer(FULL_SUM)


plot_par_sum_figure(FULL_SUM, fig_path = fig_folder)

