
##analyzed data specify the conditions

type_data <- "real.data"
norm_data <- "not_normalized"

out_folder <- paste0("./outputs/LV_MAP/", type_data, "/", norm_data, "/")
fig_folder <- paste0("./figures/LV_MAP/", type_data, "/", norm_data, "/")

#now specific number of seed and all res

seeds <- 30 

FULL_SUM_TIME <- readr::read_csv(paste0(out_folder, "FULL_SUM_TIME_VARIANCE_numseed_", seeds,".csv"))
FULL_SUM <- readr::read_csv(paste0(out_folder, "FULL_SUM_VARIANCE_numseed_", seeds,".csv"))



FULL_SUM <- FULL_SUM |> 
  dplyr::select(varName, type, rpresent, enem, grand_mean, total_sd)


FULL_SUM_TIME <- FULL_SUM_TIME |> 
  dplyr::select(varName, type, rpresent, enem, grand_mean_time, total_sd_time, time)



#here im gonna do a new plot! 


#now Im gonna merge these data frame, to have both the mean per time, and the real values

FULL_ALL_SUM_TIME <-  dplyr::left_join(FULL_SUM_TIME, FULL_SUM, by= c("varName", "type", "rpresent", "enem"))

#first a general plot that has only the values of parameters

change_for_real <- TRUE

############


if(change_for_real == TRUE){
  FULL_ALL_SUM_TIME <- change_xy_realValues(df_full = FULL_ALL_SUM_TIME)
}




##GREAT FOR SUPPLEMENTARY
plotter_time_parameters(FULL_ALL_SUM_TIME, fig_path = fig_folder)
