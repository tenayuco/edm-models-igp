
#===============================================================================================


#                                     PLOTTING SECTIONSSSSS

#========================================================================================


# ============================================================================
# 1. OUTPUT PATH CONFIGURATION
# ============================================================================
type_data <- "real.data"
norm_data <- "not_normalized"

out_folder <- paste0("./outputs/LV_MAP/", type_data, "/", norm_data, "/")
fig_folder <- paste0("./figures/LV_MAP/", type_data, "/", norm_data, "/")

out_subfolder <- paste0(out_folder,"coexistence/")
fig_subfolder <- paste0(fig_folder,"coexistence/")



#MODIFICATIONS FOR PLOTTING 


COMPLETE_DF <- read.csv(paste0(out_subfolder,"complete_coex_df.csv" )
)


##to avoid some problems of the R, im gonna remove the R present true
COMPLETE_DF <- COMPLETE_DF |>
  dplyr::filter(rpresent == FALSE)|>
  dplyr::mutate(enem = reorder(enem, mean_surv))
  

#this is just to have a long data frame
#where the types of coexistnece, theortial and real, are the same cate

COMPLETE_DF_LONG <- COMPLETE_DF |>
  dplyr::select(
    enem,
    rmse_o_mean, 
    theta_o_mean,
    grand_mean,
    total_sd,
    grand_mean_omega,
    total_sd_omega,
    mean_surv,
    type,
    varName,
    sd_surv

  ) |>
  tidyr::gather(
    key = "coexistence_variable",
    value = "coex_value",
    grand_mean_omega,
    mean_surv
  ) |>
  tidyr::gather(
    key = "coexistence_sd",
    value = "sd_value",
    total_sd_omega,
    sd_surv
  )


##now i remove the non correpsoning sd

COMPLETE_DF_LONG <- COMPLETE_DF_LONG |>
  dplyr::mutate(
    sd_value = ifelse(
      coexistence_variable == "grand_mean_omega" & coexistence_sd == "sd_surv",
      NA,
      sd_value
    )
  ) |>
  dplyr::mutate(
    sd_value = ifelse(
      coexistence_variable == "mean_surv" & coexistence_sd == "total_sd_omega",
      NA,
      sd_value
    )
  ) |>
  tidyr::drop_na()


###here Im gonna send different plots (only intra, only growth rate, only ointer)
# and then one  with all of them

## we can do loop to plot all intra, inter and growth rates

intra <- c("N.N", "P.P")
inter <- c("N.P", "P.N")
growthRate <- c("N", "P")

li_int <- list("intra"= intra, "inter"=inter, "growthRate"=growthRate)


#this is to see if there is some correlation 
#between each intra, inter, gr and coexistence (whcih there is not)

for (chosenInt in li_int){
  for(chosen_coex in c("grand_mean_omega", "mean_surv")){
## first intra
plotter_interaction_coexistence(
  COMPLETE_DF_LONG,
  chosen_coex_var = chosen_coex,
  fig_path = fig_subfolder,
  chosen_int = chosenInt, 
  num_columns = 1
)

  }
}

for(chosen_coex in c("grand_mean_omega", "mean_surv")){
##ALL
plotter_interaction_coexistence(
  COMPLETE_DF_LONG,
  chosen_coex_var = chosen_coex,
  fig_path = fig_subfolder,
  chosen_int = c(intra, inter, growthRate), 
  num_columns = 3
)
}



##now lets plot the omega vs min eta

plotter_eta_omega(COMPLETE_DF, fig_path = fig_subfolder)

###  now for each enemy the plot of coexistence (the two method)

##call the function of plot

plot_omega_surv(COMPLETE_DF_LONG, fig_path= fig_subfolder)


###on the fitting of the model



plotter_rmse_theta(completedf =  COMPLETE_DF, fig_path = fig_folder)


#now we have to couple it with biocontrol. 




    