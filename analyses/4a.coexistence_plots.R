
#===============================================================================================

# ============================================================================
# 1. OUTPUT PATH CONFIGURATION
# ============================================================================
# ============================================================================
# 1. OUTPUT PATH CONFIGURATION
# ============================================================================
type_data <- "real.data"
norm_data <- "not_normalized"
page_width <- 8.5


out_folder <- paste0("./outputs/LV_MAP/", type_data, "/", norm_data, "/")
fig_folder <- paste0("./figures/LV_MAP/", type_data, "/", norm_data, "/")

out_subfolder <- paste0(out_folder,"coexistence/")
fig_subfolder <- paste0(fig_folder,"coexistence/")


# ========================
# IMPORT THEORETICAL COEXISTENCE (ETA AND OMEGA FROM THE MODEL)
#=============================

FULL_SUM <-  read.csv(paste0(out_folder, "FULL_SUM_VARIANCE_numseed_30.csv"))


DATA_COEX <- read.csv(paste0(out_folder, "DATA_coexistence.csv"))

#this put it trhough replicate, each coeistnece measuemrem takes the awhole data series
DATA_SURV_BLOCK <- survival_time_per_run(DATA_COEX)

#this will put it the average and normalize to 1 

#6 rows! 
DATA_SURV_ENEM <- DATA_SURV_BLOCK|>
  dplyr::group_by(enem)|>
  dplyr::summarise("mean_surv" = mean(surv_coex)/20, "sd_surv" = sd(surv_coex)/20)  #area under the curve, normalized
##now we merge the both data to have the complete per block and per eem





###now we gonna put together 1. the omega, 2. the area of coexistence, and 3 the survival time.
###modifed

FULL_COEX <- dplyr::left_join(FULL_SUM, DATA_SURV_ENEM, by = "enem")
##ADD CATEG

##############
###here Imm gonna do the inversion from x, y to n,p , where p is always the top predator.
COMPLETE_DF <- xy_to_np_transformer(FULL_COEX)


utils::write.csv(COMPLETE_DF, paste0(out_folder, "data_theory_coex_df.csv"), row.names = FALSE)

#export this data frame of biocntrol, in the output section





#########


#                                     PLOTTING SECTIONSSSSS

#========================================================================================


#MODIFICATIONS FOR PLOTTING 

COMPLETE_DF <- read.csv(paste0(out_folder,"data_theory_coex_df.csv" )
)


##to avoid some problems of the R, im gonna remove the R present true
COMPLETE_DF <- COMPLETE_DF |>
  dplyr::filter(rpresent == FALSE)|>
  dplyr::mutate(enem = reorder(enem, mean_surv))
  

#this is just to have a long data frame
#where the types of coexistnece, theortial and real, are the same cate
#change we are asking now for the minimal eta, to take it also as a measurement, not both etas

COMPLETE_DF <- minEta_calculator(COMPLETE_DF)


##now lets plot the omega vs min eta

plotter_eta_omega(COMPLETE_DF, fig_path = fig_subfolder)



###now to put thme togenehert 


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
    min_eta, 
    total_sd_eta,
    type,
    varName,
    sd_surv

  ) |>
  tidyr::gather(
    key = "coexistence_variable",
    value = "coex_value",
    grand_mean_omega,
    mean_surv, 
    min_eta
  ) |>
  tidyr::gather(
    key = "coexistence_sd",
    value = "sd_value",
    total_sd_omega,
    sd_surv, 
    total_sd_eta
  )


##now i remove the non correpsoning sd

COMPLETE_DF_LONG <- COMPLETE_DF_LONG |>
  dplyr::mutate(
    sd_value = ifelse(
      coexistence_variable == "grand_mean_omega" & coexistence_sd %in% c("sd_surv", "total_sd_eta"),
      NA,
      sd_value
    )
  ) |>
  dplyr::mutate(
    sd_value = ifelse(
      coexistence_variable == "mean_surv" & coexistence_sd %in% c("total_sd_omega", "total_sd_eta"),
      NA,
      sd_value
    )
  ) |>
  dplyr::mutate(
    sd_value = ifelse(
      coexistence_variable == "min_eta" & coexistence_sd %in% c("total_sd_omega", "sd_surv"),
      NA,
      sd_value
    )
  )|>
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



###  now for each enemy the plot of coexistence (the two method)

##call the function of plot

plot_omega_surv(COMPLETE_DF_LONG, fig_path= fig_subfolder)


###on the fitting of the model



plotter_rmse_theta(completedf =  COMPLETE_DF, fig_path = fig_folder)


#now we have to couple it with biocontrol. 

###NOW im gonna do the omega surv complete (with etas) all measurements 
## and in the 


plot_omega_eta_surv(COMPLETE_DF_LONG, fig_path= fig_subfolder)

    

