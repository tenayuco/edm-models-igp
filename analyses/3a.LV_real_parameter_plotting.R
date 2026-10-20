
##analyzed data specify the conditions

type_data <- "real.data"
norm_data <- "not_normalized"

out_folder <- paste0("./outputs/LV_MAP/", type_data, "/", norm_data, "/")
fig_folder <- paste0("./figures/LV_MAP/", type_data, "/", norm_data, "/")

#now specific number of seed and all res

seeds <- 30 

FULL_DF <- readr::read_csv(paste0(out_folder, "FULL_DF_parameters_numseed_", seeds,".csv"))
FULL_SUM <- readr::read_csv(paste0(out_folder, "FULL_SUM_VARIANCE_numseed_", seeds,".csv"))

#here we add the full df related ro biocontrol.. 


#this part is to create a new column out of this data frame where i put the 
#real values of the enmies, for the plots

##PART 1 - THE PARAMETERES

###=========================

#choose which one

#part 1 REAL NAMES PARAMETERS

change_for_real <- TRUE

############


if(change_for_real == TRUE){
  FULL_DF <- change_xy_realValues(df_full = FULL_DF)
  FULL_SUM <- change_xy_realValues(df_full = FULL_SUM)
}




  
#this plot alpha and r for all simlation and enemies
plotter_full_parameters(df_full = FULL_DF, fig_subfolder = fig_subfolder)
#plot_per_treatment(out_subfolder = out_subfolder, true_values = FALSE) #we dont want the true values of the eq
              # Random seeds for data shuffling



#PLOTS PERENEMMY



##this plot the summarize of each enemy 
plot_par_sum_allconditions(df_sum = FULL_SUM, fig_subfolder = fig_subfolder)
plot_par_sum_allconditions(df_sum = FULL_SUM, fig_subfolder = fig_subfolder, plotted_type = c("a"),  scale_chosen = "free_x")
plot_par_sum_allconditions(df_sum = FULL_SUM, fig_subfolder = fig_subfolder, plotted_type = c("r"), scale_chosen = "free_x")


#-------------------------PART 2 NP NAMES PARAMETERS----------------------------



###but


