
#======================CODE TO ANALYZE THE LV LIST, AND DO THE [PLOTS]=====
#====================================================

used_subfolder <- "LV_MAP/real.data/not_normalized/"
num_enemigos <- 6
kernel_chosen <- "state" #for theplots

#this is just a trick to count how many seeds are per treatment but
out_subfolder <- paste0("./outputs/", used_subfolder)
seeds <- length(list.files(out_subfolder, pattern = ".rds", recursive = T))/num_enemigos

fig_subfolder <- paste0("./figures/", used_subfolder)

##check this and tun
if (
  file.exists(paste0(out_subfolder,"FULL_DF_parameters_","numseed_",seeds,".csv"
  ))) {
  FULL_DF <-  read.csv(paste0(out_subfolder, "FULL_DF_parameters_","numseed_",seeds,".csv"))
  print("file exist with that number of seeds")
}else{
  FULL_DF <- extract_par_all_treatment(out_subfolder = out_subfolder,coex_cal = TRUE
  ) ##generates the file  (that you can download late just to run the full parameters, but chose how many simulaciones!)
  print(head(FULL_DF))

  write.csv(FULL_DF,file = paste0(out_subfolder,"FULL_DF_parameters_","numseed_",seeds,".csv"
    )
  )
}


#importantly, the omega reported is the log 10, so we have to do 10**omega to get real omega values

FULL_DF$omega_mean <- 10^FULL_DF$omega_mean
FULL_DF$omega_dw <- 10^FULL_DF$omega_dw
FULL_DF$omega_up <- 10^FULL_DF$omega_up


#IMPORTAT MOD
FULL_SUM <- summarizer_with_variance(df_full = FULL_DF)


##save as a full and summarized 

utils::write.csv(FULL_SUM, paste0(out_subfolder, "FULL_SUM_VARIANCE.csv"))




#this part is to create a new column out of this data frame where i put the 
#real values of the enmies, for the plots


change_for_real <- TRUE
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
