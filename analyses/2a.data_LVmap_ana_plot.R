
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
  full_df <-  read.csv(paste0(out_subfolder, "FULL_DF_parameters_","numseed_",seeds,".csv"))
  print("file exist with that number of seeds")
}else{
  full_df <- extract_par_all_treatment(out_subfolder = out_subfolder,coex_cal = TRUE
  ) ##generates the file  (that you can download late just to run the full parameters, but chose how many simulaciones!)
  print(head(full_df))

  write.csv(full_df,file = paste0(out_subfolder,"FULL_DF_parameters_","numseed_",seeds,".csv"
    )
  )
}


#this part is to create a new column out of this data frame where i put the 
#real values of the enmies, for the plots


change_for_real <- TRUE
if(change_for_real == TRUE){
  full_df <- change_xy_realValues(df_full = full_df)
}

  
#this plot alpha and r for all simlation and enemies
plotter_full_parameters(df_full = full_df, fig_subfolder = fig_subfolder)
#plot_per_treatment(out_subfolder = out_subfolder, true_values = FALSE) #we dont want the true values of the eq
              # Random seeds for data shuffling




#importantly, the omega reported is the log 10, so we have to do 10**omega to get real omega values

full_df$omega_mean <- 10^full_df$omega_mean
full_df$omega_dw <- 10^full_df$omega_dw
full_df$omega_up <- 10^full_df$omega_up


#IMPORTAT MOD
full_sum <- summarizer_with_variance(df_full = full_df)


##this plot the summarize of each enemy 
plot_par_sum_allconditions(df_sum = full_sum, fig_subfolder = fig_subfolder)
plot_par_sum_allconditions(df_sum = full_sum, fig_subfolder = fig_subfolder, plotted_type = c("a"),  scale_chosen = "free_x")
plot_par_sum_allconditions(df_sum = full_sum, fig_subfolder = fig_subfolder, plotted_type = c("r"), scale_chosen = "free_x")


#==========COEXISTENCE (see if put it somehwehre else)

#for each enemy 
plot_omega_allconditions(df_sum = full_sum)
  


####-----------------------------------------------------------
#here Im gonna call the survival plot, per area, and time to extinction 





#plot_per_treatment(out_subfolder = out_subfolder, true_values = FALSE) #we dont want the true values of the eq
              # Random seeds for data shuffling