
#======================CODE TO ANALYZE THE LV LIST, AND DO THE [PLOTS]=====
#====================================================

used_subfolder <- "LV_MAP/real.data/not_normalized/"
num_enemigos <- 6
kernel_chosen <- "state" #for theplots
out_subfolder <- paste0("./outputs/", used_subfolder)
seeds <- 30 

#this is just a trick to count how many seeds are per treatment but
#seeds <- round(length(list.files(out_subfolder, pattern = ".rds", recursive = T))/num_enemigos)

fig_subfolder <- paste0("./figures/", used_subfolder)

#============================================================================
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

utils::write.csv(FULL_SUM, paste0(out_subfolder, "FULL_SUM_VARIANCE_numseed_", seeds, ".csv"), row.names = FALSE)



#==============================================================================
### here is another extracter,but for the interactions in time

##check this and tun
if (
  file.exists(paste0(out_subfolder,"DF_intTime_","numseed_",seeds,".csv"
  ))) {
  FULL_INT_TIME <-  read.csv(paste0(out_subfolder, "DF_intTime_","numseed_",seeds,".csv"))
  print("file exist with that number of seeds")
}else{
  FULL_INT_TIME <- extract_par_all_time_treatment(out_subfolder = out_subfolder)
   ##generates the file  (that you can download late just to run the full parameters, but chose how many simulaciones!)
  print(head(FULL_INT_TIME))

  utils::write.csv(FULL_INT_TIME,file = paste0(out_subfolder,"DF_intTime_","numseed_",seeds,".csv"
    ), row.names= FALSE
  )
}


FULL_SUM_TIME <- summarizer_interaction_variance(FULL_INT_TIME)

utils::write.csv(FULL_SUM_TIME, paste0(out_subfolder, "FULL_SUM_TIME_VARIANCE_numseed_", seeds, ".csv"), row.names = FALSE)
