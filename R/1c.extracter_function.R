extracter_data_frame  <- function(list_treatment_used){

### ok now im gonna run all over the lists, not so much the DF maybe similar.. 
process_list <- function(data_list){
df_total <- data.frame()
for (i in 1:length(data_list)){
  df_rt_temp <- as.data.frame(data_list[[i]])
  df_rt_temp$replicate <- i
  df_rt_temp$time <- seq(1, dim(df_rt_temp)[1])
  df_total <-  rbind(df_total, df_rt_temp)
}
return(df_total)
}


DF_RT <- process_list(data_list = list_treatment_used$r_hat_list)
DF_RT_SE <- process_list(data_list = list_treatment_used$r_se_list)
DF_ALPHA <- process_list(data_list = list_treatment_used$alpha_hat_list)
DF_ALPHA_SE <- process_list(data_list = list_treatment_used$alpha_se_list)

LONG_FULL_RT<-long_par_formatter(df_par = DF_RT, df_par_se = DF_RT_SE)
LONG_FULL_ALPHA <- long_par_formatter(df_par=DF_ALPHA, df_par_se = DF_ALPHA_SE)

  

LONG_FULL_RT$type <- "r"
LONG_FULL_ALPHA$type <- "a"

LONG_FULL <- rbind(LONG_FULL_RT, LONG_FULL_ALPHA)
  
  
LONG_FULL$numRep <- list_treatment_used$treatment[["num_rep"]]
LONG_FULL$numSeed <- list_treatment_used$treatment[["num_seed"]]
LONG_FULL$rpresent <- list_treatment_used$treatment[["rpresent"]]

LONG_FULL$enem <- list_treatment_used$treatment[["enem"]]

return(LONG_FULL)
  

}

