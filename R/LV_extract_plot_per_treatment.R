


#############################################################################################################
# Function that reads all saved treatment RDS files in a folder and binds them into one data frame
#' @param out_subfolder Character; root folder containing the treatment subfolders
#' @param coex_cal Logical; whether coexistence metrics were computed and should be extracted
#' @return A single data frame combining all treatments
#' @details Walks over all subfolders, reads each .rds file, and calls extracter_data_frame on it
#' @examples extract_par_all_treatment(out_subfolder = "out/", coex_cal = TRUE)
######################################################################################################
extract_par_all_treatment <- function(out_subfolder, coex_cal = TRUE){

full_df<-  data.frame()
  
##new method..
  # One-liner
all_dirs <- list.dirs(out_subfolder, recursive = TRUE)[-1]  # -1 removes the first element (root)

vec_treatments <- setdiff(all_dirs, dirname(all_dirs))  

for (treatment in vec_treatments){
  for (i in seq(1:length(list.files(treatment)))){
  #print(1:length(list.files(treatment)))
  list_used <- readRDS(paste0(treatment,"/" , list.files(treatment)[i]))

  LONG_FULL <- extracter_data_frame(list_used, coex_cal = coex_cal)
  
  full_df <- rbind(full_df, LONG_FULL)
  }
}



return(full_df)

}




############################################################################################################
# Function that binds a list of data frames (one per replicate) into a single long data frame
#' @param data_list A list where each element is a data frame (e.g. r_hat for one replicate)
#' @return A data frame with an added replicate column and a time column
#' @details Adds replicate index (position in list) and time (row number within each data frame)
#' @examples process_list(data_list = my_list_treatment$r_hat_list)
#########################################################################################
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



############################################################################################################
# Function that extracts all parameters and coexistence metrics from one treatment list into a long data frame
#' @param list_treatment_used A list produced by lv_map_general for one treatment
#' @param coex_cal Logical; whether coexistence metrics (omega, eta, theta) were computed
#' @return A long data frame with r, alpha, SEs, and optionally omega/eta/theta, plus treatment metadata
#' @details Uses process_list to flatten each per-replicate list into a single data frame
#' @details Joins omega, eta, and theta by replicate when coex_cal = TRUE
#' @details Adds numRep, numSeed, rpresent, and enem from the treatment metadata
#' @examples extracter_data_frame(list_treatment_used = my_list_treatment, coex_cal = TRUE)
##########################################################################################################
extracter_data_frame  <- function(list_treatment_used, coex_cal =TRUE){

### ok now im gonna run all over the lists, not so much the DF maybe similar.. 


DF_RT <- process_list(data_list = list_treatment_used$r_hat_list)
DF_RT_SE <- process_list(data_list = list_treatment_used$r_se_list)
DF_ALPHA <- process_list(data_list = list_treatment_used$alpha_hat_list)
DF_ALPHA_SE <- process_list(data_list = list_treatment_used$alpha_se_list)
  
  
 ##I add for the omega, eta 1 and eta2

if(coex_cal ==TRUE){

DF_OMEGA <- process_list(data_list = list_treatment_used$log_Omega_mean_list)
DF_OMEGA_CI_DW <-  process_list(data_list = list_treatment_used$log_Omega_cimean_list[[1]])
DF_OMEGA_CI_UP <-  process_list(data_list = list_treatment_used$log_Omega_cimean_list[[2]])

  
  
#do not need any additional form
DF_OMEGA_FULL <-  dplyr::full_join(DF_OMEGA, DF_OMEGA_CI_DW, by=c("replicate", "time"))
DF_OMEGA_FULL <-  dplyr::full_join(DF_OMEGA_FULL, DF_OMEGA_CI_UP, by=c("replicate", "time"))
names(DF_OMEGA_FULL) <- c("omega_mean", "time", "replicate", "omega_dw", "omega_up")



DF_THETA <- process_list(data_list = list_treatment_used$cv_list_sim)

DF_THETA <- DF_THETA |>
  dplyr::select(theta_o, RMSE_o, replicate)

#i can do this cause you inly have one value per replicate 
DF_THETA <- unique(DF_THETA)  
  
  
  
  
  
DF_ETA_1 <- process_list(data_list = list_treatment_used$eta1_mean_list)  
DF_ETA_1_CI_DW <-  process_list(data_list = list_treatment_used$eta1_cimean_list[[1]])
DF_ETA_1_CI_UP <-  process_list(data_list = list_treatment_used$eta1_cimean_list[[2]])

  

DF_ETA_2 <- process_list(data_list = list_treatment_used$eta2_mean_list)  
DF_ETA_2_CI_DW <-  process_list(data_list = list_treatment_used$eta2_cimean_list[[1]])
DF_ETA_2_CI_UP <-  process_list(data_list = list_treatment_used$eta2_cimean_list[[2]])

  
 
#do not need any additional form
DF_ETA_1_FULL <-  dplyr::full_join(DF_ETA_1, DF_ETA_1_CI_DW, by=c("replicate", "time"))
DF_ETA_1_FULL <-  dplyr::full_join(DF_ETA_1_FULL, DF_ETA_1_CI_UP, by=c("replicate", "time"))

names(DF_ETA_1_FULL) <- c("eta1_mean", "time", "replicate", "eta1_dw", "eta1_up")

  
#do not need any additional form
DF_ETA_2_FULL <-  dplyr::full_join(DF_ETA_2, DF_ETA_2_CI_DW, by=c("replicate", "time"))
DF_ETA_2_FULL <-  dplyr::full_join(DF_ETA_2_FULL, DF_ETA_2_CI_UP, by=c("replicate", "time"))
names(DF_ETA_2_FULL) <- c("eta2_mean", "time", "replicate", "eta2_dw", "eta2_up")

  
DF_ETA_FULL <-  dplyr::full_join(DF_ETA_1_FULL, DF_ETA_2_FULL, by=c("replicate", "time"))

  


}


#this takes the TIME average of each parameters
LONG_FULL_RT<-long_par_formatter(df_par = DF_RT, df_par_se = DF_RT_SE)
LONG_FULL_ALPHA <- long_par_formatter(df_par=DF_ALPHA, df_par_se = DF_ALPHA_SE)

  

LONG_FULL_RT$type <- "r"
LONG_FULL_ALPHA$type <- "a"

LONG_FULL <- rbind(LONG_FULL_RT, LONG_FULL_ALPHA)

  #now we merge it with the thetas..
  # 

if(coex_cal ==TRUE){
LONG_FULL <- dplyr::inner_join(LONG_FULL, DF_THETA, by="replicate")
LONG_FULL <- dplyr::inner_join(LONG_FULL, DF_OMEGA_FULL, by="replicate")
LONG_FULL <- dplyr::inner_join(LONG_FULL, DF_ETA_FULL, by="replicate")

}
  
LONG_FULL$numRep <- list_treatment_used$treatment[["num_rep"]]
LONG_FULL$numSeed <- list_treatment_used$treatment[["num_seed"]]
LONG_FULL$rpresent <- list_treatment_used$treatment[["rpresent"]]
LONG_FULL$enem <- list_treatment_used$treatment[["enem"]]

  
return(LONG_FULL)
  

}




##############################################################################################################
# Function that reshapes parameter and SE data frames into a single long format with mean and sd columns
#' @param df_par A data frame of parameter estimates with columns: replicate, time, and parameter columns
#' @param df_par_se A data frame of parameter standard errors with the same structure as df_par
#' @param replicate Character; name of the replicate column (default "replicate")
#' @return A long data frame with columns: replicate, varName, mvalue.mean, mvalue.sd
#' @details Pivots both inputs to long format, averages over time per replicate/varName, then joins
#' @details The join uses suffix .mean for df_par values and .sd for df_par_se values
#' @examples long_par_formatter(df_par = DF_RT, df_par_se = DF_RT_SE)
##########################################################################################################
long_par_formatter <- function(df_par, df_par_se, replicate= "replicate"){
outLong <-  df_par |> 
    tidyr::pivot_longer(cols= !c(replicate, time), names_to = "varName", values_to = "value") |> 
    dplyr::group_by(replicate, varName)|> 
    dplyr::summarise(mvalue = mean(value))
  
  
  outLong_sd <-  df_par_se |> 
    tidyr::pivot_longer(cols= !c(replicate, time), names_to = "varName", values_to = "value") |> 
    dplyr::group_by(replicate, varName)|> 
    dplyr::summarise(mvalue = mean(value))

  outLong_total <- dplyr::full_join(outLong, outLong_sd, by=c("varName", "replicate"), suffix= c(".mean", ".sd"))

  return(outLong_total)

}




#here is a new function to take all the full df (sumarizin oall the seeds) but of each sumulation



