## estimated parameters extractor

##The first thing to do, is download the respective data and due all the internal plots
## then per I'll do a general plot for all the treaments 

##esto es para hacerlo para todas las seeds



plotter_lv_map_treatment <- function(list_treatment_used, fig_path, num_seed, true_values =TRUE, reso='NA'){
###primero volvemos en data frames 

  
if(true_values ==TRUE){
  DF_RT_EQ <- as.double(list_treatment_used$r_eq)
  DF_ALPHA_EQ <- as.double(list_treatment_used$alpha_eq)}

if(true_values ==FALSE) {  
  DF_RT_EQ <- as.double(c(0, 0, 0))
  DF_ALPHA_EQ <- as.double(matrix(0L, ncol=3, nrow=3))}
  
#for r

DF_RT_EQ <- as.data.frame(DF_RT_EQ)
names(DF_RT_EQ) <- "par_eq"
DF_RT_EQ$varName <- c("R","N","P")

#for alpha
DF_ALPHA_EQ <- as.data.frame(DF_ALPHA_EQ)
names(DF_ALPHA_EQ) <- "par_eq"
DF_ALPHA_EQ$varName <- c("R.R", "N.R", "P.R", "R.N", "N.N", "P.N", "R.P", "N.P", "P.P")


############3


##------------now plotting the parameters---------------

###intento loco
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

#---here I all as data frames
DF_RT <- process_list(data_list = list_treatment_used$r_hat_list)
DF_RT_SE <- process_list(data_list = list_treatment_used$r_se_list)
DF_ALPHA <- process_list(data_list = list_treatment_used$alpha_hat_list)
DF_ALPHA_SE <- process_list(data_list = list_treatment_used$alpha_se_list)


###########I save all data frame in the corresponding 



#### the first plot you wanna make are the time series plot (ok?)

### the plotss

#plot the phase plot and time series

#just for the plot, i put the time steps, as 1, 2, 3.. 


##AQUI VOY, pero vamo lo meto en loop y ya

##how the parameters change in time
S <- length(DF_RT)-2
RT_TIME_PLOT <- par_time_plotter(DF_RT, num_col = S) ##to only include the varia
ALPHA_TIME_PLOT <- par_time_plotter(DF_ALPHA, num_col =S)


ggsave(RT_TIME_PLOT, filename = paste0(fig_path, "rt_time_",  "reso_",reso, "_seed_", num_seed,  ".png"),
   height = 4,
    width = 12,
    create.dir = T
  )


ggsave(ALPHA_TIME_PLOT, filename = paste0(fig_path, "alpha_time_", "reso_",reso, "_seed_", num_seed,  ".png"),
   height = 10,
    width = 12,
    create.dir = T
  )



LONG_FULL_RT<-long_par_formatter(df_par = DF_RT, df_par_se = DF_RT_SE)
LONG_FULL_ALPHA <- long_par_formatter(df_par=DF_ALPHA, df_par_se = DF_ALPHA_SE)

#now the mean and sd 

RT_MEAN_SD_PLOT <- par_mean_sd_plotter(df_par_se_long =  LONG_FULL_RT, df_par_eq= DF_RT_EQ, num_col=max(S, dim(DF_RT_EQ)[1]), trueParameters = 
true_values)
ALPHA_MEAN_SD_PLOT <- par_mean_sd_plotter(df_par_se_long =  LONG_FULL_ALPHA , df_par_eq= DF_ALPHA_EQ, num_col=max(S, dim(DF_RT_EQ)[1]), trueParameters = true_values)


ggsave(RT_MEAN_SD_PLOT, filename = paste0(fig_path, "rt_mean_",  "reso_",reso, "_seed_", num_seed, ".png"),
   height = 4,
    width = 12,
    create.dir = T
  )

ggsave(ALPHA_MEAN_SD_PLOT, filename = paste0(fig_path, "alpha_mean_", "reso_",reso, "_seed_", num_seed, ".png"),
   height = 10,
    width = 12,
    create.dir = T
  )



if(true_values ==TRUE){


##now check if it makes sense against the TRUE VALUES 
#averages #does ot work YET
ALPHA_EST <- av_comp_plotter_v2(df_par_se_long = LONG_FULL_ALPHA, df_par_eq = DF_ALPHA_EQ)
RT_EST <- av_comp_plotter_v2(df_par_se_long = LONG_FULL_RT, df_par_eq = DF_RT_EQ)

ggsave(RT_EST, filename = paste0(fig_path,"rt_acc_", "reso_",reso, "_seed_", num_seed,  ".png"),
   height = 10,
    width = 12,
    create.dir = T
  )

ggsave(ALPHA_EST, filename = paste0(fig_path, "alpha_acc_",  "reso_",reso, "_seed_", num_seed,  ".png"),
   height = 10,
    width = 12,
    create.dir = T
  )
}


# --------------------------------------------------- ----------------------------------------
##now with the re


##########this is probabcly for another code




}



plotter_full_parameters <- function(df_full, fig_subfolder){


FULL_DF_PARAMETERS_M <- df_full|> 
  dplyr::select(!replicate)|>
  dplyr::group_by(varName, type, numSeed, rpresent, numRep, enem)|> 
  dplyr::summarise_all(mean)
  
## with R present

  
enemy <-  unique(FULL_DF_PARAMETERS_M$enem)
  
for(i in c(TRUE, FALSE)){
  for(j in unique(FULL_DF_PARAMETERS_M$numRep)){

FULL_DF <-  FULL_DF_PARAMETERS_M |> 
  dplyr::filter(rpresent == i)
  
## without R

PLOT_PAR_SIM_RT <-  parameter_r_alpha_plotter(df_full = FULL_DF, par_type = "r")
PLOT_PAR_SIM_ALPHA <-  parameter_r_alpha_plotter(df_full = FULL_DF, par_type = "a")


ggsave(PLOT_PAR_SIM_RT, filename = paste0(fig_subfolder, "rt_allseed_",  "_numrep_", j, "_R_" , i , ".png"),
   height = 10,
    width = 13,
    create.dir = T
  )


ggsave(PLOT_PAR_SIM_ALPHA, filename = paste0(fig_subfolder, "alpha_allseed_",  "numrep_", j, "_R_" , i  , ".png"),
   height = 10,
    width = 13,
    create.dir = T
  )

}
}
}



plotter_full_parameters_microcosmos <- function(df_full, fig_folder){  ##WE HAVE TO CORRECTTHIS

#just if you have several treatments 
FULL_DF_PARAMETERS_M <- df_full|> 
  dplyr::select(!replicate)|> 
  dplyr::group_by(varName, type, numSeed, rpresent, numRep, enem)|> 
  dplyr::summarise_all(mean)

  
##now its the full, so i divide it

    
for(i in c(TRUE, FALSE)){
  for(j in unique(FULL_DF_PARAMETERS_M$numRep)){

FULL_DF <-  FULL_DF_PARAMETERS_M |> 
  dplyr::filter(rpresent == i)|> 
  dplyr::filter(numRep == j)
  

PLOT_PAR_RT <-  parameter_seed_plotter(df_full = FULL_DF , par_type = "r")
PLOT_PAR_ALPHA <-  parameter_seed_plotter(df_full = FULL_DF , par_type = "a")

ggsave(PLOT_PAR_RT, filename = paste0(fig_folder, "rt_allrep_allr_allseed_", "numrep_", j, "_R_" , i, ".png"),
   height = 10,
    width = 13,
    create.dir = T
  )


  

ggsave(PLOT_PAR_ALPHA, filename = paste0(fig_folder, "alpha_allrep_allr_allseed_","numrep_", j, "_R_" , i, ".png"),
   height = 10,
    width = 13,
    create.dir = T
  )
  }
}
}

















plotter_theta_microcosmos <- function(df_full, fig_folder){

#just if you have several treatments (replicatess)
FULL_DF_PARAMETERS_M <- df_full|> 
  dplyr::select(!c(replicate, varName, type))|> 
  dplyr::group_by(numSeed, rpresent, numRep, enem)|> 
  dplyr::summarise_all(mean)


PLOT_PAR_THETA <-  parameter_theta_plotter(df_full = FULL_DF_PARAMETERS_M)

  
ggsave(PLOT_PAR_THETA, filename = paste0(fig_folder, "theta_allrep_allr_allseed_", ".png"),
   height = 10,
    width = 13,
    create.dir = T
  )

}





plotter_omega_microcosmos <- function(df_sum, fig_folder){

#just if you have several treatments (replicatess)
FULL_DF_PARAMETERS_M <- df_full|> 
  dplyr::select(!c(replicate, varName, type))|> 
  dplyr::group_by(numSeed, rpresent, numRep, enem)|> 
  dplyr::summarise_all(mean)


PLOT_PAR_THETA <-  parameter_omega_plotter(df_full = FULL_DF_PARAMETERS_M)

  
ggsave(PLOT_PAR_THETA, filename = paste0(fig_folder, "omega_allrep_allr_allseed_", ".png"),
   height = 10,
    width = 13,
    create.dir = T
  )

}



plotter_save_conditions <- function(df_sum, fig_subfolder){


PLOT_GENERAL <- plot_par_allconditions(df_sum)


ggsave(PLOT_GENERAL, filename = paste0(fig_subfolder, "all_parameters.png"),
   height = 10,
    width = 13,
    create.dir = T
  )



}