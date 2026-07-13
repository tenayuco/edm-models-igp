#==========================I.B. RUN LV MAP SPATIAL KERNEL=======================================

###########2 cross validation. now we put it in the LV MAP. And here we can loop like crazy

##now we take that data base and we create the matrix and apply the cross validation across scenarios

#====================================================================================
#conditions to know wich data to take that can be set here or externally in the make.R


type_data= "simulated.data"  #important for the map to run the simu mode
simulated_data <-  "DF_DISC_LV_20.csv"  ## spurce of data
chosen_scenario <- "lblb_model_0"  ##scenario chosen, to now from where take the data
dif_cond <-  FALSE
norm_data <-  FALSE


#=================================================================


#here is the source of data 

##just to have the real parameters if needed
source("./analyses/0.set_LBLB_model.R")


#here to build the output path
if(type_data == "simulated.data"){
  out_folder= paste0("./outputs/LV_MAP/", type_data,  "/", chosen_scenario, "/")
  DATA_PRED <- read.csv(paste0("data/", type_data,  "/", chosen_scenario, "/",  simulated_data)) #have to provide 
}


#===========================change to differences================================

if (dif_cond == TRUE){
  out_folder <- paste0(out_folder, "differences/") 
    fig_folder <- paste0(fig_folder, "differences/") 

   DATA_PRED <- df_differencer_lv(DATA_PRED)
  ts_plot_inputR <- ts_plotter_data(DATA_PRED, plotted_var = c("R", "X", "Y"))
}else{
  out_folder <- paste0(out_folder, "absolute/") 
}

#===============================================================

#==============normalize

#see if normalize (min max ta gives 0s that we have to remove)
if (norm_data == TRUE){
  out_folder <- paste0(out_folder, "normalized/") 
  DATA_PRED <-  max_normalization(DATA_PRED)    ts_plot_inputR <- ts_plotter_data(DATA_PRED, plotted_var = c("R", "X", "Y"))
ts_plot_norm <- ts_plotter_data(DATA_PRED, plotted_var = c("R", "X", "Y"))

 
}else{
  out_folder <- paste0(out_folder, "absolute/") 
}


#============================

dir.create(paste0(out_folder), recursive = T)

#========================the LOOP=========================

#====================================================================================
#conditions to tun the LV map to take that can be set here or externally in the make.R

## here is what the looper needs (it uses the same folder)
v_num_rep = c(1)  #we keep it this like this (as 20 0 40 time series LV are too short for the replicates )
v_rpresent = c(FALSE, TRUE)
v_num_seed = seq(1:2) ###A lot of reshuflle 
v_enemigos <-  unique(DATA_PRED$enem)
kernel_chosen <- "state"

#=================================================================




source("./analyses/general_LV_looper.R")
#protocool for the loop it run the functions of lv map, for each treatment. 
#this has to be changed


##so here the general looper (works within enemies)
tic()
for (e in v_enemigos){
  DATA_PRED <- DATA_PRED |> 
  dplyr::filter(enem== e)
  print(head(DATA_PRED_EN))
  lv_looper_lists_general(data_used = DATA_PRED, v_num_rep, v_rpresent, v_num_seed, enemigo = e)

}
toc()



#==========================================================================







#===========================================================================================

#SO AT THIS POINT WE HAVE DONE THE HEAVY ANALYSIS 
#NOW THE MORE GENERAL PLOTTINGS

fig_folder <- paste0("./figures/LV_MAP/", type_data,  "/", chosen_scenario, "/")

#========================I.C PLOT LV SIMULATIONS ============================
source("./analyses/1_2.plot_extract_per_treatment.R")
source("./analyses/1_2.plotter_LV.R")
plot_per_treatment(out_folder, true_values = FALSE) #we dont want the true values of the eq


full_df <- extract_par_all_treatment(out_folder, coex_cal = FALSE) ##generates the file

plotter_full_parameters(df_full = full_df, fig_folder = fig_folder)

##========================================================================================


#########################################333
################################################3
