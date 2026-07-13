#==========================I.B. RUN LV MAP on DATA=======================================

#====================================================================================
#conditions to know wich data to take that can be set here or externally in the make.R


type_data= "real.data"  #important for the map to run the simu mode
DATA_IGP <- readr::read_csv("data/dataIGP_2025.csv")
chosen_scenario <- "lblb_model_0"  ##scenario chosen, to now from where take the data
dif_cond <-  FALSE
norm_data <-  FALSE

#=================================================================

#we remove the treatments that dont make sene
DATA_IGP <- DATA_IGP |> 
  dplyr::filter(!(enem == "ec+sr"))|> 
  dplyr::filter(!(enem == "ec+am"))

#========2.LV MAP======================
#here to build the output path

if(type_data == "real.data"){
  out_folder= paste0("./outputs/LV_MAP/", type_data, "/")
}


#==============================DATA PREPARATION=====================================================
#have to define value for norm and dif_cond

DATA_PRED <- df_modifier_lv(raw_data = DATA_IGP)

#select the columns you want
DATA_PRED <-  DATA_PRED |> 
     dplyr::select(block, R, X, Y, week, enem)
##remove the 0 from the data 
DATA_PRED <- zero_remover_raw(DATA_PRED)

ts_plot_normal <- ts_plotter_data(DATA_PRED, plotted_var = c("R", "X", "Y"))



#===========================change to differences=====================

if (dif_cond == TRUE){
  out_folder <- paste0(out_folder, "differences/") 
   DATA_PRED <- df_differencer_lv(DATA_PRED)
  ts_plot_inputR <- ts_plotter_data(DATA_PRED, plotted_var = c("R", "X", "Y"))

 
}else{
  out_folder <- paste0(out_folder, "absolute/") 
}


#see if normalize (min max ta gives 0s that we have to remove)
if (norm_data == TRUE){
  out_folder <- paste0(out_folder, "normalized/") 
  DATA_PRED <-  max_normalization(DATA_PRED)    ts_plot_inputR <- ts_plotter_data(DATA_PRED, plotted_var = c("R", "X", "Y"))
ts_plot_norm <- ts_plotter_data(DATA_PRED, plotted_var = c("R", "X", "Y"))

 
}else{
  out_folder <- paste0(out_folder, "absolute/") 
}


#===========================================================================================
dir.create(paste0(out_folder), recursive = T)


#========================the LOOP=========================

#====================================================================================
#conditions to tun the LV map to take that can be set here or externally in the make.R



###here to see the plotss of your new modified data

######
v_num_rep = c(1)
v_rpresent = c(FALSE, TRUE)
v_num_seed = seq(1:2)
v_enemigos <-  unique(DATA_PRED$enem)
kernel_chosen = "state"

#=================================================================

##here, contrarily to the simulation, we have to do a loop over the enemies

#looop around 
tic()
for (e in v_enemigos){
  DATA_PRED_EN <- DATA_PRED |> 
  dplyr::filter(enem ==e)
print(head(DATA_PRED_EN))
lv_looper_lists_general(data_used = DATA_PRED_EN, v_num_rep, v_rpresent, v_num_seed, enemigo= e)
}
toc()


#==========================================================================


#aparentelyn it is working.. now for the plooting 


##here is the big change 
##old
#lv_looper_lists_microcosmos(raw_data = DATA_IGP, v_enemigos = v_enemigos, v_num_seed = v_num_seed, num_rep = num_rep, rpresent = rpresent, kernel_chosen = kernel)
#source("./analyses/2b.looper_data_lv_map.R")


#===========================================================================================

#SO AT THIS POINT WE HAVE DONE THE HEAVY ANALYSIS 
#NOW THE MORE GENERAL PLOTTINGS

#========================I.C PLOT LV <MICRSO ============================

#fives the list per treamtne 

fig_folder <- paste0("./figures/LV_MAP/", type_data, "/")
  
if (dif_cond == TRUE){
fig_folder <- paste0(fig_folder, "differences/") 
}else{
  fig_folder <- paste0(fig_folder, "absolute/") 
}

source("./analyses/1_2.plot_extract_per_treatment.R")
source("./analyses/1_2.plotter_LV.R")


plot_per_treatment(out_folder, true_values = FALSE)

full_df <- extract_par_all_treatment(out_folder) ##generates the file


plotter_full_parameters_microcosmos(df_full = full_df, fig_folder = fig_folder)

## coexistence valuess. 
#plotter_theta_microcosmos(df_full = full_df, fig_folder = fig_folder)

#plotter_omega_microcosmos(df_full = full_df, fig_folder = fig_folder)

