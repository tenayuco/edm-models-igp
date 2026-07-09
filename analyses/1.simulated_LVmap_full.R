

#==========================I.B. RUN LV MAP SPATIAL KERNEL=======================================

###########2 cross validation. now we put it in the LV MAP. And here we can loop like crazy

##now we take that data base and we create the matrix and apply the cross validation across scenarios

source("./analyses/0.set_LBLB_model.R")


type_data= "simulated.data"
simulated_data <-  "DF_DISC_LV_20.csv"
chosen_scenario <- "lblb_model_0"


if(type_data == "simulated.data"){
  out_path= paste0("./outputs/LV_MAP/", type_data,  "/", chosen_scenario, "/")
  DATA_PRED <- read.csv(paste0("data/", type_data,  "/", chosen_scenario, "/",  simulated_data)) #have to provide 
}

#dir.create(paste0(out_path, "detrend_", det_method,  "/", "norm_", no_method, "/"), recursive = T)
dir.create(paste0(out_path), recursive = T)



## here is what the looper needs (it uses the same folder)
v_num_rep = c(1, 10)
v_rpresent = c(TRUE, FALSE)
v_num_seed = seq(1:5) ###A lot of reshuflle 


################NEW ALERNATIVE CODE TO WORK WITH DIFFERENCS######################3


if (dif_cond ==TRUE){
out_folder <- paste0(out_folder, "differences/") 
fig_folder <- paste0(fig_folder, "differences/") 

data_used <- data_used |> 
  dplyr::group_by(block) |> 
  dplyr::mutate(R= c(NA, diff(R)+10), N= c(NA, diff(N)+10), P= c(NA, diff(P)+10))|> 
  tidyr::drop_na()  
}


#------------------------------------------



source("./analyses/1b.looper_LV_map.R")
#protocool for the loop it run the functions of lv map, for each treatment. 
#this has to be changed
out_folder <- out_path
lv_looper_lists(data_used = DATA_PRED, v_num_rep, v_rpresent, v_num_seed, model_used)




#===========================================================================================

#SO AT THIS POINT WE HAVE DONE THE HEAVY ANALYSIS 
#NOW THE MORE GENERAL PLOTTINGS

#========================I.C PLOT LV SIMULATIONS ============================
source("./analyses/1_2.plot_extract_per_treatment.R")
source("./analyses/1_2.plotter_LV.R")
plot_per_treatment(out_folder, true_values = TRUE)
full_df <- extract_par_all_treatment(out_folder, coex_cal = FALSE) ##generates the file

plotter_full_parameters(df_full = full_df, fig_folder = fig_folder)

##========================================================================================


#########################################333
################################################3
