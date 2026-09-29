#=====================================================================================
# This code extract the experimental data and plots it
#=====================================================================================

#LOAD AND FILTER DATA for LV

#this code gets the IGP data and does the modifcations used for LV analys, and for rpesentation, and coexistence 

source("./analyses/2a.data_modification_LV.R")

#IT GIVES YOU DATA IGP AND DATA_PRED


#====================================================
##now for the representation of the data


##here we use 2 formats of data

DATA_PRED_SP_LONG <-  data_pred_forRep(DATA_PRED)
#DATA_MEAN <-  mean_formatter(DATA_PRED_SP_LONG) 


### plot and save data
plotter_data_all(DATA_PRED_SP_LONG, remove_aphid = FALSE)
plotter_data_all(DATA_PRED_SP_LONG, remove_aphid = TRUE)

##now here with normalized data per species

plotter_data_all(DATA_PRED_SP_LONG, remove_aphid = FALSE, norm_data = TRUE)
plotter_data_all(DATA_PRED_SP_LONG, remove_aphid = TRUE, norm_data = TRUE)


#plotter_data_mean(DATA_MEAN, remove_aphid = FALSE)
#plotter_data_mean(DATA_MEAN, remove_aphid = TRUE)


#now we take theherbivore as mean 

DATA_MEAN_APHID <- DATA_PRED_SP_LONG |>
  dplyr::filter(trophic == "R") |> 
  dplyr::ungroup() |> 
  tidyr::complete(enem, week, block,  fill = list(individuals = 0))|> 
  dplyr::group_by(enem, week)|> 
  dplyr::mutate(individuals = mean(individuals))|> 
  dplyr::ungroup()

DATA_SIN_APHID <- DATA_PRED_SP_LONG |>
  dplyr::filter(!(trophic == "R")) 

DATA_LONG_MEAN_APHID <- rbind(DATA_SIN_APHID, DATA_MEAN_APHID) |> 
  tidyr::drop_na()  


plotter_data_aphid_mean(DATA_LONG_MEAN_APHID)
plotter_data_aphid_mean(max_datalong_norm(DATA_LONG_MEAN_APHID), norm_data = TRUE)





####now we try the full plot
#for (enemies in unique(DATA_PRED$enem)){
#phaseplotter_ts_all(data_pred = DATA_PRED, data_long = DATA_LONG, enem_treatment = enemies)
#}

