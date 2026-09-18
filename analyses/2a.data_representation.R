#=====================================================================================
# This code extract the experimental data and plots it
#=====================================================================================

# ============================================================================
# 2. LOAD AND FILTER DATA
# ============================================================================

# Load the raw IGP dataset
DATA_IGP <- readr::read_csv("data/dataIGP_2025.csv")

# Remove treatments that don't make sense for the analysis
# (ec+sr and ec+am are excluded)
DATA_IGP <- DATA_IGP |> 
  dplyr::filter(!(enem == "ec+sr")) |> 
  dplyr::filter(!(enem == "ec+am"))


## this put in a format for LV
DATA_PRED <- df_modifier_lv(raw_data = DATA_IGP)

# Select only the columns needed for LV analysis
DATA_PRED <- DATA_PRED |> 
  dplyr::select(block, R, X, Y, week, enem)

#====================ESEENTIAL STEP============


#new herbivore
DATA_PRED <- herbivore_modification(DATA_PRED)


# Remove rows with zeros (which can cause issues in LV models)
DATA_PRED <- zero_remover_raw(DATA_PRED)

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
  dplyr::group_by(enem, week)|> 
  dplyr::mutate(individuals = mean(individuals))|> 
  dplyr::ungroup()

DATA_SIN_APHID <- DATA_PRED_SP_LONG |>
  dplyr::filter(!(trophic == "R")) 

DATA_LONG_MEAN_APHID <- rbind(DATA_SIN_APHID, DATA_MEAN_APHID)
  


plotter_data_aphid_mean(DATA_LONG_MEAN_APHID)
plotter_data_aphid_mean(max_datalong_norm(DATA_LONG_MEAN_APHID), norm_data = TRUE)





####now we try the full plot
#for (enemies in unique(DATA_PRED$enem)){
#phaseplotter_ts_all(data_pred = DATA_PRED, data_long = DATA_LONG, enem_treatment = enemies)
#}

