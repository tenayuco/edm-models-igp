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


# ============================================================================
# 3. OUTPUT PATH CONFIGURATION
# ============================================================================

if (type_data == "real.data") {
  out_folder <- paste0("./outputs/LV_MAP/", type_data, "/")
  fig_folder <- paste0("./figures/LV_MAP/", type_data, "/")

}

##here we use 2 formats of data
DATA_LONG <-  long_formatter(DATA_IGP)

DATA_MEAN <-  mean_formatter(DATA_LONG)
DATA_PRED <-  pred_formatter(DATA_LONG)



### plot and save data
plotter_data_all(DATA_LONG, remove_aphid = FALSE)
plotter_data_all(DATA_LONG, remove_aphid = TRUE)

plotter_data_mean(DATA_MEAN, remove_aphid = FALSE)
plotter_data_mean(DATA_MEAN, remove_aphid = TRUE)

####now we try the full plot
for (enemies in unique(DATA_PRED$enem)){
phaseplotter_ts_all(data_pred = DATA_PRED, data_long = DATA_LONG, enem_treatment = enemies)
}

