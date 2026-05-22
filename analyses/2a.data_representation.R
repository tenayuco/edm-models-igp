#=====================================================================================
# This code extract the experimental data and plots it
#=====================================================================================

### heres is the data
DATA_IGP <- readr::read_csv("data/dataIGP_2025.csv")

##here we use 2 formats of data
DATA_LONG <-  long_formatter(DATA_IGP)

DATA_MEAN <-  mean_formatter(DATA_LONG)
DATA_PRED <-  pred_formatter(DATA_LONG)

###coexistence propo

DATA_COEX <-  pred_coexistence_formatter(DATA_PRED)

### plot and save data
plotter_data_all(DATA_LONG, remove_aphid = FALSE)
plotter_data_all(DATA_LONG, remove_aphid = TRUE)

plotter_data_mean(DATA_MEAN, remove_aphid = FALSE)
plotter_data_mean(DATA_MEAN, remove_aphid = TRUE)


#plotter coext
plotter_coex(DATA_COEX)

####now we try the full plot
for (enemies in unique(DATA_PRED$enem)){
phaseplotter_ts_all(data_pred = DATA_PRED, data_long = DATA_LONG, enem_treatment = enemies)
}


