#=====================================================================================
# This code extract the experimental data and plots it
#=====================================================================================

### heres is the data
DATA_IGP <- readr::read_csv("data/dataIGP_2025.csv")

##here we use 2 formats of data
DATA_LONG <-  long_formatter(DATA_IGP)

DATA_MEAN <-  mean_formatter(DATA_LONG)
DATA_PRED <-  pred_formatter(DATA_LONG)


### plot and save data
plotter_data_all(DATA_LONG, remove_aphid = FALSE)
plotter_data_all(DATA_LONG, remove_aphid = TRUE)

plotter_data_mean(DATA_MEAN, remove_aphid = FALSE)
plotter_data_mean(DATA_MEAN, remove_aphid = TRUE)


for (enemies in unique(DATA_PRED$enem)){

phaseplotter_all(data_pred = DATA_PRED, trophic_1 = "pred1", trophic_2 = "pred2", enem_treatment = enemies)
phaseplotter_all_block(data_pred = DATA_PRED, trophic_1 = "pred1", trophic_2 = "pred2", enem_treatment = enemies)

}