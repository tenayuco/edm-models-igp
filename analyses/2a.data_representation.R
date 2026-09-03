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

###coexistence propo
DATA_SURV <- survival_remove_zeros(DATA_PRED) ##this chnage x and y to 0 and 1 and change 0 to 1 if 1 is next )=(110100 to 111100)
DATA_SURV <- survival_remove_zeros(DATA_SURV) ##i run it twice now to remove 0 that where followed by a 0, that was convetted to a 1 (so 11100100 to 11101100 to 11111100)

####now we try the full plot
for (enemies in unique(DATA_PRED$enem)){
phaseplotter_ts_all(data_pred = DATA_PRED, data_long = DATA_LONG, enem_treatment = enemies)
}


#---COEXISTENCE REPRESENTATIONS 

DATA_COEX <-  pred_coexistence_adder(DATA_SURV)
DATA_COEX_AV <-  coex_average(DATA_COEX)

#plotter coext
plotter_coex(DATA_COEX_AV)

##plot survival plots (X, and Y)
###HERE DO THE SURVIVAL PLOT per x y (CHECK IF USEFUK)

###now we calculate the area under the curve for these, and to have a single value per enemy 
##we do like a temporal average. 
DATA_AREA <-  area_coexistence(DATA_COEX_AV)

###now we do the first to die, this shoudl be fata

