#=======================HEADER - ALWAYS RUN THIS=========================================

rm(list = ls()) # clear memory
graphics.off() # clear graphics

library(ggplot2)
library(patchwork)

# Look for every package used in the project
# Add them to DESCRIPTION under Imports
rdeps::add_deps()

# Install/update packages listed in DESCRIPTION
devtools::install_deps(upgrade = "never")

###########RUN this alwys
# Load packages under Depends and in R
devtools::load_all()

#===================================================================================

#########################################333
#=simulation
#either enter source("./analyses/2.data_representation.R")

#
#or source if you wanto to have the full analisis 
for (kernel in c("state", "time")){
dif_cond = TRUE
  tic()
source("./analyses/1.simulated_LVmap_full.R")
toc()
}
################################################3


#============================DATA ANALYSE WITH LV MAP=============================================

#here you can add a loop to have the time or spatial kernel kernel_v= c("state", "time")

for (kernel in c("state", "time")){

tic()
source("./analyses/2.data_LVmap_full.R")
toc()
  
}






#===========================================================
#--------------------------------------CCM LOOP for different norm and detrending methods-------------------------------------------------
## loop for real data

tic()
for (metodo_det in c("none")){  ##linear detren firstDiff", "none"
  for(metodo_norm in c("minmax", "zscore", "none")){  #none zscore", "minmax
    det_method = metodo_det
    no_method = metodo_norm
    type_data =  "real.data"
    iteraciones = 1000  ##1000
    numSurro = 50 #50
    print(paste0("metodo_det_", metodo_det))
    print(paste0("no_det_", metodo_norm))
    source("./analyses/3.ccm_rigal_igp.R")
  }
}
toc()

#----------------simulated data--------------------

##conditions tested (set this manually)

#Here I run general scenaiors, that can be used by any model, changing the lenght 

#scenarios <- c("lblb_model_1", "lblb_model_2", "lblb_model_3", "lblb_model_4")
scenarios <- c("lblb_model_2")
#scenarios <- c("lblb_model_2", "lblb_model_3")
#scenarios <- c("lblb_model_3")



#long scenarios for reference
for (i in scenarios){

chosen_scenario <- i
#just to see the data
##simulate the data before!! cc
len_chosen <- 300
noise_chosen <- 0.5

source("./analyses/3c_simulatedData_CCM.R")
}

#short scenarios for use in the CCM and LV


for (i in scenarios){

chosen_scenario <- i
#just to see the data
##simulate the data before!! cc
len_chosen <- 20
noise_chosen <- 0.5

source("./analyses/3c_simulatedData_CCM.R")
}




#------------------------
##------------nnow the loop to apply the ccm in the simulated data
#scenarios <- c("lblb_model_1", "lblb_model_2", "lblb_model_3", "lblb_model_4")



#loop for simulated data

simulated_data <-  "DF_DISC_LV_20.csv"

for (i in scenarios){

chosen_scenario <- i
  
tic()
for (metodo_det in c("none")){  ##linear detren firstDiff", "none"
  for(metodo_norm in c("zscore")){  #none zscore", "minmax
    det_method = metodo_det
    no_method = metodo_norm
    type_data = "simulated.data"  #real.data
    iteraciones = 100  ##1000
    numSurro = 10 #50
    print(paste0("metodo_det_", metodo_det))
    print(paste0("no_det_", metodo_norm))
    source("./analyses/3.ccm_rigal_igp.R")
  }
}
toc()
}