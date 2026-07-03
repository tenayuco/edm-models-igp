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
  for(metodo_norm in c("minmax")){  #none zscore", "minmax
    det_method = metodo_det
    no_method = metodo_norm
    type_data =  "real.data"
    iteraciones = 100  ##1000
    numSurro = 50 #50
    print(paste0("metodo_det_", metodo_det))
    print(paste0("no_det_", metodo_norm))
    source("./analyses/3.ccm_rigal_igp.R")
  }
}
toc()

#----------------simulated data--------------------
#just to see the data
##simulate the data before!! cc
len_chosen <- 300
s_chosen = 0
noise_chosen <- 0.5
frn_chosen <- 0.4 ## esto es solo para prueba
K_chosen <- 10 ##esto es solo para pruebas 
#then pick the simulated data
#sim_data = paste0("DF_DISC_LV_S_", s_chosen, "_len_", len_chosen, ".csv")
source("./analyses/3c_simulatedData_CCM.R")

#------------------------


#loop for simulated data


for(sloop in c(0)){  #0.1, 0.5, 0.9
len_chosen <- 20  #we pick from the 200 stage, 20 points
noise_chosen <- 0.5
 frn_chosen <- 0.4 ## esto es solo para prueba  #normal value is 6
K_chosen <- 10 ##esto es solo para pruebas  #normal value is 5
s_chosen <- sloop

  #here you redoo the simulated data just in case 
source("./analyses/3c_simulatedData_CCM.R")
  
sim_data = paste0("DF_DISC_LV_S_", s_chosen, "_len_", len_chosen, ".csv")


tic()
for (metodo_det in c("none")){  ##linear detren firstDiff", "none"
  for(metodo_norm in c("minmax")){  #none zscore", "minmax
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