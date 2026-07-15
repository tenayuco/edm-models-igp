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

#================================SIMULATED DATA====================================

#1. First here I run the time series of 7 different sceanrios, 4 for the LBLB model and 3 for the PBPB model as examples of determinations. 
# it saves all the produced time series in data with the name of the scenario 

scenarios_lblb <- c("lblb_model_0", "lblb_model_1", "lblb_model_2", "lblb_model_3", "lblb_model_4")
scenarios_pbpb <- c("pbpb_model_1", "pbpb_model_2", "pbpb_model_3")

#chose your scenarios
scenarios <- c(scenarios_lblb, scenarios_pbpb)

#========================================================================

#or for short example:
scenarios_chosen <- c("pbpb_model_3")

#loop to generate multiple scenarios of simulated data 
#long scenarios for reference, shorts for the analyis
#we need to have coexistence to catch the signal 
data_folder <- paste0("./data/simulated.data/") 

for (i in scenarios_chosen){

#names of the folders
chosen_scenario <- i

if(dir.exists(paste0(data_folder, chosen_scenario))){print("you already have the data for this sceanrio")
}else {

for (len in c(300, 20)){  #we have a 300 to see the whole dynamic,s and a 20 that it the one we are using, with 10 replicates
len_chosen <- len

#this will set the correct parameters per model and create the list of models
source("./analyses/0.set_LBPB_model.R")
  
if (i %in% scenarios_lblb){chosen_MODEL <- LBLB_LV_list }
if (i %in% scenarios_pbpb){chosen_MODEL <- PBPB_LV_list }

#then it will run the analysis and save the dataframe in DAta
source("./analyses/0.simulatedData.R")
}
}
}

#===========================================================================================





#================================LV MAP FOR SIMULA==============================================

#########################################333
#=simulation
#either enter source("./analyses/2.data_representation.R")
#chosen_scenario <- "lblb_model_0"  ##scenario chosen, to now from where take the data
#
#norm_data <-  FALSE
#lv_scearios <- scenarios_lblb
lv_scenarios <- ("pbpb_model_3")

tic()
#or source if you wanto to have the full analisis 
  for(norm_data in c(FALSE, TRUE)){
    for (chosen_scenario in lv_scenarios){
 tic()
  print(paste0("norm_data", norm_data, 'chosen_sce', chosen_scenario))
 source("./analyses/1.simulated_LVmap_full.R")
 toc()

    }
  }
print("totalTIME")
 toc()

################################################3


#============================DATA ANALYSE WITH LV MAP=============================================

#here you can add a loop to have the time or spatial kernel kernel_v= c("state", "time")

kernel_chosen <- "state"
dif_cond <- FALSE

tic()
source("./analyses/2.data_LVmap_full.R")
toc()
  






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
    iteraciones = 1000  ##1000
    numSurro = 30 #50
    print(paste0("metodo_det_", metodo_det))
    print(paste0("no_det_", metodo_norm))
    source("./analyses/3.ccm_rigal_igp.R")
  }
}
toc()
}