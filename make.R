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

#====================================================================================


#====================STOCASTIC SIMULATIONS=================================

#First set the conditions for the LBLB model
source("./analyses/0.set_LBLB_model.R")



# Then run the inital conditions for that code 
# set the random value that will taken for the demographic noise
# num_rep tells you the replicates you wanna run
# the diff_len tell tyo if you want to randomly cut some time series

set.seed(3)
random_seed <- rnorm(1)
num_rep <- 10
diff_len <- FALSE

#now we set a loop for different initial conditions
#folders to save everything! 

# Set up folders for saving figures - specifies where output plots will be saved
fig_folder <- "./figures/simulation/stochastic_sim_lvmap/demographic_sto/" ### Directory path for the main figure folder
fig_subfolder <- paste0("len", len_chosen,"/", "noise_", noise_chosen, "/", "reso_", reso, "/") ## Creates a subfolder name based on time length and noise level chosen


for (l in c(100, 50)){
for (res in c(1, 0.1)){
  for (noi in c(0.1)){

reso <-  res
noise_chosen <- noi
len_chosen <- l

source("./analyses/1.stochastic_simulations.R")
}
}
}

#=========================================================================================

#============================DATA ANALYSE WITH LV MAP=============================================

DATA_IGP <- readr::read_csv("data/dataIGP_2025.csv")
source("./analyses/2.data_representation.R")


#========2.LV MAP======================

#this is if you want to run a specifc test
#chosen_enemies <- "my+aa"
#rpresent <- FALSE
#num_rep <- 1
#num_seed <- 1
#source("./analyses/2.LV_map_data.R")

#this is nice cause you loop outside
DATA_IGP <- readr::read_csv("data/dataIGP_2025.csv")


rpresent <- FALSE
num_rep <- 1


for (num_seed in seq(1:10)){
for (enem in unique(DATA_IGP$enem)){

chosen_enemies <- enem
  num_rep <- num_rep
  num_seed <- num_seed
  source("./analyses/3a.data_LV_map.R")
}
  }




###########now for plotting results

source("./analyses/3b.data_LV_map_results.R")
