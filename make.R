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

#====================I. STOCASTIC SIMULATIONS=================================
#==============RUN THIS ALWAYS

set.seed(3)
#random_seed <- rnorm(1)
diff_len <- FALSE
num_block <- 10  #how many block (time series)
reso <- 1
noise_chosen <- 0.1
len_chosen <- 100

#names of the folders
out_folder <- paste0(
  "./outputs/simulation/demoStoc_lvmap/",
  "len_",
  len_chosen,
   "/"
) ### Directory path for the main figure folder

fig_folder <- paste0(
  "./figures/simulation/demoStoc_lvmap/",
  "len_",
  len_chosen,
   "/"
) ### Directory path for the main figure folder


#====================I. A. RUN SIMULATION DATA===============================================
#First set the conditions for the LBLB model
source("./analyses/0.set_LBLB_model.R")

# Then run the inital conditions for that code
# set the random value that will taken for the demographic noise
# num_rep tells you the replicates you wanna run
# the diff_len tell tyo if you want to randomly cut some time series

#this will give the whole data series named DF_DISC_LV
source("./analyses/1.stochastic_simulations.R")

#this will generate the time series, and the plot in the respective fig folder and out folder
#=============================================================================================


#==========================I.B. RUN LV MAP SPATIAL KERNEL=======================================

###########2 cross validation. now we put it in the LV MAP. And here we can loop like crazy

##now we take that data base and we create the matrix and apply the cross validation across scenarios

## here is what the looper needs (it uses the same folder)
data_used <- utils::read.csv(paste0(out_folder, "DF_DISC_LV.csv"))
v_num_rep = c(1, 10)
v_rpresent = c(TRUE, FALSE)
v_num_seed = seq(1:2)

source("./analyses/1a.looper_LV_map.R")
#protocool for the loop it run the functions of lv map, for each treatment. 
lv_looper_lists(data_used, v_num_rep, v_rpresent, v_num_seed)

#===========================================================================================

#========================I.C PLOT LV SIMULATIONS ============================


source("./analyses/1b.plot_stochastic_LV_all_treatments.R")
plot_per_treatment(out_folder)
extract_par_all_treatment(out_folder) ##generates the file
##============================================


#########################################333
################################################3







































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


for (num_seed in seq(1:10)) {
  for (enem in unique(DATA_IGP$enem)) {
    chosen_enemies <- enem
    num_rep <- num_rep
    num_seed <- num_seed
    source("./analyses/3a.data_LV_map.R")
  }
}


###########now for plotting results

source("./analyses/3b.data_LV_map_results.R")
