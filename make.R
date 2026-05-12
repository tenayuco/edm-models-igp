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


#########################################333
#=simulation
#either enter source("./analyses/2.data_representation.R")

#or source if you wanto to have the full analisis 
source("./analyses/1.simulated_LVmap_full.R")

################################################3


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



#names of the folders
out_folder <- "./outputs/microcosmos/" ### Directory path for the main figure folder
fig_folder <- "./figures/microcosmos/"


v_num_seed <- seq(1:10)
v_enemigos <-  unique(DATA_IGP$enem)
num_rep <- 1
rpresent <- FALSE


#import the source 

lv_looper_lists_microcosmos(raw_data = DATA_IGP, v_enemigos = v_enemigos, v_num_seed = v_num_seed, num_rep = num_rep, rpresent = rpresent)



#source("./analyses/1c.plot_stochastic_LV.R")
plot_per_treatment_microcosmos(out_folder)

extract_par_all_treatment_microcosmos(out_folder) ##generates the file

###########now for plotting results

source("./analyses/3b.data_LV_map_results.R")
