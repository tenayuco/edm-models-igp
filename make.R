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
tic()
source("./analyses/1.simulated_LVmap_full.R")
toc()
################################################3


#============================DATA ANALYSE WITH LV MAP=============================================

#here you can add a loop to have the time or spatial kernel kernel_v= c("state", "time")


tic()
source("./analyses/2.data_LVmap_full.R")
toc()