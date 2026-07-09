
DATA_IGP <- readr::read_csv("data/dataIGP_2025.csv")

#to represent the data


#========2.LV MAP======================

#this is nice cause you loop outside
#names of the folders

#prefedined in the krnel
#kernel <- "time"
#kernel <- "state"


out_folder <- paste0("./outputs/microcosmos/", kernel, "_kernel/") ### Directory path for the main figure folder
fig_folder <- paste0("./figures/microcosmos/", kernel, "_kernel/")


v_num_seed <- seq(1:10)
v_enemigos <-  unique(DATA_IGP$enem)
num_rep <- 1
rpresent <- FALSE

#to represent the data
#source("./analyses/2a.data_representation.R")

#import the source 
#==========================I.B. RUN LV MAP SPATIAL KERNEL on DATA=======================================
source("./analyses/2b.looper_data_lv_map.R")



lv_looper_lists_microcosmos(raw_data = DATA_IGP, v_enemigos = v_enemigos, v_num_seed = v_num_seed, num_rep = num_rep, rpresent = rpresent, kernel_chosen = kernel)


#===========================================================================================

#SO AT THIS POINT WE HAVE DONE THE HEAVY ANALYSIS 
#NOW THE MORE GENERAL PLOTTINGS

#========================I.C PLOT LV <MICRSO ============================

#fives the list per treamtne 

source("./analyses/1_2.plot_extract_per_treatment.R")
source("./analyses/1_2.plotter_LV.R")

plot_per_treatment(out_folder, true_values = FALSE)

full_df <- extract_par_all_treatment(out_folder) ##generates the file


plotter_full_parameters_microcosmos(df_full = full_df, fig_folder = fig_folder)


plotter_theta_microcosmos(df_full = full_df, fig_folder = fig_folder)

plotter_omega_microcosmos(df_full = full_df, fig_folder = fig_folder)


###########now for plotting results
