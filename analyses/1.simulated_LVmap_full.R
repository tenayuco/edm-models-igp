
#====================================================================================

#====================I. STOCASTIC SIMULATIONS========================================
#==============Define the characteritic of the data frame=============================
set.seed(3)
#random_seed <- rnorm(1)
diff_len <- FALSE
num_block <- 10  #how many block (time series)
reso <- 1
noise_chosen <- 0.1
#len_chosen <- 20, this does not work with replicates.. 
len_chosen <- 100

#names of the folders
out_folder <- paste0("./outputs/simulation/demoStoc_lvmap/","len_",len_chosen,"/") 
fig_folder <- paste0("./figures/simulation/demoStoc_lvmap/","len_",len_chosen,"/") 


#====================I. A. RUN SIMULATION DATA===============================================
#First set the conditions for the LBLB model
source("./analyses/0.set_LBLB_model.R")

# the diff_len tell tyo if you want to randomly cut some time series

#this will give the whole data series named DF_DISC_LV
source("./analyses/1a.stochastic_simulations.R")

#model used
model_used <- LBLB_LV_list 
disc_or_cont <- "disc_stoc"  #
grow_function <- "semichemostat"   ## Selected growth function type (can be "logistic", "exponential", or "semichemostat")
stochastic_generator(model_used = model_used, disc_or_cont = disc_or_cont, noise_chosen = noise_chosen)


#this will generate the time series, and the plot in the respective fig folder and out folder
#=============================================================================================


#==========================I.B. RUN LV MAP SPATIAL KERNEL=======================================

###########2 cross validation. now we put it in the LV MAP. And here we can loop like crazy

##now we take that data base and we create the matrix and apply the cross validation across scenarios

## here is what the looper needs (it uses the same folder)
data_used <- utils::read.csv(paste0(out_folder, "DF_DISC_LV.csv"))
v_num_rep = c(1, 10)
v_rpresent = c(TRUE, FALSE)
v_num_seed = seq(1:10) ###A lot of reshuflle 

source("./analyses/1b.looper_LV_map.R")
#protocool for the loop it run the functions of lv map, for each treatment. 
lv_looper_lists(data_used, v_num_rep, v_rpresent, v_num_seed, model_used)

#===========================================================================================

#SO AT THIS POINT WE HAVE DONE THE HEAVY ANALYSIS 
#NOW THE MORE GENERAL PLOTTINGS

#========================I.C PLOT LV SIMULATIONS ============================
source("./analyses/1_2.plot_extract_per_treatment.R")
source("./analyses/1_2.plotter_LV.R")
plot_per_treatment(out_folder, true_values = TRUE)
full_df <- extract_par_all_treatment(out_folder) ##generates the file

plotter_full_parameters(df_full = full_df, fig_folder = fig_folder)

##========================================================================================


#########################################333
################################################3
