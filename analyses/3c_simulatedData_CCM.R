
#====================================================================================

#====================I. STOCASTIC SIMULATIONS========================================
#==============Define the characteritic of the data frame=============================
set.seed(3)
#random_seed <- rnorm(1)
diff_len <- FALSE
num_block <- 10  #how many block (time series)
reso <- 1
noise_chosen <- 0.01
#len_chosen <- 20, this does not work with replicates.. 
len_chosen <- 20


#names of the folders
out_folder <- paste0("./outputs/CCM/") 


#====================I. A. RUN SIMULATION DATA===============================================
#First set the conditions for the LBLB model
source("./analyses/0.set_LBLB_model.R")

# the diff_len tell tyo if you want to randomly cut some time series

#model used
model_used <- LBLB_LV_list 
disc_or_cont <- "disc_stoc"  #
grow_function <- "semichemostat"   ## Selected growth function type (can be "logistic", "exponential", or "semichemostat")
stochastic_generator_CCM(model_used = model_used, disc_or_cont = disc_or_cont, noise_chosen = noise_chosen)

