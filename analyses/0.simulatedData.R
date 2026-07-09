
#====================================================================================

#====================I. STOCASTIC SIMULATIONS========================================
#==============Define the characteritic of the data frame=============================
set.seed(3)
#random_seed <- rnorm(1)
diff_len <- FALSE
num_block <- 10  #how many block (time series)
reso <- 1

#len_chosen and noise_chosen are set in the make.R

#names of the folders
data_folder <- paste0("./data/simulated.data/") 


#====================I. A. RUN SIMULATION DATA===============================================
#First set the conditions for the LBLB model
#this will do with the chosen scenario 

model_used <- chosen_MODEL  #set externally in the makeR  
disc_or_cont <- "disc_stoc"  #
grow_function <- "semichemostat"   ## Selected growth function type (can be "logistic", "exponential", or "semichemostat")
stochastic_generator_GENERAL(model_used = model_used, disc_or_cont = disc_or_cont, noise_chosen = noise_chosen)

