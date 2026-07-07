
#====================================================================================

#====================I. STOCASTIC SIMULATIONS========================================
#==============Define the characteritic of the data frame=============================
set.seed(3)
#random_seed <- rnorm(1)
diff_len <- FALSE
num_block <- 10  #how many block (time series)
reso <- 1
#noise_chosen <- 0 #0.05
#len_chosen <- 20, this does not work with replicates.. 
#len_chosen <- 20
# s_chosen externally!!

#names of the folders
data_folder <- paste0("./data/simulated.data/") 


#====================I. A. RUN SIMULATION DATA===============================================
#First set the conditions for the LBLB model
#this will do with the chosen scenario 
source("./analyses/0.set_LBLB_model.R")


#LBLB_LV_list$parms[["S"]] <- s_chosen

##estas las tengo que silenciar porque al chile no sirven mucho 


#LBLB_LV_list$parms[["frn"]] <-  frn_chosen
#LBLB_LV_list$parms[["K"]] <-  K_chosen

# the diff_len tell tyo if you want to randomly cut some time series

#model used
model_used <- LBLB_LV_list 
disc_or_cont <- "disc_stoc"  #
grow_function <- "semichemostat"   ## Selected growth function type (can be "logistic", "exponential", or "semichemostat")
stochastic_generator_GENERAL(model_used = model_used, disc_or_cont = disc_or_cont, noise_chosen = noise_chosen)

