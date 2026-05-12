###now it is a function. inert

lv_map_sim_treatment <- function(DF_USED, rpresent, num_seed, num_rep, model_used){


DF_DISC_LV <- DF_USED
list_treatment <- list()


# -------------------------PART 2- estimated "real parameters" changing to their LV framing----------------------------------------

S <-  length(LBLB_LV_list$init)
 
###now I put here the real values of the data, according to the transformation 
fac_int <- min(reso, 0.1) ##it just to remove the NA when the resolution is to strong and the parameters should jump to negative values  
avR <- mean(DF_DISC_LV$R) #to reproduce the inflx 

  
  
par_ms = model_used[["parms"]]  
 
#check if it is the shemi
#here the rt is lackig a constant 
r_eq <- c(log(par_ms[["rho"]]*(par_ms[["K"]]/avR)*fac_int +1), log(1-par_ms[["mun"]]*fac_int), log(1-par_ms[["mup"]]*fac_int)) # set the intrinsic growth rates
names(r_eq) <- paste("sp", 1:S, sep = "") # species names
alpha_eq <- matrix(NA, nrow = S, ncol = S) # set the per capita interaction strengths matrix
colnames(alpha_eq) <- c("R","N", "P" )
rownames(alpha_eq) <-c("R","N", "P" )

#alpha_eq[1,1] <- -par_ms[["rho"]]/par_ms[["K"]]
alpha_eq[1,1] <- 0
alpha_eq[1, 2] <- -par_ms[["frn"]]
alpha_eq[1, 3] <- - par_ms[["frp"]] *  par_ms[["S"]]
alpha_eq[2, 1] <- par_ms[["En"]]* par_ms[["frn"]]
alpha_eq[2, 2] <- 0
alpha_eq[2, 3] <- - par_ms[["fnp"]] * (1- par_ms[["S"]])
alpha_eq[3, 1] <- par_ms[["Ep"]]* par_ms[["frp"]] *  par_ms[["S"]] #0.15
alpha_eq[3, 2] <-  par_ms[["Ep"]]* par_ms[["fnp"]] * (1- par_ms[["S"]]) #0.15
alpha_eq[3, 3] <- 0
alpha_eq <- alpha_eq * fac_int

list_treatment$r_eq <- r_eq
list_treatment$alpha_eq <- alpha_eq

# ----------------------------------------------------------------------------------

# -------------------------PART 3- cross validatio ----------------------------------------
#Tmax <- len_chosen

###here i removed the R
if (rpresent == FALSE){DF_DISC_LV$R <- NULL}
  #we remove these as they have a strong autocorrelation.. 


#here I reorder the replicates just in case im gonnea stick them and i chnage the names to replicate s
DF_DISC_LV_USED <- DF_DISC_LV


set.seed(num_seed)
REAS_DF <-  data.frame("block" = seq(1:10), "replicate" = sample(seq(1:10)))
DF_DISC_LV_USED <-  dplyr::full_join(DF_DISC_LV_USED, REAS_DF, by= "block")
DF_DISC_LV_USED$block <-  NULL



DF_DISC_LV_USED <-  DF_DISC_LV_USED |> 
dplyr::arrange(replicate, .by_group = FALSE)
###########3

##now I merge the values.. to make fake replicates..

size_block <- num_block/num_rep  ## HAS to be an integer


DF_DISC_LV_USED$replicate <- floor((DF_DISC_LV_USED$replicate-0.1)/size_block) +1 


#---transforms to a matrix
#num_rep <- length(unique(DF_DISC_LV_USED$replicate))

N_list_sim <- vector(mode = "list", length = num_rep)

for (i in unique(DF_DISC_LV_USED$replicate)){
  df_temp <- DF_DISC_LV_USED |> 
    dplyr::filter(replicate == i)

  df_temp$time <- NULL
  df_temp$replicate <- NULL

  N_list_sim[[i]] <- as.matrix(df_temp)
}

#I save the final N list used 
list_treatment$N_list_sim <- N_list_sim



# ================
# Cross validation
# ================


cv_list_sim <- vector(mode = "list", length = num_rep)
tic()
for (i in 1:num_rep) {
  out_cv <- LV_map_state_space_cross_validation(N_list_sim[[i]], theta_v = seq(0, 3, 0.01))
  #out_cv <- LV_map_time_cross_validation(N_list_sim[[i]], theta_v = seq(0, 3, 0.01))
  cv_list_sim[[i]] <- out_cv
}
toc()

list_treatment$cv_list_sim <- cv_list_sim




# ========================
# Estimation of parameters
# ========================
r_hat_list <- vector(mode = "list", length = num_rep)
alpha_hat_list <- vector(mode = "list", length = num_rep)
r_se_list <- vector(mode = "list", length = num_rep)
alpha_se_list <- vector(mode = "list", length = num_rep)
out_list <- vector(mode = "list", length = num_rep)
tic()
for (i in 1:num_rep) {
  out_list[[i]] <- LV_map(N_list_sim[[i]], cv_list_sim[[i]]$theta_o)
  r_hat_list[[i]] <- out_list[[i]]$r_hat
  r_se_list[[i]] <- out_list[[i]]$r_se
  alpha_hat_list[[i]] <- out_list[[i]]$alpha_hat
  alpha_se_list[[i]] <- out_list[[i]]$alpha_se
}
toc()

list_treatment$out_list <- out_list
list_treatment$r_hat_list <- r_hat_list
list_treatment$alpha_hat_list <- alpha_hat_list
list_treatment$r_se_list <- r_se_list
list_treatment$alpha_se_list <- alpha_se_list

return(list_treatment)
  
}











