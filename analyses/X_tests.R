#test LV MAP

rpresent <- TRUE
num_seed <- 1
num_rep <- 1

list_treatment<- list()

### heres is the data
DATA_USED <- DATA_PRED |> 
  dplyr::filter(enem== "ma+ol")

DATA_USED$enem <- NULL


#reshuffling of replicates
###here I change the values of the replicate to chage the order.

set.seed(num_seed)

REAS_DF <-  data.frame("block" = seq(1:10), "replicate" = sample(seq(1:10)))
DATA_USED <-  dplyr::full_join(DATA_USED, REAS_DF, by= "block")
DATA_USED$block <-  NULL
##now here a renaming
names(DATA_USED) <- c("R", "X", "Y", "time", "replicate")
###here i removed the H
if (rpresent == FALSE){DATA_USED$R <- NULL}

  #here i order by replocates
DATA_USED <-  DATA_USED |> 
dplyr::arrange(replicate, .by_group = FALSE)
###########3

##here i used the replicate to know the initial R_0 index and RF_index
  # 
  #====================
long_series <- DATA_USED |> 
  dplyr::ungroup() |> 
  dplyr::mutate(count =1) |>
  dplyr::group_by(replicate) |> 
  dplyr::summarise(long = sum(count))|> 
  dplyr::select(long)

long_series_vec <- as.vector(long_series$long)

R_F_index <-  cumsum(long_series_vec)[-10]  #9   
R_0_index <- rep(1, 10)
R_0_index[-1] <- R_0_index[-1] + R_F_index ## this gives the end of each, so by summing it we have the intial of nthe next

  #==================================
  
  
  #from now on the new teplicate will not make snse  
#####here it is just to gather in block, but keeping the new given order

size_block <- length(unique(DATA_USED$replicate))/num_rep
DATA_USED$replicate <- floor((DATA_USED$replicate-0.1)/size_block) +1   #fake block to make larger data inly work wiht zie block divisor of 10

#==================================================================================================  
  #PREDATA FOR LV
#---transforms to a matrix
N_list_sim <- vector(mode = "list", length = num_rep)

#here it takes 
for (i in unique(DATA_USED$replicate)){
  df_temp <- DATA_USED |> 
    dplyr::filter(replicate == i)

  df_temp$time <- NULL
  df_temp$replicate <- NULL

  N_list_sim[[i]] <- as.matrix(df_temp)
}


list_treatment$N_list_sim <- N_list_sim

S <-  dim(N_list_sim[[1]])[2]

#=========================================================================================  


  
# ================
# Cross validation
# ================


chosen_theta_v = seq(0, 3, 0.01)
#chosen_theta_v = seq(0, 0)
  
cv_list_sim <- vector(mode = "list", length = num_rep)

tictoc::tic()

#only to test
#N  <-  N_list_sim[[1]]

for (i in 1:num_rep) {

if(forcing_theta == TRUE){
    cv_list_sim[[i]]$theta_o <-  0
    cv_list_sim[[i]]$RMSE_o <- 0}  #here the forced theta

  else{
  print("Im doing the cross validation stuff")
  if(kernel_chosen == "state") {
out_cv <- LV_map_state_space_cross_validation_mod(N_list_sim[[i]], theta_v = chosen_theta_v, 
    R_0_index = R_0_index, R_F_index = R_F_index,  mod_XY_mat = TRUE, remove_stiching = T)
  }
if(kernel_chosen == "time") {
  ##this does not WORK FOR OUR MOD
  print("dont use time kernel")
  #out_cv <- LV_map_time_cross_validation_mod(N_list_sim[[i]], theta_v = chosen_theta_v)
  }
  cv_list_sim[[i]] <- out_cv
}
}

print("crossvalidationtime")
tictoc::toc()

png("../externalTests/ma_ol_XX.png", width = 1200, height = 800, res = 150)
plot(cv_list_sim[[1]]$observed_Y_all, cv_list_sim[[1]]$predicted_Y_all, main = "XN")
dev.off()

png::writePNG()




LV_map_state_space_cross_validation_mod_test <- function(N, theta_v = seq(0, 5, 0.05), p = 0.1, mod_XY_mat = TRUE, R_0_index, R_F_index, remove_stiching = F) {
  
  Tmax <- dim(N)[1]
  n <- length(theta_v)
  Tstart <- round(Tmax * p)
  n_species <- dim(N)[2]          # <-- number of species
  n_time <- Tmax - 1              # <-- number of prediction steps

##we create an empty list that will be added to cv_list
  fitting <- list()
  
  RMSE <- rep(NA, n)

  # ---- STORAGE: 3D array to hold predicted_Y for every theta, t, species ----
  # Dimensions: [theta, time, species]
  #predicted_Y_all <- array(NA, dim = c(n, n_time, n_species))

  # also store the actual (observed) values for comparison
  #observed_Y_all <- array(NA, dim = c(n, n_time, n_species))

  #I save this complete X for the prediction 
  X_ALL <- N
    for (ncut_index in seq(1:dim(X_ALL)[1])) {
          if (!(ncut_index %in% R_0_index)) {
             X_ALL[ncut_index, 1] <-  X_ALL[ncut_index, 1] + 400
          }
        }

  ##loop over the theta
  for (i in 1:n) {
    ESS <- 0
    theta <- theta_v[i]

    for (t in Tstart:(Tmax - 1)) {

      N.cut <- N[-((t + 1):Tmax), ]
      logN.cut <- log(N.cut)

      #now this is not used
    #  Y <- logN.cut[-1, ] - logN.cut[-t, ]
     # X <- cbind(rep(1, t - 1), N.cut[-t, ])

      #=========== procedure to modify ncut and log ncut ===========
      if (mod_XY_mat == TRUE & dim(N)[2] != 2) {
        N.cut_mod <- N.cut
        for (ncut_index in seq(1:dim(N.cut_mod)[1])) {
          if (!(ncut_index %in% R_0_index)) {
            N.cut_mod[ncut_index, 1] <- N.cut_mod[ncut_index, 1] + 400
          }
        }
        logN.cut_mod <- log(N.cut_mod)
        Y <- logN.cut[-1, ] - logN.cut_mod[-t, ]
        X <- cbind(rep(1, t - 1), N.cut_mod[-t, ])

        if (remove_stiching == T) {
          for (ncut_index in seq(1:dim(N.cut_mod[-t, ])[1])) {
            if (ncut_index %in% R_F_index) {
              X[ncut_index, 2] <- X[ncut_index - 1, 2]
              Y[ncut_index, 1] <- Y[ncut_index - 1, 1]
            }
          }
        }
      }
      #==============================================================

      d <- sqrt(colSums(((t(N.cut)[, t - 1]) - t(N.cut)[, -t])^2))
      omega <- exp(-theta * d / mean(d))

      Y.tilde <- omega * Y
      X.tilde <- omega * X
      beta_hat <- solve(t(X.tilde) %*% X.tilde, t(X.tilde)) %*% Y.tilde
      r_hat <- t(t(beta_hat[1, ]))
      alpha_hat <- t(beta_hat[-1, ])


      #it does the predicted with the modifed values, that it is were it comes from 
      predicted_Y <- X_ALL[t, ] * exp(r_hat + alpha_hat %*% X_ALL[t, ])
      observed_Y <- N[t + 1,]
      # ---- SAVE predicted_Y and observed Y ----
      # The time index relative to the storage array:
      #t_idx <- t - Tstart + 1

      
      #predicted_Y_all[i, t_idx, ] <- as.numeric(predicted_Y)

      #and the observerd with the N matrix, that is where it came from 
     # observed_Y_all[i, t_idx, ]  <- as.numeric(N[t + 1,])         # drop last element (base R))


      fitting[[i]] <- data.frame("leng_training" = t, "observed"= 
        as.numeric(observed_Y), "predicted" = as.numeric(predicted_Y))

      ESS <- ESS + sum((observed_Y - predicted_Y)^2)
    }

    RMSE[i] <- sqrt(ESS / (Tmax - 1))
    
    ## check this 
    fitting[[i]]$theta <- theta
    fitting[[i]]$rmse <- RMSE[i]

  }

  theta_o <- theta_v[which.min(RMSE)]
  RMSE_o <- min(RMSE)

  out <- list(
    theta_v = theta_v,
    RMSE = RMSE,
    theta_o = theta_o,
    RMSE_o = RMSE_o,
    fitting = fitting,
    Tstart = Tstart,
  )

  return(out)
}
