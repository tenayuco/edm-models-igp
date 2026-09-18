### LV-map toolbox

#################
## Fit the LV-map 
LV_map_mod <- function(N, theta = 0, kernel = 'state', R_0_index, R_F_index, remove_stiching=F, mod_XY_mat = FALSE) {
  S <- dim(N)[2] # number of species
  Tmax <- dim(N)[1] # number of time steps
  sp_names <- colnames(N) # species names

  r_hat <- array(NA, dim = c(Tmax - 1, S)) # array to store the fitted intrinsic growth rate
  alpha_hat <- array(NA, dim = c(Tmax - 1, S, S)) # array to store the fitted per capita interaction strengths

  r_se <- array(NA, dim = c(Tmax - 1, S)) # array to store the SE of fitted intrinsic growth rate
  alpha_se <- array(NA, dim = c(Tmax - 1, S, S)) # array to store the SE of fitted per capita interaction strengths
  Sigma_beta <- array(NA, dim = c(Tmax - 1, S, S+1, S+1)) # array to store the variance-covariance matrix of 
  # fitted intrinsic growth rate and per capita interaction strengths
  time.p <- seq(1:(Tmax-1)) #create vector of time
  
  logN <- log(N)
  Y <- logN[-1, ] - logN[-Tmax, ] # create the log ratio matrix (response variables)
  X <- cbind(rep(1, Tmax - 1), N[-Tmax, ]) # create the explanatory variables matrix
#===========procedure to modify ncut and log ncut to add the real time series of herbovore
  
if(mod_XY_mat == TRUE & dim(N)[2] !=2){
 #creata an auxiliary matrix 
  N_mod <- N ## this is not a real matrix per se
  for (n_index in seq(1:dim(N_mod)[1])){
      if (!(n_index %in%  R_0_index)){  #r0 index is defined outside the loop
        N_mod[n_index, 1] <- N_mod[n_index, 1]+400 ##so if you are not in the index of initial youll have +400
      }
    }
          
logN_mod <- log(N_mod)  ##right side of the rest

# now here we rebuuld the X and Y with this auxiliary matrix 
     Y <- logN_mod[-1, ] - logN_mod[-Tmax, ]
     X <- cbind(rep(1, Tmax - 1), N_mod[-Tmax, ])  #explanatory variable 
  
  #and the last detail is that in X and Y i repeated the last value! (less problematic than artificil stihci)    
     
  #for now, im not changin this, to see if i can get the prev results 
 if(remove_stiching == T){
  for (n_index in seq(1:dim(N_mod)[1])){ 
      if (n_index %in%  R_F_index){  #
        X[n_index, 2] <- X[n_index-1, 2]
        Y[n_index, 1] <- Y[n_index-1, 1]##
      }
    }
  }
}
#end of procedure 
#=================================================
# i dont use these values


  if (theta == 0){
    
    if (kernel == 'state') {
      
      out_reg <- weighted_regression_state_kernel(X,Y,N,S,1,Tmax,theta)
      
    } else if (kernel == 'time') {
      
      out_reg <- weighted_regression_time_kernel(X,Y,N,S,1,Tmax,time.p,theta)
      
    } else { stop("kernel must be equal to state or time")}
    
    for (t in 1:(Tmax - 1)) {
      
      r_hat[t, ] <- out_reg$r_hat # store the estimated r
      alpha_hat[t, , ] <- out_reg$alpha_hat # store the estimated alpha
      r_se[t, ] <- out_reg$r_se
      alpha_se[t, , ] <- out_reg$alpha_se
      Sigma_beta[t, , , ] <- out_reg$Sigma_beta
    }
    
  } else {
    
    if (kernel == 'state') {
      
      for (t in 1:(Tmax - 1)) {
        
        out_reg <- weighted_regression_state_kernel(X,Y,N,S,t,Tmax,theta)
        
        r_hat[t, ] <- out_reg$r_hat # store the estimated r
        alpha_hat[t, , ] <- out_reg$alpha_hat # store the estimated alpha
        r_se[t, ] <- out_reg$r_se
        alpha_se[t, , ] <- out_reg$alpha_se
        Sigma_beta[t, , , ] <- out_reg$Sigma_beta
        
      }
      
    } else if (kernel == 'time') {
      
      for (t in 1:(Tmax - 1)) {
        
        out_reg <- weighted_regression_time_kernel(X,Y,N,S,t,Tmax,time.p,theta)
        
        r_hat[t, ] <- out_reg$r_hat # store the estimated r
        alpha_hat[t, , ] <- out_reg$alpha_hat # store the estimated alpha
        r_se[t, ] <- out_reg$r_se
        alpha_se[t, , ] <- out_reg$alpha_se
        Sigma_beta[t, , , ] <- out_reg$Sigma_beta
        
      }
      
    } else { stop("kernel must be equal to state or time")}
    
  }
    
  # add species name and time points
  colnames(r_hat) <- sp_names
  rownames(r_hat) <- seq(1, Tmax - 1)
  dimnames(alpha_hat)[[1]] <- seq(1, Tmax - 1)
  dimnames(alpha_hat)[[2]] <- sp_names
  dimnames(alpha_hat)[[3]] <- sp_names

  colnames(r_se) <- sp_names
  rownames(r_se) <- seq(1, Tmax - 1)
  dimnames(alpha_se)[[1]] <- seq(1, Tmax - 1)
  dimnames(alpha_se)[[2]] <- sp_names
  dimnames(alpha_se)[[3]] <- sp_names

  # create and return the list estimated of r and alpha
  out <- list(
    r_hat = r_hat, alpha_hat = alpha_hat,
    r_se = r_se, alpha_se = alpha_se,
    Sigma_beta = Sigma_beta
  )
  return(out)
}


#################
## weighted linear regression routine for state kernel 
weighted_regression_state_kernel <- function(X,Y,N,S,t,Tmax,theta){
  # compute the vector of state weights
  # d <- sqrt(colSums(((t(logN)[, t]) - t(logN)[, -Tmax])^2))
  d <- sqrt(colSums(((t(N)[, t]) - t(N)[, -Tmax])^2))
  omega <- exp(-theta * d / mean(d))
  
  Y.tilde <- omega * Y # compute the weighted Y matrix
  X.tilde <- omega * X # compute the weighted X matrix
  beta_hat <- solve(t(X.tilde) %*% X.tilde, t(X.tilde)) %*% Y.tilde # compute the estimated r and alpha
  r_hat <- t(beta_hat[1, ]) # store the estimated r
  alpha_hat <- t(beta_hat[-1, ]) # store the estimated alpha
  
  # compute standard error
  sigma_r <- sqrt(colSums((Y.tilde - X.tilde %*% beta_hat)^2) / (Tmax - S))
  XX <- solve(t(X.tilde) %*% X.tilde)
  XXX <- diag(XX)
  r_se <- sigma_r * sqrt(XXX[1])
  alpha_se <- array(NA,dim = c(S,S))
  Sigma_beta <- array(NA, dim = c(S,S+1,S+1))
  for (i in 1:S) {
    alpha_se[i, ] <- sigma_r[i] * sqrt(XXX[-1])
    Sigma_beta[i, , ] <- (sigma_r[i])^2 * XX
  }
  
  out <- list(r_hat = r_hat, alpha_hat = alpha_hat, 
              r_se = r_se, alpha_se = alpha_se,
              Sigma_beta = Sigma_beta)
  
  return(out)
}


#################
## weighted linear regression routine for time kernel 
weighted_regression_time_kernel <- function(X,Y,N,S,t,Tmax,time.p,theta){
  # compute the vector of time weights
  d <- abs((t - 1) - time.p)
  omega <- exp(-theta * d / mean(d))
  
  Y.tilde <- omega * Y # compute the weighted Y matrix
  X.tilde <- omega * X # compute the weighted X matrix
  beta_hat <- solve(t(X.tilde) %*% X.tilde, t(X.tilde)) %*% Y.tilde # compute the estimated r and alpha
  r_hat <- t(beta_hat[1, ]) # store the estimated r
  alpha_hat <- t(beta_hat[-1, ]) # store the estimated alpha
  
  # compute standard error
  sigma_r <- sqrt(colSums((Y.tilde - X.tilde %*% beta_hat)^2) / (Tmax - S))
  XX <- solve(t(X.tilde) %*% X.tilde)
  XXX <- diag(XX)
  r_se <- sigma_r * sqrt(XXX[1])
  alpha_se <- array(NA,dim = c(S,S))
  Sigma_beta <- array(NA, dim = c(S,S+1,S+1))
  for (i in 1:S) {
    alpha_se[i, ] <- sigma_r[i] * sqrt(XXX[-1])
    Sigma_beta[i, , ] <- (sigma_r[i])^2 * XX
  }
  
  out <- list(r_hat = r_hat, alpha_hat = alpha_hat, 
              r_se = r_se, alpha_se = alpha_se,
              Sigma_beta = Sigma_beta)
  
  return(out)
}


LV_map_state_space_cross_validation_mod <- function(N, theta_v = seq(0, 5, 0.05), p = 0.1, mod_XY_mat = TRUE, R_0_index, R_F_index, remove_stiching = F) {
  
  Tmax <- dim(N)[1]
  n <- length(theta_v)
  Tstart <- round(Tmax * p)
  n_species <- dim(N)[2]          # <-- number of species
  n_time <- Tmax - 1              # <-- number of prediction steps

  RMSE <- rep(NA, n)

  # ---- STORAGE: 3D array to hold predicted_Y for every theta, t, species ----
  # Dimensions: [theta, time, species]
  predicted_Y_all <- array(NA, dim = c(n, n_time, n_species))

  # Optional: also store the actual (observed) values for comparison
  observed_Y_all <- array(NA, dim = c(n, n_time, n_species))






  X_ALL <- N
    for (ncut_index in seq(1:dim(X_ALL)[1])) {
          if (!(ncut_index %in% R_0_index)) {
             X_ALL[ncut_index, 1] <-  X_ALL[ncut_index, 1] + 400
          }
        }
  
        #X_ALL <- cbind(rep(1, Tmax - 1), N_mod[-Tmax, ])





  for (i in 1:n) {
    ESS <- 0
    theta <- theta_v[i]
    for (t in Tstart:(Tmax - 1)) {

      N.cut <- N[-((t + 1):Tmax), ]
      logN.cut <- log(N.cut)

      Y <- logN.cut[-1, ] - logN.cut[-t, ]
      X <- cbind(rep(1, t - 1), N.cut[-t, ])

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

      predicted_Y <- X_ALL[t, ] * exp(r_hat + alpha_hat %*% X_ALL[t, ])

      # ---- SAVE predicted_Y and observed Y ----
      # The time index relative to the storage array:
      t_idx <- t - Tstart + 1
      predicted_Y_all[i, t_idx, ] <- as.numeric(predicted_Y)

      
      #observed_Y_all[i, t_idx, ]  <- as.numeric(N[t + 1,])         # drop last element (base R))
      observed_Y_all[i, t_idx, ]  <- as.numeric(X_ALL[t + 1,])         # drop last element (base R))


      #ESS <- ESS + sum((N[t + 1, ] - predicted_Y)^2)

      ESS <- ESS + sum((X_ALL[t + 1, ] - predicted_Y)^2)
    }

    RMSE[i] <- sqrt(ESS / (Tmax - 1))
  }

  theta_o <- theta_v[which.min(RMSE)]
  RMSE_o <- min(RMSE)

  out <- list(
    theta_v = theta_v,
    RMSE = RMSE,
    theta_o = theta_o,
    RMSE_o = RMSE_o,
    predicted_Y_all = predicted_Y_all,   # <-- saved predictions
    observed_Y_all  = observed_Y_all,    # <-- observed values (optional)
    Tstart = Tstart,
    theta_o_idx = which.min(RMSE)        # <-- index to easily extract best theta
  )

  return(out)
}

#################
## cross validation for time kernel 
LV_map_time_cross_validation_mod <- function(N, theta_v = seq(0, 5, 0.05), p = 0.1) {
  Tmax <- dim(N)[1] # number of time steps
  n <- length(theta_v)
  Tstart <- round(Tmax * p)

  RMSE <- rep(NA, n)

  for (i in 1:n) {
    ESS <- 0 # initialize the error sum of squares to zero
    theta <- theta_v[i]

    for (t in Tstart:(Tmax - 1)) {
      time.p <- seq(1, t - 1)
      N.cut <- N[-((t + 1):Tmax), ]
      logN.cut <- log(N.cut)

#===========procedure to modify ncut and log ncut to add the real time series of herbovore
      
      N.cut_mod <- N.cut ## this is not a real matrix per se
     

 for (ncut_index in seq(1:dim(N.cut)[1])){
      if (!(ncut_index %in%  R_0_index)){  #r0 index is defined outside the loop
        N.cut_mod[ncut_index, 1] <- N.cut_mod[ncut_index, 1]+400 ##so if you are not in the index of initial youll have +400
      }
    }
      
     logN.cut_mod <- log(N.cut_mod)  ##right side of the rest

# now here we rebuuld the X and Y with this auxiliary matrix 
     Y <- logN.cut[-1, ] - logN.cut_mod[-t, ]
     X <- cbind(rep(1, t - 1), N.cut_mod[-t, ])  #explanatory variable 
    
      
       
  #and the last detail is that in X and Y i repeated the last value! (less problematic than artificil stihci)
      
       for (ncut_index in seq(1:dim(N.cut_mod)[1])){
      if (ncut_index %in%  R_F_index){  #
        X[ncut_index, 2] <- X[ncut_index-1, 2]
        Y[ncut_index, 1] <- Y[ncut_index-1, 1]##
      }
    }
      
      
      #original
      #Y <- logN.cut[-1, ] - logN.cut[-t, ]
      #X <- cbind(rep(1, t - 1), N.cut[-t, ])
#end of procedure 
#=================================================


      d <- abs((t - 1) - time.p)
      omega <- exp(-theta * d / mean(d))
      Y.tilde <- omega * Y
      X.tilde <- omega * X
      beta_hat <- solve(t(X.tilde) %*% X.tilde, t(X.tilde)) %*% Y.tilde
      r_hat <- t(t(beta_hat[1, ]))
      alpha_hat <- t(beta_hat[-1, ])

      ESS <- ESS + sum((N[t + 1, ] - N[t, ] * exp(r_hat + alpha_hat %*% N[t, ]))^2)
    }

    RMSE[i] <- sqrt(ESS / (Tmax - 1))
  }

  # smoother <- smooth.spline(theta_v, RMSE)
  # theta_o <- smoother$x[which.min(smoother$y)]
  # RMSE_o <- min(smoother$y)

  theta_o <- theta_v[which.min(RMSE)]
  RMSE_o <- min(RMSE)

  out <- list(
    theta_v = theta_v, RMSE = RMSE,
    theta_o = theta_o, RMSE_o = RMSE_o
  )

  return(out)
}
