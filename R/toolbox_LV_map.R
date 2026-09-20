### LV-map toolbox

#################
## Fit the LV-map 
LV_map_mod <- function(N, theta = 0, kernel = 'state', R_0_index, R_F_index, remove_stiching=F, mod_XY_mat = FALSE) {
  S <- dim(N)[2] # number of species
  Tmax <- dim(N)[1] # number of time steps
  sp_names <- colnames(N) # species names
  n_species <- dim(N)[2]          # <-- number of species

  r_hat <- array(NA, dim = c(Tmax - 1, S)) # array to store the fitted intrinsic growth rate
  alpha_hat <- array(NA, dim = c(Tmax - 1, S, S)) # array to store the fitted per capita interaction strengths

  r_se <- array(NA, dim = c(Tmax - 1, S)) # array to store the SE of fitted intrinsic growth rate
  alpha_se <- array(NA, dim = c(Tmax - 1, S, S)) # array to store the SE of fitted per capita interaction strengths
  Sigma_beta <- array(NA, dim = c(Tmax - 1, S, S+1, S+1)) # array to store the variance-covariance matrix of 
  # fitted intrinsic growth rate and per capita interaction strengths
  time.p <- seq(1:(Tmax-1)) #create vector of time
  

N_ALL_MOD <- N
  
  if (mod_XY_mat == TRUE & n_species>2) {
  for (n_index in seq(1:dim(N_ALL_MOD)[1])) {
      if (!(n_index %in%  R_0_index)){  #r0 index is defined outside the loop
        N_ALL_MOD[n_index, 1] <- N_ALL_MOD[n_index, 1]+400 ##so if you are not in the index of initial youll have +400
      }
    }
  }
  
  
logN <- log(N)
logN_mod <- log(N_ALL_MOD)  ##right side of the rest

    
Y <- logN[-1, ] - logN_mod[-Tmax, ]  ##NOTE the rest is not symmetric
X <- cbind(rep(1, Tmax - 1), N_ALL_MOD[-Tmax, ]) # create the explanatory variables matrix

  
  #and the last detail is that in X and Y i repeated the last value! (less problematic than doing an artificial stiching)    
     
  #for now, im not changin this, to see if i can get the prev results 
 #if(remove_stiching == T){
  #for (n_index in seq(1:dim(N_mod)[1])){ 
   #   if (n_index %in%  R_F_index){  #
    #    X[n_index, 2] <- X[n_index-1, 2]
     #   Y[n_index, 1] <- Y[n_index-1, 1]##
      #}
    #}
  #}
  
#end of procedure 
#==================================================================================================
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


#this it to run it here, but you have to prerun the N and the values of rindex
#N <- N_list_sim[[1]]

LV_map_state_space_cross_validation_mod <- function(N, theta_v = seq(0, 3, 0.01), p = 0.1, mod_XY_mat = TRUE, R_0_index, R_F_index, remove_stiching = F) {
  
  Tmax <- dim(N)[1]
  n <- length(theta_v)
  Tstart <- round(Tmax * p)
  n_species <- dim(N)[2]          # <-- number of species
  n_time <- Tmax - 1              # <-- number of prediction steps

##we create an empty list that will be added to cv_list
  fitting <- list()
  RMSE <- rep(NA, n)



  ##I first create a modified N that will be the base
## i check if the num of spec is ore than2 
  #just to have the modified matrix for prediction 
N_ALL_MOD <- N  #always a modified matrix that can be used or not 
    
if (mod_XY_mat == TRUE & n_species>2) {
  for (n_index in seq(1:dim(N_ALL_MOD)[1])) {
         if (!(n_index %in% R_0_index)) {
             N_ALL_MOD[n_index, 1] <-  N_ALL_MOD[n_index, 1] + 400
          }
        }
      }

  ##loop over the theta
  for (i in 1:n) {
    ESS <- 0
    theta <- theta_v[i]

    for (t in Tstart:(Tmax - 1)) {


      N.cut <- N[-((t + 1):Tmax), ]
      logN.cut <- log(N.cut)

#modified (have the index already!)
  #if the species are the same as the nor modified, but still important to keep both 
      #for when they are different
      N.cut_mod <- N_ALL_MOD[-((t + 1):Tmax), ]
      logN.cut_mod <- log(N.cut_mod)


      Y <- logN.cut[-1, ] - logN.cut_mod[-t, ]  ##NOTE the rest is not symmetric
      X <- cbind(rep(1, t - 1), N.cut_mod[-t, ])


      #if (remove_stiching == T) {
       #   for (n_index in seq(1:dim(N.cut[-t, ])[1])) {
        #    if (n_index %in% R_F_index) {
         #     X[n_index, 2] <- X[n_index - 1, 2]
          #    Y[n_index, 1] <- Y[n_index - 1, 1]
           # }
          #}
        #}
      
      #==============================================================
##this is not importat the source 
      d <- sqrt(colSums(((t(N.cut)[, t - 1]) - t(N.cut)[, -t])^2))
      omega <- exp(-theta * d / mean(d))

      Y.tilde <- omega * Y
      X.tilde <- omega * X
      beta_hat <- solve(t(X.tilde) %*% X.tilde, t(X.tilde)) %*% Y.tilde
      r_hat <- t(t(beta_hat[1, ]))
      alpha_hat <- t(beta_hat[-1, ])


      #it does the predicted with the modifed values, that it is were it comes from 
      predicted_Y <- N_ALL_MOD[t, ] * exp(r_hat + alpha_hat %*% N_ALL_MOD[t, ]) #the beggining
      observed_Y <- N[t + 1,]  #the end of each
      # ---- SAVE predicted_Y and observed Y ----
           # drop last element (base R))


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
    Tstart = Tstart
  )

  return(out)
  }


#################
## cross validation for time kernel 
#DOES NOT WORK
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
