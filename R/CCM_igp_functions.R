multisp_CCM_igp <- function(x, niter){
  

  #calculating the maximum E (with E< m-1) where m is the minum step of time series  
  x_count <- x |> 
    dplyr::mutate(contador =1)|> 
    dplyr::group_by(block) |> 
    dplyr::summarise(m = sum(contador))
  
  maxE <- min(x_count$m-1)
  print(paste0("maxE", maxE))
  
  x <- droplevels(x)

  if(length(levels(as.factor(x$block)))>1 & nrow(x)>=50){
    
    x_multi_ccm <- plyr::ddply(x, plyr::.(block), .fun=tibble::add_row, .parallel = F)
    Accm <- x_multi_ccm$X[-nrow(x_multi_ccm)]
    Bccm <- x_multi_ccm$Y[-nrow(x_multi_ccm)]
    
    
    Emat <- matrix(nrow=maxE-1, ncol=2)
      
    colnames(Emat) <- c("A", "B")
      
    for(E in 2:maxE) {
      Emat[E-1,"A"] <- multispatialCCM::SSR_pred_boot(A=Accm, E=E, predstep=1, tau=1)$rho 
      Emat[E-1,"B"] <- multispatialCCM::SSR_pred_boot(A=Bccm, E=E, predstep=1, tau=1)$rho
    }
      
    E_A <- which.max(na.omit(Emat[,1]))+1
    E_B <- which.max(na.omit(Emat[,2]))+1

    print(paste0("emant", E_A, E_B))
      
    if(length(E_A)>0 & length(E_B)>0){
      signal_A_out <- multispatialCCM::SSR_check_signal(A=Accm, E=E_A, tau=1, predsteplist=1:10)
      signal_B_out <- multispatialCCM::SSR_check_signal(A=Bccm, E=E_B, tau=1, predsteplist=1:10)
        
      if(summary(lm(signal_A_out$predatout$rho~signal_A_out$predatout$predstep))$coeff[2,1]<0 &
       summary(lm(signal_B_out$predatout$rho~signal_B_out$predatout$predstep))$coeff[2,1]<0){
          
        CCM_boot_A <- multispatialCCM::CCM_boot(Accm, Bccm, E_A, tau=1, iterations=niter)
        CCM_boot_B <- multispatialCCM::CCM_boot(Bccm, Accm, E_B, tau=1, iterations=niter)
        CCM_significance_test <- multispatialCCM::ccmtest(CCM_boot_A,CCM_boot_B)
      }
      if(summary(lm(signal_A_out$predatout$rho~signal_A_out$predatout$predstep))$coeff[2,1]<0 &
         summary(lm(signal_B_out$predatout$rho~signal_B_out$predatout$predstep))$coeff[2,1]>=0){
          
        CCM_boot_A <- multispatialCCM::CCM_boot(Accm, Bccm, E_A, tau=1, iterations=niter)
        CCM_significance_test <- multispatialCCM::ccmtest(CCM_boot_A,CCM_boot_A)
        CCM_significance_test[2] <- 1
      }
      if(summary(lm(signal_A_out$predatout$rho~signal_A_out$predatout$predstep))$coeff[2,1]>=0 &
         summary(lm(signal_B_out$predatout$rho~signal_B_out$predatout$predstep))$coeff[2,1]<0){
        
        CCM_boot_B <- multispatialCCM::CCM_boot(Bccm, Accm, E_B, tau=1, iterations=niter)
        CCM_significance_test <- multispatialCCM::ccmtest(CCM_boot_B,CCM_boot_B)
        CCM_significance_test[1] <- 1
      }
      if(summary(lm(signal_A_out$predatout$rho~signal_A_out$predatout$predstep))$coeff[2,1]>=0 &
         summary(lm(signal_B_out$predatout$rho~signal_B_out$predatout$predstep))$coeff[2,1]>=0){
        CCM_significance_test <- c(1,1)
      }
    }
      
    result <- data.frame(a_cause_b=CCM_significance_test[1], # a = species or X, b= pressure or Y
                           b_cause_a=CCM_significance_test[2])
    names(result) <- c(paste0("X_cause_Y"),
                         paste0("Y_cause_X"))
    
    result$E_X <- E_A
   result$E_Y <- E_B
    
  }else{result <- data.frame(X_cause_Y=NA,Y_cause_X=NA)}
  
return(result)
}