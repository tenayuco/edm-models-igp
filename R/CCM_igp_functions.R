multisp_CCM_igp <- function(x, niter, force_embedding = FALSE, E_A_real, E_B_real){
  
  #tun this line if you wanna see examples
  #x <-  DATA |> dplyr::filter(enem== "ma+ol")
  #1. Calculate optimal E



  list_CCM <- list() #created by emilio
  #calculating the maximum E (with E< m-1) where m is the minum step of time series  
  x_count <- x |> 
    dplyr::mutate(contador =1)|> 
    dplyr::group_by(block) |> 
    dplyr::summarise(m = sum(contador))

  maxE <- (min(x_count$m)/2)  #following weak whitney theorem 
  #maxE <- 4
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
    
    list_CCM$Emat <- Emat


        #E_A <- which.max(na.omit(Emat[,1])) 
    #E_B <- which.max(na.omit(Emat[,2]))

  ##new procedure to get the min embedding dime that explains at least 90% of the max possible 
    
    max_pho_E_A <- max(na.omit(Emat[,1])) 
    max_pho_E_B <- max(na.omit(Emat[,2]))
    

    min_good_pho_E_A <- min(as.data.frame(na.omit(Emat[,1]))|> dplyr::filter(na.omit(Emat[,1]) >= 0.8*max_pho_E_A))
    min_good_pho_E_B <- min(as.data.frame(na.omit(Emat[,2]))|> dplyr::filter(na.omit(Emat[,2]) >= 0.8*max_pho_E_B))





  # now we select from the original matrix. 
    
    E_A <- which((Emat[,1])== min_good_pho_E_A ) +1  #the +1 is only beacuse the E mat starts at 1
     E_B <- which((Emat[,2])== min_good_pho_E_B) +1

    ## now, here we force the embedding if need (bit then you put if for each nati ene)
    if (force_embedding ==TRUE){
      E_A = E_A_real
      E_B = E_B_real
    }
    
    
    print(paste0("emant", E_A, E_B))

    #export Ea, eb
    
    #2. Check data for nonlinear signal that is not dominated by noise
#Checks whether predictive ability of processes declines with
#increasing time distance
#See manuscript and R code for details
      
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
        CCM_boot_B <- NA
        CCM_significance_test <- multispatialCCM::ccmtest(CCM_boot_A,CCM_boot_A)
        CCM_significance_test[2] <- 1
      }
      if(summary(lm(signal_A_out$predatout$rho~signal_A_out$predatout$predstep))$coeff[2,1]>=0 &
         summary(lm(signal_B_out$predatout$rho~signal_B_out$predatout$predstep))$coeff[2,1]<0){
        CCM_boot_A <- NA

        CCM_boot_B <- multispatialCCM::CCM_boot(Bccm, Accm, E_B, tau=1, iterations=niter)
        CCM_significance_test <- multispatialCCM::ccmtest(CCM_boot_B,CCM_boot_B)
        CCM_significance_test[1] <- 1
      }
      if(summary(lm(signal_A_out$predatout$rho~signal_A_out$predatout$predstep))$coeff[2,1]>=0 &
         summary(lm(signal_B_out$predatout$rho~signal_B_out$predatout$predstep))$coeff[2,1]>=0){
                CCM_boot_A <- NA
        CCM_boot_B <- NA

        CCM_significance_test <- c(1,1)
      }
    }

    list_CCM$CCM_boot_A <-  CCM_boot_A
    list_CCM$CCM_boot_B <-  CCM_boot_B
    list_CCM$CCM_significance_test <-  CCM_significance_test

      
    result <- data.frame(a_cause_b=CCM_significance_test[1], # a = species or X, b= pressure or Y
                           b_cause_a=CCM_significance_test[2])
    names(result) <- c(paste0("X_cause_Y"),
                         paste0("Y_cause_X"))
    
    result$E_X <- E_A
   result$E_Y <- E_B
    
  }else{result <- data.frame(X_cause_Y=NA,Y_cause_X=NA)}
  
  list_CCM$result <- result
return(list_CCM)
}



data_ts_CCM <-  function(data_pred){

TIME_SERIES_ALL <- data_pred |>
  tidyr::pivot_longer(cols=c(X, Y, R), names_to = "variable", values_to = "individuals")|>
  ggplot(aes(x = week, y = individuals)) +
    geom_line(
      aes(color= as.factor(variable), group = as.factor(interaction(block, variable))),
      size = 0.5
    ) +
    geom_point(aes(color = variable), size = 1) +
    facet_wrap(~enem, scales = "free_y") +
    theme_minimal()

  return(TIME_SERIES_ALL)
}



plot_rho_CCM <-  function(rho_data){

RHO_PLOT <- rho_data |>
  ggplot(aes(x = lobs, y = rho)) +
  geom_line(aes(color = as.factor(variable)), size = 0.5) +
  geom_ribbon(aes(ymin = rho - 1.96 * rho_sdev, 
                  ymax = rho + 1.96 * rho_sdev, 
                  fill = as.factor(variable)), 
              alpha = 0.2, color = NA) +
  geom_point(aes(color = as.factor(variable))) +
  facet_wrap(~enem) +
  theme_minimal()
  
  
  return(RHO_PLOT)
}





embedding_plotter <-  function(embed_DF){


PLOT_EMBED <- embed_DF |>
  dplyr::rename(X=A, Y=B)|>
  tidyr::pivot_longer(cols=c(X, Y), names_to = "variable", values_to = "pho_embed")|>
  ggplot(aes(x =embDim +1, y =pho_embed )) +
    geom_line(
      aes(color= as.factor(variable), group = as.factor(interaction(variable))),
      size = 0.5
    ) +
    geom_point(aes(color = variable), size = 1) +
    facet_wrap(~enem) +
    theme_minimal()

  return(PLOT_EMBED)
}