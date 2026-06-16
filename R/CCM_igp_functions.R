##this function... 


embedding_ccm <- function(x, min_per= 0.8){
  

  #1. Calculate optimal E

  list_embe_ccm <- list()

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
    
    list_embe_ccm$Emat <- Emat


  ##new procedure to get the min embedding dime that explains at least 80% of the max possible 
    
    max_pho_E_A <- max(na.omit(Emat[,1])) 
    max_pho_E_B <- max(na.omit(Emat[,2]))
    

    min_good_pho_E_A <- min(as.data.frame(na.omit(Emat[,1]))|> dplyr::filter(na.omit(Emat[,1]) >= min_per*max_pho_E_A))
    min_good_pho_E_B <- min(as.data.frame(na.omit(Emat[,2]))|> dplyr::filter(na.omit(Emat[,2]) >= min_per*max_pho_E_B))


  # now we select from the original matrix. 
    
    E_A <- which((Emat[,1])== min_good_pho_E_A ) +1  #the +1 is only beacuse the E mat starts at 1
     E_B <- which((Emat[,2])== min_good_pho_E_B) +1

    ## now, here we force the embedding if need (bit then you put if for each nati ene)
    #if (force_embedding ==TRUE){
     # E_A = E_A_real
     # E_B = E_B_real
    #}
   
    
    list_embe_ccm$E_A <- E_A
    list_embe_ccm$E_B <- E_B

  }
return(list_embe_ccm)

}



multisp_CCM_igp <- function(x, niter, E_A_used, E_B_used){
  


  list_CCM <- list() #created by emilio
  #calculating the maximum E (with E< m-1) where m is the minum step of time series  
  
  x <- droplevels(x)

  if(length(levels(as.factor(x$block)))>1 & nrow(x)>=50){
    
    x_multi_ccm <- plyr::ddply(x, plyr::.(block), .fun=tibble::add_row, .parallel = F)
    Accm <- x_multi_ccm$X[-nrow(x_multi_ccm)]
    Bccm <- x_multi_ccm$Y[-nrow(x_multi_ccm)]
    
    


    #export Ea, eb
    
    #2. Check data for nonlinear signal that is not dominated by noise
#Checks whether predictive ability of processes declines with
#increasing time distance
#See manuscript and R code for details
      
    E_A <- E_A_used
    E_B <- E_B_used
    
    
    if(length(E_A)>0 & length(E_B)>0){
      signal_A_out <- multispatialCCM::SSR_check_signal(A=Accm, E=E_A, tau=1, predsteplist=1:10)
      signal_B_out <- multispatialCCM::SSR_check_signal(A=Bccm, E=E_B, tau=1, predsteplist=1:10)
      
      list_CCM$signal_A_out <-  signal_A_out
      list_CCM$signal_B_out <-  signal_B_out

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



surrogater_df <- function(data_pred){
  surro_df <- data.frame()


  surro_df <- data_pred |> 
  dplyr::group_by(block, enem) |> 
  dplyr::mutate(week = sample(week, dplyr::n(), replace = FALSE)) |>  # shuffles within each block #gives the curreng grpup size dplyr::n()
  dplyr::ungroup() |> 
  dplyr::select(block, R, Y, X, week, enem)

 # idx_shuffle = sample(1:nrow(data), nrow(data), replace = FALSE) # permutes the row order of the dataset (shuffles without replacement).
 # surro_df = data[idx_shuffle, c("block", "R", "Y", "X")] #i desorganize within block and natr
   
 #surro_df <- merge(surro_df, data[c("block", "week")], by = "block")

  return(surro_df)
  }




### functions to extract and plot

##embedding dimension 
##function to extract the embedein

embed_df_sum <- function(list_CCM){

embed_DF <- data.frame()

for (enem in names(list_CCM)){
  embed_temp <-  as.data.frame(list_CCM[[enem]]$list_embed$Emat)
  embed_temp$enem <- enem
  embed_temp$embDim <- seq(1:nrow(embed_temp))
  embed_temp$E_A <- list_CCM[[enem]]$list_embed$E_A
  embed_temp$E_B<- list_CCM[[enem]]$list_embed$E_B
  embed_DF <- rbind(embed_DF, embed_temp)

}
  
return(embed_DF)  
}



embedding_plotter <-  function(embed_DF){


PLOT_EMBED <- embed_DF |>
  dplyr::rename(X=A, Y=B, E_X=E_A, E_Y=E_B)|>

  tidyr::pivot_longer(cols=c(X, Y), names_to = "variable", values_to = "pho_embed")|>
  ggplot(aes(x =embDim +1, y =pho_embed )) +
    geom_line(
      aes(color= as.factor(variable), group = as.factor(interaction(variable))),
      size = 0.5
    ) +
    geom_point(aes(color = variable), size = 1) +
  
   # Add vertical lines for E_A and E_B
    geom_vline(aes(xintercept = E_X, color = "E_X"),
      linetype = "dashed",
      size = 0.5
    ) +
    geom_vline(aes(xintercept = E_Y, color = "E_Y"),
      linetype = "dashed",
      size = 0.5)+
  
  
    facet_wrap(~enem) +
    theme_minimal()

  return(PLOT_EMBED)
}



#################get tthe singi


ccm_sp_test <- function(list_CCM){

#this add a column to the data frame of the enemi, and will bind them 
ccm_sp_igp <- data.frame()

for (enemies in names(list_CCM)){
  list_CCM[[enemies]]$list_ccm$result$enem <- enemies
  ccm_sp_igp <-rbind(ccm_sp_igp, list_CCM[[enemies]]$list_ccm$result)  
}
  
  return(ccm_sp_igp)
}


#get the predicitvie steps



rho_preSteps <- function(list_CCM){

#this add a column to the data frame of the enemi, and will bind them 
pred_steps <- data.frame()

for (enem in names(list_CCM)){
  pred_temp_A <-  as.data.frame(list_CCM[[enem]]$list_ccm$signal_A_out$predatout)
  pred_temp_B <-  as.data.frame(list_CCM[[enem]]$list_ccm$signal_B_out$predatout)
  pred_temp_A$variable <- "X"
   pred_temp_B$variable <- "Y"


  pred_temp <- rbind(pred_temp_A,  pred_temp_B)

  pred_temp$enem <- enem

  pred_steps <- rbind(pred_steps, pred_temp)
}
  
  return(pred_steps)
}



#-------------plot

#---------------rho

##extreact the rho values for each enem and put ir into a data frame 

rho_data_sum <- function(list_CCM){

RHO_DATA <- data.frame()

for (enemies in names(list_CCM)){
  print(enemies)
  
  if(is.na(list_CCM[[enemies]]$list_ccm$CCM_boot_A)[[1]]){
    rho_X <- 0
    rho_X_sdev <- 0
    lobsX <- 0
    print("case1")
  } else {
    rho_X <- list_CCM[[enemies]]$list_ccm$CCM_boot_A$rho
    rho_X_sdev <- list_CCM[[enemies]]$list_ccm$CCM_boot_A$sdevrho
    lobsX <- list_CCM[[enemies]]$list_ccm$CCM_boot_A$Lobs
  }
  
  if(is.na(list_CCM[[enemies]]$list_ccm$CCM_boot_B)[[1]]){
    rho_Y <- 0
    rho_Y_sdev <- 0
    lobsY <- 0
    print("case2")
  } else {
    rho_Y <- list_CCM[[enemies]]$list_ccm$CCM_boot_B$rho
    rho_Y_sdev <- list_CCM[[enemies]]$list_ccm$CCM_boot_B$sdevrho
    lobsY <- list_CCM[[enemies]]$list_ccm$CCM_boot_B$Lobs
  }
  
  temp_data_X <- data.frame(
    lobs = lobsX,
    rho = rho_X,
    rho_sdev = rho_X_sdev,
    variable = "X_cause_Y"
  )

  temp_data_Y <- data.frame(
    lobs = lobsY,
    rho = rho_Y,
    rho_sdev = rho_Y_sdev,
    variable = "Y_cause_X"
  )

  temp_data <- rbind(temp_data_X, temp_data_Y)
  temp_data$enem <-  enemies

  RHO_DATA<- rbind(RHO_DATA, temp_data)
}

return(RHO_DATA)

}




multisp_CCM_old_igp <- function(x, niter, force_embedding = FALSE, E_A_real, E_B_real){
  
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
      
      list_CCM$signal_A_out <-  signal_A_out
      list_CCM$signal_B_out <-  signal_B_out

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


########rho plot against surrogates

plot_rho_data_surro_CCM <-  function(rho_data_surro, ccm_test){

  cols_rho <- c("random" = "darkgray", "data" = "darkred")

ccm_test_mod <- ccm_test |> 
  tidyr::pivot_longer(cols=c(X_cause_Y, Y_cause_X), names_to = "variable", values_to = "pvalue")


full_data <- rho_data_surro |> 
  dplyr::inner_join(ccm_test_mod, by = c("variable", "enem"))



  
RHO_PLOT <- full_data |>
  ggplot(aes(x = lobs, y = rho)) +

  
  geom_line(aes(color = as.factor(cat)), size = 0.5) +
  geom_ribbon(aes(ymin = rho - 1.96 * rho_sdev, 
                  ymax = rho + 1.96 * rho_sdev, 
                  fill = as.factor(cat)), 
              alpha = 0.2, color = NA) +
  
  geom_text(aes(x = 130, y = 0.8, label = paste("pvalue_clark =",  round(pvalue, digits = 3), sep = "")))+
  #geom_point(aes(color = as.factor(cat))) +
  facet_wrap(enem~variable) +
  theme_minimal()+
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        panel.spacing = unit(1, "lines"),
        # Increase all text sizes
        text = element_text(size = 14),  # Base text size
        axis.title = element_text(size = 12),  # Axis titles
        axis.text = element_text(size = 12),   # Axis tick labels
        strip.text = element_text(size = 12),  # Facet labels
        legend.text = element_text(size = 12), # Legend text
        legend.title = element_text(size = 12)) +# Le

  scale_color_manual(values =  cols_rho)+
    scale_fill_manual(values =  cols_rho)+
  labs(color ="Type", fill="Type")

  
  
  return(RHO_PLOT)
}





rho_pred_plotter <-  function(predSteps_DF){


PLOT_STEPS <- predSteps_DF |>
  ggplot(aes(x =predstep, y =rho)) +
    geom_line(
      aes(color= as.factor(variable), group = as.factor(interaction(variable))),
      size = 0.5
    ) +
    geom_point(aes(color = variable), size = 1) +
    facet_wrap(~enem) +
    theme_minimal()

  return(PLOT_STEPS)
}



plot_ts_emb_step_rho_per_enem <- function(rho_data_surro, ccm_test, data_ts, predSteps_DF){


  for (enem in unique()){}















}