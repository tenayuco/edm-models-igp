



run_ccm_per_enem <-  function(data_prep, niter, min_per=0.8, use_surrogate =F){
  
full_list_ccm <- list()

 ##NOW I HAVE TO SEND THE REAL EMBEDDING PER ENEM 
  
for (enemy in unique(data_prep$enem)) {
 
  data_enem <- data_prep |> 
  dplyr::filter(enem == enemy)
  print(enemy)


  ##if surrogate ==TRUE, uses the surrogate 

  


  ##1. calculate the embedding dimensions

  list_embed <- embedding_ccm(x= data_enem, min_per= min_per)

###2. run the surro
  
  ##3. run the CCM with the data

  ##used embed

  E_A_used <-  list_embed$E_A
  E_B_used <-  list_embed$E_B


## 
  
  if(use_surrogate ==TRUE){
  data_enem <-  surrogater_df(data_enem)}


  list_ccm <- multisp_CCM_igp(x = data_enem, niter = niter, E_A_used = E_A_used, E_B_used = E_B_used)

  ## 4. run the CCM with the surrogates 



 # Create a nested list with enemy name and results
  full_list_ccm[[as.character(enemy)]] <- list(
    enem = enemy,
    list_embed = list_embed,
    list_ccm = list_ccm
  )


}
  return(full_list_ccm)

}


run_ccm_surro_per_enem <-  function(data_prep, niter, min_per=0.8, num_surro=10){
  
full_list_ccm <- list()

 ##NOW I HAVE TO SEND THE REAL EMBEDDING PER ENEM 
  
for (enemy in unique(data_prep$enem)) {
 
  data_enem <- data_prep |> 
  dplyr::filter(enem == enemy)
  print(enemy)


  ##1. calculate the embedding dimensions of the real data (to have it here)

  list_embed <- embedding_ccm(x= data_enem, min_per= min_per)

###2. run the surro
  
  data_surro <-  surrogater_df(data_enem)
  
  ##3. run the CCM with the data

  ##used embed

  E_A_used <-  list_embed$E_A
  E_B_used <-  list_embed$E_B


  ## run the surrogate, and the ccm per surrog

  list_ccm <- multisp_CCM_igp(x = data_surro, niter = niter, E_A_used = E_A_used, E_B_used = E_B_used)

  ## 4. run the CCM with the surrogates 



 # Create a nested list with enemy name and results
  full_list_ccm[[as.character(enemy)]] <- list(
    enem = enemy,
    list_embed = list_embed,
    list_ccm = list_ccm
  )


}
  return(full_list_ccm)

}



















run_total_enemies_CCM <-  function(data_prep, niter, force_embedding = FALSE, df_embedding_CCM= NULL){
  
full_list_ccm <- list()

 ##NOW I HAVE TO SEND THE REAL EMBEDDING PER ENEM 
  
for (enemy in unique(data_prep$enem)) {

  if (force_embedding== T){
  df_embedding_CCM_enem <- df_embedding_CCM|> 
    dplyr::filter(enem == enemy)
  }
  data_enem <- data_prep |> 
    dplyr::filter(enem == enemy)
  print(enemy)
  list_ccm_enem <- multisp_CCM_igp(x = data_enem, niter = niter, force_embedding = force_embedding, E_A_real=df_embedding_CCM_enem[["E_A"]], E_B_real=df_embedding_CCM_enem[["E_B"]])
  
  # Create a nested list with enemy name and results
  full_list_ccm[[as.character(enemy)]] <- list(
    enem = enemy,
    list_ccm_enem = list_ccm_enem
  )

}
  return(full_list_ccm)

}
  


##extreact the rho values for each enem and put ir into a data frame 

rho_data_sum <- function(list_CCM){

RHO_DATA <- data.frame()

for (enemies in names(list_CCM)){
  print(enemies)
  
  if(is.na(list_CCM[[enemies]]$list_ccm_enem$CCM_boot_A)[[1]]){
    rho_X <- 0
    rho_X_sdev <- 0
    lobsX <- 0
    print("case1")
  } else {
    rho_X <- list_CCM[[enemies]]$list_ccm_enem$CCM_boot_A$rho
    rho_X_sdev <- list_CCM[[enemies]]$list_ccm_enem$CCM_boot_A$sdevrho
    lobsX <- list_CCM[[enemies]]$list_ccm_enem$CCM_boot_A$Lobs
  }
  
  if(is.na(list_CCM[[enemies]]$list_ccm_enem$CCM_boot_B)[[1]]){
    rho_Y <- 0
    rho_Y_sdev <- 0
    lobsY <- 0
    print("case2")
  } else {
    rho_Y <- list_CCM[[enemies]]$list_ccm_enem$CCM_boot_B$rho
    rho_Y_sdev <- list_CCM[[enemies]]$list_ccm_enem$CCM_boot_B$sdevrho
    lobsY <- list_CCM[[enemies]]$list_ccm_enem$CCM_boot_B$Lobs
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


##embedding dimension 
##function to extract the embedein

embed_df_sum <- function(list_CCM){

embed_DF <- data.frame()

for (enem in names(list_CCM)){
  embed_temp <-  as.data.frame(list_CCM[[enem]]$list_ccm_enem$Emat)
  embed_temp$enem <- enem
  embed_temp$embDim <- seq(1:nrow(embed_temp))
  embed_DF <- rbind(embed_DF, embed_temp)

}
  
return(embed_DF)  
}



ccm_sp_test <- function(list_CCM){

#this add a column to the data frame of the enemi, and will bind them 
ccm_sp_igp <- data.frame()

for (enemies in names(list_CCM)){
  list_CCM[[enemies]]$list_ccm_enem$result$enem <- enemies
  ccm_sp_igp <-rbind(ccm_sp_igp, list_CCM[[enemies]]$list_ccm_enem$result)  
}
  
  return(ccm_sp_igp)
}



rho_preSteps <- function(list_CCM){

#this add a column to the data frame of the enemi, and will bind them 
pred_steps <- data.frame()

for (enem in names(list_CCM)){
  pred_temp_A <-  as.data.frame(list_CCM[[enem]]$list_ccm_enem$signal_A_out$predatout)
  pred_temp_B <-  as.data.frame(list_CCM[[enem]]$list_ccm_enem$signal_B_out$predatout)
  pred_temp_A$variable <- "X simplex"
    pred_temp_B$variable <- "Y simplex"


  pred_temp <- rbind(pred_temp_A,  pred_temp_B)

  pred_temp$enem <- enem

  pred_steps <- rbind(pred_steps, pred_temp)
}
  
  return(pred_steps)
}