
##thi sis the only function here


run_ccm_per_enem <-  function(data_prep, niter, min_per=0.8){
  
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










run_ccm_per_enem_surro <-  function(data_surro, niter, list_data){
  
full_list_ccm <- list()

 ##NOW I HAVE TO SEND THE REAL EMBEDDING PER ENEM 
  
for (enemy in unique(data_surro$enem)) {
 
  data_enem <- data_surro |> 
  dplyr::filter(enem == enemy)
  print(enemy)


E_A_used <-  list_data[[enemy]]$list_embed$E_A
E_B_used <-  list_data[[enemy]]$list_embed$E_B



  ##if surrogate ==TRUE, uses the surrogate 

###2. run the surro
  
  ##3. run the CCM with the data

  ##used embed


  list_ccm <- multisp_CCM_igp(x = data_enem, niter = niter, E_A_used = E_A_used, E_B_used = E_B_used)

  ## 4. run the CCM with the surrogates 



 # Create a nested list with enemy name and results
  full_list_ccm[[as.character(enemy)]] <- list(
    enem = enemy,
    list_embed = list_data[[enemy]]$list_embed,  ## in this case is the same as the data
    list_ccm = list_ccm
  )


}
  return(full_list_ccm)

}
