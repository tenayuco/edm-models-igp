


run_total_enemies_CCM <-  function(data_prep, niter){
  
full_list_ccm <- list()

for (enemy in unique(data_prep$enem)) {
  data_enem <- data_prep |> 
    dplyr::filter(enem == enemy)
  print(enemy)
  list_ccm_enem <- multisp_CCM_igp(x = data_enem, niter = niter)
  
  # Create a nested list with enemy name and results
  full_list_ccm[[as.character(enemy)]] <- list(
    enem = enemy,
    list_ccm_enem = list_ccm_enem
  )

}
  return(full_list_ccm)

}
  