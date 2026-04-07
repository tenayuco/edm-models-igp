

###do the functions as basic as you can, to readapt them. Then
## in the analyss 

ts_solve <- function(model_list, variable_par = NULL){
  
  #mod_parms, mod_times, mod_init, facet_1, minPar,  maxPar, resolution){
  


  ## mvariable ara 

  out<-deSolve::ode(func=model_list[["igp_model"]], y=model_list[["igp_init"]], times=model_list[["igp_times"]],parms=model_list[["igp_parms"]]) |> 
      as.data.frame()
  
  out$parameterValue <- variable_par
    
  return(out)
}


