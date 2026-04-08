

###do the functions as basic as you can, to readapt them. Then
## in the analyss 

ode_solve <- function(model_list, variable_par = NULL){
  
  #mod_parms, mod_times, mod_init, facet_1, minPar,  maxPar, resolution){
  


  ## mvariable ara 

  out<-deSolve::ode(func=model_list[["igp_model_cont"]], y=model_list[["igp_init"]], times=model_list[["igp_times_cont"]],parms=model_list[["igp_parms"]]) |> 
      as.data.frame()
  
  out$parameterValue <- variable_par
    
  return(out)
}



# Solve with discrete method
# Note: dt must match the time step in times (or be consistent)

ode_solve_discrete <- function(model_list, variable_par = NULL){

out <- deSolve::ode(
  y = model_list[["igp_init"]],
  times =model_list[["igp_times_disc"]],
  func = model_list[["igp_model_disc"]],
  parms =model_list[["igp_parms"]],
  method = "iteration",
  dt = 0.01) |> 
  as.data.frame() # This should equal times[2] - times[1]
  
  out$parameterValue <- variable_par

  return(out)
}


ode_solve_discrete_stochastic <- function(model_list, variable_par = NULL){

out <- deSolve::ode(
  y = model_list[["igp_init"]],
  times =model_list[["igp_times_disc"]],
  func = model_list[["igp_model_disc_stoc"]],
  parms =model_list[["igp_parms"]],
  method = "iteration",
  dt = 0.001) |> 
  as.data.frame() # This should equal times[2] - times[1]
  
  out$parameterValue <- variable_par

  return(out)
}


