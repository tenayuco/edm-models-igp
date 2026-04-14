







#DF_SIM_DISC <- ode_solve_discrete(model_list = LBLB_list, variable_par = NULL)
#full_plot(outDF = DF_SIM_DISC, tmax = 500, disc_cont = "discrete")

#DF_SIM_CONT <- ode_solve(model_list = LBLB_list, variable_par = NULL)
#full_plot(outDF = DF_SIM_CONT, tmax = 500, disc_cont = "continuous")


#DF_SIM_DISC_STOC <- ode_solve_discrete_stochastic(model_list = LBLB_list, variable_par = NULL)
#full_plot(outDF = DF_SIM_DISC_STOC, tmax = 500, disc_cont = "stochastic")


##plot data

#plot(TIME_SERIES_DF)
############dsimpldie lv

#DF_SIM_CONT_LV <- ode_solve(model_list = LBLB_LV_list, variable_par = "NULL")
#full_plot(outDF = DF_SIM_CONT_LV, tmax = 100, disc_cont = "continuous_LV")


###
#DF_SIM_DISC_LV <- ode_solve_discrete(model_list = LBLB_LV_list, variable_par = "NULL")
#full_plot(outDF = DF_SIM_DISC_LV, tmax = 100, disc_cont = "discrete_LV")



###try with some random death reate



###########simpler way...

model_used <- LBLB_LV_list ##it includes initial conditions, parameters and equations
disc_or_cont <- "disc"
d_t <- 0.1

DF_DISC_LV<- deSolve::ode(
  y = model_used[["init"]],
  times = seq(from = 1, to = 1000, by = d_t), ##check the step is 0.01 for euler method
  func = model_used[[paste0("model_", disc_or_cont)]],
  parms =model_used[["parms"]],
  method = "iteration",
  dt = d_t) |> 
  as.data.frame() 
  
full_plot(outDF = DF_DISC_LV, tmax = 100, disc_cont = paste0(disc_or_cont, "_LV"))




########continuosu deter  (i could put this in a function )
model_used <- LBLB_LV_list ##it includes initial conditions, parameters and equations
disc_or_cont <- "cont"
d_t <- 0.05 #better for continous 

DF_CONT_LV<- deSolve::ode(
  y = model_used[["init"]],
  times = seq(from = 1, to = 1000, by = d_t), ##check the step is 0.01 for euler method
  func = model_used[[paste0("model_", disc_or_cont)]],
  parms =model_used[["parms"]]) |> 
  as.data.frame() 
  
full_plot(outDF = DF_CONT_LV, tmax = 100, disc_cont = paste0(disc_or_cont, "_LV"))

