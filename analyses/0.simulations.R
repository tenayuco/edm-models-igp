
###try with some random death reate






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




###########pho stochastic

model_used <- LBLB_LV_list ##it includes initial conditions, parameters and equations
disc_or_cont <- "disc_stoc"  #(disc, cont, disc_stoc)
d_t <- 0.1

DF_DISC_LV<- deSolve::ode(
  y = model_used[["init"]],
  times = seq(from = 1, to = 1000, by = d_t), ##check the step is 0.01 for euler method
  func = model_used[[paste0("model_", disc_or_cont)]],
  parms =model_used[["parms"]],
  method = "iteration",
  dt = d_t) |> 
  as.data.frame() 
  
full_plot(outDF = DF_DISC_LV, tmin=150, tmax = 200, disc_cont = paste0(disc_or_cont, "_LV"), reso=0)
