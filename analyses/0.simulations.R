




DF_SIM_DIS <- ode_solve_discrete(model_list = LBLB_list, variable_par = NULL)

DF_SIM_CONT <- ode_solve(model_list = LBLB_list, variable_par = NULL)

##plot data

#plot(TIME_SERIES_DF)
full_plot(outDF = DF_SIM_DIS, tmax = 500, disc_cont = "discrete")

full_plot(outDF = DF_SIM_CONT, tmax = 500, disc_cont = "continuous")
