




DF_SIM_DISC <- ode_solve_discrete(model_list = LBLB_list, variable_par = NULL)
full_plot(outDF = DF_SIM_DISC, tmax = 500, disc_cont = "discrete")

DF_SIM_CONT <- ode_solve(model_list = LBLB_list, variable_par = NULL)
full_plot(outDF = DF_SIM_CONT, tmax = 500, disc_cont = "continuous")


DF_SIM_DISC_STOC <- ode_solve_discrete_stochastic(model_list = LBLB_list, variable_par = NULL)
full_plot(outDF = DF_SIM_DISC_STOC, tmax = 500, disc_cont = "stochastic")


##plot data

#plot(TIME_SERIES_DF)
############dsimpldie lv

DF_SIM_CONT_LV <- ode_solve(model_list = LBLB_LV_list, variable_par = "NULL")
full_plot(outDF = DF_SIM_CONT_LV, tmax = 100, disc_cont = "continuous_LV")


###
DF_SIM_DISC_LV <- ode_solve_discrete(model_list = LBLB_LV_list, variable_par = "NULL")
full_plot(outDF = DF_SIM_DISC_LV, tmax = 100, disc_cont = "discrete_LV")



###try with some random death reate



