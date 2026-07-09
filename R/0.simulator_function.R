

# -------------------------PART 1 - run the stochastic IGP model ----------------------------------------
# the folder are set up externallu
stochastic_generator_GENERAL <- function(model_used, disc_or_cont, noise_chosen){

DF_DISC_LV <-  data.frame()  # Initialize empty data frame to store all simulation results


# Loop through each replicate simulation
for (i in seq(1:num_block)){


d_t <- 0.01  # Time step for numerical integration
par_ms = model_used[["parms"]]  
par_ms[["s_d"]] = noise_chosen 

# Run the differential equation solver using Euler method (iteration)
DF_temp<- deSolve::ode(
  y = model_used[["init"]],
  times = seq(from = 1, to = 500, by = d_t), ##check the step is 0.01 for euler method
  func = model_used[[paste0("model_", disc_or_cont)]],
  parms =par_ms,
  method = "iteration",
  dt = d_t) |> 
  as.data.frame() 

###import data frame and convert it to a matrix
### Select a subset of the time series data
tmin = 300 ##this burns the inital lag, but then you see better the trend
len_rep = len_chosen  
tmax = tmin+len_chosen*reso
  
# If diff_len is TRUE, randomly vary the time series length by up to 20%
if (diff_len ==TRUE){tmax = tmin + round(tmin+len_chosen*d_t*abs(rnorm(1,mean=0,sd=0.2)), 2)}  ##is beacuse d_t is 0.01  

  # Filter the data to only include the selected time window
DF_temp<- DF_temp |>  
       dplyr::filter(time %in%  seq(tmin, tmax, reso))

DF_temp$block <- i
DF_DISC_LV <- rbind(DF_DISC_LV, DF_temp) 
}  

  
 ##extract the original plot, from which you take the data..  
 
#s_used <- par_ms[["S"]]  
  
  
TS_PLOT <- ts_plotter(outDF = DF_DISC_LV, plotted_var = c("R", "N", "P"))  + ggtitle("scenario= ", chosen_scenario)  
PHASE_PLOT_PN <- phase_plotter(outDF = DF_DISC_LV, var1 = "N", var2 = "P")
PHASE_PLOT_PR <- phase_plotter(outDF = DF_DISC_LV, var1 = "R", var2 = "P")
PHASE_PLOT_NR <- phase_plotter(outDF = DF_DISC_LV, var1 = "R", var2 = "N")

FULL_PLOT <-  TS_PLOT + (PHASE_PLOT_PN/PHASE_PLOT_PR/PHASE_PLOT_NR)

  
  
ggsave(FULL_PLOT, filename = paste0(data_folder, chosen_scenario, "/", 
   "fullPlot", "_reso_",reso, "_grow_", grow_function, "_noise_", noise_chosen, "_len_", len_chosen,  ".png"),
   height = 10,
    width = 12,
    create.dir = T
  ) 
  
  
  
  
DF_DISC_LV <-  DF_DISC_LV |> 
  dplyr::rename(X=N, Y=P)|>
  dplyr::mutate(week = time-tmin)|>
  dplyr::filter(week>0)

DF_DISC_LV$time <- NULL
DF_DISC_LV$enem <- "xx+yy"

  
  
dir.create(data_folder, chosen_scenario, recursive = TRUE)



utils::write.csv(DF_DISC_LV, file= paste0(data_folder, chosen_scenario, "/", "DF_DISC_LV_", len_chosen, ".csv"), row.names= FALSE)

saveRDS(par_ms, paste0(data_folder, chosen_scenario, "/", "parameters_used.rds"))
  
  
# ------------------------------------------------------------------------------------------

}