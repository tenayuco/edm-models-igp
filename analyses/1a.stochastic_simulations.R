

# -------------------------PART 1 - run the stochastic IGP model ----------------------------------------
# Set up folders for saving figures - specifies where output plots will be saved
#fig_folder <- "./figures/simulation/demoStoc_lvmap/" ### Directory path for the main figure folder
#fig_subfolder <- paste0("len_", len_chosen,"/", "noise_", noise_chosen, "/", "numrep_", num_rep, "/", "R_", rpresent, "/") ## Creates a subfolder name based on time length and noise level chosen

DF_DISC_LV <-  data.frame()  # Initialize empty data frame to store all simulation results


# Loop through each replicate simulation
for (i in seq(1:num_block)){

#model used
model_used <- LBLB_LV_list 
disc_or_cont <- "disc_stoc"  #
d_t <- 0.01  # Time step for numerical integration
par_ms = model_used[["parms"]]  
par_ms[["s_d"]] = noise_chosen 
grow_function <- "semichemostat"   ## Selected growth function type (can be "logistic", "exponential", or "semichemostat")

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
tmin = 200
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


dir.create(out_folder, recursive = TRUE)

utils::write.csv(DF_DISC_LV, file= paste0(out_folder, "DF_DISC_LV.csv"), row.names= FALSE)
# ------------------------------------------------------------------------------------------


TS_PLOT <- ts_plotter(outDF = DF_DISC_LV, plotted_var = c("R", "N", "P"))
PHASE_PLOT_PN <- phase_plotter(outDF = DF_DISC_LV, var1 = "N", var2 = "P")
PHASE_PLOT_PR <- phase_plotter(outDF = DF_DISC_LV, var1 = "R", var2 = "P")
PHASE_PLOT_NR <- phase_plotter(outDF = DF_DISC_LV, var1 = "R", var2 = "N")

FULL_PLOT <-  TS_PLOT + (PHASE_PLOT_PN/PHASE_PLOT_PR/PHASE_PLOT_NR)


ggsave(FULL_PLOT, filename = paste0(fig_folder, 
   "fullPlot", "_reso_",reso, "_grow_", grow_function, "_noise_", noise_chosen, "_len_", len_chosen, ".png"),
   height = 10,
    width = 12,
    create.dir = T
  )

rm(TS_PLOT,  PHASE_PLOT_NR, PHASE_PLOT_PR, PHASE_PLOT_PN, FULL_PLOT)

