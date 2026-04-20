
###########pho stochastic
DF_DISC_LV <-  data.frame()

for (i in seq(1:10)){

model_used <- LBLB_LV_list ##it includes initial conditions, parameters and equations
disc_or_cont <- "disc_stoc"  #(disc, cont, disc_stoc)
d_t <- 0.01
par_ms =model_used[["parms"]]
par_ms[["s_d"]] = 0.1 ##this is the noise generator 

DF_temp<- deSolve::ode(
  y = model_used[["init"]],
  times = seq(from = 1, to = 1000, by = d_t), ##check the step is 0.01 for euler method
  func = model_used[[paste0("model_", disc_or_cont)]],
  parms =par_ms,
  method = "iteration",
  dt = d_t) |> 
  as.data.frame() 


###import data frame and convert it to a matrix
tmin = 100
tmax =200
reso= 1
disc_count = "disc_stoc"

DF_temp<- DF_temp |>  
       dplyr::filter(time %in%  seq(tmin, tmax, reso))

DF_temp$replicate <- i

DF_DISC_LV <- rbind(DF_DISC_LV, DF_temp)
  
}  



TS_PLOT <- ts_plotter(outDF = DF_DISC_LV, plotted_var = c("R", "N", "P"), replicate = "replicate")
PHASE_PLOT_PN <- phase_plotter(outDF = DF_DISC_LV, var1 = "N", var2 = "P", replicate = "replicate")
PHASE_PLOT_PR <- phase_plotter(outDF = DF_DISC_LV, var1 = "R", var2 = "P", replicate = "replicate")
PHASE_PLOT_NR <- phase_plotter(outDF = DF_DISC_LV, var1 = "R", var2 = "N", replicate = "replicate")

FULL_PLOT <-  TS_PLOT + (PHASE_PLOT_PN/PHASE_PLOT_PR/PHASE_PLOT_NR)

 ggsave(
    FULL_PLOT,
    filename = paste0("./figures/simulation/fullPlot_", "tmin_", tmin,"_tmax_", tmax,  "_mode_", disc_count, "_reso_",reso,  ".png"),
    height = 10,
    width = 8,
    create.dir = T
  )