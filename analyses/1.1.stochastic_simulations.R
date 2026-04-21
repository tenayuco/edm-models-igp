
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

# ggsave(
 #   FULL_PLOT,
  #  filename = paste0("./figures/simulation/fullPlot_", "tmin_", tmin,"_tmax_", tmax,  "_mode_", disc_count, "_reso_",reso,  ".png"),
   # height = 10,
    #width = 8,
    #create.dir = T
  #)


#---transforms to a matrix
N_list_sim <- vector(mode = "list", length = 9)

for (i in unique(DF_DISC_LV$replicate)){
  df_temp <- DF_DISC_LV |> 
    dplyr::filter(replicate == i)

  df_temp$time <- NULL
  df_temp$replicate <- NULL

  N_list_sim[[i]] <- as.matrix(df_temp)
}

###########aqui vou....

S <- dim(df_temp)[2]
Tmax <- dim(df_temp)[1]


# ================
# Cross validation
# ================
cv_list_sim <- vector(mode = "list", length = 9)
tic()
for (i in 1:9) {
  out_cv <- LV_map_state_space_cross_validation(N_list_sim[[i]], theta_v = seq(0, 3, 0.01))
  cv_list_sim[[i]] <- out_cv
}
toc()


# ========================
# Estimation of parameters
# ========================
r_hat_list <- vector(mode = "list", length = 9)
alpha_hat_list <- vector(mode = "list", length = 9)
r_se_list <- vector(mode = "list", length = 9)
alpha_se_list <- vector(mode = "list", length = 9)
out_list <- vector(mode = "list", length = 9)
tic()
for (i in 1:9) {
  out_list[[i]] <- LV_map(N_list_sim[[i]], cv_list_sim[[i]]$theta_o)
  r_hat_list[[i]] <- out_list[[i]]$r_hat
  r_se_list[[i]] <- out_list[[i]]$r_se
  alpha_hat_list[[i]] <- out_list[[i]]$alpha_hat
  alpha_se_list[[i]] <- out_list[[i]]$alpha_se
}
toc()


##------------now plotting the parameters---------------

###intento loco
process_list <- function(data_list){
df_total <- data.frame()
for (i in 1:9){
  df_rt_temp <- as.data.frame(data_list[[i]])
  df_rt_temp$replicate <- i
  df_rt_temp$time <- seq(1, dim(df_rt_temp)[1])
  df_total <-  rbind(df_total, df_rt_temp)
}
return(df_total)
}

#---here I all as data frames
DF_RT <- process_list(data_list = r_hat_list)
DF_RT_SE <- process_list(data_list = r_se_list)
DF_ALPHA <- process_list(data_list = alpha_hat_list)
DF_ALPHA_SE <- process_list(data_list = alpha_se_list)

##how the parameters change in time
RT_TIME_PLOT <- par_time_plotter(DF_RT)
ALPHA_TIME_PLOT <- par_time_plotter(DF_ALPHA)

#now the mean and sd 

RT_MEAN_SD_PLOT <- par_mean_sd_plotter(df_par = DF_RT, df_par_se = DF_RT_SE, num_col =S)
ALPHA_MEAN_SD_PLOT <- par_mean_sd_plotter(df_par = DF_ALPHA, df_par_se = DF_ALPHA_SE, num_col=S)
#-------------------------------

