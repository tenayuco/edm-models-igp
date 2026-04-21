
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

ggsave(FULL_PLOT, filename = paste0("./figures/simulation/fullPlot_", "tmin_", tmin,"_tmax_", tmax,  "_mode_", disc_count, "_reso_",reso,  ".png"),
   height = 10,
    width = 12,
    create.dir = T
  )


#--------------------------------

#-------------------------------
###########aqui vou....cambiar eso..

S <-  length(LBLB_LV_list$init)
Tmax <- dim(DF_temp)[1]


###now I put here the real values of the data, according to the transformation 
dt <- 0.01

r_eq <- c(log(1-LBLB_LV_parms[["mup"]]*dt), log(1-LBLB_LV_parms[["mun"]]*dt), log(1+LBLB_LV_parms[["rho"]]*dt)) # set the intrinsic growth rates
names(r_eq) <- paste("sp", 1:S, sep = "") # species names
alpha_eq <- matrix(NA, nrow = S, ncol = S) # set the per capita interaction strengths matrix
colnames(alpha_eq) <- c("R","N", "P" )
rownames(alpha_eq) <-c("R","N", "P" )

alpha_eq[1,1] <- 0
alpha_eq[1, 2] <- LBLB_LV_parms[["Ep"]]* LBLB_LV_parms[["fnp"]] * (1- LBLB_LV_parms[["S"]]) #0.15
alpha_eq[1, 3] <- LBLB_LV_parms[["Ep"]]* LBLB_LV_parms[["frp"]] *  LBLB_LV_parms[["S"]] #0.15
alpha_eq[2, 1] <- - LBLB_LV_parms[["fnp"]] * (1- LBLB_LV_parms[["S"]])
alpha_eq[2, 2] <- 0
alpha_eq[2, 3] <- LBLB_LV_parms[["En"]]* LBLB_LV_parms[["frn"]]
alpha_eq[3, 1] <- - LBLB_LV_parms[["frp"]] *  LBLB_LV_parms[["S"]]
alpha_eq[3, 2] <-  -LBLB_LV_parms[["frn"]]
#alpha[3, 3] <- -LBLB_LV_parms[["rho"]]/LBLB_LV_parms[["K"]]
alpha_eq[3, 3] <- 0
alpha_eq <- alpha_eq * dt


#for r
DF_R_EQ <- as.double(r_eq)
DF_R_EQ <- as.data.frame(DF_R_EQ)
names(DF_R_EQ) <- "par_eq"
DF_R_EQ$varName <- c("R","N","P")




#for alpha
DF_ALPHA_EQ <- as.double(alpha_eq)
DF_ALPHA_EQ <- as.data.frame(DF_ALPHA_EQ)
names(DF_ALPHA_EQ) <- "par_eq"
DF_ALPHA_EQ$varName <- c("R.R","R.N","R.P","N.R","N.N","N.P","P.R","P.N","P.P")



#--------------------------------now 


#---transforms to a matrix
N_list_sim <- vector(mode = "list", length = 9)

for (i in unique(DF_DISC_LV$replicate)){
  df_temp <- DF_DISC_LV |> 
    dplyr::filter(replicate == i)

  df_temp$time <- NULL
  df_temp$replicate <- NULL

  N_list_sim[[i]] <- as.matrix(df_temp)
}



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
RT_TIME_PLOT <- par_time_plotter(DF_RT, num_col =S)
ALPHA_TIME_PLOT <- par_time_plotter(DF_ALPHA, num_col =S)

#now the mean and sd 

RT_MEAN_SD_PLOT <- par_mean_sd_plotter(df_par_se_long =  long_formatter(df_par = DF_RT, df_par_se = DF_RT_SE), df_par_eq= DF_R_EQ, num_col=S)
ALPHA_MEAN_SD_PLOT <- par_mean_sd_plotter(df_par_se_long =  long_formatter(df_par=DF_ALPHA, df_par_se = DF_ALPHA_SE), df_par_eq= DF_ALPHA_EQ, num_col=S)


##now check if it makes sense.. 
#averages #does ot work YET
RT_EST <- av_comp_plotter(df_par_se_long = long_formatter(df_par = DF_RT, df_par_se = DF_RT_SE), df_par_eq = DF_R_EQ)
ALPHA_EST <- av_comp_plotter(df_par_se_long = long_formatter(df_par = DF_ALPHA, df_par_se = DF_ALPHA_SE), df_par_eq = DF_ALPHA_EQ)
