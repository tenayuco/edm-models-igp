
###########pho stochastic

fig_folder <- "./figures/simulation/stochastic_sim_lvmap/demographic_sto/"###here you pik the subfolder you want
fig_subfolder <- paste0("num_rep_", num_rep, "/",  "len", tmax_len_chosen,"/" , "diff_len", diff_len,  "/", "noise_", noise_chosen, "/") ##you modify the one you want






DF_DISC_LV <-  data.frame()

for (i in seq(1:num_rep)){

model_used <- LBLB_LV_list ##it includes initial conditions, parameters and equations
disc_or_cont <- "disc_stoc"  #(disc, cont, disc_stoc)
d_t <- 0.01
par_ms =model_used[["parms"]]
par_ms[["s_d"]] = noise_chosen ##this is the noise generator
#grow_function <- "logistic"   ##can be "logistic" or "exponential"
grow_function <- "semichemostat"   ##can be "logistic" or "exponential"
#grow_function <- "exponential"   ##can be "logistic" or "exponential"

  
  
  #par_ms[["grow_function"]] = "semichemostat" ##can be "logistic" or "exponential"

DF_temp<- deSolve::ode(
  y = model_used[["init"]],
  times = seq(from = 1, to = 500, by = d_t), ##check the step is 0.01 for euler method
  func = model_used[[paste0("model_", disc_or_cont)]],
  parms =par_ms,
  method = "iteration",
  dt = d_t) |> 
  as.data.frame() 


###import data frame and convert it to a matrix
## at the same time, chose the length of the data series (make ir variable)
tmin = 200
tmax_len = tmax_len_chosen
tmax = tmin+tmax_len
  
if (diff_len ==TRUE){tmax = tmin + round(tmax_len-tmax_len*abs(rnorm(1,mean=0,sd=0.2)), 0)}  

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

ggsave(FULL_PLOT, filename = paste0(fig_folder, 
  fig_subfolder, "fullPlot_", "tmin_", tmin,"_tmax_", tmax,  "_mode_", disc_count, "_reso_",reso, "grow_", grow_function,  ".png"),

   height = 10,
    width = 12,
    create.dir = T
  )

rm(TS_PLOT,  PHASE_PLOT_NR, PHASE_PLOT_PR, PHASE_PLOT_PN, FULL_PLOT)
#--------------------------------

#-------------------------------
###########aqui vou....cambiar eso..

S <-  length(LBLB_LV_list$init)
Tmax <- tmax_len


###now I put here the real values of the data, according to the transformation 
fac_int <- 0.9
avR <- mean(DF_DISC_LV$R) #to reproduce the inflx 

#check if it is the shemi
#here the rt is lackig a constant 
r_eq <- c(log(par_ms[["rho"]]*(par_ms[["K"]]/avR)*fac_int +1), log(1-par_ms[["mun"]]*fac_int), log(1-par_ms[["mup"]]*fac_int)) # set the intrinsic growth rates
names(r_eq) <- paste("sp", 1:S, sep = "") # species names
alpha_eq <- matrix(NA, nrow = S, ncol = S) # set the per capita interaction strengths matrix
colnames(alpha_eq) <- c("R","N", "P" )
rownames(alpha_eq) <-c("R","N", "P" )

#alpha_eq[1,1] <- -par_ms[["rho"]]/par_ms[["K"]]
alpha_eq[1,1] <- 0
alpha_eq[1, 2] <- -par_ms[["frn"]]
alpha_eq[1, 3] <- - par_ms[["frp"]] *  par_ms[["S"]]
alpha_eq[2, 1] <- par_ms[["En"]]* par_ms[["frn"]]
alpha_eq[2, 2] <- 0
alpha_eq[2, 3] <- - par_ms[["fnp"]] * (1- par_ms[["S"]])
alpha_eq[3, 1] <- par_ms[["Ep"]]* par_ms[["frp"]] *  par_ms[["S"]] #0.15
alpha_eq[3, 2] <-  par_ms[["Ep"]]* par_ms[["fnp"]] * (1- par_ms[["S"]]) #0.15
alpha_eq[3, 3] <- 0
alpha_eq <- alpha_eq * fac_int



#for r
DF_R_EQ <- as.double(r_eq)
DF_R_EQ <- as.data.frame(DF_R_EQ)
names(DF_R_EQ) <- "par_eq"
DF_R_EQ$varName <- c("R","N","P")




#for alpha
DF_ALPHA_EQ <- as.double(alpha_eq)
DF_ALPHA_EQ <- as.data.frame(DF_ALPHA_EQ)
names(DF_ALPHA_EQ) <- "par_eq"
DF_ALPHA_EQ$varName <- c("R.R", "N.R", "P.R", "R.N", "N.N", "P.N", "R.P", "N.P", "P.P")



#--------------------------------now 


#---transforms to a matrix
N_list_sim <- vector(mode = "list", length = num_rep)

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
cv_list_sim <- vector(mode = "list", length = num_rep)
tic()
for (i in 1:num_rep) {
  out_cv <- LV_map_state_space_cross_validation(N_list_sim[[i]], theta_v = seq(0, 3, 0.01))
  cv_list_sim[[i]] <- out_cv
}
toc()


# ========================
# Estimation of parameters
# ========================
r_hat_list <- vector(mode = "list", length = num_rep)
alpha_hat_list <- vector(mode = "list", length = num_rep)
r_se_list <- vector(mode = "list", length = num_rep)
alpha_se_list <- vector(mode = "list", length = num_rep)
out_list <- vector(mode = "list", length = num_rep)
tic()
for (i in 1:num_rep) {
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
for (i in 1:num_rep){
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





ggsave(RT_TIME_PLOT, filename = paste0(fig_folder, fig_subfolder, "rt_time_", "tmin_", tmin,"_tmax_", tmax,  "_mode_", disc_count, "_reso_",reso, "grow_", grow_function,  ".png"),
   height = 4,
    width = 12,
    create.dir = T
  )


ggsave(ALPHA_TIME_PLOT, filename = paste0(fig_folder, fig_subfolder, "alpha_time_", "tmin_", tmin,"_tmax_", tmax,  "_mode_", disc_count, "_reso_",reso, "grow_", grow_function,  ".png"),
   height = 10,
    width = 12,
    create.dir = T
  )



#now the mean and sd 

RT_MEAN_SD_PLOT <- par_mean_sd_plotter(df_par_se_long =  long_par_formatter(df_par = DF_RT, df_par_se = DF_RT_SE), df_par_eq= DF_R_EQ, num_col=S)
ALPHA_MEAN_SD_PLOT <- par_mean_sd_plotter(df_par_se_long =  long_par_formatter(df_par=DF_ALPHA, df_par_se = DF_ALPHA_SE), df_par_eq= DF_ALPHA_EQ, num_col=S)


ggsave(RT_MEAN_SD_PLOT, filename = paste0(fig_folder, fig_subfolder, "rt_mean_", "tmin_", tmin,"_tmax_", tmax,  "_mode_", disc_count, "_reso_",reso, "grow_", grow_function,  ".png"),
   height = 4,
    width = 12,
    create.dir = T
  )

ggsave(ALPHA_MEAN_SD_PLOT, filename = paste0(fig_folder, fig_subfolder, "alpha_mean_", "tmin_", tmin,"_tmax_", tmax,  "_mode_", disc_count, "_reso_",reso, "grow_", grow_function,  ".png"),
   height = 10,
    width = 12,
    create.dir = T
  )




##now check if it makes sense.. 
#averages #does ot work YET
ALPHA_EST <- av_comp_plotter_v2(df_par_se_long = long_par_formatter(df_par = DF_ALPHA, df_par_se = DF_ALPHA_SE), df_par_eq = DF_ALPHA_EQ)
RT_EST <- av_comp_plotter_v2(df_par_se_long = long_par_formatter(df_par = DF_RT, df_par_se = DF_RT_SE), df_par_eq = DF_R_EQ)

ggsave(RT_EST, filename = paste0(fig_folder, fig_subfolder,"rt_acc", "tmin_", tmin,"_tmax_", tmax,  "_mode_", disc_count, "_reso_",reso, "grow_", grow_function,  ".png"),
   height = 10,
    width = 12,
    create.dir = T
  )

ggsave(ALPHA_EST, filename = paste0(fig_folder, fig_subfolder, "alpha_acc_", "tmin_", tmin,"_tmax_", tmax,  "_mode_", disc_count, "_reso_",reso, "grow_", grow_function,  ".png"),
   height = 10,
    width = 12,
    create.dir = T
  )

