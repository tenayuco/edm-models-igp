


df_modifier_lv <- function(raw_data, chosen_enemies){

##here we use 2 formats of data
DATA_LONG <-  long_formatter(raw_data)

##here we remove the 0 


DATA_PRED <-  pred_formatter(DATA_LONG) 


DATA_USED <- DATA_PRED |> 
  dplyr::filter(enem == chosen_enemies)|> 
  dplyr::select(block, R, X, Y, week)
  
  
return(DATA_USED)  
}




lv_map_microcosmos <- function(df_used, rpresent, num_seed, num_rep){

list_treatment<- list()


### heres is the data

DATA_USED <- df_used

###here I change the values of the replicate to chage the order.

set.seed(num_seed)

REAS_DF <-  data.frame("block" = seq(1:10), "replicate" = sample(seq(1:10)))

DATA_USED <-  dplyr::full_join(DATA_USED, REAS_DF, by= "block")
DATA_USED$block <-  NULL

#here it just to remove the 0 values.. 

for (i in seq(1, dim(DATA_USED)[1])){

if(DATA_USED[i,]$R ==0){DATA_USED[i,]$R  <- max(DATA_USED$R)/100 * abs(rnorm(1,0,0.5))}
if(DATA_USED[i,]$X ==0){DATA_USED[i,]$X  <- max(DATA_USED$X)/100 * abs(rnorm(1,0,0.5))} 
if(DATA_USED[i,]$Y ==0){DATA_USED[i,]$Y  <- max(DATA_USED$Y)/100 * abs(rnorm(1,0,0.5))} 

}





##now here a renaming
names(DATA_USED) <- c("R", "X", "Y", "time", "replicate")

###here i removed the H
if (rpresent == FALSE){DATA_USED$R <- NULL}
  #we remove these as they have a strong autocorrelation.. 


DATA_USED <-  DATA_USED |> 
dplyr::arrange(replicate, .by_group = FALSE)
###########3



#TS_PLOT_DATA <- ts_plotter(outDF = DATA_USED, plotted_var = c("R", "N", "P"), replicate = "replicate", legenda = "right")



#####here it is just to gather in block, but keeping the new given order

size_block <- length(unique(DATA_USED$replicate))/num_rep
DATA_USED$replicate <- floor((DATA_USED$replicate-0.1)/size_block) +1   #fake block to make larger data inly work wiht zie block divisor of 10

#---transforms to a matrix
N_list_sim <- vector(mode = "list", length = num_rep)

#here it takes 
for (i in unique(DATA_USED$replicate)){
  df_temp <- DATA_USED |> 
    dplyr::filter(replicate == i)

  df_temp$time <- NULL
  df_temp$replicate <- NULL

  N_list_sim[[i]] <- as.matrix(df_temp)
}


list_treatment$N_list_sim <- N_list_sim

S <-  dim(N_list_sim[[1]])[2]


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

list_treatment$cv_list_sim <- cv_list_sim


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

list_treatment$out_list <- out_list
list_treatment$r_hat_list <- r_hat_list
list_treatment$alpha_hat_list <- alpha_hat_list
list_treatment$r_se_list <- r_se_list
list_treatment$alpha_se_list <- alpha_se_list

  
  



return(list_treatment)
  
}




















  

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





ggsave(RT_TIME_PLOT, filename = paste0(fig_folder, fig_subfolder, "rt_time_", "num_rep_", num_rep, "_seed_", num_seed, ".png"),
   height = 4,
    width = 12,
    create.dir = T
  )


ggsave(ALPHA_TIME_PLOT, filename = paste0(fig_folder, fig_subfolder, "alpha_time_", "num_rep_", num_rep, "_seed_", num_seed, ".png"),
   height = 10,
    width = 12,
    create.dir = T
  )


###########3












#now the mean and sd 


LONG_FULL_RT<-long_par_formatter(df_par = DF_RT, df_par_se = DF_RT_SE)
LONG_FULL_ALPHA <- long_par_formatter(df_par=DF_ALPHA, df_par_se = DF_ALPHA_SE)

RT_MEAN_SD_PLOT <- par_mean_sd_plotter(df_par_se_long = LONG_FULL_RT , df_par_eq= NA, trueParameters = FALSE,  num_col=S)
ALPHA_MEAN_SD_PLOT <- par_mean_sd_plotter(df_par_se_long = LONG_FULL_ALPHA , df_par_eq= NA, trueParameters= FALSE, num_col=S)




ggsave(RT_MEAN_SD_PLOT, filename = paste0(fig_folder, fig_subfolder, "rt_mean_", "num_rep_", num_rep, "_seed_", num_seed, ".png"),
   height = 4,
    width = 12,
    create.dir = T
  )

ggsave(ALPHA_MEAN_SD_PLOT, filename = paste0(fig_folder, fig_subfolder, "alpha_mean_", "num_rep_", num_rep, "_seed_", num_seed,  ".png"),
   height = 10,
    width = 12,
    create.dir = T
  )

LONG_FULL_RT$type <- "r"
LONG_FULL_ALPHA$type <- "a"


LONG_FULL <- rbind(LONG_FULL_RT, LONG_FULL_ALPHA)

LONG_FULL$enem <-  chosen_enemies
LONG_FULL$numRep <-  num_rep
LONG_FULL$numSeed <-  num_seed


# Set up folders for saving figures - specifies where output plots will be saved
out_folder <- "./outputs/microcosmos/" ### Directory path for the main figure folder
out_subfolder <- paste0("R_", rpresent,  "/") ## Creates a subfolder name based on time length and noise level chosen

dir.create(paste0(out_folder, out_subfolder), recursive = TRUE)

write.csv(LONG_FULL, file= paste0(out_folder, out_subfolder, "DF_parameters_", chosen_enemies, "_rep", num_rep, "_seed_", num_seed, ".csv"))
  
  
  
