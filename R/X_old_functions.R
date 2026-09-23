
#OLD FUNCTION NOT USED FOR NOW
old_df_differencer_lv <- function(data_pred){
   data_dif<- data_pred |> 
    dplyr::group_by(block, enem) |>  # Group by both replicate AND enemy
    dplyr::mutate(
      R = c(NA, diff(R) - 400),  # A2 - (A1 + 300)  #the 1000 is to avoid negative values, that the lv map can not use beacuse of the log trnas
      X = c(NA, diff(X)),        # Just the difference
      Y = c(NA, diff(Y))         # Just the difference
    ) |> 
    tidyr::drop_na() |> 
    dplyr::ungroup()  # Optional: remove grouping after

  return(data_dif)
}



plotter_lv_map_treatment <- function(list_treatment_used, fig_path, num_seed, true_values =TRUE, reso='NA'){
###primero volvemos en data frames 

  
if(true_values ==TRUE){
  DF_RT_EQ <- as.double(list_treatment_used$r_eq)
  DF_ALPHA_EQ <- as.double(list_treatment_used$alpha_eq)}

if(true_values ==FALSE) {  
  DF_RT_EQ <- as.double(c(0, 0, 0))
  DF_ALPHA_EQ <- as.double(matrix(0L, ncol=3, nrow=3))}
  
#for r

DF_RT_EQ <- as.data.frame(DF_RT_EQ)
names(DF_RT_EQ) <- "par_eq"
DF_RT_EQ$varName <- c("R","N","P")

#for alpha
DF_ALPHA_EQ <- as.data.frame(DF_ALPHA_EQ)
names(DF_ALPHA_EQ) <- "par_eq"
DF_ALPHA_EQ$varName <- c("R.R", "N.R", "P.R", "R.N", "N.N", "P.N", "R.P", "N.P", "P.P")


############3


##------------now plotting the parameters---------------

###intento loco
process_list <- function(data_list){
df_total <- data.frame()
for (i in 1:length(data_list)){
  df_rt_temp <- as.data.frame(data_list[[i]])
  df_rt_temp$replicate <- i
  df_rt_temp$time <- seq(1, dim(df_rt_temp)[1])
  df_total <-  rbind(df_total, df_rt_temp)
}
return(df_total)
}

#---here I all as data frames
DF_RT <- process_list(data_list = list_treatment_used$r_hat_list)
DF_RT_SE <- process_list(data_list = list_treatment_used$r_se_list)
DF_ALPHA <- process_list(data_list = list_treatment_used$alpha_hat_list)
DF_ALPHA_SE <- process_list(data_list = list_treatment_used$alpha_se_list)


###########I save all data frame in the corresponding 



#### the first plot you wanna make are the time series plot (ok?)

### the plotss

#plot the phase plot and time series

#just for the plot, i put the time steps, as 1, 2, 3.. 


##AQUI VOY, pero vamo lo meto en loop y ya

##how the parameters change in time
S <- length(DF_RT)-2
RT_TIME_PLOT <- par_time_plotter(DF_RT, num_col = S) ##to only include the varia
ALPHA_TIME_PLOT <- par_time_plotter(DF_ALPHA, num_col =S)


ggsave(RT_TIME_PLOT, filename = paste0(fig_path, "rt_time_",  "reso_",reso, "_seed_", num_seed,  ".png"),
   height = 4,
    width = 12,
    create.dir = T
  )


ggsave(ALPHA_TIME_PLOT, filename = paste0(fig_path, "alpha_time_", "reso_",reso, "_seed_", num_seed,  ".png"),
   height = 10,
    width = 12,
    create.dir = T
  )



LONG_FULL_RT<-long_par_formatter(df_par = DF_RT, df_par_se = DF_RT_SE)
LONG_FULL_ALPHA <- long_par_formatter(df_par=DF_ALPHA, df_par_se = DF_ALPHA_SE)

#now the mean and sd 

RT_MEAN_SD_PLOT <- par_mean_sd_plotter(df_par_se_long =  LONG_FULL_RT, df_par_eq= DF_RT_EQ, num_col=max(S, dim(DF_RT_EQ)[1]), trueParameters = 
true_values)
ALPHA_MEAN_SD_PLOT <- par_mean_sd_plotter(df_par_se_long =  LONG_FULL_ALPHA , df_par_eq= DF_ALPHA_EQ, num_col=max(S, dim(DF_RT_EQ)[1]), trueParameters = true_values)


ggsave(RT_MEAN_SD_PLOT, filename = paste0(fig_path, "rt_mean_",  "reso_",reso, "_seed_", num_seed, ".png"),
   height = 4,
    width = 12,
    create.dir = T
  )

ggsave(ALPHA_MEAN_SD_PLOT, filename = paste0(fig_path, "alpha_mean_", "reso_",reso, "_seed_", num_seed, ".png"),
   height = 10,
    width = 12,
    create.dir = T
  )



if(true_values ==TRUE){


##now check if it makes sense against the TRUE VALUES 
#averages #does ot work YET
ALPHA_EST <- av_comp_plotter_v2(df_par_se_long = LONG_FULL_ALPHA, df_par_eq = DF_ALPHA_EQ)
RT_EST <- av_comp_plotter_v2(df_par_se_long = LONG_FULL_RT, df_par_eq = DF_RT_EQ)

ggsave(RT_EST, filename = paste0(fig_path,"rt_acc_", "reso_",reso, "_seed_", num_seed,  ".png"),
   height = 10,
    width = 12,
    create.dir = T
  )

ggsave(ALPHA_EST, filename = paste0(fig_path, "alpha_acc_",  "reso_",reso, "_seed_", num_seed,  ".png"),
   height = 10,
    width = 12,
    create.dir = T
  )
}


# --------------------------------------------------- ----------------------------------------
##now with the re


##########this is probabcly for another code




}





#unused function
merger_data_frame_treatment <- function(out_folder){


all_df_csv <- list.files(out_folder, recursive = TRUE, pattern = ".csv")  #

  general_csv <-  data.frame()

  for (i in all_df_csv){
    df <- read.csv(i)
    general_csv <- rbind(general_csv, i)
    }
  
  return(general_csv)
}



###
##im letting like that see if i have to erase it 
old_parameter_seed_sim_plotter <- function(df_full = FULL_DF_PARAMETERS, par_type = "r"){

  var_order <- c("P.P", "N.N", "N.P", "P.N", "R.R", "R.P", "P.R", "R.N", "N.R", "P", "N", "R")

  par_plot <- df_full |> 
    dplyr::filter(type == par_type) |> 
    dplyr::mutate(varName = factor(varName, levels = var_order)) |>
    ggplot(aes(x= varName, y= mvalue.mean)) +
    geom_errorbar(aes(ymin=mvalue.mean- 1*mvalue.sd,  ymax=mvalue.mean+ 1*mvalue.sd, color= as.factor(numSeed)), width=.2,
                 position=position_dodge(0.3))+
    geom_point(aes(color= as.factor(numSeed)), position=position_dodge(0.3))+

    xlab("Replicate and variable") +

   # geom_segment(data= df_par_se_long,  aes(x = min_x-0.5, y = 0, xend = max_x+0.5, yend = 0), color= "black", linetype= "dashed")+

    facet_grid(numRep~rpresent, labeller = labeller(.rows = label_both, .cols = label_both))+
    #facet_grid(numRep~rpresent, scales = "free", labeller = labeller(.rows = label_both, .cols = label_both))+

    geom_hline(yintercept = 0, color= "black", linetype= "dashed")+
     scale_color_viridis_d() +
    ggtitle(paste0("kernel_chosen ", kernel_chosen)) +

    theme_bw()

  return(par_plot)
}

extract_all_simulation<- function(out_subfolder = out_sim_folder) {


full_sim_df <-  data.frame()
  
##new method..
  # One-liner
all_df_sim <- list.files(out_subfolder, recursive = TRUE, pattern = ".csv")
  
sim_names <-  list.dirs(out_subfolder, recursive = FALSE, full.names = F)

  for (i in seq(1: length(sim_names))){

    sim_df <-  read.csv(paste0(out_subfolder, all_df_sim[i]))
    sim_df$real_sim_name <-  sim_names[i]
    full_sim_df <-  rbind(full_sim_df, sim_df)
}
  
return(full_sim_df)


}

  