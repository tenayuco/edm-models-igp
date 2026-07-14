###functions of plots related 






par_time_plotter <- function(df_par, replicate= "replicate", plotted_var = c("N", "P"), num_col=2){


  outLong <-  df_par |> 
    tidyr::pivot_longer(cols= !c(replicate, time), names_to = "varName", values_to = "value") 
    
  par_ts <- outLong |> 
    ggplot(aes(x=time, y=value)) +
    geom_line(aes(color= as.factor(replicate), group=replicate), linewidth=1) + 
    xlab("Time") +
    facet_wrap(~varName, ncol = num_col)+
    #facet_grid(varName~replicate, scales = "free")+

    theme_bw()
  
  return(par_ts)
}



long_par_formatter <- function(df_par, df_par_se, replicate= "replicate"){
outLong <-  df_par |> 
    tidyr::pivot_longer(cols= !c(replicate, time), names_to = "varName", values_to = "value") |> 
    dplyr::group_by(replicate, varName)|> 
    dplyr::summarise(mvalue = mean(value))
  
  
  outLong_sd <-  df_par_se |> 
    tidyr::pivot_longer(cols= !c(replicate, time), names_to = "varName", values_to = "value") |> 
    dplyr::group_by(replicate, varName)|> 
    dplyr::summarise(mvalue = mean(value))

  outLong_total <- dplyr::full_join(outLong, outLong_sd, by=c("varName", "replicate"), suffix= c(".mean", ".sd"))

  return(outLong_total)

}



par_mean_sd_plotter <- function(df_par_se_long, trueParameters = FALSE, df_par_eq, replicate= "replicate", num_col=2){


   min_x <- min(df_par_se_long$replicate)
   max_x <- max(df_par_se_long$replicate)

  par_mean_sd <- df_par_se_long |> 
    ggplot(aes(x= replicate, y= mvalue.mean)) +
    geom_point()+
    geom_errorbar(aes(ymin=mvalue.mean- 1*mvalue.sd,  ymax=mvalue.mean+ 1*mvalue.sd), width=.2,
                 position=position_dodge(0.05))+
    xlab("Replicate") +

    geom_segment(data= df_par_se_long,  aes(x = min_x-0.5, y = 0, xend = max_x+0.5, yend = 0), color= "black", linetype= "dashed")+


    facet_wrap(~varName, scales = "free", ncol= num_col)+
    #facet_grid(varName~replicate, scales = "free")+

    theme_bw()
  
  if (trueParameters == TRUE){
      par_mean_sd <- par_mean_sd +  geom_segment(data= df_par_eq,  aes(x = min_x-0.5, y = par_eq , xend = max_x +0.5, yend = par_eq), color= "darkred", linetype= "dashed")

  }
  
  return(par_mean_sd)
}



av_comp_plotter <- function(df_par_se_long, df_par_eq){

  DF_average <- df_par_se_long |> 
    dplyr::ungroup() |> 
    dplyr::group_by(varName) |> 
    dplyr::summarise_all(mean)

  DF_average$replicate <- NULL
 
 
  DF_average <- dplyr::full_join(DF_average, df_par_eq, by =c("varName")) 


  par_rep_av <- DF_average |> 
    ggplot(aes(x= varName, y= mvalue.mean)) +
    geom_point()+
    geom_errorbar(aes(ymin=mvalue.mean- 1*mvalue.sd,  ymax=mvalue.mean+ 1*mvalue.sd), width=.2,
                 position=position_dodge(0.05)) +
    geom_point(aes(x = varName, y= par_eq), color= "blue", size=3, shape=17)+
    theme_bw()

   return(par_rep_av)
  

}



av_comp_plotter_v2 <- function(df_par_se_long, df_par_eq){

  DF_average <- df_par_se_long |> 
    dplyr::ungroup() |> 
    dplyr::group_by(varName) |> 
    dplyr::summarise_all(mean)

  DF_average$replicate <- NULL
 
 
  DF_average <- dplyr::full_join(DF_average, df_par_eq, by =c("varName")) 

shapes_used <- c(21, 22, 23, 24, 21, 22, 23, 24, 21, 22, 23, 24)

  par_rep_av <- DF_average |> 
    ggplot(aes(x= par_eq, y=mvalue.mean)) +
    geom_errorbar(aes(ymin=mvalue.mean- 1*mvalue.sd,  ymax=mvalue.mean+ 1*mvalue.sd), color= "black", width=0.01,
                 position=position_dodge(0.05)) +
    geom_point(aes(fill= varName, shape=varName), size=4)+
  
    geom_vline(xintercept = 0, color= "black", linetype= "dashed")+
     geom_hline(yintercept = 0, color= "black", linetype= "dashed")+
    geom_abline(intercept = 0, slope =1,color= "black", linetype= "dashed" )+
    
    theme_bw()+
    scale_fill_viridis_d()+
    scale_color_viridis_d() +
    scale_shape_manual(values=shapes_used) +
    labs(x = "True values", y= "Estimated values (+- s.e.)")

   return(par_rep_av)
  

}



####this is for the long frame 



parameter_seed_plotter <- function(df_full = FULL_DF_PARAMETERS, par_type = "r"){


  par_plot <- df_full |> 
    dplyr::filter(type == par_type) |> 
    ggplot(aes(x= varName, y= mvalue.mean)) +
    geom_errorbar(aes(ymin=mvalue.mean- 1*mvalue.sd,  ymax=mvalue.mean+ 1*mvalue.sd, color= as.factor(numSeed)), width=.2,
                 position=position_dodge(0.3))+
    geom_point(aes(color= as.factor(numSeed)), position=position_dodge(0.3))+

    xlab("Replicate and variable") +
    ggtitle(paste0("kernel_chosen ", kernel_chosen)) +

    facet_wrap(~enem, scales = "free", ncol= 3)+
    geom_hline(yintercept = 0, color= "black", linetype= "dashed")+
     scale_color_viridis_d() +

    theme_bw()

  return(par_plot)
}



###

parameter_seed_sim_plotter <- function(df_full = FULL_DF_PARAMETERS, par_type = "r"){

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




parameter_r_alpha_plotter <- function(df_full = FULL_DF_PARAMETERS, par_type = "r"){

  var_order <- c("Y.Y", "X.X", "X.Y", "Y.X", "R.R", "R.Y", "Y.R", "R.X", "X.R", "Y", "X", "R")

  par_plot <- df_full |> 
    dplyr::filter(type == par_type) |> 
    dplyr::mutate(varName = factor(varName, levels = var_order)) |>

    ggplot(aes(x= varName, y= mvalue.mean)) +
    geom_errorbar(aes(ymin=mvalue.mean- 1*mvalue.sd,  ymax=mvalue.mean+ 1*mvalue.sd, color= as.factor(numSeed)), width=.2,
                 position=position_dodge(0.3))+
    geom_point(aes(color= as.factor(numSeed)), position=position_dodge(0.3))+

    xlab("Replicate and variable") +
    ggtitle(paste0("kernel_chosen ", kernel_chosen)) +

    facet_wrap(~enem, scales = "free", ncol= 3)+
    
    geom_hline(yintercept = 0, color= "black", linetype= "dashed")+
     scale_color_viridis_d() +

    theme_bw()

  return(par_plot)
}




parameter_omega_plotter <- function(df_full = FULL_DF_PARAMETERS){


  par_plot <- df_full |> 
    
    ggplot(aes(x= replicate, y= 10^omega_mean)) +
    geom_errorbar(aes(ymin=10^omega_dw,  ymax= 10^omega_up, color= as.factor(numSeed)), width=.2,
                 position=position_dodge(0.3))+
    geom_point(aes(color= as.factor(numSeed)), position=position_dodge(0.3))+

    xlab("Replicate") +
    ggtitle(paste0("kernel_chosen ", kernel_chosen)) +

   # geom_segment(data= df_par_se_long,  aes(x = min_x-0.5, y = 0, xend = max_x+0.5, yend = 0), color= "black", linetype= "dashed")+

    facet_wrap(~enem, ncol= 3)+
    geom_hline(yintercept = 0, color= "black", linetype= "dashed")+
     scale_color_viridis_d() +


    #facet_grid(varName~replicate, scales = "free")+

    theme_bw()

  return(par_plot)
}


###all conditions plotter 

##histogram of all parameters, just to see the distribution before choosing a way of summarizing

summarizer_with_variance <- function(df_full){
full_df_sum <- df_full |> 
  dplyr::ungroup()|> 
  dplyr::select(!replicate)|> 
  dplyr::select(!dif_cond)|> 
  dplyr::select(!norm)|> 
  dplyr::group_by(varName, type, numRep, rpresent, enem)|> 
  dplyr::summarise(grand_mean = mean(mvalue.mean), var_between = var(mvalue.mean), var_within= mean(mvalue.sd**2)) |> 
  dplyr::mutate(total_sd =sqrt(var_between+var_within))

  return(full_df_sum)
}


plot_par_allconditions <- function(df_sum){
  var_order <- c("Y.Y", "X.X", "X.Y", "Y.X", "R.R", "R.Y", "Y.R", "R.X", "X.R", "Y", "X", "R")
  par_plot <- df_sum |> 
   # dplyr::filter(type == par_type) |> 
    dplyr::mutate(varName = factor(varName, levels = var_order)) |>
    ggplot(aes(x= varName, y= grand_mean)) +
    geom_errorbar(aes(ymin=grand_mean- 1*total_sd,  ymax=grand_mean+ 1*total_sd, color= as.factor(rpresent)), width=.2,
                 position=position_dodge(0.3))+
    geom_point(aes(color= as.factor(rpresent)), position=position_dodge(0.3))+

    xlab("Replicate and variable") +
    ggtitle(paste0("kernel_chosen ", kernel_chosen)) +

    facet_wrap(~enem, scales = "free", ncol= 3)+
    
    geom_hline(yintercept = 0, color= "black", linetype= "dashed")+
     scale_color_viridis_d() +

    theme_bw()

  return(par_plot)

}