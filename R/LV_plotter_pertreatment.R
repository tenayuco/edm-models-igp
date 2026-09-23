############################################################################################################
# Function that replaces generic X and Y labels in varName with the actual species names
#' @param df_full A data frame with columns: enem and varName
#' @return The same data frame with X and Y in varName replaced by the matching species name
#' @details X is replaced by the species in X (cc, ol, sr, am, aa) that appears in the enem string
#' @details Y is replaced by the species in Y (ma, my, ac, ec) that appears in the enem string
#' @examples change_xy_realValues(df_full = my_full_data)
##########################################################################
change_xy_realValues <-  function(df_full){

X <- c("cc", "ol", "sr", "am", "aa")
Y <- c("ma", "my", "ac", "ec")


#change all the x 
for (x in X){
  print(x)
  for (i in seq(1:dim(df_full)[1])){
      if(grepl(x, df_full$enem[[i]])){
        df_full$varName[[i]] <- gsub("X", x, df_full$varName[[i]])
      }
  }
}



for (y in Y){
  print(y)
  for (i in seq(1:dim(df_full)[1])){
      if(grepl(y, df_full$enem[[i]])){
        df_full$varName[[i]] <- gsub("Y", y, df_full$varName[[i]])
      }
  }
}
 
return(df_full)  
  
  
}




##########################################################################################################
# Function that averages parameters across replicates and saves r and alpha plots per rpresent/numRep
#' @param df_full A data frame with columns: varName, type, numSeed, rpresent, numRep, enem, replicate
#' @param fig_subfolder Character; path to the folder where the plots will be saved
#' @return Nothing; saves one PNG per par_type (r, alpha) per rpresent/numRep combination
#' @details Averages all columns across replicates, grouped by varName, type, numSeed, rpresent, numRep, enem
#' @details Loops over rpresent and numRep, filters the averaged data, and calls parameter_r_alpha_plotter
#' @examples plotter_full_parameters(df_full = my_full_data, fig_subfolder = "figs/")
########################################################################################################

plotter_full_parameters <- function(df_full, fig_subfolder){


FULL_DF_PARAMETERS_M <- df_full|> 
  dplyr::select(!replicate)|>
  dplyr::group_by(varName, type, numSeed, rpresent, numRep, enem)|> 
  dplyr::summarise_all(mean)
  
  
enemy <-  unique(FULL_DF_PARAMETERS_M$enem)
  
for(i in unique(FULL_DF_PARAMETERS_M$rpresent)){
  for(j in unique(FULL_DF_PARAMETERS_M$numRep)){

FULL_DF <-  FULL_DF_PARAMETERS_M |> 
  dplyr::filter(rpresent == i)|> 
  dplyr::filter(numRep == j)
  

PLOT_PAR_SIM_RT <-  parameter_r_alpha_plotter(df_full = FULL_DF, par_type = "r")
PLOT_PAR_SIM_ALPHA <-  parameter_r_alpha_plotter(df_full = FULL_DF, par_type = "a")


ggsave(PLOT_PAR_SIM_RT, filename = paste0(fig_subfolder, "rt_allseed_",  "_numrep_", j, "_R_" , i , ".png"),
   height = 10,
    width = 13,
    create.dir = T
  )


ggsave(PLOT_PAR_SIM_ALPHA, filename = paste0(fig_subfolder, "alpha_allseed_",  "numrep_", j, "_R_" , i  , ".png"),
   height = 10,
    width = 13,
    create.dir = T
  )

}
}
}

############################################################################################################
# Function that builds an r or alpha parameter plot faceted by enem
#' @param df_full A data frame already filtered to one rpresent/numRep combination
#' @param par_type Character; either "r" for growth rates or "a" for interaction terms
#' @return A ggplot object with points and error bars, faceted by enem
#' @details Error bars are mean +/- sd, colored by numSeed
#' @details A dashed line at y = 0 is added as reference
#' @examples parameter_r_alpha_plotter(df_full = my_filtered_data, par_type = "r")
####################################################################################
parameter_r_alpha_plotter <- function(df_full = FULL_DF_PARAMETERS, par_type = "r"){

  #var_order <- c("Y.Y", "X.X", "X.Y", "Y.X", "R.R", "R.Y", "Y.R", "R.X", "X.R", "Y", "X", "R")

  par_plot <- df_full |> 
    dplyr::filter(type == par_type) |> 
   # dplyr::mutate(varName = factor(varName, levels = var_order)) |>

    ggplot(aes(x= varName, y= mvalue.mean)) +
    geom_errorbar(aes(ymin=mvalue.mean- 1*mvalue.sd,  ymax=mvalue.mean+ 1*mvalue.sd, color= as.factor(numSeed)), width=.2,
                 position=position_dodge(0.3))+
    geom_point(aes(color= as.factor(numSeed)), position=position_dodge(0.3))+

    xlab("Replicate and variable") +
    ggtitle(paste0("kernel_chosen ", kernel_chosen)) +

    #facet_wrap(~enem, scales = "free", ncol= 3)+
    facet_wrap(~enem, ncol= 3, scales = "free_x")+

    
    geom_hline(yintercept = 0, color= "black", linetype= "dashed")+
     scale_color_viridis_d() +

    theme_bw()

  return(par_plot)
}


###############################################################################################################
# Function that plots summarized parameters (r and alpha) across all conditions, faceted by enem and type
#' @param df_sum A summarized data frame produced by summarizer_with_variance
#' @param fig_subfolder Character; path to the folder where the plot will be saved
#' @return Nothing; saves a PNG of the plot
#' @details Error bars show grand_mean +/- total_sd, dodged by type and rpresent
#' @details Points use color for type and shape for rpresent (21 = empty circle, 17 = filled triangle)
#' @details Facets are enem x type with free x scales
#' @examples plot_par_sum_allconditions(df_sum = my_sum_df, fig_subfolder = "figs/")
##################################################################################################
plot_par_sum_allconditions <- function(df_sum, fig_subfolder, scale_chosen= "free", plotted_type = c("r", "a")){

  df_sum <- df_sum |> 
    dplyr::filter(type %in% plotted_type)

  n_col= length(plotted_type)*2

  par_plot <- df_sum |> 
    ggplot(aes(x= varName, y= grand_mean)) +
    geom_errorbar(aes(ymin=grand_mean- 1*total_sd,  ymax=grand_mean+ 1*total_sd, group= interaction(type, rpresent),  color= as.factor(type)), width=.2,
                 position=position_dodge(0.6), linewidth=1)+
    geom_point(aes(color= as.factor(type), shape=as.factor(rpresent)), fill="white",  position=position_dodge(0.6), size=3)+
    scale_shape_manual(
      values = c("FALSE" = 21, "TRUE" = 17),  # 1 = empty circle, 17 = filled triangle
      name = "rpresent"
    )+

    xlab("Replicate and variable") +
    ggtitle(paste0("kernel_chosen ", kernel_chosen)) +


    facet_wrap(enem~type, scales = scale_chosen, ncol= n_col)+
    
    geom_hline(yintercept = 0, color= "black", linetype= "dashed")+
     scale_color_viridis_d(begin=0, end= 0.7, option = "A", direction = 1) +

    theme_bw()

  
  ggsave(par_plot, filename = paste0(fig_subfolder, "all_parameters_scale",scale_chosen, "_variables_", paste0(plotted_type, collapse = "_"),    ".png"),
   height = 12,
    width = n_col*4,
    create.dir = T)
  #return(par_plot)

}






plotter_theta_microcosmos <- function(df_full, fig_folder){

#just if you have several treatments (replicatess)
FULL_DF_PARAMETERS_M <- df_full|> 
  dplyr::select(!c(replicate, varName, type))|> 
  dplyr::group_by(numSeed, rpresent, numRep, enem)|> 
  dplyr::summarise_all(mean)


PLOT_PAR_THETA <-  parameter_theta_plotter(df_full = FULL_DF_PARAMETERS_M)

  
ggsave(PLOT_PAR_THETA, filename = paste0(fig_folder, "theta_allrep_allr_allseed_", ".png"),
   height = 10,
    width = 13,
    create.dir = T
  )

}


plotter_omega_microcosmos <- function(df_full, fig_folder){

#just if you have several treatments (replicatess)
FULL_DF_PARAMETERS_M <- df_full|> 
  dplyr::select(!c(replicate, varName, type))|> 
  dplyr::group_by(numSeed, rpresent, numRep, enem)|> 
  dplyr::summarise_all(mean)


PLOT_PAR_OMEGA <-  parameter_omega_plotter(df_full = FULL_DF_PARAMETERS_M)

  
ggsave(PLOT_PAR_OMEGA, filename = paste0(fig_folder, "omega_allrep_allr_allseed_", ".png"),
   height = 10,
    width = 13,
    create.dir = T
  )

}





