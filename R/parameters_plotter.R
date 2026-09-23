###functions of plots related 


########function to plot per treatment

plot_per_treatment <- function(out_subfolder, true_values){

##new method..
  # One-liner
all_dirs <- list.dirs(out_subfolder, recursive = TRUE)[-1]  # -1 removes the first element (root)

vec_treatments <- setdiff(all_dirs, dirname(all_dirs))

for (treatment in vec_treatments){
  for (i in seq(1:length(list.files(treatment,  pattern = ".rds")))){
  num_seed <- i
  print(list.files(treatment,  pattern = ".rds")[i])
    #fig subdolder is defined externallhy, waht out! 
  list_used <- readRDS(paste0(treatment,"/" , list.files(treatment,  pattern = ".rds")[i]))
  fig_path <- paste0(fig_subfolder, stringr::str_remove(treatment, out_subfolder), "/")
  plotter_lv_map_treatment(list_used, fig_path = fig_path, num_seed = num_seed, true_values = true_values)
  }
}
}




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




################33



plot_omega_allconditions <- function(df_sum){
  par_plot <- df_sum |> 
    ggplot(aes(x= enem, y= grand_mean_omega)) +
    geom_errorbar(aes(ymin=grand_mean_omega- 1*total_sd_omega,  ymax=grand_mean_omega+ 1*total_sd_omega, group= interaction(enem, rpresent),  color= as.factor(enem)), width=.2,
                 position=position_dodge(0.6), linewidth=1)+
    geom_point(aes(color= as.factor(enem), shape=as.factor(rpresent)), fill="white",  position=position_dodge(0.6), size=3)+
    scale_shape_manual(
      values = c("FALSE" = 21, "TRUE" = 17),  # 1 = empty circle, 17 = filled triangle
      name = "rpresent"
    )+

    xlab("Replicate and variable") +
    ggtitle(paste0("kernel_chosen ", kernel_chosen)) +
    
    geom_hline(yintercept = 0, color= "black", linetype= "dashed")+
     scale_color_viridis_d(begin=0, end= 0.7, option = "A", direction = 1) +

    theme_bw()

  
  ggsave(par_plot, filename = paste0(fig_subfolder, "omega_values.png"),
   height = 8,
    width = 8,
    create.dir = T
  )
  
}




network_plotters <- function(chosen_enem){


ONLY_INT <-  NET_DF |> 
  dplyr::filter(enem== chosen_enem) |> 
  dplyr::mutate(varName = ifelse(varName == "N", "N.X", 
                          ifelse(varName== "P", "P.Y", varName)))|> 
  dplyr::select(varName, grand_mean_pro, significance)|> 
  tidyr::separate(col=varName, into= c("target", "source"))|> 
  dplyr::relocate(source, target)   # <-- swap order

NODES <- data.frame(name = c("X", "Y", "N", "P"))

network <- igraph::graph_from_data_frame(d=ONLY_INT, vertices=NODES, directed=T) 




#done with deepseek


# --- Edge widths ---
# Handle negatives: scale to positive range
gm <- igraph::E(network)$grand_mean_pro
w  <- abs(gm)                    # or: scales::rescale(gm, to = c(1, 6))
#w  <- w*10 + 0.5       # keep a minimum visible width
w  <- 20*sqrt(w)       # keep a minimum visible width

# --- Edge colors: sign of grand_mean (optional but helpful) ---
edge_col <- ifelse(gm >= 0, "steelblue", "firebrick")
# --- Edge line types: solid when significant, dotted otherwise ---
edge_lty <- ifelse(igraph::E(network)$significance == "s", 1, 2)

# --- Plot ---

print(w)

png(paste0("./figures/LV_MAP/real.data/network/network_", chosen_enem,".png"),
    width = 1200, height = 1200, res = 200)
  

plot(
  network,
  edge.width     = w,
  edge.lty = edge_lty,
  edge.color     = edge_col,
  edge.curved    = 0.2,           # gentle curve; set to 0 to keep straight
  edge.loop.angle =  3/2*pi ,       # rotate self-loops so they don't overlap
  vertex.size    = c(X = 5, Y = 5, N = 20, P = 20)[igraph::V(network)$name],
  vertex.color   = "white",
  vertex.frame.color = "grey30",
  vertex.label.color  = "black",
  vertex.label.cex    = 1.1,
  layout         = matrix(
                     c(1.2, 1.2,      # X
                       0, 1.2,      # Y
                       1, 1,      # N
                       0.2, 1),     # P
                     ncol = 2, byrow = TRUE),
  rescale= FALSE, 
  main = as.character(chosen_enem)
)



# --- Close device LAST ---
dev.off()


}