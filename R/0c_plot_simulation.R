ts_plotter <- function(outDF, plotted_var = c("R", "N", "P"), replicate= "replicate", legenda= "none"){

  outLong <-  outDF  |> 
    tidyr::pivot_longer(cols= plotted_var, names_to = "varName", values_to = "value") |> 
    dplyr::filter(varName %in% plotted_var)
    
  
  RNP_ts <- outLong |> 
    dplyr::mutate(orden= dplyr::case_when(varName=="P" ~ 2,
                         varName=="N" ~1,
                        varName == "R" ~0)) |> 
    ggplot(aes(x=time, y=value)) +
    geom_line(aes(color= as.factor(replicate)), linewidth=1) + 
    xlab("Time") +
    facet_wrap(~forcats::fct_reorder(varName, orden, .desc = TRUE), scales = "free", ncol = 1)+
    #facet_grid(forcats::fct_reorder(varName, orden, .desc = TRUE)~replicate, scales = "free")+
    scale_colour_viridis_d()+

    theme_bw()+ 
    theme(plot.subtitle = element_text(hjust = 0.5, size = 12), 
      text = element_text(size = 12),
      axis.text.x=element_text(angle=60, hjust=1, size = 12),
    legend.position = legenda)
 
  return(RNP_ts)
}


  
phase_plotter <- function(outDF, var1= "N", var2= "P", replicate= "replicate"){

  PP <- outDF |> 
    ggplot(aes(x= !!sym(var1), y=!!sym(var2)))+
   # geom_path(aes(colour= time), linewidth=1)+
     geom_point(aes(color= as.factor(replicate)))+
    scale_colour_viridis_d()+
    theme_bw()+
    theme(legend.position = "none")

   return(PP)


 
}


