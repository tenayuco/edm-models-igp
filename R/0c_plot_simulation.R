full_plot <- function(outDF, plotted_var = c("R", "N", "P"), tmax=2000, disc_cont){
  
  outDF$N <-  outDF$Nl + outDF$Na

  outDF <- outDF |> 
    dplyr::filter(time <= tmax)

  outLong <-  outDF  |> 
    tidyr::pivot_longer(cols= (R:N), names_to = "varName", values_to = "value") |> 
    dplyr::filter(varName %in% plotted_var)
    
  
  RNP_ts <- outLong |> 
    dplyr::mutate(orden= dplyr::case_when(varName=="P" ~ 2,
                           varName=="N" ~1,
                           varName == "R" ~0)) |> 
    ggplot(aes(x=time, y=value)) +
    geom_line(aes(color= varName), linewidth=1) + 
    geom_point(color="black")+
    xlab("Time") +
    xlim(0, tmax)+
    facet_wrap(~forcats::fct_reorder(varName, orden, .desc = TRUE), scales = "free", ncol = 1)+
    theme_bw()+ 
    theme(plot.subtitle = element_text(hjust = 0.5, size = 12), 
      text = element_text(size = 12),
      axis.text.x=element_text(angle=60, hjust=1, size = 12),
    legend.position = "none")
  
  
  RNP_PN <- outDF |> 
    ggplot(aes(x= N, y=P))+
    geom_path(aes(colour= time), linewidth=1)+
     geom_point(color="black")+
    scale_colour_viridis_c()+
    theme_bw()+
    theme(legend.position = "none")

  RNP_PR <- outDF |> 
    ggplot(aes(x= R, y=P))+
    geom_path(aes(colour= time), linewidth=1)+
     geom_point(color="black")+
    scale_colour_viridis_c()+
    theme_bw()+
    theme(legend.position = "none")

  RNP_NR <- outDF |> 
    ggplot(aes(x= R, y=N))+
    geom_path(aes(colour= time), linewidth=1)+
     geom_point(color="black")+
    scale_colour_viridis_c()+
    theme_bw()+
    theme(legend.position = "none")
  

  FULL_PLOT <- RNP_ts + (RNP_PN / RNP_PR /RNP_NR)

  ggsave(
    FULL_PLOT,
    filename = paste0("./figures/simulation/fullPlot_", "tmax_", tmax, "_mode_", disc_cont, ".png"),
    height = 10,
    width = 8,
    create.dir = T
  )
}