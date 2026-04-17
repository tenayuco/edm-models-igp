full_plot <- function(outDF, plotted_var = c("R", "N", "P"), disc_cont= "notSpec"){
  

  if ("Nl" %in% names(outDF)){
  outDF$N <-  outDF$Nl + outDF$Na}

  #outDF <- outDF |> 
   # dplyr::filter(time <= tmax)|> 
    #dplyr::filter(time >= tmin)

  ###now, here we remove by the resolution



  outLong <-  outDF  |> 
    tidyr::pivot_longer(cols= c(R,N,P), names_to = "varName", values_to = "value") |> 
    dplyr::filter(varName %in% plotted_var)
    
  
  RNP_ts <- outLong |> 
    dplyr::mutate(orden= dplyr::case_when(varName=="P" ~ 2,
                           varName=="N" ~1,
                           varName == "R" ~0)) |> 
    ggplot(aes(x=time, y=value)) +
    geom_line(aes(color= varName), linewidth=1) + 
  #  geom_point(color="black")+
    xlab("Time") +
  #  xlim(tmin, tmax)+
    facet_wrap(~forcats::fct_reorder(varName, orden, .desc = TRUE), scales = "free", ncol = 1)+
    theme_bw()+ 
    theme(plot.subtitle = element_text(hjust = 0.5, size = 12), 
      text = element_text(size = 12),
      axis.text.x=element_text(angle=60, hjust=1, size = 12),
    legend.position = "none")
  
  
  RNP_PN <- outDF |> 
    ggplot(aes(x= N, y=P))+
   # geom_path(aes(colour= time), linewidth=1)+
     geom_point(color="black")+
    scale_colour_viridis_c()+
    theme_bw()+
    theme(legend.position = "none")

  RNP_PR <- outDF |> 
    ggplot(aes(x= R, y=P))+
   # geom_path(aes(colour= time), linewidth=1)+
     geom_point(color="black")+
    scale_colour_viridis_c()+
    theme_bw()+
    theme(legend.position = "none")

  RNP_NR <- outDF |> 
    ggplot(aes(x= R, y=N))+
    #geom_path(aes(colour= time), linewidth=1)+
     geom_point(color="black")+
    scale_colour_viridis_c()+
    theme_bw()+
    theme(legend.position = "none")
  

  FULL_PLOT <- RNP_ts + (RNP_PN / RNP_PR /RNP_NR)
  reso <- round(max(outLong$time)/(dim(outLong)[1]-1),3)

  ggsave(
    FULL_PLOT,
    filename = paste0("./figures/simulation/fullPlot_", "tmin_", min(outLong$time),"_tmax_", max(outLong$time),  "_mode_", disc_cont, "_reso_",reso,  ".png"),
    height = 10,
    width = 8,
    create.dir = T
  )
}