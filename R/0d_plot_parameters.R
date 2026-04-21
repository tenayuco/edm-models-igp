###functions of plots related 






par_time_plotter <- function(df_par, replicate= "replicate", plotted_var = c("N", "P")){


  outLong <-  df_par |> 
    tidyr::pivot_longer(cols= !c(replicate, time), names_to = "varName", values_to = "value") 
    
  
  par_ts <- outLong |> 
    ggplot(aes(x=time, y=value)) +
    geom_line(aes(color= as.factor(replicate), group=replicate), linewidth=1) + 
    xlab("Time") +
    facet_grid(~varName, scales = "free")+
    #facet_grid(varName~replicate, scales = "free")+

    theme_bw()
  
  return(par_ts)
}




par_mean_sd_plotter <- function(df_par, df_par_se, replicate= "replicate", num_col=2){


  outLong <-  df_par |> 
    tidyr::pivot_longer(cols= !c(replicate, time), names_to = "varName", values_to = "value") |> 
    dplyr::group_by(replicate, varName)|> 
    dplyr::summarise(mvalue = mean(value)) 
  
  outLong_sd <-  df_par_se |> 
    tidyr::pivot_longer(cols= !c(replicate, time), names_to = "varName", values_to = "value") |> 
    dplyr::group_by(replicate, varName)|> 
    dplyr::summarise(mvalue = mean(value)) 

  outLong_total <- dplyr::full_join(outLong, outLong_sd, by=c("varName", "replicate"), suffix= c(".mean", ".sd"))


  par_mean_sd <- outLong_total |> 
    ggplot(aes(x= replicate, y= mvalue.mean)) +
    geom_point()+
    geom_errorbar(aes(ymin=mvalue.mean- 1.96*mvalue.sd,  ymax=mvalue.mean+ 1.96*mvalue.sd), width=.2,
                 position=position_dodge(0.05))+
    xlab("Replicate") +
    geom_segment(aes(x = min(replicate)-0.5, y = 0, xend = max(replicate)+0.5, yend = 0), color= "darkred", linetype= "dashed")+

    facet_wrap(~varName, scales = "free", ncol= num_col)+
    #facet_grid(varName~replicate, scales = "free")+

    theme_bw()
  
  return(par_mean_sd)
}



