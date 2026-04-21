###functions of plots related 






rt_time_plotter <- function(df_rt, replicate= "replicate", plotted_var = c("N", "P")){


  outLong <-  df_rt |> 
    tidyr::pivot_longer(cols= !c(replicate, time), names_to = "varName", values_to = "value") 
    
  
  rt_ts <- outLong |> 
    ggplot(aes(x=time, y=value)) +
    geom_line(aes(color= as.factor(replicate), group=replicate), linewidth=1) + 
    xlab("Time") +
    facet_grid(~varName, scales = "free")+
    #facet_grid(varName~replicate, scales = "free")+

    theme_bw()
  
  return(rt_ts)
}



alpha_time_plotter <- function(df_alpha, replicate= "replicate"){


  outLong <-  df_alpha |> 
    tidyr::pivot_longer(cols= !c(replicate, time), names_to = "varName", values_to = "value") 
    

  alpha_ts <- outLong |> 
    ggplot(aes(x=time, y=value)) +
    geom_line(aes(color= as.factor(replicate), group=replicate), linewidth=1) + 
    xlab("Time") +
    facet_grid(~varName, scales = "free")+
    #facet_grid(varName~replicate, scales = "free")+

    theme_bw()
  
  return(alpha_ts)
}



rt_alpha_mean_sd_plotter <- function(df_alpha, df_alpha_sd, replicate= "replicate"){


  outLong <-  df_alpha |> 
    tidyr::pivot_longer(cols= !c(replicate, time), names_to = "varName", values_to = "value") |> 
    dplyr::group_by(replicate, varName)|> 
    dplyr::summarise(mvalue = mean(value)) 
  
  outLong_sd <-  df_alpha_sd |> 
    tidyr::pivot_longer(cols= !c(replicate, time), names_to = "varName", values_to = "value") |> 
    dplyr::group_by(replicate, varName)|> 
    dplyr::summarise(mvalue = mean(value)) 

  outLong_total <- dplyr::full_join(outLong, outLong_sd, by=c("varName", "replicate"), suffix= c(".mean", ".sd"))


  alpha_mean_sd <- outLong_total |> 
    ggplot(aes(x= replicate, y= mvalue.mean)) +
    geom_point()+
    geom_errorbar(aes(ymin=mvalue.mean- 1.96*mvalue.sd,  ymax=mvalue.mean+ 1.96*mvalue.sd), width=.2,
                 position=position_dodge(0.05))+
    xlab("Replicate") +
    facet_grid(~varName, scales = "free")+
    #facet_grid(varName~replicate, scales = "free")+

    theme_bw()
  
  return(alpha_ts)
}



