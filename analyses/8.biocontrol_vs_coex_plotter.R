
#import created dateframe biocontrol and of coexistnece

type_data <- "real.data"
norm_data <- "not_normalized"

out_folder <- paste0("./outputs/LV_MAP/", type_data, "/", norm_data, "/")
fig_folder <- paste0("./figures/LV_MAP/", type_data, "/", norm_data, "/")



DATA_BIO <- read.csv(paste0(out_folder, "DATA_biocontrol.csv"))
DATA_COEX <- read.csv(paste0(out_folder, "DATA_coexistence.csv"))


##first we plot biocontrol on its own

plot_total_bio(DATA_BIO, var_plotted = "R_consumed", fig_path =fig_folder)
plot_total_bio(DATA_BIO, var_plotted = "R", fig_path =fig_folder)
plot_total_bio_week(DATA_BIO, var_plotted = "R_consumed", fig_path =fig_folder)
plot_total_bio_week(DATA_BIO, var_plotted = "R", fig_path =fig_folder)




#now we gonna do the coexistnece, which can also be tracked per block and compare with biocontorl



####PLOTTING COEXISTENCE AND BIOCONTROL FROM DATA###########################


#this average all the replicates and keep the week 
DATA_COEX_AV <- coex_average(DATA_COEX)

#this plot this coexstnece and area stuff to visualizae
plotter_coex_step(DATA_COEX_AV, fig_path = fig_folder)




#NOW PER BLOCK

##now what is the biocontorl per block


##para tener las means 
DATA_BIO_ENEM_BLOCK <- DATA_BIO |>
  dplyr::group_by(enem, block)|>
  dplyr::summarise("mean_R_consumed" = mean(R_consumed), "sd_R_consumed" = sd(R_consumed), 
                    "mean_R"= mean(R), "sd_R"= sd(R), 
                  "mean_R_bin"= mean(R_bin), "sd_R_bin"= sd(R_bin))


###nthis tell you in each reaplicate, the time to extinction to each predator, and
#the coexistnece time (the first one to surve)
#60 rows! 
DATA_SURV_BLOCK <- survival_time_per_run(DATA_COEX)


##now we merge the both data to have the complete per block and per eem

DATA_BIO_COEX_BLOCK <- dplyr::left_join(DATA_BIO_ENEM_BLOCK, DATA_SURV_BLOCK, by = c("enem", "block"))


##plt 


PLOT_BIO_COEX_BLOCK <- DATA_BIO_COEX_BLOCK |> 
    ggplot(aes(x = surv_coex, y = mean_R)) +
    geom_pointrange(aes(ymin = mean_R-sd_R, ymax =  mean_R +sd_R,  
                  fill = enem, shape= enem), size =1)  +
     # facet_wrap(~enem, ncol = num_columns) +
    theme_bw()+
    #labs(subtitle= paste0(chosen_coex_var, " vs ", paste(chosen_int, collapse = "_")), x= "Magnitude of interaction")+
    scale_fill_viridis_d(option = "inferno")+
    scale_shape_manual(values = c(21, 22, 23, 24, 25, 21))+
   theme(axis.text=element_text(size=12))
  
  ggsave(
    coex_int_plot,
    filename = paste0(fig_path, "coex_int_", chosen_coex_var, "_var_", paste(chosen_int, collapse = "_"),  ".png"),
    height = 9,
    width = 12,
    create.dir = T
  )




















#=PER ENEMY



























#thisgives you te average per enemy. #6 rows!
DATA_SURV_AV <- survival_time_average(DATA_SURV)
#=======================================================================================


#










##para tener las means 
DATA_BIO_ENEM <- DATA_BIO |>
  dplyr::group_by(enem)|>
  dplyr::summarise("mean_R_consumed" = mean(R_consumed), "sd_R_consumed" = sd(R_consumed), 
                    "mean_R"= mean(R), "sd_R"= sd(R), 
                  "mean_R_bin"= mean(R_bin), "sd_R_bin"= sd(R_bin))



write.csv(DATA_BIO_ENEM, "./outputs/summ_meansR.csv")









