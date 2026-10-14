
#import created dateframe biocontrol and of coexistnece

type_data <- "real.data"
norm_data <- "not_normalized"

out_folder <- paste0("./outputs/LV_MAP/", type_data, "/", norm_data, "/")
fig_folder <- paste0("./figures/LV_MAP/", type_data, "/", norm_data, "/")



DATA_BIO <- read.csv(paste0(out_folder, "DATA_biocontrol.csv"))
DATA_COEX <- read.csv(paste0(out_folder, "DATA_coexistence.csv"))



##I add a modification to put R left
# assuming that 600 the initia input is 1. So we will have stuff above the 1
DATA_BIO$R <- DATA_BIO$R/400


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

plot_bio_coex_block(data_bio_coex_block = DATA_BIO_COEX_BLOCK, fig_path = fig_folder)









#=PER ENEMY======================================================================0


##para tener las means 
DATA_BIO_ENEM <- DATA_BIO |>
  dplyr::group_by(enem)|>
  dplyr::summarise("mean_R_consumed" = mean(R_consumed), "sd_R_consumed" = sd(R_consumed), 
                    "mean_R"= mean(R), "sd_R"= sd(R), 
                  "mean_R_bin"= mean(R_bin), "sd_R_bin"= sd(R_bin))


###nthis tell you in each reaplicate, the time to extinction to each predator, and
#the coexistnece time (the first one to surve)
#60 rows! 
DATA_SURV_ENEM <- DATA_SURV_BLOCK|>
  dplyr::group_by(enem)|>
  dplyr::summarise("mean_surv" = mean(surv_coex)/20, "sd_surv" = sd(surv_coex)/20)  #area under the curve, normalized
##now we merge the both data to have the complete per block and per eem

DATA_BIO_COEX_ENEM <- dplyr::left_join(DATA_BIO_ENEM, DATA_SURV_ENEM, by = c("enem"))



###PLOTS FIG res#####################

final_fig_folder <- paste0(fig_folder, "figuresPaper/")



plot_fig_bio_coex(data_bio_coex = DATA_BIO_COEX_ENEM, fig_path = final_fig_folder)












##para tener las means 
DATA_BIO_ENEM <- DATA_BIO |>
  dplyr::group_by(enem)|>
  dplyr::summarise("mean_R_consumed" = mean(R_consumed), "sd_R_consumed" = sd(R_consumed), 
                    "mean_R"= mean(R), "sd_R"= sd(R), 
                  "mean_R_bin"= mean(R_bin), "sd_R_bin"= sd(R_bin))



write.csv(DATA_BIO_ENEM, "./outputs/summ_meansR.csv")









