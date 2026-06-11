##1. DATA preparation


DATA_IGP <- readr::read_csv("data/dataIGP_2025.csv")

source("./analyses/3a.CCM_prepare_data.R")
source("./analyses/3b.runCCM.R")

#fist prepre data

DATA_PRED <- prep_data(raw_data_igp = DATA_IGP)

plot_ts <- data_ts_CCM(DATA_PRED)

detrend_data = TRUE

DATA <-  norm_detrend_data(data_pred = DATA_PRED, de_trend = detrend_data, remove_last_ceros =TRUE)


plot_ts_prep <-  data_ts_CCM(DATA)

ggsave(plot_ts,filename = paste0("./outputs/CCM/", "detrend_", detrend_data,  "/", "plot_ts.png"),height = 10,width = 10,create.dir = T
  )


#hist_data(DATA)  #too see the length of replicates.. 


list_CCM_total <- run_total_enemies_CCM(data_prep = DATA, niter= 1000)



saveRDS(list_CCM_total, file  = paste0("./outputs/CCM/", "detrend_", detrend_data,  "/", 
"list_CCM_total.rds"))
 ##plot tjis data

  ###

##now

#############run from here if.. 

list_CCM_total <- readRDS("./outputs/CCM/detrend_TRUE/list_CCM_total.rds")


#condensed results

##embedding dimension

embed_DF <- data.frame()

for (enem in names(list_CCM_total)){
  embed_temp <-  as.data.frame(list_CCM_total[[enem]]$list_ccm_enem$Emat)
  embed_temp$enem <- enem
  embed_temp$embDim <- seq(1:nrow(embed_temp))
  embed_DF <- rbind(embed_DF, embed_temp)

}


##plottinh embedding 
embedding_plotter(embed_DF)



#this add a column to the data frame of the enemi, and will bind them 
ccm_sp_igp <- data.frame()

for (enemies in names(list_CCM_total)){
  list_CCM_total[[enemies]]$list_ccm_enem$result$enem <- enemies
  ccm_sp_igp <-rbind(ccm_sp_igp, list_CCM_total[[enemies]]$list_ccm_enem$result)  
}

  
dir.create(paste0("./outputs/CCM/", detrend_data)) 
write.csv(ccm_sp_igp, paste0("./outputs/CCM/", "detrend_", detrend_data, "/", "ccm_sp_igp.csv"))
############now lets plot the embedding dimension and the rho

##first the pho

#function to plot the rho for each time series and for each enem 

##extreact the rho values for each enem and put ir into a data frame 

RHO_DATA <- data.frame()

for (enemies in names(list_CCM_total)){
  print(enemies)
  
  if(is.na(list_CCM_total[[enemies]]$list_ccm_enem$CCM_boot_A)[[1]]){
    rho_X <- 0
    rho_X_sdev <- 0
    lobsX <- 0
    print("case1")
  } else {
    rho_X <- list_CCM_total[[enemies]]$list_ccm_enem$CCM_boot_A$rho
    rho_X_sdev <- list_CCM_total[[enemies]]$list_ccm_enem$CCM_boot_A$sdevrho
    lobsX <- list_CCM_total[[enemies]]$list_ccm_enem$CCM_boot_A$Lobs
  }
  
  if(is.na(list_CCM_total[[enemies]]$list_ccm_enem$CCM_boot_B)[[1]]){
    rho_Y <- 0
    rho_Y_sdev <- 0
    lobsY <- 0
    print("case2")
  } else {
    rho_Y <- list_CCM_total[[enemies]]$list_ccm_enem$CCM_boot_B$rho
    rho_Y_sdev <- list_CCM_total[[enemies]]$list_ccm_enem$CCM_boot_B$sdevrho
    lobsY <- list_CCM_total[[enemies]]$list_ccm_enem$CCM_boot_B$Lobs
  }
  
  temp_data_X <- data.frame(
    lobs = lobsX,
    rho = rho_X,
    rho_sdev = rho_X_sdev,
    variable = "X"
  )

  temp_data_Y <- data.frame(
    lobs = lobsY,
    rho = rho_Y,
    rho_sdev = rho_Y_sdev,
    variable = "Y"
  )

  temp_data <- rbind(temp_data_X, temp_data_Y)
  temp_data$enem <-  enemies

  RHO_DATA<- rbind(RHO_DATA, temp_data)
}

write.csv(RHO_DATA,  paste0("./outputs/CCM/", "detrend_", detrend_data, "/",  "RHO_DATA.csv"))

rho_plot <- plot_rho_CCM(RHO_DATA)

ggsave(
    rho_plot,
    filename = paste0("./outputs/CCM/", "detrend_", detrend_data, "/", "rho_plot.png"),
    height = 10,
    width = 10,
    create.dir = T
  )











#to run rest:
#so normaly it wull be donde by ene
#x<- chosen_data |> dplyr::filter(enem=="cc+ma")


#this is how the data looks 

### now we gonna run the test on the surrogates spatial

surro_df <- surrogater_all_enem_df(data=DATA_PRED)
plot_ts_surro <-  data_ts_CCM(surro_df)

ggsave(plot_ts_surro,filename = paste0("./outputs/CCM/", "detrend_", detrend_data,  "/", "plot_ts_surro.png"),height = 10,width = 10,create.dir = T
  )


#now I gonna apply the cCM to this new stuff, what would be the embedding and all of that.. 