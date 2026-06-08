##code for explorations 

DATA_IGP <- readr::read_csv("data/dataIGP_2025.csv")


##here we use 2 formats of data
DATA_LONG <-  long_formatter(DATA_IGP)
DATA_MEAN <-  mean_formatter(DATA_LONG)
DATA_PRED <-  pred_formatter(DATA_LONG)

### we gonna detrend.. so this is hard, and I wonder how it looks. 


#here I create a simple function to see how the data changes..


##here normalized not detrended 
##normalization
DATA_PRED_norm <-DATA_PRED  |> 
  dplyr::group_by(enem) |> 
  dplyr::mutate(R = R/max(R, na.rm = TRUE), X = X/max(X, na.rm = TRUE), Y = Y/max(Y, na.rm = TRUE))


##we detrend using first differences

DATA_PRED_std<-DATA_PRED |> 
  dplyr::group_by(block, enem) |> 
  dplyr::mutate(X= c(NA, diff(X)), Y= c(NA, diff(Y)), R= c(NA, diff(R)))|> 
  tidyr::drop_na()  

##normalization
DATA_PRED_std_norm <-DATA_PRED_std  |> 
  dplyr::group_by(enem) |> 
  dplyr::mutate(R = R/max(R, na.rm = TRUE), X = X/max(X, na.rm = TRUE), Y = Y/max(Y, na.rm = TRUE))










typeData = c("norm", "diff_norm", "abs", "diff_abs")


for (data in typeData){

if (data== "norm"){chosen_data <- DATA_PRED_norm}
if (data== "diff_norm"){chosen_data <- DATA_PRED_std_norm}
if (data== "abs"){chosen_data <- DATA_PRED}
if (data== "diff_abs"){chosen_data <- DATA_PRED_std}


 ##plot tjis data

  ###
plot_ts <-  data_ts_CCM(chosen_data)

ggsave(
    plot_ts,
    filename = paste0("./outputs/CCM/", data, "/", "plot_ts.png"),
    height = 10,
    width = 10,
    create.dir = T
  )

  
  
full_list_ccm <- list()

for (enemies in unique(chosen_data$enem)) {
  data_enem <- chosen_data |> 
    dplyr::filter(enem == enemies)
  print(enemies)
  list_ccm_enem <- multisp_CCM_igp(x = data_enem, niter = 1000)
  
  # Create a nested list with enemy name and results
  full_list_ccm[[as.character(enemies)]] <- list(
    enem = enemies,
    list_ccm_enem = list_ccm_enem
  )
}

#condensed results
#this add a column to the data frame of the enemi, and will bind them 
ccm_sp_igp <- data.frame()

for (enemies in names(full_list_ccm)){
  full_list_ccm[[enemies]]$list_ccm_enem$result$enem <- enemies
  ccm_sp_igp <-rbind(ccm_sp_igp, full_list_ccm[[enemies]]$list_ccm_enem$result)  
}

  
dir.create(paste0("./outputs/CCM/", data)) 
write.csv(ccm_sp_igp, paste0("./outputs/CCM/", data, "/", "ccm_sp_igp.csv"))
############now lets plot the embedding dimension and the rho

##first the pho

#function to plot the rho for each time series and for each enem 

##extreact the rho values for each enem and put ir into a data frame 

RHO_DATA <- data.frame()

for (enemies in names(full_list_ccm)){
  print(enemies)
  
  if(is.na(full_list_ccm[[enemies]]$list_ccm_enem$CCM_boot_A)[[1]]){
    rho_X <- 0
    rho_X_sdev <- 0
    lobsX <- 0
    print("case1")
  } else {
    rho_X <- full_list_ccm[[enemies]]$list_ccm_enem$CCM_boot_A$rho
    rho_X_sdev <- full_list_ccm[[enemies]]$list_ccm_enem$CCM_boot_A$sdevrho
    lobsX <- full_list_ccm[[enemies]]$list_ccm_enem$CCM_boot_A$Lobs
  }
  
  if(is.na(full_list_ccm[[enemies]]$list_ccm_enem$CCM_boot_B)[[1]]){
    rho_Y <- 0
    rho_Y_sdev <- 0
    lobsY <- 0
    print("case2")
  } else {
    rho_Y <- full_list_ccm[[enemies]]$list_ccm_enem$CCM_boot_B$rho
    rho_Y_sdev <- full_list_ccm[[enemies]]$list_ccm_enem$CCM_boot_B$sdevrho
    lobsY <- full_list_ccm[[enemies]]$list_ccm_enem$CCM_boot_B$Lobs
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

write.csv(RHO_DATA,  paste0("./outputs/CCM/", data, "/",  "RHO_DATA.csv"))

rho_plot <- plot_rho_CCM(RHO_DATA)

ggsave(
    rho_plot,
    filename = paste0("./outputs/CCM/", data, "/", "rho_plot.png"),
    height = 10,
    width = 10,
    create.dir = T
  )


}
#plot(full_list_ccm$`cc+ma`$list_ccm_enem$CCM_boot_A$rho)








#to run rest:
#so normaly it wull be donde by ene
#x<- chosen_data |> dplyr::filter(enem=="cc+ma")


#this is how the data looks 

