DATA_IGP <- readr::read_csv("data/dataIGP_2025.csv")


##here we use 2 formats of data
DATA_LONG <-  long_formatter(DATA_IGP)
DATA_MEAN <-  mean_formatter(DATA_LONG)
DATA_PRED <-  pred_formatter(DATA_LONG)

### we gonna detrend.. so this is hard, and I wonder how it looks. 

##we detrend
DATA_PRED_std <-DATA_PRED  |> 
 dplyr::mutate(X=detrend_data(X), Y=detrend_data(Y), R=detrend_data(R))

##we dont deterdn
DATA_PRED_std <- DATA_PRED

ccm_sp_igp <- data.frame()

for (enemies in unique(DATA_PRED_std$enem)){
  DATA_PRED_enem <- DATA_PRED_std |> 
  dplyr::filter(enem==enemies)
  ccm_enem <-  multisp_CCM_igp(x = DATA_PRED_enem, niter = 1000)
  ccm_enem$enem <- enemies
  ccm_sp_igp <-rbind(ccm_sp_igp, ccm_enem)  
}

#so normaly it wull be donde by ene
#DATA_PRED_std_ex  <- DATA_PRED_std |> 
 # dplyr::filter(enem=="cc+ma")


#this is how the data looks 

