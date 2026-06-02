DATA_IGP <- readr::read_csv("data/dataIGP_2025.csv")


##here we use 2 formats of data
DATA_LONG <-  long_formatter(DATA_IGP)
DATA_MEAN <-  mean_formatter(DATA_LONG)
DATA_PRED <-  pred_formatter(DATA_LONG)

###

DATA_TEST <-  DATA_PRED |> 
  dplyr::filter(enem== "cc+ma")



#this is how the data looks 


ccm_sp <- plyr::ddply(df_press4, .(Species), .fun = multisp_CCM, niter=1000, .parallel = F, .progress = "text")