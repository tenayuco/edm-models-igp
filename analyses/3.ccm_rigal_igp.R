#This code will do every step of the CCM process. The basal functions will be stores in /R and the subprotocols as 3.a, 3.b, 3.c

#1. import data for subprotocols. 

source("./analyses/3a.CCM_prepare_data.R")
source("./analyses/3b.runCCM.R")



##1. DATA preparation  (all enemies together)


DATA_IGP <- readr::read_csv("data/dataIGP_2025.csv")
DATA_PRED <- prep_data(raw_data_igp = DATA_IGP)
#plot_ts <- data_ts_CCM(DATA_PRED)

detrend_data = T


DATA <-  norm_detrend_data(data_pred = DATA_PRED, de_trend = detrend_data)


###examples ofthe surro
DATA_PRED_SURRO <- surrogater_df(DATA_PRED)
DATA_SURRO <-  norm_detrend_data(data_pred = DATA_PRED_SURRO, de_trend = detrend_data)

#some plots to show
plot_ts_pred <-  data_ts_CCM(DATA_PRED)
plot_ts_data <-  data_ts_CCM(DATA)
plot_ts_pred_surro <-  data_ts_CCM(DATA_PRED_SURRO)
plot_ts_surro <-  data_ts_CCM(DATA_SURRO)

ggsave(plot_ts_pred,filename = paste0("./outputs/CCM/", "detrend_", detrend_data,  "/", "plot_ts_pred.png"),height = 10,width = 10,create.dir = T
  )
ggsave(plot_ts_data,filename = paste0("./outputs/CCM/", "detrend_", detrend_data,  "/", "plot_ts_data.png"),height = 10,width = 10,create.dir = T
  )
ggsave(plot_ts_pred_surro,filename = paste0("./outputs/CCM/", "detrend_", detrend_data,  "/", "plot_pred_surro.png"),height = 10,width = 10,create.dir = T
  )
ggsave(plot_ts_surro,filename = paste0("./outputs/CCM/", "detrend_", detrend_data,  "/", "plot_ts_surro.png"),height = 10,width = 10,create.dir = T
  )


#hist_data(DATA)  #too see the length of replicates.. 


##2run tests 
iteraciones <- 100

###this is with the data 
list_CCM_total <- run_ccm_per_enem(data_prep = DATA, niter= iteraciones, min_per = 0.8)

##this we save 

saveRDS(list_CCM_total, file  = paste0("./outputs/CCM/", "detrend_", detrend_data,  "/", 
"list_CCM_total_", "niter_", iteraciones, ".rds"))



### now from this we can get some of the infoo lets run the surrogates and save it in a mega data frame
### test of the surrogetes
  
#  if(use_surrogate ==TRUE){
 # data_enem <-  surrogater_df(data_enem)}

numSurro =100



#data_ts_CCM(data_pred = DATA_PRED)
#data_ts_CCM(data_pred = DATA_SURRO)

mega_list_CCM_surrogates <- list()

for (i in (1:numSurro)){

  DATA_PRED_SURRO <- surrogater_df_twin(DATA_PRED)
  DATA_SURRO <-  norm_detrend_data(data_pred = DATA_PRED_SURRO, de_trend = detrend_data)
  #print(data_ts_CCM(DATA_SURRO))
#plot_ts_prep <-  data_ts_CCM(DATA)
#print(paste0("surro_", i))
list_CCM_surro <- run_ccm_per_enem_surro(data_surro = DATA_SURRO, niter= iteraciones, list_data = list_CCM_total)

 
RHO_SURRO <- rho_data_sum(list_CCM = list_CCM_surro)

print(plot_rho_CCM(RHO_SURRO)) 
  
# Create a nested list with enemy name and results
  mega_list_CCM_surrogates[[as.character(i)]] <- list(
    surro = i,
    list_CCM_surro = list_CCM_surro
  )
  
}



saveRDS(mega_list_CCM_surrogates, file  = paste0("./outputs/CCM/", "detrend_", detrend_data,  "/", 
"mega_list_CCM_surrogates_", "niter_", iteraciones, ".rds"))



#################PART 2/ Extracting infor and plotting #########################33




#list_CCM_total <- run_ccm_per_enem(data_prep = DATA, niter= 10, min_per = 0.8, use_surrogate = F)



### let start with data



list_CCM_total <- readRDS("./outputs/CCM/detrend_TRUE/list_CCM_total_niter_100.rds")
mega_list_CCM_surrogates <- readRDS("./outputs/CCM/detrend_TRUE/mega_list_CCM_surrogates_niter_100.rds")


#condensed results


EMBED_DF <- embed_df_sum(list_CCM = list_CCM_total)

##plottinh embedding 
# 
PLOT_EMB <- embedding_plotter(EMBED_DF)

ggsave(
    PLOT_EMB,
    filename = paste0("./outputs/CCM/", "detrend_", detrend_data, "/", "rho_embedding_plot.png"),
    height = 10,
    width = 10,
    create.dir = T
  )

### function to create the signficante test data frame

#---------------------extract significancy----------------------------------------



###now to run the CCM_test that also summarizes the embedding dim 
CCM_TEST <- ccm_sp_test(list_CCM = list_CCM_total)


dir.create(paste0("./outputs/CCM/", detrend_data)) 
write.csv(CCM_TEST, paste0("./outputs/CCM/", "detrend_", detrend_data, "/", "ccm_sp_igp.csv"))
############now lets plot the embedding dimension and the rho

##first the pho

#function to plot the rho for each time series and for each enem 



RHO_DATA <- rho_data_sum(list_CCM = list_CCM_total)

write.csv(RHO_DATA,  paste0("./outputs/CCM/", "detrend_", detrend_data, "/",  "RHO_DATA.csv"))

RHO_PLOT <- plot_rho_CCM(RHO_DATA)

#ggsave(
 #   RHO_PLOT,
  #  filename = paste0("./outputs/CCM/", "detrend_", detrend_data, "/", "rho_plot.png"),
   # height = 10,
   # width = 10,
    #create.dir = T
 # )




###now the predictive steps

PRED_STEPS_DATA <- rho_preSteps(list_CCM = list_CCM_total)
RHO_STEPS_PLOT <- rho_pred_plotter(predSteps_DF = PRED_STEPS_DATA)

ggsave(
    RHO_STEPS_PLOT,
    filename = paste0("./outputs/CCM/", "detrend_", detrend_data, "/", "rho_steps_plot.png"),
    height = 10,
    width = 10,
    create.dir = T
  )



#############now lets do it for multiple and save the rho, the deviation 

TOTAL_RHO_SURRO <- data.frame()

for (numSurro in names(mega_list_CCM_surrogates)){
RHO_DATA_SURRO_temp <- rho_data_sum(list_CCM = mega_list_CCM_surrogates[[numSurro]]$list_CCM_surro)
RHO_DATA_SURRO_temp$surro <- numSurro
TOTAL_RHO_SURRO <- rbind(TOTAL_RHO_SURRO, RHO_DATA_SURRO_temp)
}


#now we summarize this shit  by the MEAN of this shit 

TOTAL_RHO_SURRO_SUM <- TOTAL_RHO_SURRO |> 
  dplyr::group_by(enem, lobs, variable)|> 
  dplyr::summarise(rho = mean(rho), rho_sdev = mean(rho_sdev))


#RHO_PLOT_SUR <- plot_rho_CCM(TOTAL_RHO_SURRO_SUM)


write.csv(TOTAL_RHO_SURRO_SUM,  paste0("./outputs/CCM/", "detrend_", detrend_data, "/",  "surrogate_rho.csv"))

##now i merge them..

RHO_DATA$cat <- "data"
TOTAL_RHO_SURRO_SUM$cat <- "random"

RHO_DATA_SURRO <- rbind(RHO_DATA, TOTAL_RHO_SURRO_SUM)


PLOT_RHO_DATA_SURRO <- plot_rho_data_surro_CCM(rho_data_surro = RHO_DATA_SURRO, ccm_test = CCM_TEST)



ggsave(
    PLOT_RHO_DATA_SURRO,
    filename = paste0("./outputs/CCM/", "detrend_", detrend_data, "/", "rho_data_surro.png"),
    height = 10,
    width = 14,
    create.dir = T
  )

############  now the mega plot loop per enemy 


