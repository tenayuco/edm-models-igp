#This code will do every step of the CCM process. The basal functions will be stores in /R and the subprotocols as 3.a, 3.b, 3.c

#1. import data for subprotocols. 

source("./analyses/3a.CCM_prepare_data.R")
source("./analyses/3b.runCCM.R")



##1. DATA preparation  (all enemies together)


DATA_IGP <- readr::read_csv("data/dataIGP_2025.csv")
DATA_PRED <- prep_data(raw_data_igp = DATA_IGP)

##lets remove ec+am, ec+sr, no real utility as the data is super ba

DATA_PRED <-  DATA_PRED |> dplyr::filter(enem!="ec+am")|> dplyr::filter(enem!="ec+sr")

#plot_ts <- data_ts_CCM(DATA_PRED)

#check if you defined them outside or inside the scritp
#det_method = "firstDiff"
#no_method = "zscore"

##creates the general conection for the output

dir.create(paste0("./outputs/CCM/", "detrend_", det_method,  "/", "norm_", no_method, "/"), recursive = T)


#the methods are..
DATA <-  norm_detrend_data(data_pred = DATA_PRED, detrend_method = det_method,  
                             norm_method = no_method)


###examples ofthe surro
#DATA_PRED_SURRO <- surrogater_df(DATA_PRED)
#DATA_SURRO <-  norm_detrend_data(data_pred = DATA_PRED_SURRO, de_trend = detrend_data)

#some plots to show
#plot_ts_pred <-  data_ts_CCM(DATA_PRED)
#plot_ts_data <-  data_ts_CCM(DATA)
#plot_ts_pred_surro <-  data_ts_CCM(DATA_PRED_SURRO)
#plot_ts_surro <-  data_ts_CCM(DATA_SURRO)

#hist_data(DATA)  #too see the length of replicates.. 


##2run tests 

#check if the iteraction are defined outside or inside 
#iteraciones <- 10  # estas son las iteraciones de clark, en realidad lo que me importa es el final L

###this is with the data 
tic()
list_CCM_total <- run_ccm_per_enem(data_prep = DATA, niter= iteraciones, min_per = 1)  #el min per es paraver cual escoger de  E
toc()

##this we save 

saveRDS(list_CCM_total, file  = paste0("./outputs/CCM/", "detrend_", det_method,  "/", "norm_", no_method, "/", 
"list_CCM_total_", "niter_", iteraciones, ".rds"))



### now from this we can get some of the infoo lets run the surrogates and save it in a mega data frame
### test of the surrogetes
  
#  if(use_surrogate ==TRUE){
 # data_enem <-  surrogater_df(data_enem)}
#check if it is defined outsie 
#numSurro =50 ##esto es relevante 



#data_ts_CCM(data_pred = DATA_PRED)
#data_ts_CCM(data_pred = DATA_SURRO)

tic()
mega_list_CCM_surrogates <- list()

for (i in (1:numSurro)){

  DATA_PRED_SURRO <- surrogater_df_twin(DATA_PRED)
  DATA_SURRO <-  norm_detrend_data(data_pred = DATA_PRED_SURRO, detrend_method = det_method,  
                             norm_method = no_method)
#plot_ts_prep <-  data_ts_CCM(DATA)

list_CCM_surro <- run_ccm_per_enem_surro(data_surro = DATA_SURRO, niter= iteraciones, list_data = list_CCM_total)
 
RHO_SURRO <- rho_data_sum(list_CCM = list_CCM_surro)

  
# Create a nested list with enemy name and results
  mega_list_CCM_surrogates[[as.character(i)]] <- list(
    surro = i,
    list_CCM_surro = list_CCM_surro
  )
  
}
toc()


saveRDS(mega_list_CCM_surrogates, file  = paste0("./outputs/CCM/", "detrend_", det_method,  "/", "norm_", no_method, "/", 
"mega_list_CCM_", "surro_", numSurro, "_niter_", iteraciones, ".rds"))



#################PART 2/ Extracting infor and plotting #########################33




### let start with data
##so here you take the method of detrend and norm that you want. 
## and also check how many iterations you want
used_det_method = det_method
used_no_method = no_method

##if you want to choose different
#used_det_method = "firstDiff"
#used_no_method = "zscore"

used_path= paste0("./outputs/CCM/", "detrend_", used_det_method,  "/", "norm_", used_no_method, "/")
fig_path= paste0("./figures/CCM/", "detrend_", used_det_method,  "/", "norm_", used_no_method, "/")


list_CCM_total <- readRDS(paste0(used_path, "list_CCM_total_niter_", iteraciones, ".rds"))
mega_list_CCM_surrogates <- readRDS(paste0(used_path, "mega_list_CCM_surro_", numSurro, "_niter_", iteraciones,".rds"))


#condensed results


EMBED_DF <- embed_df_sum(list_CCM = list_CCM_total)

##plottinh embedding 
# 
PLOT_EMB <- embedding_plotter(EMBED_DF) + ggtitle(paste0("detrend_", used_det_method, "_norm_", used_no_method))

ggsave(
    PLOT_EMB,
    filename = paste0(fig_path, "rho_embedding_plot", "_it_", iteraciones, ".png"),
    height = 10,
    width = 20,
    create.dir = T
  )

### function to create the signficante test data frame

#---------------------extract significancy----------------------------------------



###now to run the CCM_test that also summarizes the embedding dim 
CCM_TEST <- ccm_sp_test(list_CCM = list_CCM_total)


write.csv(CCM_TEST, paste0(used_path, "ccm_sp_igp",  "_it_", iteraciones,".csv"))
############now lets plot the embedding dimension and the rho

##first the pho

#function to plot the rho for each time series and for each enem 



RHO_DATA <- rho_data_sum(list_CCM = list_CCM_total)

write.csv(RHO_DATA,  paste0(fig_path,  "RHO_DATA",  "_it_", iteraciones,". csv"))

RHO_PLOT <- plot_rho_CCM(RHO_DATA)+ ggtitle(paste0("detrend_", used_det_method, "_norm_", used_no_method))


ggsave(
    RHO_PLOT,
    filename = paste0(fig_path, "rho_plot",  "_it_", iteraciones,".png"),
    height = 10,
    width = 15,
    create.dir = T
  )




###now the predictive steps

PRED_STEPS_DATA <- rho_preSteps(list_CCM = list_CCM_total)
RHO_STEPS_PLOT <- rho_pred_plotter(predSteps_DF = PRED_STEPS_DATA)+ ggtitle(paste0("detrend_", used_det_method, "_norm_", used_no_method))


ggsave(
    RHO_STEPS_PLOT,
    filename = paste0(fig_path, "rho_steps_plot",  "_it_", iteraciones,".png"),
    height = 10,
    width = 15,
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
  dplyr::summarise(rho_median = median(rho, na.rm = T), q5 = quantile(rho, 0.05, na.rm = T), q95 =quantile(rho, 0.95, na.rm = T))


#RHO_PLOT_SUR <- plot_rho_CCM(TOTAL_RHO_SURRO_SUM)


write.csv(TOTAL_RHO_SURRO_SUM,  paste0(used_path,  "surrogate_rho",  "_it_", iteraciones,".csv"))

##now i merge them..
##but I leave only the mean of the real data... 

RHO_DATA$rho_median <- RHO_DATA$rho # we remove this.. 
RHO_DATA$rho <- NULL
RHO_DATA$rho_sdev <- NULL # we remove this.. 
RHO_DATA$q5 <- NA
RHO_DATA$q95 <- NA

RHO_DATA$cat <- "data"
TOTAL_RHO_SURRO_SUM$cat <- "random"

RHO_DATA_SURRO <- rbind(RHO_DATA, TOTAL_RHO_SURRO_SUM)


PLOT_RHO_DATA_SURRO <- plot_rho_data_surro_CCM(rho_data_surro = RHO_DATA_SURRO, ccm_test = CCM_TEST)+ ggtitle(paste0("detrend_", used_det_method, "_norm_", used_no_method))




ggsave(
    PLOT_RHO_DATA_SURRO,
    filename = paste0(fig_path, "rho_data_surro",  "_it_", iteraciones,".png"),
    height = 10,
    width = 14,
    create.dir = T
  )

############  now the mega plot loop per enemy 


