#This code will do every step of the CCM process. The basal functions will be stores in /R and the subprotocols as 3.a, 3.b, 3.c

#1. import data for subprotocols. 

source("./analyses/3a.CCM_prepare_data.R")
source("./analyses/3b.runCCM.R")



##1. DATA preparation  (all enemies together)


DATA_IGP <- readr::read_csv("data/dataIGP_2025.csv")

#fist prepre data

DATA_PRED <- prep_data(raw_data_igp = DATA_IGP)
#plot_ts <- data_ts_CCM(DATA_PRED)

detrend_data = T

DATA <-  norm_detrend_data(data_pred = DATA_PRED, de_trend = T)
plot_ts_prep <-  data_ts_CCM(DATA)

ggsave(plot_ts,filename = paste0("./outputs/CCM/", "detrend_", detrend_data,  "/", "plot_ts.png"),height = 10,width = 10,create.dir = T
  )


#hist_data(DATA)  #too see the length of replicates.. 


##2run tests 


###this is with the data 
list_CCM_total <- run_ccm_per_enem(data_prep = DATA, niter= 10, min_per = 0.8, use_surrogate = F)

##this we save 

saveRDS(list_CCM_total, file  = paste0("./outputs/CCM/", "detrend_", detrend_data,  "/", 
"list_CCM_total.rds"))



### now from this we can get some of the infoo lets run the surrogates and save it in a mega data frame

numSurro =10

mega_list_CCM_surrogates <- list()


for (i in (1:numSurro)){


list_CCM_surro <- run_ccm_per_enem(data_prep = DATA, niter= 10, min_per = 0.8, use_surrogate = T)

# Create a nested list with enemy name and results
  mega_list_CCM_surrogates[[as.character(i)]] <- list(
    surro = i,
    list_CCM_surro = list_CCM_surro
  )
  
}



#list_CCM_total <- run_ccm_per_enem(data_prep = DATA, niter= 10, min_per = 0.8, use_surrogate = F)








list_CCM_total <- readRDS("./outputs/CCM/detrend_TRUE/list_CCM_total.rds")






list_CCM_total_surro <- run_ccm_per_enem(data_prep = DATA, niter= 10, min_per = 0.8, use_surrogate = T)





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

ggsave(
    RHO_PLOT,
    filename = paste0("./outputs/CCM/", "detrend_", detrend_data, "/", "rho_plot.png"),
    height = 10,
    width = 10,
    create.dir = T
  )




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





#to run rest:
#so normaly it wull be donde by ene
#x<- chosen_data |> dplyr::filter(enem=="cc+ma")


#this is how the data looks 

### now we gonna run the test on the surrogates spatial

#SURRO_DF <- surrogater_all_enem_df(data=DATA_PRED)

SURRO_DF <- surrogater_all_df(data = DATA_PRED)

#plot_ts_surro <-  data_ts_CCM(surro_df)

#ggsave(plot_ts_surro,filename = paste0("./outputs/CCM/", "detrend_", detrend_data,  "/", "plot_ts_surro.png"),height = 10,width = 10,create.dir = T
 # )


#now I gonna apply the cCM to this new stuff, what would be the embedding and all of that.. 


### first we put the embeddin optim data fra 


#this add a column to the data frame of the enemi, and will bind them 

dfEmbeddingCCM <- CCM_TEST[c("enem", "E_X", "E_Y")] |> 
  dplyr::rename(E_A= E_X, E_B=E_Y)


###now lets see this surrogate.. 


#############now lets do it for multiple and save the rho, the deviation 

numSurro =100

TOTAL_RHO_SURRO <- data.frame()

for (i in (1:numSurro)){
SURRO_DF <- surrogater_all_df(data = DATA_PRED)
list_CCM_surro <- run_total_enemies_CCM(data_prep =SURRO_DF , niter= 100, df_embedding_CCM = dfEmbeddingCCM, force_embedding = T)  
RHO_DATA_SURRO_temp <- rho_data_sum(list_CCM = list_CCM_surro)
RHO_DATA_SURRO_temp$surro <- i
TOTAL_RHO_SURRO <- rbind(TOTAL_RHO_SURRO, RHO_DATA_SURRO_temp)
}


#now we summarize this shit  

TOTAL_RHO_SURRO_SUM <- TOTAL_RHO_SURRO |> 
  dplyr::group_by(enem, lobs, variable)|> 
  dplyr::summarise(rho = mean(rho), rho_sdev = mean(rho_sdev))


write.csv(TOTAL_RHO_SURRO_SUM,  paste0("./outputs/CCM/", "detrend_", detrend_data, "/",  "surrogate_rho.csv"))


RHO_PLOT_SURRO_TOTAL <- plot_rho_CCM(TOTAL_RHO_SURRO_SUM)


ggsave(
    RHO_PLOT_SURRO_TOTAL,
    filename = paste0("./outputs/CCM/", "detrend_", detrend_data, "/", "rho_plot_surro.png"),
    height = 10,
    width = 10,
    create.dir = T
  )



####now, i have to do two things
## do a plot mixing informations of ther clark p test, and each X_casue, against the surrogates. 


###soo here I can import ir grom the generated!! change that. 



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