
#==============================================

# ============================================================================
# 1. OUTPUT PATH CONFIGURATION
# ============================================================================
type_data <- "real.data"
norm_data <- "not_normalized"

out_folder <- paste0("./outputs/LV_MAP/", type_data, "/", norm_data, "/")


out_subfolder <- paste0(out_folder,"coexistence/")




#============================


#LOAD AND FILTER DATA for LV

#this code gets the IGP data and does the modifcations used for LV analys, and for rpesentation, and coexistence 

source("./analyses/2a.data_modification_LV.R")


##add the cpoexistence
DATA_COEX <- pred_coexistence_adder(DATA_PRED)  #this add coexistence per week and completes the data with NA

# ============================================================================
# 4. SURVIVAL AND AREA PLOT
# ============================================================================

## this has the first average between blocks but no time.
#basically it gives the proportion of survivail per week per enemy

DATA_COEX_AV <- coex_average(DATA_COEX)


#this plot this coexstnece and area stuff to visualizae
plotter_coex_step(DATA_COEX_AV, fig_path = fig_folder)



# ===========================================================================
# 5. SURVIVAL AND AREA CALCULATION AND DATA FRAME
# ============================================================================

###HERE DO THE SURVIVAL PLOT per x y (CHECK IF USEFUK)

###now we calculate the area under the curve for these, and to have a single value per enemy
##we do like a temporal average.

###nthis tell you in each reaplicate, the time to extinction to each predator, and
#the coexistnece time (the first one to surve)
DATA_SURV <- survival_time_per_run(DATA_COEX)


#thisgives you te average per enemy.
DATA_SURV_AV <- survival_time_average(DATA_SURV)
#=======================================================================================


# ========================
# IMPORT THEORETICAL COEXISTENCE (ETA AND OMEGA FROM THE MODEL)
#=============================


###now we gonna put together 1. the omega, 2. the area of coexistence, and 3 the survival time.

##here you specify wich one wou want #it has to be 36 rows (6 interaaction per 6 enemies, )
FULL_SUM <- read.csv(paste0(out_folder, "FULL_SUM_VARIANCE.csv"))






#here the names are still the x and y assigned 





###comple
COMPLETE_DF <- dplyr::left_join(FULL_SUM, DATA_SURV_AV, by = "enem")
##ADD CATEG

#dont actita this
#DF_SUM_LV_CCM <- read.csv("./data/summ_lv_ccm_R.csv")
#COMPLETE_DF <- dplyr::left_join(COMPLETE_DF, DF_SUM_LV_CCM, by = c("enem"))

##############
###here Imm gonna do the inversion from x, y to n,p , where p is always the top predator.
COMPLETE_DF <- xy_to_np_transformer(COMPLETE_DF)


dir.create(out_subfolder)

utils::write.csv(COMPLETE_DF, paste0(out_subfolder, "complete_coex_df.csv"))














