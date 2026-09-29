
#==============================================

# ============================================================================
# 1. OUTPUT PATH CONFIGURATION
# ============================================================================
type_data <- "real.data"

if (type_data == "real.data") {
  out_folder <- paste0("./outputs/LV_MAP/", type_data, "/", "coexistence/")
  fig_folder <- paste0("./figures/LV_MAP/", type_data, "/", "coexistence/")
}


# ========================
# IMPORT THEORETICAL COEXISTENCE (ETA AND OMEGA FROM THE MODEL)
#=============================


###now we gonna put together 1. the omega, 2. the area of coexistence, and 3 the survival time.

##here you specify wich one wou want #it has to be 36 rows (6 interaaction per 6 enemies, )
FULL_SUM <- read.csv(
  "./outputs/LV_MAP/real.data/not_normalized/FULL_SUM_VARIANCE.csv"
)

#here the names are still the x and y assigned 


#============================


#LOAD AND FILTER DATA for LV

#this code gets the IGP data and does the modifcations used for LV analys, and for rpesentation, and coexistence 

source("./analyses/2a.data_modification_LV.R")

## let see if I can ADD the complete df 


DATA_COEX <- pred_coexistence_adder(DATA_PRED)  #this add coexistence per week and completes the data with NA

# ============================================================================
# 4. SURVIVAL AND AREA PLOT
# ============================================================================

## this has the first average between blocks but no time.
#basically it gives the proportion of survivail per week per enemy

DATA_COEX_AV <- coex_average(DATA_COEX)

#this plot this coexstnece and area stuff to visualizae
plotter_coex_area(DATA_COEX_AV, fig_path = fig_folder)

##plot survival plots (X, and Y)
plotter_survival(DATA_COEX_AV, fig_path = fig_folder)

# ===========================================================================
# 5. SURVIVAL AND AREA CALCULATION AND DATA FRAME
# ============================================================================

###HERE DO THE SURVIVAL PLOT per x y (CHECK IF USEFUK)

###now we calculate the area under the curve for these, and to have a single value per enemy
##we do like a temporal average.
DATA_AREA <- area_coexistence(DATA_COEX_AV)

###nthis tell you in each reaplicate, the time to extinction to each predator, and
#the coexistnece time (the first one to surve)
DATA_SURV <- survival_time_per_run(DATA_COEX)


#thisgives you te average per enemy.
DATA_SURV_AV <- survival_time_average(DATA_SURV)
#=======================================================================================





###comple
COMPLETE_DF <- dplyr::left_join(FULL_SUM, DATA_AREA, by = "enem")
COMPLETE_DF <- dplyr::left_join(COMPLETE_DF, DATA_SURV_AV, by = "enem")
##ADD CATEG

#dont actita this
DF_SUM_LV_CCM <- read.csv("./data/summ_lv_ccm_R.csv")
COMPLETE_DF <- dplyr::left_join(COMPLETE_DF, DF_SUM_LV_CCM, by = c("enem"))

##############
###here Imm gonna do the inversion from x, y to n,p , where p is always the top predator.
COMPLETE_DF <- xy_to_np_transformer(COMPLETE_DF)


dir.create(out_folder)

utils::write.csv(COMPLETE_DF, paste0(out_folder, "complete_coex_df.csv"))
























#===============================================================================================


#                                     PLOTTING SECTIONSSSSS

#========================================================================================

#MODIFICATIONS FOR PLOTTING 


type_data <- "real.data"

if (type_data == "real.data") {
  out_folder <- paste0("./outputs/LV_MAP/", type_data, "/", "coexistence/")
  fig_folder <- paste0("./figures/LV_MAP/", type_data, "/", "coexistence/")
}


COMPLETE_DF <- read.csv(
  "./outputs/LV_MAP/real.data/coexistence/complete_coex_df.csv"
)


##to avoid some problems of the R, im gonna remove the R present true
COMPLETE_DF <- COMPLETE_DF |>
  dplyr::filter(rpresent == FALSE)


COMPLETE_DF_LONG <- COMPLETE_DF |>
  dplyr::select(
    enem,
    grand_mean,
    total_sd,
    grand_mean_omega,
    total_sd_omega,
    mean_surv,
    type,
    varName,
    sd_surv,
    ccm_caus,
    lv_caus,
    igp_comp
  ) |>
  tidyr::gather(
    key = "coexistence_variable",
    value = "coex_value",
    grand_mean_omega,
    mean_surv
  ) |>
  tidyr::gather(
    key = "coexistence_sd",
    value = "sd_value",
    total_sd_omega,
    sd_surv
  )


##now i remove the non correpsoning sd

COMPLETE_DF_LONG <- COMPLETE_DF_LONG |>
  dplyr::mutate(
    sd_value = ifelse(
      coexistence_variable == "grand_mean_omega" & coexistence_sd == "sd_surv",
      NA,
      sd_value
    )
  ) |>
  dplyr::mutate(
    sd_value = ifelse(
      coexistence_variable == "mean_surv" & coexistence_sd == "total_sd_omega",
      NA,
      sd_value
    )
  ) |>
  tidyr::drop_na()


###here Im gonna send different plots (only intra, only growth rate, only ointer)
# and then one  with all of them



## first intra
plotter_interaction_coexistence(
  COMPLETE_DF_LONG,
  chosen_coex_var = "grand_mean_omega",
  fig_path = fig_folder,
  chosen_int = c("N.N", "P.P"), 
  num_columns = 1
)
plotter_interaction_coexistence(
  COMPLETE_DF_LONG,
  chosen_coex_var = "mean_surv",
  fig_path = fig_folder,
  chosen_int = c("N.N", "P.P"),
  num_columns = 1
)


## INTER
plotter_interaction_coexistence(
  COMPLETE_DF_LONG,
  chosen_coex_var = "grand_mean_omega",
  fig_path = fig_folder,
  chosen_int = c("N.P", "P.N"), 
  num_columns = 1
)
plotter_interaction_coexistence(
  COMPLETE_DF_LONG,
  chosen_coex_var = "mean_surv",
  fig_path = fig_folder,
  chosen_int = c("N.P", "P.N"),
  num_columns = 1
)


## Growth rates
plotter_interaction_coexistence(
  COMPLETE_DF_LONG,
  chosen_coex_var = "grand_mean_omega",
  fig_path = fig_folder,
  chosen_int = c("N", "P"), 
  num_columns = 1
)
plotter_interaction_coexistence(
  COMPLETE_DF_LONG,
  chosen_coex_var = "mean_surv",
  fig_path = fig_folder,
  chosen_int = c("N", "P"),
  num_columns = 1
)


##ALL
plotter_interaction_coexistence(
  COMPLETE_DF_LONG,
  chosen_coex_var = "grand_mean_omega",
  fig_path = fig_folder,
  chosen_int = c("N.N", "P.P", "N.P", "P.N", "N", "P"), 
  num_columns = 3
)
plotter_interaction_coexistence(
  COMPLETE_DF_LONG,
  chosen_coex_var = "mean_surv",
  fig_path = fig_folder,
  chosen_int = c("N.N", "P.P", "N.P", "P.N", "N", "P"), 
  num_columns = 3
)



##now lets plot the omega vs min eta

plotter_eta_omega(COMPLETE_DF, fig_path = fig_folder)





#plotter_meanSurv_omega(COMPLETE_DF, fig_path = fig_external_folder)

###on the fitting of the model

THETA_VALUES <- read.csv(
  "./outputs/LV_MAP/real.data/absolute/not_normalized/FULL_THETA_numseed_30.csv"
)

THETA_VALUES_NORM <- read.csv(
  "./outputs/LV_MAP/real.data/absolute/not_normalized/FULL_THETA_numseed_30.csv"
)


THETA_VALUES_SUM <- THETA_VALUES |> 
  dplyr::ungroup() |> 
  dplyr::group_by(enem, norm, dif_cond, numRep, rpresent) |> 
  dplyr::summarise(theta_o_mean = mean(theta_o), RMSE_o_mean = mean(RMSE_o),
theta_o_sd = sd(theta_o), RMSE_o_sd = sd(RMSE_o))


plotter_rmse_theta(theta_df_sum = THETA_VALUES_SUM, fig_path = fig_folder)


#





#==========COEXISTENCE (see if put it somehwehre else)

#for each enemy 
plot_omega_allconditions(df_sum = full_sume)
  

#still missing to replot the chnages of variables in time 

#plot_per_treatment(out_subfolder = out_subfolder, true_values = FALSE) #we dont want the true values of the eq
              # Random seeds for data shuffling


