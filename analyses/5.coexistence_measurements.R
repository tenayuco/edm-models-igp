# ============================================================================
# COEXISTENCE REPRESENTATIONS 
# ============================================================================
# Purpose:take the results of the lotka volterra mapping 
#and plot them agains the survey and the omega
# ============================================================================
#DONT FORGET TO RUN FIRST PART OF MAKE R
# ============================================================================
# 1. A LOAD DATA FOR SURVIVAL AND AREA 


# Load the raw IGP dataset
DATA_IGP <- readr::read_csv("data/dataIGP_2025.csv")

# Remove treatments that don't make sense for the analysis
# (ec+sr and ec+am are excluded)
DATA_IGP <- DATA_IGP |> 
  dplyr::filter(!(enem == "ec+sr")) |> 
  dplyr::filter(!(enem == "ec+am"))

#.2 LOAD DATA FOR SIMULATED DATA====================

#==============================================

# ============================================================================
# 3. OUTPUT PATH CONFIGURATION
# ============================================================================
type_data <- "real.data"

if (type_data == "real.data") {
  out_folder <- paste0("./outputs/LV_MAP/", type_data, "/", "coexistence/")
  fig_folder <- paste0("./figures/LV_MAP/", type_data, "/", "coexistenceFigures/")

}


# ============================================================================
# 4. DATA FORMAT and Coexistence adder
# ============================================================================

##we first put it in clean and long format
DATA_PRED <- df_modifier_lv(raw_data = DATA_IGP)

##then we just make coexistence 0 and 1, and transform 0 to 1 if 1-0-1 or 1-0-0-1
###coexistence propo
DATA_COEX <- binary_remove_zeros(DATA_PRED) ##this chnage x and y to 0 and 1 and change 0 to 1 if 1 is next )=(110100 to 111100)
DATA_COEX<- binary_remove_zeros(DATA_COEX) ##i run it twice now to remove 0 that where followed by a 0, that was convetted to a 1 (so 11100100 to 11101100 to 11111100)
DATA_COEX <-  pred_coexistence_adder(DATA_COEX)

# ============================================================================
# 4. SURVIVAL AND AREA PLOT
# ============================================================================

## this has the first average between blocks but no time. 
#basically it gives the proportion of survivail per week per enemy 

DATA_COEX_AV <-  coex_average(DATA_COEX)

#this plot this coexstnece and area stuff to visualizae 
plotter_coex_area(DATA_COEX_AV, fig_path = fig_folder)



##here is the survival plots per enem 

##plot survival plots (X, and Y)
plotter_survival(DATA_COEX_AV, fig_path = fig_folder)




# ============================================================================
# 5. SURVIVAL AND AREA CALCULATION AND DATA FRAME
# ============================================================================

###HERE DO THE SURVIVAL PLOT per x y (CHECK IF USEFUK)

###now we calculate the area under the curve for these, and to have a single value per enemy 
##we do like a temporal average. 
DATA_AREA <-  area_coexistence(DATA_COEX_AV)

###nthis tell you in each reaplicate, the time to extinction to each predator, and 
#the coexistnece time (the first one to surve)
DATA_SURV <-  survival_time_per_run(DATA_COEX)


#thisgives you te average per enemy. 
DATA_SURV_AV <- survival_time_average(DATA_SURV)
#=======================================================================================


# ============================================================================
# 6. OMEGA VALUES FROM THE LOTKA VOLTERRA AND MERGIND DATA FRAME
# ============================================================================


###now we gonna put together 1. the omega, 2. the area of coexistence, and 3 the survival time. 

##here you specify wich one wou want 
full_df <-  read.csv("./outputs/LV_MAP/real.data/absolute/not_normalized/FULL_DF_parameters_numseed_30.csv")


#importantly, the omega reported is the log 10, so we have to do 10**omega to get real omega values

full_df$omega_mean <- 10^full_df$omega_mean
full_df$omega_dw <- 10^full_df$omega_dw
full_df$omega_up <- 10^full_df$omega_up



##this is a full summarizer of both sources of variance, of the LV BS, and my resticking between the 30 runs. 
full_sum <- summarizer_with_variance(df_full = full_df)




##add the cate of igp and causality
DF_SUM_LV_CCM <- read.csv("./data/summ_lv_ccm_R.csv")




###comple
COMPLETE_DF <-   dplyr::left_join(full_sum, DATA_AREA, by= "enem")
COMPLETE_DF <-   dplyr::left_join(COMPLETE_DF, DATA_SURV_AV, by= "enem")
##ADD CATEG
COMPLETE_DF <-  dplyr::left_join(COMPLETE_DF , DF_SUM_LV_CCM, by= "enem")


##############
###here Imm gonna do the inversion from x, y to n,p , where p is always the top predator.
COMPLETE_DF <-  xy_to_np_transformer(COMPLETE_DF) 



COMPLETE_DF_LONG <- COMPLETE_DF |> 
  dplyr::select(enem,grand_mean, total_sd, grand_mean_omega,total_sd_omega , mean_surv, type, varName, sd_surv, ccm_caus, lv_caus, igp_comp)|> 
  tidyr::gather(key= "coexistence_variable", value= "coex_value", grand_mean_omega, mean_surv)|> 
    tidyr::gather(key= "coexistence_sd", value= "sd_value", total_sd_omega, sd_surv)

  
##now i remove the non correpsoning sd

COMPLETE_DF_LONG <-  COMPLETE_DF_LONG |> 
  dplyr::mutate(sd_value= ifelse(coexistence_variable == "grand_mean_omega" & coexistence_sd == "sd_surv" , NA, sd_value)) |> 
    dplyr::mutate(sd_value= ifelse(coexistence_variable == "mean_surv" & coexistence_sd == "total_sd_omega" , NA, sd_value)) |> 
   tidyr::drop_na()

###now some ploting!!
#importantly, the plotting will be done with absolute valies 


plotter_interaction_coexistence(COMPLETE_DF_LONG, chosen_coex_var = "grand_mean_omega", fig_path = fig_folder)
plotter_interaction_coexistence(COMPLETE_DF_LONG, chosen_coex_var = "mean_surv", fig_path = fig_folder)



plotter_cat_coexistence(COMPLETE_DF_LONG, chosen_coex_var = "grand_mean_omega", fig_path = fig_folder)
plotter_cat_coexistence(COMPLETE_DF_LONG, chosen_coex_var = "mean_surv", fig_path = fig_folder)

