#---COEXISTENCE REPRESENTATIONS 

#LOAD the LOTJA VOLTERRA DATA



# ============================================================================
# 2. LOAD AND FILTER DATA
# ============================================================================

# Load the raw IGP dataset
DATA_IGP <- readr::read_csv("data/dataIGP_2025.csv")

# Remove treatments that don't make sense for the analysis
# (ec+sr and ec+am are excluded)
DATA_IGP <- DATA_IGP |> 
  dplyr::filter(!(enem == "ec+sr")) |> 
  dplyr::filter(!(enem == "ec+am"))


# ============================================================================
# 3. OUTPUT PATH CONFIGURATION
# ============================================================================

if (type_data == "real.data") {
  out_folder <- paste0("./outputs/LV_MAP/", type_data, "/")
  fig_folder <- paste0("./figures/LV_MAP/", type_data, "/")

}

#=====================================


##here we use 2 formats of data
DATA_PRED <- df_modifier_lv(raw_data = DATA_IGP)

###coexistence propo
DATA_BIN <- binary_remove_zeros(DATA_PRED) ##this chnage x and y to 0 and 1 and change 0 to 1 if 1 is next )=(110100 to 111100)
DATA_BIN <- binary_remove_zeros(DATA_BIN) ##i run it twice now to remove 0 that where followed by a 0, that was convetted to a 1 (so 11100100 to 11101100 to 11111100)




DATA_COEX <-  pred_coexistence_adder(DATA_BIN)
DATA_COEX_AV <-  coex_average(DATA_COEX)

#plotter coext
plotter_coex(DATA_COEX_AV)

##plot survival plots (X, and Y)
plotter_survival(DATA_COEX_AV)
###HERE DO THE SURVIVAL PLOT per x y (CHECK IF USEFUK)

###now we calculate the area under the curve for these, and to have a single value per enemy 
##we do like a temporal average. 
DATA_AREA <-  area_coexistence(DATA_COEX_AV)

###now we do the first to die, this shoudl be fata

DATA_SURV <-  survival_time_per_run(DATA_COEX)
DATA_SURV_AV <- survival_time_average(DATA_SURV)



###now we gonna put together 1. the omega, 2. the area of coexistence, and 3 the survival time. 

##here you specify wich one wou want 
full_df <-  read.csv("./outputs/LV_MAP/real.data/absolute/not_normalized/FULL_DF_parameters_numseed_30.csv")
full_sum <- summarizer_with_variance(df_full = full_df)

##
COMPLETE_DF <-   dplyr::left_join(full_sum, DATA_AREA, by= "enem")
COMPLETE_DF <-   dplyr::left_join(COMPLETE_DF, DATA_SURV_AV, by= "enem")


##############
#now some plottings!! Or I can add the IGP and shit 

## just some prepltting
## SO, we remove the area calculation cause it is equivalente to the first survival measure!! IMPORTANTA RESULT




COMPLETE_DF_LONG <- COMPLETE_DF |> 
  dplyr::select(enem,grand_mean, total_sd, grand_mean_omega,total_sd_omega , mean_surv, type, varName, sd_surv)|> 
  tidyr::gather(key= "coexistence_variable", value= "coex_value", grand_mean_omega, mean_surv)|> 
    tidyr::gather(key= "coexistence_SD", value= "coex_sd", total_sd_omega, sd_surv)

  
  ## IT DPES SOETHING WRONG

#BUT if I remove thecase that dont correspond togerher, ill sort it out (this is the correpsongin mean and sd, set to na 
#if not, and then remove the columns with na
#shoudl wolr
 # dplyr::mutate(sd_surv = sd_surv |> replace_when(coexistence_variable %in% c("grand_omega_mean", "mean_area"))

