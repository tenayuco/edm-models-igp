##INCOMPLETE CODE, just saving this for the futiere 

# ============================================================================
# 7. EXTRSCT SIMULATED. SEE... 
# ============================================================================
out_sim_folder <- paste0("./outputs/LV_MAP/", "simulated.data", "/")


ALL_SIM_DF <- extract_all_simulation(out_subfolder = out_sim_folder)

ALL_SIM_DF$omega_mean <- 10^ALL_SIM_DF$omega_mean
ALL_SIM_DF$omega_dw <- 10^ALL_SIM_DF$omega_dw
ALL_SIM_DF$omega_up <- 10^ALL_SIM_DF$omega_up

FULL_SIM_SUM <- summarizer_with_variance(df_full = ALL_SIM_DF)
