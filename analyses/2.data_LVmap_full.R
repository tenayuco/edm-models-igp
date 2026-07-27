# ============================================================================
# LV MAP SPATIAL KERNEL - Real Data Analysis
# ============================================================================
# Purpose: Run LV (Lotka-Volterra) spatial kernel models on real experimental data
#          from IGP (Intraguild Predation) experiments
# ============================================================================

# ============================================================================
# 1. DATA SOURCE CONFIGURATION
# ============================================================================



type_data= "real.data"  #data from the experiments
dif_cond <-  FALSE

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


# ============================================================================
# 4. DATA PREPARATION
# ============================================================================

# Prepare data for LV analysis (format columns, handle missing values, etc.)
DATA_PRED <- df_modifier_lv(raw_data = DATA_IGP)

# Select only the columns needed for LV analysis
DATA_PRED <- DATA_PRED |> 
  dplyr::select(block, R, X, Y, week, enem)

# Remove rows with zeros (which can cause issues in LV models)
DATA_PRED <- zero_remover_raw(DATA_PRED)

# Create time series plots to visualize the raw data
#ts_plot_normal <- ts_plotter_data(DATA_PRED, plotted_var = c("R", "X", "Y"))



# ============================================================================
# 5. DATA TRANSFORMATIONS AND OUTPUT FIGURES PATH
# ============================================================================

# Option 1: Convert to differences (if dif_cond = TRUE)
# This transforms the data from absolute values to changes between time points
if (dif_cond == TRUE) {
  out_subfolder <- paste0(out_folder, "differences/")
  fig_subfolder <- paste0(fig_folder, "differences/")
  DATA_PRED <- df_differencer_lv(DATA_PRED)
  #ts_plot_inputR <- ts_plotter_data(DATA_PRED, plotted_var = c("R", "X", "Y"))
} else {
  out_subfolder <- paste0(out_folder, "absolute/")
  fig_subfolder <- paste0(fig_folder, "absolute/")
}

# Option 2: Normalize data using min-max scaling (if norm_data = TRUE)
# This scales all variables to range [0,1]
if (norm_data == TRUE) {
  out_subfolder <- paste0(out_subfolder, "normalized/")
  fig_subfolder <- paste0(fig_subfolder, "normalized/")

  DATA_PRED <- max_normalization(DATA_PRED)
 # ts_plot_inputR <- ts_plotter_data(DATA_PRED, plotted_var = c("R", "X", "Y"))
  #ts_plot_norm <- ts_plotter_data(DATA_PRED, plotted_var = c("R", "X", "Y"))
} else {
  out_subfolder <- paste0(out_subfolder, "not_normalized/")
  fig_subfolder <- paste0(fig_subfolder, "not_normalized/")
}

# ============================================================================
# 6. CREATE OUTPUT DIRECTORY
# ============================================================================

dir.create(paste0(out_subfolder), recursive = TRUE)


# ============================================================================
# 7. CROSS-VALIDATION LOOP CONFIGURATION
# ============================================================================

# Parameters for the LV map cross-validation
v_num_rep <- c(1)                     # Number of replicates (fixed due to limited time points)
v_rpresent <- c(FALSE)  #c(FALSE, TRUE)          # Whether to include R (resource) in the model
v_num_seed <- seq(1:30)                # Random seeds for data shuffling
v_enemigos <- unique(DATA_PRED$enem)  # List of enemy species/treatments to analyze
kernel_chosen <- "state"              # Kernel type for the LV model



# ============================================================================
# 8. RUN LV MAP ANALYSIS
# ============================================================================

# Run the analysis separately for each enemy treatment
# Note: For real data, we iterate over enemies (unlike simulated data)
tictoc::tic()  # Start timing

for (e in v_enemigos) {
  # Filter data for current enemy
  DATA_PRED_EN <- DATA_PRED |> 
    dplyr::filter(enem == e)
  
  print(head(DATA_PRED_EN))  # Debug: show first few rows
  
  # Run LV cross-validation for this enemy
  lv_looper_lists_general(
    data_used = DATA_PRED_EN, 
    v_num_rep = v_num_rep, 
    v_rpresent = v_rpresent, 
    v_num_seed = v_num_seed, 
    enemigo = e
  )
}

tictoc::toc()  # End timing and display elapsed time


#=========================================================================

#===========================================================================================

#SO AT THIS POINT WE HAVE DONE THE HEAVY ANALYSIS 
#NOW THE MORE GENERAL PLOTTINGS

#========================I.C PLOT LV <MICRSO ============================

##i dont wanna run tis

#plot_per_treatment(out_subfolder = out_subfolder, true_values = FALSE) #we dont want the true values of the eq


##this is to take the full df that might existe already 
simulaciones <- length(v_num_seed)


##check this and tun
if (
  file.exists(paste0(out_subfolder,"FULL_DF_parameters_","numseed_",simulaciones,".csv"
  ))) {
  full_df <-  read.csv(paste0(out_subfolder, "FULL_DF_parameters_","numseed_",simulaciones,".csv"))
  print("file exist")
}else{
  full_df <- extract_par_all_treatment(out_subfolder = out_subfolder,coex_cal = TRUE
  ) ##generates the file  (that you can download late just to run the full parameters, but chose how many simulaciones!)
  print(head(full_df))
  
  write.csv(full_df,file = paste0(out_subfolder,"FULL_DF_parameters_","numseed_",simulaciones,".csv"
    )
  )
}

###NEW CODE TO CHANGE THE CREATE A COLUMN OF REAL VALUES 
#full_df  <- read.csv("./outputs/LV_MAP/real.data/absolute/not_normalized/FULL_DF_parameters_numseed_30.csv")

#this is the original assingmeng



change_for_real <- TRUE

if(change_for_real == TRUE){
  full_df <- change_xy_realValues(df_full = full_df)
}

###############3
  

plotter_full_parameters(df_full = full_df, fig_subfolder = fig_subfolder)

#plotter_full_parameters_microcosmos(df_full = full_df, fig_folder = fig_folder)
#full plot

full_sum <- summarizer_with_variance(df_full = full_df)

#plotter_save_conditions(df_sum = full_sum, fig_subfolder = fig_subfolder, abs_norm_values = "norm")
plotter_save_conditions(df_sum = full_sum, fig_subfolder = fig_subfolder, abs_norm_values = "abs")


#==========



## coexistence valuess. 
#plotter_theta_microcosmos(df_full = full_df, fig_folder = fig_folder)

plotter_omega_microcosmos(df_full = full_df, fig_folder = fig_folder)

###

plot_omega_allconditions(df_sum = full_sum)
  