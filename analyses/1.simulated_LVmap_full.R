# ============================================================================
# LV MAP SPATIAL KERNEL - Cross Validation Analysis
# ============================================================================
# Purpose: Run cross-validation for LV (Lotka-Volterra) spatial kernel models
#          across different scenarios and parameter combinations
# ============================================================================

# ============================================================================
# 1. DATA SOURCE CONFIGURATION
# ============================================================================
#these can be looped in the make.R

type_data <- "simulated.data" #important for the map to run the simu mode
simulated_data <- "DF_DISC_LV_20.csv" ## source of data
#chosen_scenario <- "lblb_model_0"  ##scenario chosen, to now from where take the data
dif_cond <- FALSE
#norm_data <-  FALSE

# ============================================================================
# 2. CREATE OUTPUT FOLDER PATH AND IMPORT DATA
# ============================================================================

DATA_PRED <- read.csv(paste0(
  "data/",
  type_data,
  "/",
  chosen_scenario,
  "/",
  simulated_data
)) #have to provide

#here is the source of data

##just to have the real parameters if needed
source("./analyses/0.set_LBPB_model.R")


#here to build the output path
if (type_data == "simulated.data") {
  out_folder <- paste0(
    "./outputs/LV_MAP/",
    type_data,
    "/",
    chosen_scenario,
    "/"
  )
  fig_folder <- paste0(
    "./figures/LV_MAP/",
    type_data,
    "/",
    chosen_scenario,
    "/"
  )
}


# ===================================== =======================================
# 3. DATA TRANSFORMATIONS AND OUTPUT FIGRES
# ============================================================================

# Option 1: Convert to differences (if dif_cond = TRUE)
if (dif_cond == TRUE) {
  out_subfolder <- paste0(out_folder, "differences/")
  fig_subfolder <- paste0(fig_folder, "differences/")
  DATA_PRED <- df_differencer_lv(DATA_PRED)
  ts_plot_inputR <- ts_plotter_data(DATA_PRED, plotted_var = c("R", "X", "Y"))
} else {
  out_subfolder <- paste0(out_folder, "absolute/")
  fig_subfolder <- paste0(fig_folder, "absolute/")
}

# Option 2: Normalize data (min-max scaling, if norm_data = TRUE)
if (norm_data == TRUE) {
  out_subfolder <- paste0(out_subfolder, "normalized/")
  fig_subfolder <- paste0(fig_subfolder, "normalized/")

  DATA_PRED <- max_normalization(DATA_PRED)
  ts_plot_inputR <- ts_plotter_data(DATA_PRED, plotted_var = c("R", "X", "Y"))
  ts_plot_norm <- ts_plotter_data(DATA_PRED, plotted_var = c("R", "X", "Y"))
} else {
  out_subfolder <- paste0(out_subfolder, "not_normalized/")
  fig_subfolder <- paste0(fig_subfolder, "not_normalized/")
}

# Create output directory
dir.create(paste0(out_subfolder), recursive = TRUE)

#============================

# ============================================================================
# 4. CROSS-VALIDATION LOOP CONFIGURATION
# ============================================================================

#====================================================================================
#conditions to tun the LV map to take that can be set here or externally in the make.R

# Parameters for the LV map cross-validation loop
v_num_rep <- c(1) # Number of replicates (fixed at 1 due to short time series)
v_rpresent <- c(FALSE, TRUE) # Whether to include R (resource) in the model
v_num_seed <- seq(1:30) # Random seeds for data shuffling/re-sampling
v_enemigos <- unique(DATA_PRED$enem) # List of enemy species/treatments to analyze
kernel_chosen <- "state" # Kernel type to use


# ============================================================================
# 5. RUN CROSS-VALIDATION
# ============================================================================

# Run the LV map cross-validation for each enemy
tictoc::tic() # Start timing
for (e in v_enemigos) {
  # Filter data for current enemy
  DATA_PRED_EN <- DATA_PRED |>
    dplyr::filter(enem == e)

  print(head(DATA_PRED_EN)) # Debug: show first few rows

  # Run cross-validation for this enemy
  lv_looper_lists_general(
    #need the outsubfoder..
    data_used = DATA_PRED_EN,
    v_num_rep = v_num_rep,
    v_rpresent = v_rpresent,
    v_num_seed = v_num_seed,
    enemigo = e
  )
}
tictoc::toc() # End timing and display elapsed time


#==========================================================================

#Part II. PLOTTING

#

#===========================================================================================

#SO AT THIS POINT WE HAVE DONE THE HEAVY ANALYSIS
#NOW THE MORE GENERAL PLOTTINGS

#========================I.C PLOT LV SIMULATIONS ============================


#activate this for theplot per treatment 


#plot_per_treatment(out_subfolder = out_subfolder, true_values = FALSE) #we dont want the true values of the eq

simulaciones <- length(v_num_seed)


##check this and tun
if (
  file.exists(paste0(out_subfolder,"FULL_DF_parameters_","numseed_",simulaciones,".csv"
  ))) {
  full_df <-  read.csv(paste0(out_subfolder, "FULL_DF_parameters_","numseed_",simulaciones,".csv"))
  print("file exist")
}else{
  full_df <- extract_par_all_treatment(out_subfolder = out_subfolder,coex_cal = FALSE
  ) ##generates the file  (that you can download late just to run the full parameters, but chose how many simulaciones!)
  print(head(full_df))

  write.csv(full_df,file = paste0(out_subfolder,"FULL_DF_parameters_","numseed_",simulaciones,".csv"
    )
  )
}


plotter_full_parameters(df_full = full_df, fig_subfolder = fig_subfolder)

##========================================================================================
#full plot

full_sum <- summarizer_with_variance(df_full = full_df)

plotter_save_conditions(df_sum = full_sum, fig_subfolder = fig_subfolder, abs_norm_values = "abs")


#==========

##how to call the full df wo running everything

#call the fig folder you want

#########################################333
################################################3
