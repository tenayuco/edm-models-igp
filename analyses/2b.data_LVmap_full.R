# ============================================================================
# LV MAP SPATIAL KERNEL - Real Data Analysis
# ============================================================================
# Purpose: Run LV (Lotka-Volterra) spatial kernel models on real experimental data
#          from IGP (Intraguild Predation) experiments
# ============================================================================

# ============================================================================

# I. PREPARATION OF DATA

# ===========================================================================


# ============================================================================
# 1. CONDITIONS FOR DATA 
# ===========================================================================


#these are conditions for the DATA, before the LV-map
norm_data  <- FALSE # if the data is normalized
type_data= "real.data"  #data from the experiments
# Load the raw IGP dataset
DATA_IGP <- readr::read_csv("data/dataIGP_2025.csv")

# ============================================================================
# 2. LOAD AND FILTER DATA
# ===========================================================================

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
# 4. DATA PREPARATION FOR LOTKA VOLTERRA
# ============================================================================

# Prepare data for LV analysis (format columns, handle missing values, etc.)
DATA_PRED <- df_modifier_lv(raw_data = DATA_IGP)

# Select only the columns needed for LV analysis
DATA_PRED <- DATA_PRED |> 
  dplyr::select(block, R, X, Y, week, enem)

# Remove fake zeros
DATA_PRED <- zero_remover_raw(DATA_PRED)

# ============================================================================
# 5. DATA TRANSFORMATIONS AND OUTPUT FIGURES PATH
# ============================================================================
if (norm_data == TRUE) {
  out_subfolder <- paste0(out_folder, "normalized/")
  fig_subfolder <- paste0(fig_folder, "normalized/")
  DATA_PRED <- max_normalization(DATA_PRED)  #the other options are max_normalization_per_trophic and min_max_normalization
} else {
  out_subfolder <- paste0(out_folder, "not_normalized/")
  fig_subfolder <- paste0(fig_folder, "not_normalized/")
}

# ============================================================================
# 6. CREATE OUTPUT DIRECTORY
# ============================================================================
dir.create(paste0(out_subfolder), recursive = TRUE)





# ============================================================================

# II. LOTKA VOLTERRA ANALYSIS

# ===========================================================================

# ============================================================================
# 7. CROSS-VALIDATION LOOP CONFIGURATION
# ============================================================================

# Parameters for the LV map cross-validation
v_num_rep <- c(1)                     # Number of replicates (fixed due to limited time points)
v_rpresent <- c(FALSE) #c(FALSE, TRUE)  #         # Whether to include R (resource) in the model
v_num_seed <- seq(1:2)                # Random seeds for data shuffling
v_enemigos <- unique(DATA_PRED$enem)  # List of enemy species/treatments to analyze
kernel_chosen <- "state"              # Kernel type for the LV model
forcing_theta <- FALSE ## TRUE is you want to fix a theta 0


# ============================================================================
# 8. RUN LV MAP ANALYSIS
# ============================================================================
#CHECK BEFORE RUNNING 


#This loops over each of the enemies and save it in different folder. 

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