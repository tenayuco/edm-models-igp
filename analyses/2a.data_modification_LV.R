#============================================================================
# 1. LOAD AND FILTER DATA
# ===========================================================================

# Remove treatments that don't make sense for the analysis
# (ec+sr and ec+am are excluded)

DATA_IGP <- readr::read_csv("data/dataIGP_2025.csv")


DATA_IGP <- DATA_IGP |> 
  dplyr::filter(!(enem == "ec+sr")) |> 
  dplyr::filter(!(enem == "ec+am"))



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