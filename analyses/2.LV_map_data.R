num_rep <- length(unique(DATA_MA_OL$replicate))

DATA_MA_OL <- DATA_PRED |> 
  dplyr::filter(enem == "ma+ol")|> 
  dplyr::select(block, herbivore, pred_1, pred_2, week)

names(DATA_MA_OL) <- c("replicate", "R", "N", "P", "time")


#---transforms to a matrix
N_list_sim <- vector(mode = "list", length = num_rep)

for (i in unique(DATA_MA_OL$replicate)){
  df_temp <- DATA_MA_OL |> 
    dplyr::filter(replicate == i)

  df_temp$time <- NULL
  df_temp$replicate <- NULL

  N_list_sim[[i]] <- as.matrix(df_temp)
}



# ================
# Cross validation
# ================
cv_list_sim <- vector(mode = "list", length = num_rep)
tic()
for (i in 1:num_rep) {
  
  out_cv <- LV_map_state_space_cross_validation(N_list_sim[[i]], theta_v = seq(0, 3, 0.01))
  cv_list_sim[[i]] <- out_cv
}
toc()
