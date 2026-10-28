
#==============================================

# ============================================================================
# 1. OUTPUT PATH CONFIGURATION
# ============================================================================
type_data <- "real.data"
norm_data <- "not_normalized"

out_folder <- paste0("./outputs/LV_MAP/", type_data, "/", norm_data, "/")

#============================





#LOAD AND FILTER DATA for LV

#this code gets the IGP data and does the modifcations used for LV analys, and for rpesentation, and coexistence 

DATA_PRED <- readr::read_csv("data/data_pred.csv")



#lets start with the bicontorl

#IT GIVES YOU DATA IGP AND DATA_PRED

DATA_BIO <- DATA_PRED

DATA_BIO  <- DATA_BIO |>
  dplyr::mutate("R_input" = R +400)|>
  dplyr::group_by(block, enem)|>
  dplyr::arrange(week)|>
  dplyr::mutate("R_consumed" = dplyr::lag(R_input)- R)|>
  dplyr::ungroup()|>
  dplyr::select(-R_input)


for (i in seq(1:dim(DATA_BIO)[1])){
  if(is.na(DATA_BIO$R_consumed[[i]])){
    DATA_BIO$R_consumed[[i]] = 600 - DATA_BIO$R[[i]] 
}
}



###another columns of the binary secctions 


DATA_BIO$R_bin <- 0

for (i in seq(1:dim(DATA_BIO)[1])){
  if(DATA_BIO$R[[i]]==1){
    DATA_BIO$R_bin[[i]] = 1 
}
}




utils::write.csv(DATA_BIO, paste0(out_folder, "DATA_biocontrol.csv"), row.names = FALSE)


#==========================================================


#COEXISTENCE 


#============================================================0

#now we save this data frame, that has 831 row (is similar to pred, but with coexistence )


###now we gonna add the colun of coexistence but without averaging, yet. that is only for the plits 


## Im gonna add the coexistence in general and the biocontrol wtithour 
#removing the time specificifity . 

##add the cpoexistence
DATA_COEX <- pred_coexistence_adder(DATA_PRED)  #this add coexistence per week and completes the data with NA



utils::write.csv(DATA_COEX, paste0(out_folder, "DATA_coexistence.csv"), row.names = FALSE)








