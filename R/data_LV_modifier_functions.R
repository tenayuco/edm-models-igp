# ============================================================================
# FUNCTIONS TO MODIFY FOR LV MAP
# ============================================================================

###########################################################################################
# Function that applies the long and prediction formatters to raw data
#' @param raw_data A data frame containing the raw data to be processed
#' @return A data frame in wide format with trophic levels as columns
#' @details This function is a wrapper that chains long_formatter and pred_formatter
#' @details It first reshapes the data to long format, then back to wide format by trophic level
#' #' @examples df_modifier_lv(raw_data = my_raw_data_IGP)
###########################################################################################

df_modifier_lv <- function(raw_data){
  DATA_LONG <-  long_formatter(raw_data)
  DATA_PRED <-  pred_formatter(DATA_LONG) 
  return(DATA_PRED)  
}



###HEADER###################################################################################
# Function that cleans zeros from prediction data by replacing them with 1 when preced by a 1
#' @param data_pred A data frame in wide format with columns: enem, block, X, Y, R
#' @return A data frame with zeros replaced by 1 and rows containing 0 for X or Y removed
#' @details For X and Y, a 0 is replaced by 1 when preceded by a non-zero value (a real 0 is two consecutive 0s)
#' @details For R, every 0 is replaced by 1
#' @details NAs (e.g. species not found in the first week) are also replaced by 1
#' @details Finally, rows where X or Y are still 0 are filtered out
#' @examples zero_remover_raw(data_pred = df_modifier_lv(my_raw_data))
#########HEADER############################################################################

zero_remover_raw <- function(data_pred){
    ##here we remove the 0 

data_pred_nozero <- data_pred |>
    dplyr::group_by(enem, block) |>
    dplyr::mutate(X = ifelse(X == 0 & dplyr::lag(X)> 0, 1, X),  #with the predators a real 0 is when you have two conse 0
                  Y = ifelse(Y == 0 & dplyr::lag(Y)> 0, 1, Y),
                  R = ifelse(R == 0, 1, R))
  
## then the function creats Na when in the first week they are not found so we haveto make them also 1
data_pred_nozero[is.na(data_pred_nozero)] <- 1

  
#now we gonna remove the rows where we have zeros either for X or for Y (normally there are not zeros for R now) 
data_pred_nozero <- data_pred_nozero |> 
  dplyr::filter(!(X ==0))|> 
  dplyr::filter(!(Y ==0))
return(data_pred_nozero)
}


###############################################################################
# Function that applies min-max normalization to X, Y, and R within each enem group
#' @param data_pred A data frame with columns: enem, X, Y, R (columns should already be selected)
#' @return A data frame with X, Y, R each scaled to the [0, 1] range within each enem group
#' @details Each column is scaled separately using (value - min) / (max - min) per enem group
#' @details Zeros are then replaced by 0.01 to avoid issues with log transforms downstream
#' @details The grouping is removed with ungroup() before the zero replacement step
#' @examples min_max_normalization(data_pred = my_data_pred)
#############################################################################

min_max_normalization <- function(data_pred){

  data_norm <- data_pred |>  # Keep enem column

    dplyr::group_by(enem) |> 
    dplyr::mutate(
      # Scale to [0,1] range
      R= (R - min(R)) / (max(R) - min(R)),
      X= (X - min(X)) / (max(X) - min(X)),
      Y = (Y - min(Y)) / (max(Y) - min(Y))
    )|> 
    dplyr::ungroup()
  
  
  data_norm$R[data_norm$R==0] <-  0.01
  data_norm$X[data_norm$X==0] <- 0.01
  data_norm$Y[data_norm$Y==0] <- 0.01
  
  
  
  return(data_norm)
}


##############################################################################################
# Function that normalizes X, Y, and R by their group-wise maximum
#' @param data_pred A data frame with columns: enem, X, Y, R (columns should already be selected)
#' @return A data frame with X, Y, R scaled between 0 and 1 within each enem group
#' @details The maximum is computed across X, Y, and R together within each enem group
#' @details The same maximum value is used to divide all three columns, preserving their relative proportions
#' @details NAs are ignored when computing the maximum (na.rm = TRUE)
#' @examples max_normalization(data_pred = my_data_pred)
##############################################################################################

max_normalization <- function(data_pred){

data_norm <- data_pred |> #normally already selected the columns 
    dplyr::group_by(enem) |>  # Group by enemy
    dplyr::mutate(R = R/max(R, X, Y, na.rm = TRUE), 
                  X = X/max(R, X, Y, na.rm = TRUE), 
                  Y = Y/max(R, X, Y, na.rm = TRUE)) |> 
    dplyr::ungroup()  # Remove grouping
  return(data_norm)

}


##############################################################################################
# Function that normalizes X, Y, and R each by their own group-wise maximum
#' @param data_pred A data frame with columns: enem, X, Y, R (columns should already be selected)
#' @return A data frame with X, Y, R each scaled between 0 and 1 within each enem group
#' @details Unlike max_normalization, the maximum is computed separately for each column within each enem group
#' @details This means the relative proportions between X, Y, and R are not preserved
#' @details NAs are ignored when computing the maximum (na.rm = TRUE)
#' @examples max_normalization_pertrophic(data_pred = my_data_pred)
##############################################################################################

max_normalization_pertrophic <- function(data_pred){

data_norm <- data_pred |> #normally already selected the columns 
    dplyr::group_by(enem) |>  # Group by enemy
    dplyr::mutate(R = R/max(R, na.rm = TRUE), 
                  X = X/max(X, na.rm = TRUE), 
                  Y = Y/max(Y, na.rm = TRUE)) |> 
    dplyr::ungroup()  # Remove grouping
  return(data_norm)

}




##########################################################################################
# Function that normalizes individuals by the group-wise maximum in long format data
#' @param data_long A data frame in long format with columns: enem, species, individuals
#' @return A data frame in long format with individuals scaled between 0 and 1 within each enem/species group
#' @details The maximum is computed separately for each combination of enem and species
#' @details NAs are ignored when computing the maximum (na.rm = TRUE)
#' @examples max_datalong_norm(data_long = my_data_long)
##########################################################################################
max_datalong_norm_pertrophic <- function(data_long){

data_long_norm <- data_long |> #normally already selected the columns 
    dplyr::group_by(enem, species) |>  # Group by enemy and species 
    dplyr::mutate(individuals = individuals/max(individuals, na.rm = TRUE)) |> 
    dplyr::ungroup()  # Remove grouping
  return(data_long_norm)

}






change_xy_realValues <-  function(df_full){

X <- c("cc", "ol", "sr", "am", "aa")
Y <- c("ma", "my", "ac", "ec")


#change all the x 
for (x in X){
  print(x)
  for (i in seq(1:dim(df_full)[1])){
      if(grepl(x, df_full$enem[[i]])){
        df_full$varName[[i]] <- gsub("X", x, df_full$varName[[i]])
      }
  }
}



for (y in Y){
  print(y)
  for (i in seq(1:dim(df_full)[1])){
      if(grepl(y, df_full$enem[[i]])){
        df_full$varName[[i]] <- gsub("Y", y, df_full$varName[[i]])
      }
  }
}
 
return(df_full)  
  
  
}



