##here Im gonna put all the functions related to the LV map with data 



df_modifier_lv <- function(raw_data){

  ##here we use 2 formats of data
  DATA_LONG <-  long_formatter(raw_data)

  ##here we remove the 0 
  DATA_PRED <-  pred_formatter(DATA_LONG) 


  
  return(DATA_PRED)  
}


df_modifier_lv_erase <- function(raw_data, chosen_enemies){

##here we use 2 formats of data
DATA_LONG <-  long_formatter(raw_data)

##here we remove the 0 


DATA_PRED <-  pred_formatter(DATA_LONG) 


DATA_USED <- DATA_PRED |> 
  dplyr::filter(enem == chosen_enemies)|> 
  dplyr::select(block, R, X, Y, week)  

#I add a normalization That we did had.. 
DATA_USED <- DATA_USED |> 
  dplyr::mutate(R = R/max(R, na.rm = TRUE), X = X/max(X, na.rm = TRUE), Y = Y/max(Y, na.rm = TRUE) )
  
return(DATA_USED)  
}


df_differencer_lv <- function(data_pred){
   data_dif<- data_pred |> 
    dplyr::group_by(block, enem) |>  # Group by both replicate AND enemy
    dplyr::mutate(
      R = c(NA, diff(R) - 300),  # A2 - (A1 + 300)  #the 1000 is to avoid negative values, that the lv map can not use beacuse of the log trnas
      X = c(NA, diff(X)),        # Just the difference
      Y = c(NA, diff(Y))         # Just the difference
    ) |> 
    tidyr::drop_na() |> 
    dplyr::ungroup()  # Optional: remove grouping after

  return(data_dif)
}


min_max_normalization <- function(data_pred){

  data_norm <- data_pred |>  # Keep enem column

    dplyr::group_by(enem) |> 
    dplyr::mutate(
      # Scale to [0,1] range
      R= (R - min(R)) / (max(R) - min(R)),
      X= (X - min(X)) / (max(X) - min(X)),
      Y = (Y - min(Y)) / (max(Y) - min(Y))
    )
  
  
  data_norm$R[data_norm$R==0] <-  0.01
  data_norm$X[data_norm$X==0] <- 0.01
  data_norm$Y[data_norm$Y==0] <- 0.01
  
  
  
  return(data_norm)
}

max_normalization <- function(data_pred){

data_norm <- data_pred |> #normally already selected the columns 
    dplyr::group_by(enem) |>  # Group by enemy
    dplyr::mutate(R = R/max(R, na.rm = TRUE), 
                  X = X/max(X, na.rm = TRUE), 
                  Y = Y/max(Y, na.rm = TRUE)) |> 
    dplyr::ungroup()  # Remove grouping
  return(data_norm)

}


zero_remover_raw <- function(data_pred){
    ##here we remove the 0 

 data_pred$R[data_pred$R==0] <-  1
  data_pred$X[data_pred$X==0] <- 1
data_pred$Y[data_pred$Y==0] <- 1
return(data_pred)
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
