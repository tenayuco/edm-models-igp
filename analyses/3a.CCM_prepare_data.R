
prep_data <- function(raw_data_igp){
DATA_LONG <-  long_formatter(raw_data_igp)
DATA_PRED <-  pred_formatter(DATA_LONG)
  
  return(DATA_PRED)

}



norm_detrend_data <- function(data_pred, detrend_method = "firstDiff", de_trend = T){

##here we use 2 formats of data


### we gonna detrend.. so this is hard, and I wonder how it looks. 

#here I create a simple function to see how the data changes..

##2. Two different types, with and without first differences. 
##here normalized not detrended 

if (de_trend == T) {

  #here we gonna adapt the method
DATA_PRED<-data_pred |> 
  dplyr::group_by(block, enem) |> 
  dplyr::mutate(X= c(NA, diff(X)), Y= c(NA, diff(Y)), R= c(NA, diff(R)))|> 
  tidyr::drop_na()  
}
  
##normalization
DATA_PRED <-DATA_PRED  |> 
  dplyr::group_by(enem) |> 
  dplyr::mutate(R = R/max(R, na.rm = TRUE), X = X/max(X, na.rm = TRUE), Y = Y/max(Y, na.rm = TRUE))



  return(DATA_PRED)

}

#prefilter


hist_data <- function(data){

x <- data |> 
    dplyr::mutate(contador =1)|> 
    dplyr::group_by(block, enem) |> 
    dplyr::summarise(m = sum(contador))

hist_x <- x |> ggplot(aes(m))+ 
  geom_histogram(aes(fill= as.factor(block)), binwidth = 1)+
    facet_wrap(~enem)+ 
    theme_minimal()+
    scale_fill_viridis_d()+
    xlab("Length of replicate")

  return(hist_x)
}


### create one surrogate from wthe data 

surrogater_all_enem_df_old <- function(data){
  surro_df <- data.frame()
  for (enemy in unique(data$enem)){

    for(block in unique(data$block)){


  data_enem_block <-  data |> 
    dplyr::filter(enem == enemy)|> 
    dplyr::filter(block == block)
  
    print(head(data_enem_block))

  idx_shuffle = sample(1:nrow(data_enem_block), nrow(data_enem_block), replace = FALSE) # permutes the row order of the dataset (shuffles without replacement).
  print(idx_shuffle)
    surro_df_temp = data_enem_block[idx_shuffle, c("R", "Y", "X")] #i desorganize within block and natr
    surro_df_temp$week <- data_enem_block$week  #i add the week, to show the desorgani
    surro_df_temp$enem <- enemy  #i add these names that do not change
    surro_df_temp$block <- block
  surro_df <- rbind(surro_df, surro_df_temp)
    }
}
  return(surro_df)
  }



surrogater_all_df <- function(data){
  surro_df <- data.frame()

  idx_shuffle = sample(1:nrow(data), nrow(data), replace = FALSE) # permutes the row order of the dataset (shuffles without replacement).
  surro_df = data[idx_shuffle, c("block", "enem", "week", "R", "Y", "X")] #i desorganize within block and natr
   
  return(surro_df)
  }

