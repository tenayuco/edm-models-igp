
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

data_mod <- data_pred  
  
if (de_trend == T) {

  #here we gonna adapt the method
data_mod<-data_mod |> 
  dplyr::group_by(block, enem) |> 
  dplyr::mutate(X= c(NA, diff(X)), Y= c(NA, diff(Y)), R= c(NA, diff(R)))|> 
  tidyr::drop_na()  
}
  
##normalization
data_mod <-data_mod  |> 
  dplyr::group_by(enem) |> 
  dplyr::mutate(R = R/max(R, na.rm = TRUE), X = X/max(X, na.rm = TRUE), Y = Y/max(Y, na.rm = TRUE))



  return(data_mod)

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


