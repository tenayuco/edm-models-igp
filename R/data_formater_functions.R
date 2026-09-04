
#' Function to change the formats of the data
#'
#' @param file_path the path to the data we wand to import
#' @return the db corresponding to the file
#' @examples
#' load_data()



long_formatter <- function(da_ta) {

X <- c("cc", "ol", "sr", "am", "aa")
Y <- c("ma", "my", "ac", "ec")
  
#pred_NP <- c("ac", "am", "ol", "cc", "my", "ma")
#pred_N <- c("aa", "sr")
#pred_P <- c("ec")
  
herbivore <-c("mp")

  DATA_IGP_LONG <- da_ta |> 
  tidyr::pivot_longer(c(mp, ac, am, ma , cc, my, ol, aa, sr, ec), names_to = "species", values_to = "individuals")|> 
  dplyr::filter(!(is.na(individuals)))|> 
  #dplyr::mutate(trophic_def = ifelse(species %in% pred_NP, "pred_NP", ifelse(species %in% pred_N, "pred_N", ifelse(species %in% pred_P, "pred_P", "herbivore")))) |> 
  dplyr::mutate(trophic = ifelse(species %in% X, "X", ifelse(species %in% Y, "Y", "R")))  
  
  return(DATA_IGP_LONG)
}




mean_formatter <- function(data_long) {
DATA_MEAN <- data_long |> 
  dplyr::group_by(enem, week, species, trophic) |> 
  dplyr::summarise(meanIndividuals = mean(individuals, na.rm = TRUE), sdIndividuals = sd(individuals, na.rm = TRUE))
return(DATA_MEAN)
}




pred_formatter <- function(data_long) {
DATA_IGP_WIDER <- data_long |> 
  dplyr::select(!species)|> 
   # dplyr::select(!troph_sp)|> 
  tidyr::pivot_wider(names_from = "trophic", values_from = "individuals")
return(DATA_IGP_WIDER)
}



##so Ineed some kind of code to tell it that if I find a 0, but the next value is not a 0, change it to 1. 
##after summarizing to 1 and 0 


binary_remove_zeros <- function(data_pred){
  data_surv <- data_pred |>
    dplyr::group_by(enem, block) |> 
    dplyr::mutate(X = ifelse(X > 0, 1, 0),  #this checks, if you are higher that 0, then you are 1 
  Y =ifelse(Y > 0, 1, 0))|> 
 dplyr::group_by(enem, block)|> 
  dplyr::mutate(X = ifelse(X == 0 & dplyr::lead(X) == 1, 1, X),   #this part tells,  if you are 0, but the next one is a 1, then youll be a 1
  Y = ifelse(Y == 0 & dplyr::lead(Y) == 1, 1, Y))
return(data_surv)
}

pred_coexistence_adder <- function(data_surv){
data_coex <- data_surv|> 
  dplyr::ungroup() |> 
  dplyr::mutate(coex = ifelse(X > 0 & Y > 0, 1, 0))|> 
  dplyr::select(enem, block, week, coex, X, Y) |> 
  tidyr::complete(enem, block, week)
return(data_coex)
}


coex_average <- function(data_coex) {

  
  ###de aqui saco el promedio (y esta bien porque ewsta normalizado a 1)
  
data_coex$coex[is.na(data_coex$coex)] <- 0
data_coex$X[is.na(data_coex$X)] <- 0 
data_coex$Y[is.na(data_coex$Y)] <- 0 


  data_coex_av <- data_coex|>
  dplyr::ungroup() |> 

  dplyr::group_by(enem, week) |> 
  dplyr::summarise(mean_coex= mean(coex), mean_X = mean(X), mean_Y = mean(Y))

return(data_coex_av)
}

area_coexistence <- function(data_coex_av) {

  data_area <- data_coex_av|>
  dplyr::ungroup() |> 

  dplyr::group_by(enem) |> 
  dplyr::summarise(mean_area= mean(mean_coex))

return(data_area)
}



survival_time_per_run <- function(data_coex){


  data_coex$coex[is.na(data_coex$coex)] <- 0
data_coex$X[is.na(data_coex$X)] <- 0 
data_coex$Y[is.na(data_coex$Y)] <- 0 


  data_survi_per_run<- data_coex|>
  dplyr::ungroup() |> 
  dplyr::group_by(enem, block) |> 
  dplyr::summarise(surv_coex= sum(coex), surv_X = sum(X), surv_Y = sum(Y))
  
  return(data_survi_per_run)
}



survival_time_average <- function(data_survi_per_run) {

  
  ###de aqui saco el promedio (y esta bien porque ewsta normalizado a 1)

  data_surv_av <- data_survi_per_run|>
  dplyr::ungroup() |> 

  dplyr::group_by(enem) |> 
  dplyr::summarise(mean_surv= mean(surv_coex), sd_surv = sd(surv_coex))

return(data_surv_av)
}