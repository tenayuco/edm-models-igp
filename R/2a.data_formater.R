
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



