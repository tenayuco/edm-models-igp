
#' Function to change the formats of the data
#'
#' @param file_path the path to the data we wand to import
#' @return the db corresponding to the file
#' @examples
#' load_data()



long_formatter <- function(da_ta) {

pred_1 <- c("cc", "ol", "sr", "am", "aa")
pred_2 <- c("ma", "my", "ac", "ec", "ac")
herbivore <-c("mp")

  DATA_IGP_LONG <- da_ta |> 
  tidyr::pivot_longer(c(mp, ac, am, ma , cc, my, ol, aa, sr, ec), names_to = "species", values_to = "individuals")|> 
  dplyr::filter(!(is.na(individuals)))|> 
  dplyr::mutate(trophic = ifelse(species %in% pred_1, "pred1", ifelse(species %in% pred_2, "pred2", "herbivore")))
return(DATA_IGP_LONG)
}




mean_formatter <- function(data_long) {
DATA_MEAN <- data_long |> 
  dplyr::group_by(enem, week, species, trophic) |> 
  dplyr::summarise(meanIndividuals = mean(individuals, na.rm = TRUE), sdIndividuals = sd(individuals, na.rm = TRUE))
return(DATA_MEAN)
}


#wide_formatter_perGroup <- function(data_long) {

#DATA_IGP_WIDER <- data_long |> 
  #tidyr::pivot_wider(names_from = "species", values_from = "individuals")
#return(DATA_IGP_WIDER)
#}





pred_formatter <- function(data_long) {
DATA_IGP_WIDER <- data_long |> 
  tidyr::pivot_wider(names_from = "trophic", values_from = "individuals")
return(DATA_IGP_WIDER)
}
