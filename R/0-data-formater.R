
#' Function to change the formats of the data
#'
#' @param file_path the path to the data we wand to import
#' @return the db corresponding to the file
#' @examples
#' load_data()



long_formatter <- function(da_ta) {
DATA_IGP_LONG <- da_ta |> 
  tidyr::pivot_longer(c(mp, ac, am, ma , cc, my, ol, aa, sr, ec), names_to = "species", values_to = "individuals")|> 
  dplyr::filter(!(is.na(individuals)))
return(DATA_IGP_LONG)
}




mean_formatter <- function(data_long) {
DATA_MEAN <- data_long |> 
  dplyr::group_by(enem, week, species) |> 
  dplyr::summarise(meanIndividuals = mean(individuals, na.rm = TRUE), sdIndividuals = sd(individuals, na.rm = TRUE))
return(DATA_MEAN)
}



wide_formatter <- function(data_long) {
DATA_IGP_WIDER <- data_long |> 
  tidyr::pivot_wider(names_from = "species", values_from = "individuals")
return(DATA_IGP_WIDER)
}
