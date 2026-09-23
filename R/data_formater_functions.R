

##########################################################################################################
# Function that reshapes raw data from wide to long format
#' @param da_ta A data frame in wide format with species as columns
#' @return A data frame in long format with columns: species, individuals, and trophic
#' @details Pivots species columns (mp, ac, am, ma, cc, my, ol, aa, sr, ec) into long format
#' @details Removes rows with NA individuals
#' @details Assigns trophic level: "X" for cc, ol, sr, am, aa; "Y" for ma, my, ac, ec; "R" for others (e.g. mp)
#' #' @examples long_formatter(da_ta = my_raw_data)
#######################################################################################################

long_formatter <- function(da_ta) {

X <- c("cc", "ol", "sr", "am", "aa")
Y <- c("ma", "my", "ac", "ec")

herbivore <-c("mp")

  DATA_IGP_LONG <- da_ta |> 
  tidyr::pivot_longer(c(mp, ac, am, ma , cc, my, ol, aa, sr, ec), names_to = "species", values_to = "individuals")|> 
  dplyr::filter(!(is.na(individuals)))|> 
  dplyr::mutate(trophic = ifelse(species %in% X, "X", ifelse(species %in% Y, "Y", "R")))  
  return(DATA_IGP_LONG)
}


########################################################################################################
# Function that reshapes long format data into wide format by trophic level
#' @param data_long A data frame in long format with columns: species, individuals, and trophic
#' @return A data frame in wide format with trophic levels (X, Y, R) as columns
#' @details Removes the species column and pivots trophic into columns
#' @details Each row represents an observation with individuals counts per trophic level
#' #' @examples pred_formatter(data_long = my_long_data) after long_formatter
#####################################################################################################

pred_formatter <- function(data_long) {
DATA_IGP_WIDER <- data_long |> 
  dplyr::select(!species)|> 
  tidyr::pivot_wider(names_from = "trophic", values_from = "individuals")
return(DATA_IGP_WIDER)
}



data_pred_forRep <- function(data_pred){
 
vecX <- c("cc", "ol", "sr", "am", "aa")
vecY <- c("ma", "my", "ac", "ec")

  data_long <- data_pred |> 
    tidyr::pivot_longer(cols=c("R", "X", "Y"), names_to ="trophic", values_to = "individuals") |> 
    tidyr::separate(enem, into = c("spA", "spB"), sep = "\\+", remove=F)

  data_x <- data_long |> 
    dplyr::filter(trophic== "X")|> 
    dplyr::mutate(species = dplyr::if_else(spA %in% vecX, spA ,dplyr::if_else(spB %in% vecX, spB, "mp")))

  data_y <- data_long |> 
    dplyr::filter(trophic== "Y")|> 
    dplyr::mutate(species = dplyr::if_else(spA %in% vecY, spA ,dplyr::if_else(spB %in% vecY, spB, "mp")))
  
  data_r <- data_long |> 
    dplyr::filter(trophic== "R")|> 
    dplyr::mutate(species = "mp")

  data_long_sp <- rbind(data_x, data_y, data_r)|> 
    dplyr::select(!(c(spA, spB)))
  
  return(data_long_sp)

}

mean_formatter <- function(data_pred_sp) {
  
data_mean <- data_long |> 
  dplyr::group_by(enem, week, species, trophic) |> 
  dplyr::summarise(meanIndividuals = mean(individuals, na.rm = TRUE), sdIndividuals = sd(individuals, na.rm = TRUE))
return(data_mean)
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


#function to get the area under normalized curve between 0 to 1
area_coexistence <- function(data_coex_av) {

  data_area <- data_coex_av|>
  dplyr::ungroup() |> 

  dplyr::group_by(enem) |> 
  dplyr::summarise(mean_area= mean(mean_coex))

return(data_area)
}

##so this one summarized over the week, to see the proportion of species that survived 

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

##this function is supercumbersome, but it is correct

xy_to_np_transformer <- function(complete_df){


complete_mod <- complete_df |> 
    dplyr::mutate(varName = dplyr::case_when(
        # For these enem values: replace X→N AND Y→P
        enem %in% c("ac+am", "cc+ma", "my+aa") ~ gsub("Y", "P", gsub("X", "N", varName)),
        # For these enem values: replace X→P AND Y→N
        enem %in% c("ac+ol", "ma+ol", "cc+my") ~ gsub("Y", "N", gsub("X", "P", varName)),

        enem %in% c("xx+yy") ~ gsub("Y", "P", gsub("X", "N", varName)),
        TRUE ~ varName
    ))

return(complete_mod)
}
