
##########################################################################################################
# Function that adds a predicted coexistence indicator and completes the design grid
#' @param data_surv A data frame containing columns enem, block, week, X, and Y
#' @return A data frame with columns enem, block, week, coex, X, and Y, completed over all enem x block x week combinations
#' @details Ungroups the input and creates coex = 1 when both X > 0 and Y > 0, else 0
#' @details Keeps only the columns enem, block, week, coex, X, and Y
#' @details Expands the data to a complete grid of enem x block x week using tidyr::complete(), filling absent combinations with NA
#' #' @examples pred_coexistence_adder(data_surv = my_survival_data)
#######################################################################################################

pred_coexistence_adder <- function(data_surv){
data_coex <- data_surv|> 
  dplyr::ungroup() |> 
  dplyr::mutate(coex = ifelse(X > 0 & Y > 0, 1, 0))|> 
  dplyr::select(enem, block, week, coex, X, Y) |> 
  tidyr::complete(enem, block, week)
return(data_coex)
}

###################################################################################################
# Function that computes average predicted coexistence across blocks for each enemy x week combination
#' @param data_coex A data frame containing columns enem, block, week, coex, X, and Y
#' @return A data frame with columns enem, week, mean_coex, mean_X, and mean_Y
#' @details Replaces NA values in coex, X, and Y with 0 before summarising
#' @details Ungroups the input, then groups by enem and week
#' @details Computes mean_coex = mean(coex), mean_X = mean(X), and mean_Y = mean(Y) within each enem x week combination
#' #' @examples coex_average(data_coex = my_coexistence_data)
###################################################################################################
coex_average <- function(data_coex) {
  
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




plotter_coex_area <- function(data_coex, fig_path) {

coex_plot <- data_coex |>
    ggplot(aes(x = week, y = mean_coex)) +
    geom_area(fill = "darkgreen", alpha = 0.3) +
    geom_line(size = 1) +
    facet_wrap(~enem) +
    theme_minimal()

  
  ggsave(coex_plot,filename = paste0(fig_path, "coexistence_area", ".png"),
    height = 9,
    width = 10,
    create.dir = T)
  

}



plotter_survival <- function(data_coex_av, fig_path) {

data_coex_av_long <-  data_coex_av |> 
tidyr::gather(key= "mean_species", value= "value", mean_X, mean_Y)

survival_plot <- data_coex_av_long  |>
    ggplot(aes(x = week, y = value)) +
    geom_line(size = 1, aes(color= mean_species)) +
    facet_wrap(~enem) +
    theme_minimal()
ggsave(
    survival_plot,
    filename = paste0(fig_path, "survival_plot", ".png"),
    height = 9,
    width = 10,
    create.dir = T
  )

  
}
