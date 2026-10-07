
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
  

data_coex$coex[is.na(data_coex$coex)] <- 0
data_coex$X[is.na(data_coex$X)] <- 0 
data_coex$Y[is.na(data_coex$Y)] <- 0 
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



#########################################################################
# Function that plots the mean coexistence indicator over time as a step plot
#' @param data_coex_av A data frame containing columns week, mean_coex, and enem
#' @param fig_path A character string giving the directory path where the figure will be saved
#' @return Saves a PNG file to fig_path and returns the ggplot object invisibly
#' @details Builds a step plot of mean_coex vs. week (direction "vh"), with one colored line per enem and points overlaid
#' @details Sets x-axis breaks at every integer week between the min and max observed week
#' @details Colors lines using the manual palette enemCol, rotates x-axis labels 45 degrees, and applies a minimal theme
#' @details Saves the plot as "coexistence_plot.png" (height 9, width 10) in fig_path, creating the directory if needed
#' @examples plotter_coex_step(data_coex_av = my_coex_avg_data, fig_path = "figures/")
#########################################################################
plotter_coex_step <- function(data_coex_av, fig_path) {

  coex_plot <- data_coex_av |>
    ggplot(aes(x = week, y = mean_coex)) +
    geom_step(aes(color = enem, linetype= enem),
              direction = "vh", linewidth = 1) + 
        geom_point(aes(shape= enem, fill=enem))+

    # vertical first, then horizontal
   scale_color_viridis_d(option = "inferno", begin = 0, end = 1) +
       scale_fill_viridis_d(option = "inferno", begin = 0, end = 1) +

    scale_linetype_manual(values = c(
      "my+aa" = 1, "cc+my" = 1, "ac+ol" = 1,
      "ac+am" = 1, "cc+ma" = 1, "ma+ol" = 2
    )) +
    scale_x_continuous(
      breaks = seq(min(data_coex_av$week), max(data_coex_av$week), by = 1)
    ) +
    scale_shape_manual(values = c(21, 21, 22, 23, 24, 25))+

    labs(x= "Time (weeks)", y= "Coexistence", 
    color= "NE combination", 
    fill= "NE combination", 
    linetype="NE combination", 
  shape= "NE combination" )+
    theme_bw(base_size = 13) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x  = element_text(angle = 45, hjust = 1, size = 11),
      axis.text.y  = element_text(size = 11),
      axis.title   = element_text(size = 13),
      legend.text  = element_text(size = 11),
      legend.title = element_text(size = 12),
      strip.text   = element_text(size = 12)
    )

  ggsave(coex_plot,
         filename = paste0(fig_path, "coexistence_plot", ".png"),
         height = 6,
         width = 12,
         create.dir = TRUE)
}
  


