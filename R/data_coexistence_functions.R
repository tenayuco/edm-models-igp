
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



#########################################################################
# Function that summarizes survival time per experimental run
#' @param data_coex A data frame containing columns enem, block, week, coex, X, and Y
#' @return A data frame with columns enem, block, surv_coex, surv_X, and surv_Y
#' @details Ungroups the input, then groups by enem and block
#' @details Computes surv_coex, surv_X, and surv_Y as the sum of coex, X, and Y
#'   respectively, collapsing across weeks within each enem x block combination
#' @details NA values in coex, X, or Y are removed before summing (na.rm = TRUE),
#'   so absent combinations filled by pred_coexistence_adder() contribute 0 rather
#'   than propagating NA
#' @examples survival_time_per_run(data_coex = my_coex_data)
#########################################################################

survival_time_per_run <- function(data_coex) {

  data_survi_per_run <- data_coex |>
    dplyr::ungroup() |>
    dplyr::group_by(enem, block) |>
    dplyr::summarise(
      surv_coex = sum(coex, na.rm = TRUE),
      surv_X    = sum(X,    na.rm = TRUE),
      surv_Y    = sum(Y,    na.rm = TRUE)
    )

  return(data_survi_per_run)
}


#########################################################################
# Function that computes the average survival time per enemy combination
#' @param data_survi_per_run A data frame containing columns enem, block, surv_coex,
#'   surv_X, and surv_Y (output of survival_time_per_run())
#' @return A data frame with columns enem, mean_surv, and sd_surv
#' @details Ungroups the input, then groups by enem
#' @details Computes mean_surv and sd_surv as the mean and standard deviation of
#'   surv_coex across blocks within each enem level
#' @details NA values in surv_coex are removed before computing (na.rm = TRUE)
#' @details Averaging is valid because surv_coex is normalized to the same scale
#'   across blocks (each block contributes a comparable number of weeks)
#' @examples survival_time_average(data_survi_per_run = my_surv_per_run_data)
#########################################################################

survival_time_average <- function(data_survi_per_run) {

  ### Here I compute the average (this is fine because it's normalized to 1)

  data_surv_av <- data_survi_per_run |>
    dplyr::ungroup() |>
    dplyr::group_by(enem) |>
    dplyr::summarise(
      mean_surv = mean(surv_coex, na.rm = TRUE),
      sd_surv   = sd(surv_coex,   na.rm = TRUE)
    )

  return(data_surv_av)
}


##################################################################################################################################################
# Function that plots the mean coexistence indicator over time as a step plot
#' @param data_coex_av A data frame containing columns week, mean_coex, and enem
#' @param fig_path A character string giving the directory path where the figure will be saved
#' @return Saves a PNG file to fig_path and returns the ggplot object invisibly
#' @details Reorders enem as a factor with levels my+aa, ac+am, cc+ma, ac+ol, cc+my, ma+ol
#'   so legend, color, fill, and shape mappings follow that order
#' @details Builds a step plot of mean_coex vs. week (direction "vh") with color, fill,
#'   linetype, and shape mapped to enem; uses viridis "inferno" and manual linetypes/shapes
#' @details Sets x-axis breaks at every integer week between the min and max observed week
#' @details Applies theme_bw() with grid lines removed and x-axis labels rotated 45 degrees
#' @details Saves the plot as "coexistence_plot.png" (height 6, width 12) in fig_path,
#'   creating the directory if needed
#' @examples plotter_coex_step(data_coex_av = my_coex_avg_data, fig_path = "figures/")
#########################################################################

plotter_coex_step <- function(data_coex_av, fig_path) {


  #data_coex_av <- data_coex_av |>
   # dplyr::mutate(
    #  enem = factor(enem, levels = enemOrder)
    #)

  coex_plot <- data_coex_av |>
    ggplot(aes(x = week, y = mean_coex)) +
    geom_step(aes(color = enem, linetype = enem),
              direction = "vh", linewidth = 0.7) +
    geom_point(aes(shape = enem, fill = enem), size=2) +
    scale_color_viridis_d(option = "inferno", begin = 0.1, end = 0.9, direction=1) +
    scale_fill_viridis_d(option  = "inferno", begin = 0.1, end = 0.9, direction =1) +
    scale_linetype_manual(values = c(
      "my+aa" = 1, "ac+am" = 1, "cc+ma" = 1,
      "ac+ol" = 1, "cc+my" = 1, "ma+ol" = 2
    )) +
    scale_x_continuous(
      breaks = seq(min(data_coex_av$week), max(data_coex_av$week), by = 1)
    ) +
    scale_shape_manual(values = enemShapes) +
    labs(
      x = "Time (weeks)", y = "Coexistence",
      color = "NE combination", fill = "NE combination",
      linetype = "NE combination", shape = "NE combination"
    ) +
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
    ) +
    guides(color = "legend", fill = "legend",
           linetype = "legend", shape = "legend")

  ggsave(coex_plot,
         filename = paste0(fig_path, "coexistence_plot", ".png"),
         height = 6,
         width = 12,
         create.dir = TRUE)

}


