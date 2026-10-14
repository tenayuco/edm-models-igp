
plot_total_bio <- function(df_biocontrol, var_plotted = "R_consumed", fig_path){

df_biocontrol_long <- df_biocontrol |>
  tidyr::pivot_longer(c(R_consumed, R_bin, R), names_to = "biocontrol_variable", values_to = "biocontrol_value")

df_biocontrol_long <- df_biocontrol_long |>
  dplyr::filter(biocontrol_variable == var_plotted)
  
PLOT_TOTAL_CON <- df_biocontrol_long|>
    ggplot(aes(x = enem, y = biocontrol_value)) +
  geom_boxplot(aes(fill=enem))+
    scale_fill_viridis_d(option = "inferno", begin = 0.1, end = 0.9, direction=1) +
    theme_bw()

  ggsave(PLOT_TOTAL_CON,
    filename = paste0(fig_path, "BIOCONTROL/" ,  "biocontrol_total_", var_plotted, ".png"),
    height = 8,
    width = 12,
    create.dir = T)


}



plot_total_bio_week <- function(df_biocontrol, var_plotted = "R_consumed", fig_path){

df_biocontrol_long <- df_biocontrol |>
  tidyr::pivot_longer(c(R_consumed, R_bin, R), names_to = "biocontrol_variable", values_to = "biocontrol_value")

df_biocontrol_long <- df_biocontrol_long |>
  dplyr::filter(biocontrol_variable == var_plotted)
  
  
PLOT_TOTAL_CON_WEEK <- df_biocontrol_long|>
    ggplot(aes(x = week, y = biocontrol_value)) +
  geom_boxplot(aes(fill=enem, group = week))+
  facet_wrap(~enem)+
    scale_fill_viridis_d(option = "inferno", begin = 0.1, end = 0.9, direction=1) +
    theme_bw()


  ggsave(PLOT_TOTAL_CON_WEEK,
    filename = paste0(fig_path, "BIOCONTROL/",  "biocontrol_total_week", var_plotted, ".png"),
    height = 8,
    width = 12,
    create.dir = T)

}




plot_bio_coex_block <-  function(data_bio_coex_block, fig_path){


PLOT_BIO_COEX_BLOCK <- data_bio_coex_block |> 
    ggplot(aes(x = surv_coex, y = mean_R_bin)) +
    geom_pointrange(aes(ymin = mean_R_bin-sd_R_bin, ymax =  mean_R_bin+sd_R_bin,  
                  fill = enem, shape= enem), size =1)  +
     # facet_wrap(~enem, ncol = num_columns) +
    theme_bw()+
    #labs(subtitle= paste0(chosen_coex_var, " vs ", paste(chosen_int, collapse = "_")), x= "Magnitude of interaction")+
    scale_fill_viridis_d(option = "inferno")+
    scale_shape_manual(values = c(21, 22, 23, 24, 25, 21))+
   theme(axis.text=element_text(size=12))
  
  ggsave(
    PLOT_BIO_COEX_BLOCK,
    filename = paste0(fig_path, "bio_coex_block",  ".png"),
    height = 9,
    width = 12,
    create.dir = T
  )

}





plot_fig_bio_coex <-  function(data_bio_coex, fig_path){

PLOT_BIO_COEX_ENEM<- data_bio_coex |> 
    ggplot(aes(x = mean_surv, y = 1-(mean_R))) +
    geom_pointrange(aes(ymin = 1-(mean_R-sd_R), ymax =  1-(mean_R+sd_R), 
                  fill = enem, shape= enem), size =1, alpha=0.7)  +
  geom_pointrange(aes(xmin = mean_surv - sd_surv, xmax = mean_surv + sd_surv,
                  fill = enem, shape= enem), size =1, alpha=0.7)  +
  geom_hline(yintercept = 0.33, linetype=2)+
    geom_vline(xintercept = 0.33, linetype=2)+
  geom_hline(yintercept = 0.66, linetype=2)+
    geom_vline(xintercept = 0.66, linetype=2)+
  xlim(0,1)+

     # facet_wrap(~enem, ncol = num_columns) +
    theme_bw()+
    labs(x= "Coexistence (average survival of both natural enemies (NE))", y= "Herbivore suppression (1-P(aphids left))", fill= "Natural enemies (NE)", shape= "Natural enemies (NE)")+
    scale_fill_viridis_d(option = "inferno")+
    scale_shape_manual(values = c(21, 22, 23, 24, 25, 21))+
  theme(text=element_text(size=14)) #change font size of all text
       # axis.text=element_text(size=20), #change font size of axis text
       # axis.title=element_text(size=20), #change font size of axis titles
       # plot.title=element_text(size=20), #change font size of plot title
       # legend.text=element_text(size=20), #change font size of legend text
       # legend.title=element_text(size=20)) #change font size of legend title


    ggsave(
    PLOT_BIO_COEX_ENEM,
    filename = paste0(fig_path, "FIG_bio_coex",  ".png"),
    height = 6,
    width = 12,
    create.dir = T
  )

  
  }







###3lets put the coexistecen plotter here also

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





plot_omega_surv <- function(complete_df_long, fig_path){

  COMPLETE_ONLY_COEX <- complete_df_long |> 
  dplyr::select(enem, coexistence_variable, coexistence_sd, coex_value, sd_value, rmse_o_mean)

COMPLETE_ONLY_COEX <- unique(COMPLETE_ONLY_COEX)

##now to normalize each value of coexistence

COMPLETE_ONLY_COEX_NORM <- COMPLETE_ONLY_COEX |>
  dplyr::group_by(coexistence_variable)|>
  dplyr::mutate( sd_value = sd_value/max(coex_value))|>
  dplyr::mutate(coex_value = coex_value/max(coex_value))|>
  dplyr::ungroup() |>
  dplyr::mutate(funDiff = dplyr::if_else(enem %in% c("cc+ma", "ac+am"), "Low", "High"), 
modelFit = 1* (rmse_o_mean<50))


  ##now we reorganize the eme colimn to 



  
  par_plot <- COMPLETE_ONLY_COEX_NORM  |> 
    ggplot(aes(
        x = enem, 
        y = coex_value,
        linetype = as.factor(modelFit),   # column driving the line style
        shape    = funDiff      # column driving the point shape
    )) +
    geom_errorbar(
        aes(
            ymin  = coex_value - 1*sd_value,
            ymax  = coex_value + 1*sd_value,
            color = as.factor(coexistence_variable)
        ),
        width    = .2,
        position = position_dodge(0.6),
        linewidth = 1
    ) +
    geom_point(
        aes(color = as.factor(coexistence_variable)),
        fill     = "white",
        position = position_dodge(0.6),
        size     = 3,
        stroke   = 1
    ) +
    xlab("enem") +
    scale_color_viridis_d(begin = 0, end = 0.7, option = "A", direction = 1) +

    # --- line type: Good = solid, Bad = dashed ---
    scale_linetype_manual(values = c(
        "0" = 2,
        "1"  = 1
    )) +

    # --- shape: High = filled circle, Low = empty triangle ---
    scale_shape_manual(values = c(
        "High" = 16,   # filled circle
        "Low"  = 2     # empty triangle
    )) +

    theme_bw()
  
  ggsave(par_plot, filename = paste0(fig_path, "omega_surv_values.png"),
   height = 8,
    width = 12,
    create.dir = T
  )
  
}
