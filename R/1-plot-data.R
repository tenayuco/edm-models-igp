#' Function to load data
#'
#' @param file_path the path to the data we wand to import
#' @return the db corresponding to the file
#' @examples
#

speciesCol <- c(
        "ac" = "black",
        "am" = "#996600",
        "cc" = "#00BCD4",
        "my" = "#339900",
        "ma" = "orange",
        "ol" = "red",
        "aa" = "#FF9999",
        "ec" = "#FFCC00",
        "sr" = "#CCFF66", 
        "mp" = "darkgreen"
      )

###here all
plotter_data_all <- function(data_long, remove_aphid=FALSE) {
  aphid = ""

  if(remove_aphid ==TRUE){
    aphid = "noAphids"
    data_long <- data_long |> 
      dplyr::filter(species != "mp")
  }

  TIME_SERIES_ALL <- data_long |>
    ggplot(aes(x = week, y = individuals)) +
    geom_line(
      aes(color = species, group = as.factor(interaction(block, species))),
      size = 0.5
    ) +
    geom_point(aes(color = species), size = 1) +
    facet_wrap(~enem, scales = "free_y") +
    scale_color_manual(
      values = speciesCol
    ) +
    theme_minimal()

  ggsave(
    TIME_SERIES_ALL,
    filename = paste0("./figures/time-series-all-", aphid, ".png"),
    height = 9,
    width = 10,
    create.dir = T
  )
}

plotter_data_mean <- function(data_mean, remove_aphid=FALSE) {
    aphid = ""

  if(remove_aphid ==TRUE){
        aphid = "noAphids"

    data_mean <- data_mean |> 
      dplyr::filter(species != "mp")
  }
  TIME_SERIES_MEAN <- data_mean |>
    ggplot(aes(x = week, y = meanIndividuals)) +
     geom_line(
      aes(color = species),
      size = 0.5
    ) +
    geom_point(aes(color = species), size = 1) +
    geom_errorbar(aes(
      ymin = meanIndividuals - sdIndividuals,
      ymax = meanIndividuals + sdIndividuals,
      color = species
    )) +
    facet_wrap(~enem, scales = "free_y") +
      scale_color_manual(
      values = speciesCol
    ) +
    theme_minimal()
  ggsave(
    TIME_SERIES_MEAN,
    filename = paste0("./figures/time-series-mean-", aphid, ".png"),
    height = 9,
    width = 10,
    create.dir = T
  )
}



phaseplotter_all_block <- function(data_pred, trophic_1 = "pred1", trophic_2 = "pred2", enem_treatment = "cc+ma") {

 
  PHASE_ALL <- data_pred |>
    dplyr::filter(enem == enem_treatment)|>
    ggplot(aes(x = !!sym(trophic_1), y = !!sym(trophic_2))) +
    geom_line(
      aes(color = as.factor(block)),
      size = 0.5
    ) +
    geom_point(aes(color = as.factor(block)), size = 1) +
    facet_wrap(~block, scales = "free", ncol = 3) +
    theme_minimal() +
    scale_color_viridis_d()

  ggsave(
    PHASE_ALL,
    filename = paste0("./figures/phase-plot-all_block_", enem_treatment, "_", trophic_1, "_", trophic_2,    ".png"),
    height = 9,
    width = 10,
    create.dir = T
  )
}


phaseplotter_all <- function(data_pred, trophic_1 = "pred1", trophic_2 = "pred2", enem_treatment = "cc+ma") {

 
  PHASE_ALL <- data_pred |>
    dplyr::filter(enem == enem_treatment)|>
    ggplot(aes(x = !!sym(trophic_1), y = !!sym(trophic_2))) +
    geom_line(
      aes(color = as.factor(block)),
      size = 0.5
    ) +
    geom_point(aes(color = as.factor(block)), size = 1) +
    theme_minimal() +
    scale_color_viridis_d()

  ggsave(
    PHASE_ALL,
    filename = paste0("./figures/phase-plot-all_", enem_treatment, "_", trophic_1, "_", trophic_2,    ".png"),
    height = 9,
    width = 10,
    create.dir = T
  )
}

