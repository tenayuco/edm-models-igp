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
plotter_data_all <- function(data_long) {
  TIME_SERIES_ALL <- data_long |>
    ggplot(aes(x = week, y = individuals)) +
    geom_line(
      aes(color = species, group = as.factor(interaction(block, species))),
      size = 0.5
    ) +
    geom_point(aes(color = species), size = 1) +
    facet_wrap(~enem) +
    scale_color_manual(
      values = speciesCol
    ) +
    theme_minimal()

  ggsave(
    TIME_SERIES_ALL,
    filename = "./figures/time-series-all.png",
    height = 9,
    width = 10,
    create.dir = T
  )
}

plotter_data_mean <- function(data_mean) {
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
    facet_wrap(~enem) +
      scale_color_manual(
      values = speciesCol
    ) +
    theme_minimal()
  ggsave(
    TIME_SERIES_MEAN,
    filename = "./figures/time-series-mean.png",
    height = 9,
    width = 10,
    create.dir = T
  )
}
