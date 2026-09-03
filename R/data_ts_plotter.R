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


speciesCol_trop <- c(
  "Y=ac" = "black",
  "X=am" = "#996600",
  "X=cc" = "#00BCD4",
  "Y=my" = "#339900",
  "Y=ma" = "orange",
  "X=ol" = "red",
  "X=aa" = "#FF9999",
  "Y=ec" = "#FFCC00",
  "X=sr" = "#CCFF66",
  "R=mp" = "darkgreen"
)

###here all
plotter_data_all <- function(data_long, remove_aphid = FALSE) {
  aphid = ""

  data_long <-  data_long |> 
    tidyr::unite("fullname", c(trophic, species), sep= "=", remove= FALSE)

  if (remove_aphid == TRUE) {
    aphid = "noAphids"
    data_long <- data_long |>
      dplyr::filter(species != "mp")
  }


  TIME_SERIES_ALL <- data_long |>
    ggplot(aes(x = week, y = individuals)) +
    geom_line(
      aes(color = fullname, group = as.factor(interaction(block, species))),
      size = 0.5
    ) +
    geom_point(aes(color = fullname), size = 1) +
    facet_wrap(~enem, scales = "free_y") +
    scale_color_manual(
      values = speciesCol_trop
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

plotter_data_mean <- function(data_mean, remove_aphid = FALSE) {
  aphid = ""

    data_mean <-  data_mean |> 
    tidyr::unite("fullname", c(trophic, species), sep= "=", remove= FALSE)

  if (remove_aphid == TRUE) {
    aphid = "noAphids"

    data_mean <- data_mean |>
      dplyr::filter(species != "mp")
  }
  TIME_SERIES_MEAN <- data_mean |>
    ggplot(aes(x = week, y = meanIndividuals)) +
    geom_line(
      aes(color = fullname),
      size = 0.5
    ) +
    geom_point(aes(color = fullname), size = 1) +
    geom_errorbar(aes(
      ymin = meanIndividuals - sdIndividuals,
      ymax = meanIndividuals + sdIndividuals,
      color = fullname
    )) +
    facet_wrap(~enem, scales = "free_y") +
    scale_color_manual(
      values = speciesCol_trop
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


phaseplotter_all_block <- function(data_pred,trophic_1 = "pred1",trophic_2 = "pred2",enem_treatment = "cc+ma") {
  PHASE_ALL <- data_pred |>
    dplyr::filter(enem == enem_treatment) |>
    ggplot(aes(x = !!sym(trophic_1), y = !!sym(trophic_2))) +
    # geom_path(aes(colour= week),
    ## color= "black",
    #  size = 1
    #) +
    geom_point(aes(fill = as.factor(block)), size = 2, shape = 21) +
    facet_wrap(~block, scales = "free", ncol = 3) +
    theme_minimal() +
    scale_fill_viridis_d()
  #scale_color_viridis_c()

  ggsave(
    PHASE_ALL,
    filename = paste0(
      "./figures/treatment_block/phase-plot-all_block_",
      enem_treatment,
      "_",
      trophic_1,
      "_",
      trophic_2,
      ".png"
    ),
    height = 9,
    width = 10,
    create.dir = T
  )
}


phaseplotter_all <- function(data_pred,trophic_1 = "X",trophic_2 = "Y",enem_treatment = "cc+ma") {PHASE_ALL <- data_pred |>
    dplyr::filter(enem == enem_treatment) |>
    ggplot(aes(x = !!sym(trophic_1), y = !!sym(trophic_2))) +
    #geom_path(
    # aes(color= as.factor(block), group= as.factor(week)),
    #size = 0.5
    #) +
    geom_point(aes(fill = as.factor(block)), size = 5, shape = 21) +
    theme_minimal() +
    scale_fill_viridis_d()

  ggsave(
    PHASE_ALL,
    filename = paste0(
      "./figures/treatment/phase-plot-all_",
      enem_treatment,
      "_",
      trophic_1,
      "_",
      trophic_2,
      ".png"
    ),
    height = 9,
    width = 10,
    create.dir = T
  )
}


phaseplotter_ts_all <- function(data_pred,data_long,enem_treatment = "cc+ma") {
  data_long <- data_long |>
    dplyr::filter(enem == enem_treatment)


  sp_X <- unique(data_long$species[data_long$trophic == "X"]) 
  Xname <- paste0("X = ",sp_X)
  
  sp_Y <- unique(data_long$species[data_long$trophic == "Y"]) 
  Yname <- paste0("Y = ", sp_Y)

  sp_R <- unique(data_long$species[data_long$trophic == "R"]) 
  Rname <- paste0("R = ",sp_R)

  # First, separate your data into two groups
  data_XY <- data_long|> dplyr::filter(trophic %in% c("X", "Y"))
  data_R <- data_long|> dplyr::filter(trophic == "R")

  # Calculate a scaling factor to bring herbivore values to similar range as predators
  # For example, if predators range 0-100 and herbivores 0-500:
  scale_factor <- max(data_XY$individuals, na.rm = TRUE) / max(data_R$individuals, na.rm = TRUE)

  # Or use a specific multiplier (adjust based on your data)
  #scale_factor <- 0.1 # if herbivores are ~10x larger than predators

  TIME_SERIES_ALL <- ggplot() +
    geom_line(data = data_XY, aes(x = week,y = individuals,color = species, group = as.factor(interaction(block, species))), size = 0.5) +
    geom_point(data = data_XY, aes(x = week, y = individuals, color = species), size = 1) +

    geom_line(data = data_R, aes(x = week, y = individuals * scale_factor, color = species, group = as.factor(interaction(block, species))),
size = 0.5) +

    geom_point(data = data_R,aes(x = week, y = individuals * scale_factor, color = species),
      size = 1) +
  scale_color_manual(values = speciesCol) +
    scale_y_continuous(
      name = paste0("Predator abundance (", Xname, ", " , Yname, ")"),
      sec.axis = sec_axis(
        ~ . / scale_factor,
        name = paste0("Herbivore abundance (", Rname, ")")
      )
    ) +
    theme_minimal() 
 

  PHASE_XY <- data_pred |>
    dplyr::filter(enem == enem_treatment) |>
    ggplot(aes(x = X, y = Y)) +
    geom_point(aes(fill = as.factor(block)), size = 3, shape = 21) +
    theme_minimal() +
    scale_fill_viridis_d() +
   # theme(legend.position = "none") +
    xlab(Xname) +
    ylab(Yname) +
    labs(fill= "block")

  PHASE_RX <- data_pred |>
    dplyr::filter(enem == enem_treatment) |>
    ggplot(aes(x = R, y = X)) +
    geom_point(aes(fill = as.factor(block)), size = 3, shape = 21) +
    theme_minimal() +
    scale_fill_viridis_d() +
    #theme(legend.position = "none") +
    xlab(Rname) +
    ylab(Xname)+
    labs(fill= "block")

  PHASE_RY <- data_pred |>
    dplyr::filter(enem == enem_treatment) |>
    ggplot(aes(x = R, y = Y)) +
    geom_point(aes(fill = as.factor(block)), size = 3, shape = 21) +
    theme_minimal() +
    scale_fill_viridis_d() +
    #theme(legend.position = "none") +
    xlab(Rname) +
    ylab(Yname)+
    labs(fill= "block")

  FULL_PLOT <- (TIME_SERIES_ALL + PHASE_XY) /
    (PHASE_RX + PHASE_RY)

  ggsave(
    FULL_PLOT,
    filename = paste0("./figures/treatment/fullplot_new_", enem_treatment, ".png"),
    height = 9,
    width = 11,
    create.dir = T
  )
}

plotter_coex <- function(data_coex) {

coex_plot <- data_coex |>
    ggplot(aes(x = week, y = mean_coex)) +
    geom_area(fill = "darkgreen", alpha = 0.3) +
    geom_line(size = 1) +
    facet_wrap(~enem) +
    theme_minimal()


  ggsave(
    coex_plot,
    filename = paste0("./figures/coexistence", ".png"),
    height = 9,
    width = 10,
    create.dir = T
  )
}



plotter_survival <- function(data_coex_av) {

data_coex_av_long <-  data_coex_av |> 
tidyr::gather(key= "mean_species", value= "value", mean_X, mean_Y)

survival_plot <- data_coex_av_long  |>
    ggplot(aes(x = week, y = value)) +
    geom_line(size = 1, aes(color= mean_species)) +
    facet_wrap(~enem) +
    theme_minimal()


  ggsave(
    survival_plot,
    filename = paste0("./figures/survival_plot", ".png"),
    height = 9,
    width = 10,
    create.dir = T
  )
}




ts_plotter_data <- function(data_pred, plotted_var = c("R", "X", "Y")){

  outLong <-  data_pred  |> 
    tidyr::pivot_longer(cols= plotted_var, names_to = "varName", values_to = "value") |> 
    dplyr::filter(varName %in% plotted_var)
    
  
  RNP_ts <- outLong |> 
    ggplot(aes(x=week, y=value)) +
    geom_line(aes(color= varName, group = as.factor(interaction(block, varName))), linewidth=1) + 
    geom_point(aes(color= varName, group = as.factor(interaction(block, varName)))) + 
    xlab("Time") +
    facet_wrap(~enem, scales = "free_y") +
    scale_color_viridis_d() +
    
    # scale_color_manual(
     # values = speciesCol_trop
    #) +
    theme_minimal()
 
  return(RNP_ts)
}

