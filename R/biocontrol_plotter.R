
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
