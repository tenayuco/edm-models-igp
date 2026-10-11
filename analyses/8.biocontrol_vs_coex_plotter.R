
#import created dateframe biocontrol and of coexistnece

type_data <- "real.data"
norm_data <- "not_normalized"

out_folder <- paste0("./outputs/LV_MAP/", type_data, "/", norm_data, "/")
fig_folder <- paste0("./figures/LV_MAP/", type_data, "/", norm_data, "/")


DATA_COEX <- read.csv(paste0(out_subfolder,"complete_coex_df.cs" )
)



#plotting
PLOT_TOTAL_CON <- DATA_BIOCONTROL|>
    ggplot(aes(x = enem, y = R_consumed)) +
  geom_boxplot(aes(fill=enem))+
  scale_fill_viridis_d()+
    theme_minimal()

PLOT_TOTAL_R <- DATA_BIOCONTROL|>
    ggplot(aes(x = enem, y = R)) +
  geom_boxplot(aes(fill=enem))+
  scale_fill_viridis_d()+
    theme_minimal()



###ahora hacemos el resumen por semana pa ver como cambia en el tiempo

##
PLOT_TOTAL_CON_WEEK <- DATA_BIOCONTROL|>
    ggplot(aes(x = week, y = R_consumed)) +
  geom_boxplot(aes(fill=enem, group = week))+
  facet_wrap(~enem)+
  scale_fill_viridis_d()+
    theme_minimal()


PLOT_TOTAL_R_WEEK <- DATA_BIOCONTROL|>
    ggplot(aes(x = week, y = R)) +
  geom_boxplot(aes(fill=enem, group = week))+
  facet_wrap(~enem)+
  scale_fill_viridis_d()+
    theme_minimal()


#plotting




##para tener las means 
DATA_BIOCONTROL_WEEK_ENEM <- DATA_BIOCONTROL |>
  dplyr::group_by(enem)|>
  dplyr::summarise("mean_R_consumed" = mean(R_consumed), "sd_R_consumed" = sd(R_consumed), 
                    "mean_R"= mean(R), "sd_R"= sd(R), 
                  "mean_R_bin"= mean(R_bin), "sd_R_bin"= sd(R_bin))


dir.create("./outputs/biocontrol/")

write.csv(DATA_BIOCONTROL_WEEK_ENEM, "./outputs/biocontrol/meansR.csv")


###

  ggsave(PLOT_TOTAL_R,
    filename = paste0("./figures/biocontrol/total_R", ".png"),
    height = 8,
    width = 12,
    create.dir = T)

  ggsave(PLOT_TOTAL_R_WEEK,
    filename = paste0("./figures/biocontrol/total_Rweek", ".png"),
    height = 8,
    width = 12,
    create.dir = T)

  ggsave(PLOT_TOTAL_CON,
    filename = paste0("./figures/biocontrol/total_con", ".png"),
    height = 8,
    width = 12,
    create.dir = T)

  ggsave(PLOT_TOTAL_CON_WEEK,
    filename = paste0("./figures/biocontrol/total_con_week", ".png"),
    height = 8,
    width = 12,
    create.dir = T)







###ociocisas de terre

PLOT_P <- DATA_PRED|>

  dplyr::select(-R) |>
  tidyr::pivot_longer(cols = c(X, Y), names_to ="Predator", values_to = "individuals")|>
  ggplot(aes(x = week)) +
  geom_boxplot(aes(y = individuals, fill= as.factor(Predator), group = interaction(week, Predator)))+
  facet_wrap(~enem, scales = "free")+
  scale_fill_viridis_d()+
    theme_minimal()