
#LOAD AND FILTER DATA for LV

#this code gets the IGP data and does the modifcations used for LV analys, and for rpesentation, and coexistence 

source("./analyses/2a.data_modification_LV.R")

#IT GIVES YOU DATA IGP AND DATA_PRED

DATA_BIOCONTROL <- DATA_PRED

DATA_BIOCONTROL$R_plus_input <- DATA_PRED$R +400

DATA_BIOCONTROL  <- DATA_BIOCONTROL |>
  dplyr::group_by(block, enem)|>
  dplyr::arrange(week)|>
  dplyr::mutate("R_consumed" = dplyr::lag(R_plus_input)- R)|>
  dplyr::ungroup()


for (i in seq(1:dim(DATA_BIOCONTROL)[1])){
  if(is.na(DATA_BIOCONTROL$R_consumed[[i]])){
    DATA_BIOCONTROL$R_consumed[[i]] = 600 - DATA_BIOCONTROL$R[[i]] 
}
}


###another modificaction

DATA_BIOCONTROL$R_bin <- 1

for (i in seq(1:dim(DATA_BIOCONTROL)[1])){
  if(DATA_BIOCONTROL$R[[i]]==1){
    DATA_BIOCONTROL$R_bin[[i]] = 0 
}
}







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