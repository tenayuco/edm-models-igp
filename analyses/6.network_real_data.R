

COMPLETE_DF <- read.csv(
  "./outputs/LV_MAP/real.data/coexistence/complete_coex_df.csv"
)


##ok ima do a pre-scaling categorization process. 
## were for each value of interaction i will put 

NET_DF <-  COMPLETE_DF|> 
  dplyr::select(enem, varName, grand_mean)|> 
  dplyr::ungroup()|> 
  #dplyr::group_by(varName) |> 
  dplyr::mutate(grand_mean_pro = 1* grand_mean/max(abs(grand_mean)))


dir.create("./figures/LV_MAP/real.data/network/", recursive = T)


for(enemigo in unique(NET_DF$enem)){
network_plotters(chosen_enem = enemigo)}
