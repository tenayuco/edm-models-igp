

type_data= "real.data"


DATA_IGP <- readr::read_csv("data/dataIGP_2025.csv")

#to represent the data


#========2.LV MAP======================

#this is nice cause you loop outside
#names of the folders

#prefedined in the krnel
#kernel <- "time"
#kernel <- "state"




if(type_data == "real.data"){
  out_path= paste0("./outputs/LV_MAP/", type_data, "/")
}

dir.create(paste0(out_path), recursive = T)


out_folder <- out_path


v_num_rep = c(1)
v_rpresent = c(FALSE)
v_num_seed = seq(1:10)

#import the source 
#==========================I.B. RUN LV MAP SPATIAL KERNEL on DATA=======================================
source("./analyses/general_LV_looper.R")


DATA_PRED <- df_modifier_lv(raw_data = DATA_IGP)

DATA_PRED <- DATA_PRED |> 
  dplyr::filter(!(enem == "ec+sr"))|> 
  dplyr::filter(!(enem == "ec+am"))

v_enemigos <-  unique(DATA_PRED$enem)


##here if you wqnt yto work with differences (to try now)

dif_cond <-  FALSE

if (dif_cond == TRUE){
  out_folder <- paste0(out_folder, "differences/") 
  fig_folder <- paste0(fig_folder, "differences/") 

  DATA_PRED<- DATA_PRED |> 
    dplyr::group_by(block, enem) |>  # Group by both replicate AND enemy
    dplyr::mutate(
      R = c(NA, diff(R) - 300),  # A2 - (A1 + 300)
      N = c(NA, diff(N)),        # Just the difference
      P = c(NA, diff(P))         # Just the difference
    ) |> 
    tidyr::drop_na() |> 
    dplyr::ungroup()  # Optional: remove grouping after
}





kernel_chosen = "state"

#looop around 
tic()
for (enem in v_enemigos){
  DATA_PRED <- DATA_PRED |> 
    dplyr::filter(enem ==enem)
lv_looper_lists_general(data_used = DATA_PRED, v_num_rep, v_rpresent, v_num_seed, enemigo= enem)
}
toc()


#aparentelyn it is working.. now for the plooting 


##here is the big change 
##old
#lv_looper_lists_microcosmos(raw_data = DATA_IGP, v_enemigos = v_enemigos, v_num_seed = v_num_seed, num_rep = num_rep, rpresent = rpresent, kernel_chosen = kernel)
#source("./analyses/2b.looper_data_lv_map.R")


#===========================================================================================

#SO AT THIS POINT WE HAVE DONE THE HEAVY ANALYSIS 
#NOW THE MORE GENERAL PLOTTINGS

#========================I.C PLOT LV <MICRSO ============================

#fives the list per treamtne 

#fig_folder <- paste0("./figures/LV_MAP/", type_data, "/")


#source("./analyses/1_2.plot_extract_per_treatment.R")
#source("./analyses/1_2.plotter_LV.R")



#plot_per_treatment(out_folder, true_values = FALSE)

#full_df <- extract_par_all_treatment(out_folder) ##generates the file


#plotter_full_parameters_microcosmos(df_full = full_df, fig_folder = fig_folder)

## coexistence valuess. 
#plotter_theta_microcosmos(df_full = full_df, fig_folder = fig_folder)

#plotter_omega_microcosmos(df_full = full_df, fig_folder = fig_folder)

