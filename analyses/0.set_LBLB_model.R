##########################################################################################################
# Analysis that produce a database with all the characteristic of each model
#' @param igpParms (defined by the user)
#' @param phiVal (defined by the user)
#' @return the igp_combinations as a list saved in your data folder
#' @details run it only if you dont have it in your data base
#' @details The K and S are set, but those ones change within each simulation
#############################################################################################################


#####################set LBLB_LV charactersitics  --------------------------


##2. then you define the parameters common to all the models

#model LVLV (4 possible scenarios)

#scenario1. 

if (chosen_scenario == "lblb_model_1"){

LBLB_LV_parms <- c(
  rho = 1,
  K = 5,
  En = 0.5,
  Ep = 0.5,
  frp = 0.6,   ### if wwe suppose both r and n to be biomass, we extract that when n=r=1/2 hn (to reach the linear assumtoption), then frp should be cp/4
  fnp = 0.6,
  frn = 5, ## hrtr is cn/2
  mun = 1.1,
  mup = 0.33, ## to compare with hin, they have mun = 1 (Tp) + 0.3,  and mup = 0.3 (maintenance) + 0.03 death rate
 #here is dfi
  S = 0.1  #S =0.5
) #here the Sr different

}



if (chosen_scenario == "lblb_model_2"){ #VEry cherry pocking btw

LBLB_LV_parms <- c(
  rho = 1,
  K = 1,  ##here is the change
  En = 0.5,
  Ep = 0.5,
  frp = 2,   
  fnp = 0.4,
  frn = 0.4, ## here is the change   
  mun = 0.03,
  mup = 0.8,
 #here is dfi
  S = 0.5  #S =0.5  #fullfill polis and holt
) 

}



if (chosen_scenario == "lblb_model_3"){

LBLB_LV_parms <- c(
  rho = 1,
  K = 5,  
  En = 0.5,
  Ep = 0.5,
  frp = 0.6,   
  fnp = 0.6,
  frn = 5, 
  mun = 1.1, 
  mup = 0.33,
 #here is dfi
  S = 0.9  #S =0.5
) 

}


if (chosen_scenario == "lblb_model_4"){

LBLB_LV_parms <- c(
  rho = 1,
  K = 5,  
  En = 0.5,
  Ep = 0.5,
  frp = 0.6,   
  fnp = 0.6,
  frn = 0.4, ## here is the change   
  mun = 0.33, ##we change this to see if we increase the coexistence 
  mup = 0.33,
 #here is dfi
  S = 0.1  #S =0.5
) 

}




LBLB_LV_list = list(
    parms = LBLB_LV_parms,
    model_cont = LBLB_LV_cont_model, 
    model_disc = LBLB_LV_disc_model, 
    model_disc_stoc = LBLB_LV_disc_stoc_model, 
    init = c(R = 1e-01, N = 1e-01, P = 1e-01))



