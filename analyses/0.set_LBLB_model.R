##########################################################################################################
# Analysis that produce a database with all the characteristic of each model
#' @param igpParms (defined by the user)
#' @param phiVal (defined by the user)
#' @return the igp_combinations as a list saved in your data folder
#' @details run it only if you dont have it in your data base
#' @details The K and S are set, but those ones change within each simulation
#############################################################################################################


##2. then you define the parameters common to all the models
igpParms <- c(
  rho = 1,
  K = 1.5,
  En = 0.5,
  Ep = 0.5,
  Cn = 10,
  Cp = 2.5,
  mun = 1.1,
  mup = 0.33, ## to compare with hin, they have mun = 1 (Tp) + 0.3,  and mup = 0.3 (maintenance) + 0.03 death rate
  Hn = 1,
  Hp = 1,
  mn = 0.2,
  mp = 0.2, #here is dfi
  S = 0.5
) #here the Sr different


igpInit <- c(R = 1e-01, Nl = 1e-01, Na = 1e-01, P = 1e-01)

igpTimes_cont <- seq(from = 1, to = 2000, by = .05) #this is the step for the integration
igpTimes_disc <- seq(from = 1, to = 2000, by = 0.01) #this is the step for the integration


LBLB_list = list(
    igp_parms = igpParms,
    igp_model_cont = igp_model_LBLB, 
    igp_model_disc = igp_model_LBLB_disc, 
    igp_model_disc_stoc = igp_model_LBLB_disc_stoc, ##from the rnp function
    igp_init = igpInit,
  igp_times_cont = igpTimes_cont,
igp_times_disc = igpTimes_disc)