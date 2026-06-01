ccm_data_out<- multispatialCCM::make_ccm_data()
Accm<-ccm_data_out$Accm
Bccm<-ccm_data_out$Bccm
time<-ccm_data_out$time_ccm


########-----------------------------------my modification

TS_DF <- data.frame(time= time, A = Accm, B= Bccm)

TS_DF$rep <- 0

rep_count <- 0

for (i in seq(1:dim(TS_DF)[1])){
  if(is.na(TS_DF$time[i])){
    rep_count <- rep_count +1
  }
  TS_DF$rep[i] <-  rep_count
}

TS_DF <-  tidyr::drop_na(TS_DF)

TS_DF_LONG <- TS_DF  |>  tidyr::pivot_longer(cols = A:B, names_to = "variable", values_to = "values")

###ggpplit the data

library(ggplot2)

TS_PLOT <- TS_DF_LONG  |> 
  ggplot(aes(x = time, y = values)) +
    geom_path(aes(colour= variable),
      size = 1
    ) +
    geom_point(aes(fill = as.factor(variable)), size = 1.5, shape= 21) +
    facet_wrap(~rep, scales = "free", ncol = 4)

###### transform the data back to list

## first pivot wider to have all the replicates
## then add an NA underneath
## then pivot back to longer
## then add it to list 

########


#Calculate optimal E

maxE<-5 #Maximum E to test
#Matrix for storing output
Emat<-matrix(nrow=maxE-1, ncol=2); colnames(Emat)<-c("A", "B")

#Loop over potential E values and calculate predictive ability
#of each process for its own dynamics
for(E in 2:maxE) {
#Uses defaults of looking forward one prediction step (predstep)
#And using time lag intervals of one time step (tau)
Emat[E-1,"A"]<-multispatialCCM::SSR_pred_boot(A=Accm, E=E, predstep=1, tau=1)$rho
Emat[E-1,"B"]<-multispatialCCM::SSR_pred_boot(A=Bccm, E=E, predstep=1, tau=1)$rho
}


#Look at plots to find E for each process at which
#predictive ability rho is maximized
matplot(2:maxE, Emat, type="l", col=1:2, lty=1:2,
xlab="E", ylab="rho", lwd=2)
legend("bottomleft", c("A", "B"), lty=1:2, col=1:2, lwd=2, bty="n")

#Results will vary depending on simulation.
#Using the seed we provide,
#maximum E for A should be 2, and maximum E for B should be 3.
#For the analyses in the paper, we use E=2 for all simulations.
E_A<-2
E_B<-3


#Check data for nonlinear signal that is not dominated by noise
#Checks whether predictive ability of processes declines with
#increasing time distance
#See manuscript and R code for details
signal_A_out<-multispatialCCM::SSR_check_signal(A=Accm, E=E_A, tau=1,
predsteplist=1:10)
signal_B_out<-multispatialCCM::SSR_check_signal(A=Bccm, E=E_B, tau=1,
predsteplist=1:10)


#Run the CCM test
#E_A and E_B are the embedding dimensions for A and B.
#tau is the length of time steps used (default is 1

#iterations is the number of bootsrap iterations (default 100)
# Does A "cause" B?
#Note - increase iterations to 100 for consistant results
CCM_boot_A<-multispatialCCM::CCM_boot(Accm, Bccm, E_A, tau=1, iterations=10)
# Does B "cause" A?
CCM_boot_B<-multispatialCCM::CCM_boot(Bccm, Accm, E_B, tau=1, iterations=10)


#Test for significant causal signal
#See R function for details
(CCM_significance_test<-multispatialCCM::ccmtest(CCM_boot_A,
CCM_boot_B))


#Plot results
plotxlimits<-range(c(CCM_boot_A$Lobs, CCM_boot_B$Lobs))
#Plot "A causes B"
plot(CCM_boot_A$Lobs, CCM_boot_A$rho, type="l", col=1, lwd=2,
xlim=c(plotxlimits[1], plotxlimits[2]), ylim=c(0,1),
xlab="L", ylab="rho")
#Add +/- 1 standard error
matlines(CCM_boot_A$Lobs,
cbind(CCM_boot_A$rho-CCM_boot_A$sdevrho,
CCM_boot_A$rho+CCM_boot_A$sdevrho),
lty=3, col=1)
#Plot "B causes A"
lines(CCM_boot_B$Lobs, CCM_boot_B$rho, type="l", col=2, lty=2, lwd=2)
#Add +/- 1 standard error
matlines(CCM_boot_B$Lobs,
cbind(CCM_boot_B$rho-CCM_boot_B$sdevrho,
CCM_boot_B$rho+CCM_boot_B$sdevrho),
lty=3, col=2)
legend("topleft",
c("A causes B", "B causes A"),
lty=c(1,2), col=c(1,2), lwd=2, bty="n")