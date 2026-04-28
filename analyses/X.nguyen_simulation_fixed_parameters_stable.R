rm(list = ls()) # clear memory
graphics.off() # clear graphics
source("./code_nguyen_R/toolbox_LV_map.R") # load the LV-map functions
source("./code_nguyen_R/toolbox_plot.R") # load the plot functions
source("./code_nguyen_R/toolbox_coexistence.R")
require(tictoc) # for measuring execution time
require(latex2exp) # for latex annotation on graph

set.seed(1)

# =======================
### 1. simulate the data
# =======================
# parameters



# =======================
### 1. simulate the data
# =======================
# parameters
S <- 3 # number of species
dt <- 0.01
LBLB_LV_parms[["S"]] <- 0.5
LBLB_LV_parms[["K"]] <- 10
r <- c(log(1-LBLB_LV_parms[["mup"]]*dt), log(1-LBLB_LV_parms[["mun"]]*dt), log(1+LBLB_LV_parms[["rho"]]*dt)) # set the intrinsic growth rates
#lambda <- 1  ## I change this 
noise <- 0.01
names(r) <- paste("sp", 1:S, sep = "") # species names
alpha <- matrix(NA, nrow = S, ncol = S) # set the per capita interaction strengths matrix
colnames(alpha) <- paste("sp", 1:S, sep = "")
rownames(alpha) <- paste("sp", 1:S, sep = "")
alpha[1, 1] <- 0
alpha[1, 2] <- LBLB_LV_parms[["Ep"]]* LBLB_LV_parms[["fnp"]] * (1- LBLB_LV_parms[["S"]]) #0.15
alpha[1, 3] <- LBLB_LV_parms[["Ep"]]* LBLB_LV_parms[["frp"]] *  LBLB_LV_parms[["S"]] #0.15
alpha[2, 1] <- - LBLB_LV_parms[["fnp"]] * (1- LBLB_LV_parms[["S"]])
alpha[2, 2] <- 0
alpha[2, 3] <- LBLB_LV_parms[["En"]]* LBLB_LV_parms[["frn"]]
alpha[3, 1] <- - LBLB_LV_parms[["frp"]] *  LBLB_LV_parms[["S"]]
alpha[3, 2] <-  -LBLB_LV_parms[["frn"]]
#alpha[3, 3] <- -LBLB_LV_parms[["rho"]]/LBLB_LV_parms[["K"]]
alpha[3, 3] <- 0

#r <- r * lambda
alpha <- alpha * dt
# simulate the Lotka-Volterra model with environmental noise
Tmax <- 300 # set the number of time steps
N <- matrix(NA, nrow = Tmax, ncol = S) # set population dynamics
alpha_t <- array(NA, dim = c(Tmax - 1, S, S)) # get alpha at each time point for plotting
r_t <- array(NA, dim = c(Tmax - 1, S))
N[1, ] <- c(0.1, 0.1, 0.1) # set the initial densities
colnames(N) <- paste("sp", 1:S, sep = "")
for (i in 2:Tmax) {
  N[i, ] <- N[i - 1, ] * exp(r + alpha %*% N[i - 1, ] + rnorm(S, mean = 0, sd =100* abs(noise * r))) #changed it no abs
  alpha_t[i - 1, , ] <- alpha
  r_t[i - 1, ] <- r
}
Omega <- 10^log_Omega_f3(alpha)
eta <- eta_fn(alpha, r)$eta
# plot the densities

DF_N <-  as.data.frame(N)
DF_N$time <- seq(1:Tmax)


plot(DF_N)

names(DF_N) <- c("P", "N", "R", "time")
full_plot(outDF = DF_N, tmax = 300, disc_cont ="nguyen_LV")


