#rm(list = ls()) # clear memory
#graphics.off() # clear graphics
#source("./code_nguyen_R/toolbox_LV_map.R") # load the LV-map functions
#source("./code_nguyen_R/toolbox_plot.R") # load the plot functions
#source("./code_nguyen_R/toolbox_coexistence.R")# load the coexistence function
#library(tictoc) # for measuring execution time
#require(latex2exp)
# ========================
# Data preparation Blasius
# ========================
N_list <- vector(mode = "list", length = 9)
for (i in 1:9) {
  filename <- paste("data/data_nguyen/C", i, "_clean.csv", sep = "")
  d <- read.csv(file = filename, header = T)
  if (i == 5) {
    d_cut <- d[1:86, ]
  } else {
    d_cut <- d[1:139, ]
  }
  print(d_cut)
  N <- cbind(d_cut$algae, d_cut$rotifers) # extract algae of rotifers densities
  colnames(N) <- c("algae", "rotifers")
  N_list[[i]] <- N
}

S <- dim(N)[2]
Tmax <- dim(N)[1]

#---------------------------simple plot from matrix with replicates-----------------------------------------
DF_NGUYEN_LIST <- data.frame()
for (i in 1:9){
  df_temp <- as.data.frame(N_list[i])
  df_temp$replicate <- i
  df_temp$time <- seq(1, dim(df_temp)[1])
  DF_NGUYEN_LIST <-  rbind(DF_NGUYEN_LIST, df_temp)
}

names(DF_NGUYEN_LIST) <- c("N", "P", "replicate", "time")

TS_PLOT_NGUYEN <- ts_plotter(outDF = DF_NGUYEN_LIST, plotted_var = c("N", "P"), replicate = "replicate")

#---------------------------------------


# ================
# Cross validation
# ================
cv_list <- vector(mode = "list", length = 9)
tic()
for (i in 1:9) {
  out_cv <- LV_map_state_space_cross_validation(N_list[[i]], theta_v = seq(0, 3, 0.01))
  cv_list[[i]] <- out_cv
}
toc()


# ========================
# Estimation of parameters
# ========================
r_hat_list <- vector(mode = "list", length = 9)
alpha_hat_list <- vector(mode = "list", length = 9)
r_se_list <- vector(mode = "list", length = 9)
alpha_se_list <- vector(mode = "list", length = 9)
out_list <- vector(mode = "list", length = 9)
tic()
for (i in 1:9) {
  out_list[[i]] <- LV_map(N_list[[i]], cv_list[[i]]$theta_o)
  r_hat_list[[i]] <- out_list[[i]]$r_hat
  r_se_list[[i]] <- out_list[[i]]$r_se
  alpha_hat_list[[i]] <- out_list[[i]]$alpha_hat
  alpha_se_list[[i]] <- out_list[[i]]$alpha_se
}
toc()


##------------now plotting the parameters---------------
DF_RT <- data.frame()
for (i in 1:9){
  df_rt_temp <- as.data.frame(r_hat_list[[i]])
  df_rt_temp$replicate <- i
  df_rt_temp$time <- seq(1, dim(df_rt_temp)[1])
  DF_RT <-  rbind(DF_RT, df_rt_temp)
}

DF_RT_SD <- data.frame()
for (i in 1:9){
  df_rt_temp <- as.data.frame(r_se_list[[i]])
  df_rt_temp$replicate <- i
  df_rt_temp$time <- seq(1, dim(df_rt_temp)[1])
  DF_RT_SD <-  rbind(DF_RT_SD, df_rt_temp)
}


DF_ALPHA <- data.frame()
for (i in 1:9){
  df_al_temp <- as.data.frame(alpha_hat_list[[i]])
  df_al_temp$replicate <- i
  df_al_temp$time <- seq(1, dim(df_al_temp)[1])
  DF_ALPHA <-  rbind(DF_ALPHA, df_al_temp)
}

DF_ALPHA_SD <- data.frame()
for (i in 1:9){
  df_al_temp <- as.data.frame(alpha_se_list[[i]])
  df_al_temp$replicate <- i
  df_al_temp$time <- seq(1, dim(df_al_temp)[1])
  DF_ALPHA_SD <-  rbind(DF_ALPHA_SD, df_al_temp)
}

RT_PLOT <- rt_plotter(df_rt = DF_RT)
ALPHA_PLOT <- alpha_plotter(df_alpha= DF_ALPHA)





# =========================
# Calculating omega and eta
# =========================
log_Omega_mean_list <- array(NA, dim = 9)
eta1_mean_list <- array(NA, dim = 9)
eta2_mean_list <- array(NA, dim = 9)
log_Omega_cimean_list <- array(NA, dim = c(9, 2))
eta1_cimean_list <- array(NA, dim = c(9, 2))
eta2_cimean_list <- array(NA, dim = c(9, 2))


tic()
for (i in 1:9) {
  o_coexistence <- coexistence_metrics_f2(out_list[[i]])
  log_Omega_mean_list[i] <- mean(o_coexistence$log_Omega_hat)
  eta1_mean_list[i] <- mean(o_coexistence$eta_hat[, 1])
  eta2_mean_list[i] <- mean(o_coexistence$eta_hat[, 2])
  log_Omega_cimean_list[i, ] <- colMeans(o_coexistence$log_Omega_ci)
  eta1_cimean_list[i, ] <- colMeans(o_coexistence$eta_ci[, 1, ])
  eta2_cimean_list[i, ] <- colMeans(o_coexistence$eta_ci[, 2, ])
}
toc()





















# ========================
# PLOT FIGURE IN MAIN TEXT
# ========================

frame_include <- FALSE

# --------
# Figure 3
# --------
filename <- "mainfig_empirical_chemostat"
#png(filename = paste(filename, ".png", sep = ""), width = 9, height = 8, units = "in", res = 300)
pdf(file = paste(filename, ".pdf", sep = ""), width = 9, height = 8)
layout(matrix(c(1, 2, 3, 4, 5, 5),
  nrow = 2, ncol = 3, byrow = TRUE
))
se_len <- 0.05

margin <- c(0.5, 0.7, 0.3, 0) # bottom, left, top, right
par(mai = margin)
# Intrinsic growth rate
plot(NA,
  xlim = c(0.9, 4.1), ylim = c(0.5, 2), xlab = "", ylab = "",
  frame = frame_include, cex.axis = 1.5, xaxt = "n"
)
mtext("A", 3, line = 1, cex = 1, font = 2, at = 0)
xtick <- seq(1, 4)
xlabel <- c(
  "Exp C3", "Exp C2", "Exp C4", "Exp C1"
)
axis(side = 1, at = xtick, labels = xlabel, cex = 3.5)
mtext("Intrinsic growth rate (r)", 2, line = 3, cex = 1.)
points(seq(1, 4), c(
  mean(r_hat_list[[3]][, 1]), mean(r_hat_list[[2]][, 1]),
  mean(r_hat_list[[4]][, 1]), mean(r_hat_list[[1]][, 1])
), pch = 16, col = c("violet", "violetred", "violetred2", "violetred4"), cex = 2.)
arrows(seq(1, 4), c(
  mean(r_hat_list[[3]][, 1]), mean(r_hat_list[[2]][, 1]),
  mean(r_hat_list[[4]][, 1]), mean(r_hat_list[[1]][, 1]),
  mean(r_hat_list[[3]][, 1]), mean(r_hat_list[[2]][, 1]),
  mean(r_hat_list[[4]][, 1]), mean(r_hat_list[[1]][, 1])
),
y1 = c(
  mean(r_hat_list[[3]][, 1]) + 1.96 * mean(r_se_list[[3]][, 1]),
  mean(r_hat_list[[2]][, 1]) + 1.96 * mean(r_se_list[[2]][, 1]),
  mean(r_hat_list[[4]][, 1]) + 1.96 * mean(r_se_list[[4]][, 1]),
  mean(r_hat_list[[1]][, 1]) + 1.96 * mean(r_se_list[[1]][, 1]),
  mean(r_hat_list[[3]][, 1]) - 1.96 * mean(r_se_list[[3]][, 1]),
  mean(r_hat_list[[2]][, 1]) - 1.96 * mean(r_se_list[[2]][, 1]),
  mean(r_hat_list[[4]][, 1]) - 1.96 * mean(r_se_list[[4]][, 1]),
  mean(r_hat_list[[1]][, 1]) - 1.96 * mean(r_se_list[[1]][, 1])
), length = se_len, angle = 90,
col = c("violet", "violetred", "violetred2", "violetred4")
)
###############
# Intraspecific interactions
plot(NA,
  xlim = c(0.9, 4.1), ylim = c(-2, -0.5), xlab = "", ylab = "",
  frame = frame_include, cex.axis = 1.5, xaxt = "n"
)
mtext("B", 3, line = 1, cex = 1, font = 2, at = 0)
mtext("Intraspecific interaction between algae", 2, line = 3, cex = 1.)
xtick <- seq(1, 4)
xlabel <- c(
  "Exp C3", "Exp C2", "Exp C4", "Exp C1"
)
axis(side = 1, at = xtick, labels = xlabel, cex = 3.5)
points(seq(1, 4), c(
  mean(alpha_hat_list[[3]][, 1, 1]), mean(alpha_hat_list[[2]][, 1, 1]),
  mean(alpha_hat_list[[4]][, 1, 1]), mean(alpha_hat_list[[1]][, 1, 1])
), pch = 15, cex = 2., col = c("violet", "violetred", "violetred2", "violetred4"))
arrows(seq(1, 4), c(
  mean(alpha_hat_list[[3]][, 1, 1]), mean(alpha_hat_list[[2]][, 1, 1]),
  mean(alpha_hat_list[[4]][, 1, 1]), mean(alpha_hat_list[[1]][, 1, 1]),
  mean(alpha_hat_list[[3]][, 1, 1]), mean(alpha_hat_list[[2]][, 1, 1]),
  mean(alpha_hat_list[[4]][, 1, 1]), mean(alpha_hat_list[[1]][, 1, 1])
), y1 = c(
  mean(alpha_hat_list[[3]][, 1, 1]) + 1.96 * mean(alpha_se_list[[3]][, 1, 1]),
  mean(alpha_hat_list[[2]][, 1, 1]) + 1.96 * mean(alpha_se_list[[2]][, 1, 1]),
  mean(alpha_hat_list[[4]][, 1, 1]) + 1.96 * mean(alpha_se_list[[4]][, 1, 1]),
  mean(alpha_hat_list[[1]][, 1, 1]) + 1.96 * mean(alpha_se_list[[1]][, 1, 1]),
  mean(alpha_hat_list[[3]][, 1, 1]) - 1.96 * mean(alpha_se_list[[3]][, 1, 1]),
  mean(alpha_hat_list[[2]][, 1, 1]) - 1.96 * mean(alpha_se_list[[2]][, 1, 1]),
  mean(alpha_hat_list[[4]][, 1, 1]) - 1.96 * mean(alpha_se_list[[4]][, 1, 1]),
  mean(alpha_hat_list[[1]][, 1, 1]) - 1.96 * mean(alpha_se_list[[1]][, 1, 1])
), length = se_len, angle = 90, col = c("violet", "violetred", "violetred2", "violetred4"))

# Per capita death rate
margin <- c(0.5, 0.7, 0.3, 0.1) # bottom, left, top, right
par(mai = margin)
plot(NA,
  xlim = c(0.9, 4.1), ylim = c(-0.05, 0), xlab = "", ylab = "",
  frame = frame_include, cex.axis = 1.5, xaxt = "n"
)
mtext("C", 3, line = 1, cex = 1, font = 2, at = 0)
mtext("Death rate of algae by rotifers", 2, line = 3, cex = 1.)
xtick <- seq(1, 4)
xlabel <- c(
  "Exp C3", "Exp C2", "Exp C4", "Exp C1"
)
axis(side = 1, at = xtick, labels = xlabel, cex = 3.5)

points(seq(1, 4), c(
  mean(alpha_hat_list[[3]][, 1, 2]), mean(alpha_hat_list[[2]][, 1, 2]),
  mean(alpha_hat_list[[4]][, 1, 2]), mean(alpha_hat_list[[1]][, 1, 2])
), pch = 17, cex = 2., col = c("violet", "violetred", "violetred2", "violetred4"))
arrows(seq(1, 4), c(
  mean(alpha_hat_list[[3]][, 1, 2]), mean(alpha_hat_list[[2]][, 1, 2]),
  mean(alpha_hat_list[[4]][, 1, 2]), mean(alpha_hat_list[[1]][, 1, 2]),
  mean(alpha_hat_list[[3]][, 1, 2]), mean(alpha_hat_list[[2]][, 1, 2]),
  mean(alpha_hat_list[[4]][, 1, 2]), mean(alpha_hat_list[[1]][, 1, 2])
), y1 = c(
  mean(alpha_hat_list[[3]][, 1, 2]) + 1.96 * mean(alpha_se_list[[3]][, 1, 2]),
  mean(alpha_hat_list[[2]][, 1, 2]) + 1.96 * mean(alpha_se_list[[2]][, 1, 2]),
  mean(alpha_hat_list[[4]][, 1, 2]) + 1.96 * mean(alpha_se_list[[4]][, 1, 2]),
  mean(alpha_hat_list[[1]][, 1, 2]) + 1.96 * mean(alpha_se_list[[1]][, 1, 2]),
  mean(alpha_hat_list[[3]][, 1, 2]) - 1.96 * mean(alpha_se_list[[3]][, 1, 2]),
  mean(alpha_hat_list[[2]][, 1, 2]) - 1.96 * mean(alpha_se_list[[2]][, 1, 2]),
  mean(alpha_hat_list[[4]][, 1, 2]) - 1.96 * mean(alpha_se_list[[4]][, 1, 2]),
  mean(alpha_hat_list[[1]][, 1, 2]) - 1.96 * mean(alpha_se_list[[1]][, 1, 2])
), length = se_len, angle = 90, col = c("violet", "violetred", "violetred2", "violetred4"))



# Omega
opacity <- 0.1
margin <- c(0.5, 0.7, 0.2, 0.) # top, left, bottom, right
par(mai = margin)
plot(NA,
  xlim = c(0.1, 8), ylim = c(10^(-1.1), 10^(-0.3)), frame = frame_include,
  xlab = "", ylab = "", xaxt = "n", cex.axis = 1.5, log = "y"
)
mtext("D", 3, line = 1, cex = 1, font = 2, at = -1.8)
# Environment 1
points(seq(0.5, 1.5, 0.3), 10^log_Omega_mean_list[1:4], pch = 22, col = "red", cex = 1.)
arrows(seq(0.5, 1.5, 0.3), 10^log_Omega_mean_list[1:4],
  y1 = c(10^log_Omega_cimean_list[1:4, 1], 10^log_Omega_cimean_list[1:4, 2]),
  length = se_len / 2, angle = 90, col = "red"
)
lines(seq(0.5, 1.5, 0.2), rep(mean(10^log_Omega_mean_list[1:4]), 6), col = "red", lty = 1, lwd = 2)

# Environment 2
points(3, 10^log_Omega_mean_list[5], pch = 22, col = "red", cex = 1.)
arrows(3, 10^log_Omega_mean_list[5],
  y1 = c(10^log_Omega_cimean_list[5, 1], 10^log_Omega_cimean_list[5, 2]),
  length = se_len / 2, angle = 90, col = "red"
)
lines(seq(2.5, 3.5, 0.2), rep(10^log_Omega_mean_list[5], 6), col = "red", lty = 1, lwd = 2)

# Environment 3
points(c(4.8, 5.2), 10^log_Omega_mean_list[8:9], pch = 22, col = "red", cex = 1.)
arrows(c(4.8, 5.2), 10^log_Omega_mean_list[8:9],
  y1 = c(10^log_Omega_cimean_list[8:9, 1], 10^log_Omega_cimean_list[8:9, 2]),
  length = se_len / 2, angle = 90, col = "red"
)
lines(seq(4.5, 5.5, 0.2), rep(mean(10^log_Omega_mean_list[8:9]), 6), col = "red", lty = 1, lwd = 2)

# Environment 4
points(c(6.8, 7.2), 10^log_Omega_mean_list[6:7], pch = 22, col = "red", cex = 1.)
lines(seq(6.5, 7.5, 0.2), rep(mean(10^log_Omega_mean_list[6:7]), 6), col = "red", lty = 1, lwd = 2)
arrows(c(6.8, 7.2), 10^log_Omega_mean_list[6:7],
  y1 = c(10^log_Omega_cimean_list[6:7, 1], 10^log_Omega_cimean_list[6:7, 2]),
  length = se_len / 2, angle = 90, col = "red"
)
mtext("Niche difference", 2, line = 3, cex = 1)
mtext(TeX(r"($(\Omega)$)"), 2, line = 2.8, cex = 1, at = 0.32)
mtext("Low \n nutrition", 1, line = 2, cex = 0.9, at = 1)
mtext("High \n outflux", 1, line = 2, cex = 0.9, at = 3)
mtext("Forced \n nutrition", 1, line = 2, cex = 0.9, at = 5)
mtext("High \n nutrition", 1, line = 2, cex = 0.9, at = 7)
xtick <- c(1, 3, 5, 7)
axis(side = 1, at = xtick, labels = FALSE)



# Eta
margin <- c(0.5, 0.7, 0.2, 0.1) # top, left, bottom, right
par(mai = margin)
plot(NA,
  xlim = c(0.1, 12), ylim = c(0., 1.6), frame = frame_include,
  xlab = "", ylab = "", xaxt = "n", cex.axis = 1.5
)
mtext("E", 3, line = 1, cex = 1, font = 2, at = -1.4)
# Environment 1
points(seq(0.4, 1.4, 0.3), eta1_mean_list[1:4], pch = 24, col = "seagreen", cex = 1.)
lines(seq(0.4, 1.4), rep(mean(eta1_mean_list[1:4]), 2), col = "seagreen", lty = 1, lwd = 2)
points(seq(1.7, 2.7, 0.3), eta2_mean_list[1:4], pch = 25, col = "hotpink", cex = 1.)
lines(seq(1.7, 2.7), rep(mean(eta2_mean_list[1:4]), 2), col = "hotpink", lty = 1, lwd = 2)
arrows(seq(0.4, 1.4, 0.3), eta1_mean_list[1:4],
  y1 = c(eta1_cimean_list[1:4, 1], eta1_cimean_list[1:4, 2]),
  length = se_len / 2, angle = 90, col = "seagreen"
)
arrows(seq(1.7, 2.7, 0.3), eta2_mean_list[1:4],
  y1 = c(eta2_cimean_list[1:4, 1], eta2_cimean_list[1:4, 2]),
  length = se_len / 2, angle = 90, col = "hotpink"
)

# Environment 2
points(4, eta1_mean_list[5], pch = 24, col = "seagreen", cex = 1.)
lines(seq(3.5, 4.5), rep(eta1_mean_list[5], 2), col = "seagreen", lty = 1, lwd = 2)
arrows(4, eta1_mean_list[5],
  y1 = c(eta1_cimean_list[5, 1], eta1_cimean_list[5, 2]),
  length = se_len / 2, angle = 90, col = "seagreen"
)
points(5, eta2_mean_list[5], pch = 25, col = "hotpink", cex = 1.)
arrows(5, eta2_mean_list[5],
  y1 = c(eta2_cimean_list[5, 1], eta2_cimean_list[5, 2]),
  length = se_len / 2, angle = 90, col = "hotpink"
)
lines(seq(4.5, 5.5), rep(eta2_mean_list[5], 2), col = "hotpink", lty = 1, lwd = 2)

# Environment 3
points(c(6.9, 7.1), eta1_mean_list[8:9], pch = 24, col = "seagreen", cex = 1.)
lines(seq(6.5, 7.5), rep(mean(eta1_mean_list[8:9]), 2), col = "seagreen", lty = 1, lwd = 2)
arrows(c(6.9, 7.1), eta1_mean_list[8:9],
  y1 = c(eta1_cimean_list[8:9, 1], eta1_cimean_list[8:9, 2]),
  length = se_len / 2, angle = 90, col = "seagreen"
)
points(c(7.9, 8.1), eta2_mean_list[8:9], pch = 25, col = "hotpink", cex = 1.)
lines(seq(7.5, 8.5), rep(mean(eta2_mean_list[8:9]), 2), col = "hotpink", lty = 1, lwd = 2)
arrows(c(7.9, 8.1), eta2_mean_list[8:9],
  y1 = c(eta2_cimean_list[8:9, 1], eta2_cimean_list[8:9, 2]),
  length = se_len / 2, angle = 90, col = "hotpink"
)
# Environment 4
points(c(9.9, 10.1), eta1_mean_list[6:7], pch = 24, col = "seagreen", cex = 1.)
lines(seq(9.5, 10.5), rep(mean(eta1_mean_list[6:7]), 2), col = "seagreen", lty = 1, lwd = 2)
arrows(c(9.9, 10.1), eta1_mean_list[6:7],
  y1 = c(eta1_cimean_list[6:7, 1], eta1_cimean_list[6:7, 2]),
  length = se_len / 2, angle = 90, col = "seagreen"
)
points(c(10.9, 11.1), eta2_mean_list[6:7], pch = 25, col = "hotpink", cex = 1.)
lines(seq(10.5, 11.5), rep(mean(eta2_mean_list[6:7]), 2), col = "hotpink", lty = 1, lwd = 2)
arrows(c(10.9, 11.1), eta2_mean_list[6:7],
  y1 = c(eta2_cimean_list[6:7, 1], eta2_cimean_list[6:7, 2]),
  length = se_len / 2, angle = 90, col = "hotpink"
)
xtick <- c(1.5, 4.5, 7.5, 10.5)
axis(side = 1, at = xtick, labels = FALSE)

mtext("Resistance angles", 2, line = 2.5, cex = 1)
mtext(TeX(r"($(\eta)$)"), 2, line = 2.3, cex = 1, at = 1.25)
mtext("Low nutrition", 1, line = 1.5, cex = 1, at = 1.5)
mtext("High outflux", 1, line = 1.5, cex = 1, at = 4.5)
mtext("Forced nutrition", 1, line = 1.5, cex = 1, at = 7.5)
mtext("High nutrition", 1, line = 1.5, cex = 1, at = 10.5)


legend(
  "bottomright",
  bty = "n",
  col = c("seagreen", "hotpink"),
  legend = c(TeX(r"($\eta_{algae}$)"), TeX(r"($\eta_{rotifer}$)")),
  pch = 19, cex = 1.5
)

dev.off()






























# =========================
# PLOT SUPPLEMENTARY FIGURE
# =========================
supp_res <- 200


# Figure infering parameters
filename <- "suppfig_empirical_chemostat_inference"
png(filename = paste(filename, ".png", sep = ""), width = 8, height = 10,
    units = "in", res = supp_res)
layout(matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2))

expcol_list <- hcl.colors(9, palette = "Dynamic")
label_size <- 1.2


# Panel A
miny <- min(unlist(r_hat_list))
maxy <- max(unlist(r_hat_list))
margin <- c(0.4, 0.7, 0.5, 0.1)
par(mai = margin)
plot(NA,
  xlim = c(1, 140), ylim = c(miny, maxy + 0.5), xlab = "", ylab = "", 
  cex.axis = label_size, frame = frame_include
)
mtext("Intrinsic growth rate (r)", 2, line = 2.5, cex = label_size)
mtext("Intrinsic growth rate of algae", 3, line = 1, cex = label_size, font = 2)
mtext("A", 3, line = 1, font = 2, cex = label_size, at = -20)
for (i in 1:9) {
  Tmax <- dim(r_hat_list[[i]])[1]
  lines(r_hat_list[[i]][, 1], col = expcol_list[[i]], lwd = 2)
  polygon(c(seq(1, Tmax), rev(seq(1, Tmax))),
    c(
      r_hat_list[[i]][, 1] + 1.96 * r_se_list[[i]][, 1],
      rev(r_hat_list[[i]][, 1] - 1.96 * r_se_list[[i]][, 1])
    ),
    col = adjustcolor(expcol_list[[i]], alpha.f = 0.2), border = F
  )
}
legend("bottomright", legend = c(
  "Exp C1", "Exp C2", "Exp C3", "Exp C4",
  "Exp C5", "Exp C6", "Exp C7", "Exp C8", "Exp C9"
), col = expcol_list, ncol = 5, lty = 1, bty = "n", cex = 1.2)



# Panel B
par(mai = margin)
plot(NA,
  xlim = c(1, 140), ylim = c(-3, 0), xlab = "", ylab = "", 
  cex.axis = label_size, frame = frame_include
)
for (i in 1:9) {
  Tmax <- dim(r_hat_list[[i]])[1]
  lines(alpha_hat_list[[i]][, 1, 1], col = expcol_list[[i]], lwd = 2)
  polygon(c(seq(1, Tmax), rev(seq(1, Tmax))),
    c(
      alpha_hat_list[[i]][, 1, 1] + 1.96 * alpha_se_list[[i]][, 1, 1],
      rev(alpha_hat_list[[i]][, 1, 1] - 1.96 * alpha_se_list[[i]][, 1, 1])
    ),
    col = adjustcolor(expcol_list[[i]], alpha.f = 0.2), border = F
  )
}
mtext("Per capita interaction", 2, line = 2.5, cex = label_size)
mtext("Intraspecific interaction of algae", 3, line = 1, cex = label_size, font = 2)
mtext("B", 3, line = 1, cex = 1.5, font = 2, at = -20)



# Panel C
par(mai = c(0.6, 0.7, 0.5, 0.1))
plot(NA,
  xlim = c(1, 140), ylim = c(-0.06, 0.01), xlab = "", ylab = "", 
  cex.axis = label_size, frame = frame_include
)
for (i in 1:9) {
  Tmax <- dim(r_hat_list[[i]])[1]
  lines(alpha_hat_list[[i]][, 1, 2], col = expcol_list[[i]], lwd = 2)
  polygon(c(seq(1, Tmax), rev(seq(1, Tmax))),
    c(
      alpha_hat_list[[i]][, 1, 2] + 1.96 * alpha_se_list[[i]][, 1, 2],
      rev(alpha_hat_list[[i]][, 1, 2] - 1.96 * alpha_se_list[[i]][, 1, 2])
    ),
    col = adjustcolor(expcol_list[[i]], alpha.f = 0.2), border = F
  )
}
mtext("Day", 1, line = 3, cex = label_size)
mtext("Per capita interaction", 2, line = 2.5, cex = label_size)
mtext("Death of algae by predation", 3, line = 1, cex = label_size, font = 2)
mtext("C", 3, line = 1, cex = label_size, at = -20, font = 2)



# Panel D
margin <- c(0.4, 0.7, 0.5, 0.1)

par(mai = margin)
plot(NA,
  xlim = c(1, 140), ylim = c(-0.7, 0.7),
  xlab = "", ylab = "", cex.axis = label_size, frame = frame_include
)
for (i in 1:9) {
  Tmax <- dim(r_hat_list[[i]])[1]
  lines(r_hat_list[[i]][, 2], col = expcol_list[[i]], lwd = 2)
  polygon(c(seq(1, Tmax), rev(seq(1, Tmax))),
    c(
      r_hat_list[[i]][, 2] + 1.96 * r_se_list[[i]][, 2],
      rev(r_hat_list[[i]][, 2] - 1.96 * r_se_list[[i]][, 2])
    ),
    col = adjustcolor(expcol_list[[i]], alpha.f = 0.2), border = F
  )
}
mtext("Intrinsic growth rate (r)", 2, line = 2.5, cex = label_size)
mtext("Intrinsic growth rate of rotifers", 3, line = 1, cex = label_size, font = 2)
mtext("D", 3, line = 1, cex = label_size, at = -20, font = 2)


# Panel E
par(mai = margin)
plot(NA,
  xlim = c(1, 140), ylim = c(-0.07, 0.02), xlab = "", ylab = "", 
  cex.axis = label_size, frame = frame_include
)
for (i in 1:9) {
  Tmax <- dim(r_hat_list[[i]])[1]
  lines(alpha_hat_list[[i]][, 2, 2], col = expcol_list[[i]], lwd = 2)
  polygon(c(seq(1, Tmax), rev(seq(1, Tmax))),
    c(
      alpha_hat_list[[i]][, 2, 2] + 1.96 * alpha_se_list[[i]][, 2, 2],
      rev(alpha_hat_list[[i]][, 2, 2] - 1.96 * alpha_se_list[[i]][, 2, 2])
    ),
    col = adjustcolor(expcol_list[[i]], alpha.f = 0.2), border = F
  )
}
mtext("Per capita interaction", 2, line = 2.5, cex = label_size)
mtext("Intraspecific interaction of rotifers", 3, line = 1, cex = label_size, font = 2)
mtext("E", 3, line = 1, cex = label_size, at = -20, font = 2)


# Panel F
par(mai = c(0.6, 0.6, 0.5, 0.1))
plot(NA,
  xlim = c(1, 140), ylim = c(-0.1, 1.5), xlab = "", ylab = "", 
  cex.axis = label_size, frame = frame_include
)
for (i in 1:9) {
  Tmax <- dim(r_hat_list[[i]])[1]
  lines(alpha_hat_list[[i]][, 2, 1], col = expcol_list[[i]], lwd = 2)
  polygon(c(seq(1, Tmax), rev(seq(1, Tmax))),
    c(
      alpha_hat_list[[i]][, 2, 1] + 1.96 * alpha_se_list[[i]][, 2, 1],
      rev(alpha_hat_list[[i]][, 2, 1] - 1.96 * alpha_se_list[[i]][, 2, 1])
    ),
    col = adjustcolor(expcol_list[[i]], alpha.f = 0.2), border = F
  )
}
mtext("Per capita interaction", 2, line = 2.5, cex = label_size)
mtext("Day", 1, line = 3, cex = label_size)
mtext("Predation rate of rotifers", 3, line = 1, cex = label_size, font = 2)
mtext("F", 3, line = 1, cex = label_size, at = -20, font = 2)

dev.off()





# Cross validation

filename <- "suppfig_empirical_chemostat_cv"
panel_label <- c("A", "B", "C", "D", "E", "F", "G", "H", "I")
png(filename = paste(filename, ".png", sep = ""), width = 8, height = 8,
    units = "in", res = supp_res)
layout(matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), ncol = 3, nrow = 3))
ylim_list = list(c(11, 13), c(8, 25), c(8, 11), 
                 c(9, 50), c(6, 10), c(0, 250), 
                 c(20, 140), c(7, 11), c(10, 13))
for (i in 1:9) {
  par(mai = c(0.5, 0.7, 0.5, 0.1))
  plot(cv_list[[i]]$theta_v, cv_list[[i]]$RMSE, xlab = "", ylab = "", 
       cex.axis = 1.5, frame = frame_include, ylim = ylim_list[[i]])
  mtext("RMSE", 2, line = 3, cex = label_size)
  mtext(TeX(r"($\theta_s$)"), 1, line = 3, cex = label_size)
  mtext(paste("Exp C_", i, sep = ""),
    cex = label_size, font = 2, line = 1
  )
  mtext(panel_label[i], cex = label_size, font = 2, at = -0.5, line = 1)
  lines(cv_list[[i]]$theta_v, cv_list[[i]]$RMSE, col = "red")
  lines(c(cv_list[[i]]$theta_o, cv_list[[i]]$theta_o),
    c(min(cv_list[[i]]$RMSE), max(cv_list[[i]]$RMSE)),
    col = "blue", lty = 2
  )
}
dev.off()




# Population dynamics
filename <- "suppfig_empirical_chemostat_population"
png(filename = paste(filename, ".png", sep = ""), width = 10, 
    height = 12, units = "in", res = supp_res)
layout(matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow = 9, ncol = 1))
margin <- c(0.1, 0.9, 0.05, 0.1)
for (i in 1:8) {
  miny <- min(N_list[[i]])
  maxy <- max(N_list[[i]])
  par(mai = margin)
  Tmax <- dim(N_list[[i]])[1]
  plot(NA,
    xlim = c(1, 140), ylim = c(miny, maxy), xlab = "", ylab = "", cex.axis = label_size, 
    log = "y", xaxt = "n", frame = frame_include
  )
  lines(seq(1, Tmax), N_list[[i]][, 1], col = "green")
  lines(seq(1, Tmax), N_list[[i]][, 2], col = "red")
}
margin <- c(0.5, 0.9, 0.1, 0.1)
par(mai = margin)
plot(NA,
  xlim = c(1, 140), ylim = c(miny, maxy), xlab = "", ylab = "", 
  cex.axis = label_size, log = "y", frame = frame_include
)
mtext("Day", 1, line = 2.5, cex = 1.2)
lines(seq(1, Tmax), N_list[[9]][, 1], col = "green")
lines(seq(1, Tmax), N_list[[9]][, 2], col = "red")

dev.off()
