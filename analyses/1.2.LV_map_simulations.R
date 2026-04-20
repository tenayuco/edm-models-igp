
##------------------------------------------

DF_DISC_LV_USED$time <- NULL
DF_LV_N <-  as.matrix(DF_DISC_LV_USED)


### 2. LV-map
# perform the cross validation in order to estimate the best theta values
tic()
out_cv <- LV_map_state_space_cross_validation(DF_LV_N, theta_v = seq(0, 5, 0.05), p = 0.1)
out_cv_t <- LV_map_time_cross_validation(DF_LV_N, theta_v = seq(0, 5, 0.05), p = 0.1)
toc()

# run the LV-map for the best theta value
tic()
out <- LV_map(DF_LV_N, theta = out_cv$theta_o)
out_t <- LV_map(DF_LV_N, theta = out_cv_t$theta_o, kernel = "time")
toc()

# Calculating Omega and eta
tic()
out_coexistence <- coexistence_metrics_f3(out)
out_coexistence_t <- coexistence_metrics_f3(out_t)
toc()





############ how is th deal with real data
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





#####





##--------------------hasta aqui y vemos..
# ------------------
### Inference of parameters
# ------------------
colist <- c("#377EB8", "#FF7F00", "#4DAF4A")
labelsize <- 1.2
tp <- 25
supp_res <- 200
frame_include <- FALSE
noise <- 0.01
Tmax <- dim(DF_LV_N)[1]

if (noise == 0.02) {
  limcv <- c(0.028, 0.048)
} else if (noise == 0.01) {
  limcv <- c(0.014, 0.020)
} else if (noise == 0.005) {
  limcv <- c(0.0065, 0.009)
}


filename <- paste("suppfig_simulation_fixed_parameters_stable_", noise, sep = "")

png(filename = paste(filename, ".png", sep = ""), width = 10, height = 15, 
    units = "in", res = supp_res)

layout(matrix(c(1, 2, 3, 4, 5, 6, 1, 7, 8, 9, 10, 11), nrow = 6, ncol = 2))

# Panel A
par(mai = c(0.5, 0.6, 0.2, 0.2)) # bottom, left, top, right
plot(NA, ylim = c(0.2, 2), xlim = c(1, Tmax), log = "y", xlab = "", ylab = "", 
     cex.axis = labelsize, frame = frame_include)
for (i in 1:S) {
  lines(N[, i], col = colist[i])
}
mtext("Time", 1, line = 2.5, cex = labelsize)
mtext("Densities", 2, line = 2.5, cex = labelsize)
legend("topright",
       legend = c("sp1", "sp2", "sp3"), lty = 1, col = colist,
       ncol = 3, bty = "n", cex = labelsize
)

# Panel B
par(mai = c(0.4, 0.6, 0.4, 0.)) # bottom, left, top, right
plot(out_cv$theta_v, out_cv$RMSE, ylim=limcv, 
     ylab = "", xlab = "", cex.axis = labelsize, frame = frame_include
)
lines(out_cv$theta_v, out_cv$RMSE, col = "red")
lines(c(out_cv$theta_o, out_cv$theta_o),limcv,
      col = "blue", lty = 2
)
mtext(TeX(r"($\theta$)"), 1, line = 2, cex = labelsize)
mtext("RMSE", 2, line = 2.5, cex = labelsize)
mtext("State-space weighting kernel", 3, line = 1, cex = labelsize, font = 2)


# Panel C
par(mai = c(0.4, 0.6, 0., 0.)) # bottom, left, top, right
plot(NA, ylim = c(0.5, 1.5), xlim = c(-tp, Tmax + tp), ylab = "", xlab = "", 
     cex.axis = labelsize, frame = frame_include)
for (i in 1:S) {
  points(out$r_hat[, i], col = colist[i], cex = labelsize)
  polygon(c(seq(1, Tmax - 1), rev(seq(1, Tmax - 1))),
          c(out$r_hat[, i] + 1.96 * out$r_se[, i], 
            rev(out$r_hat[, i] - 1.96 * out$r_se[, i])),
          col = adjustcolor(colist[i], alpha.f = 0.2), border = F
  )
  lines(c(-tp, Tmax + tp), c(r[i], r[i]), col = colist[i], lty = 1, lwd = 2)
}
mtext("r", 2, line = 2.5, cex = labelsize)

# Panel D
par(mai = c(0.4, 0.6, 0., 0.)) # bottom, left, top, right
plot(NA,
     ylim = c(-1.5, 0), xlim = c(-tp, Tmax + tp),
     ylab = "", xlab = "", main = "", cex.axis = labelsize, frame = frame_include
)
for (i in 1:S) {
  points(out$alpha_hat[, 1, i], col = colist[i])
  polygon(c(seq(1, Tmax - 1), rev(seq(1, Tmax - 1))),
          c(out$alpha_hat[, 1, i] + 1.96 * out$alpha_se[, 1, i], 
            rev(out$alpha_hat[, 1, i] - 1.96 * out$alpha_se[, 1, i])),
          col = adjustcolor(colist[i], alpha.f = 0.2), border = F
  )
  lines(c(-tp, Tmax + tp), c(alpha[1, i], alpha[1, i]), col = colist[i], lty = 2, lwd = 2)
}
mtext(TeX(r"($\alpha$)"), 2, line = 2.5, cex = labelsize)

for (i in 1:S) {
  points(out$alpha_hat[, 2, i], col = colist[i])
  polygon(c(seq(1, Tmax - 1), rev(seq(1, Tmax - 1))),
          c(out$alpha_hat[, 2, i] + 1.96 * out$alpha_se[, 2, i], 
            rev(out$alpha_hat[, 2, i] - 1.96 * out$alpha_se[, 2, i])),
          col = adjustcolor(colist[i], alpha.f = 0.2), border = F
  )
  lines(c(-tp, Tmax + tp), c(alpha[2, i], alpha[2, i]), col = colist[i], lty = 3, lwd = 2)
}

for (i in 1:S) {
  points(out$alpha_hat[, 3, i], col = colist[i])
  polygon(c(seq(1, Tmax - 1), rev(seq(1, Tmax - 1))),
          c(out$alpha_hat[, 3, i] + 1.96 * out$alpha_se[, 3, i], 
            rev(out$alpha_hat[, 3, i] - 1.96 * out$alpha_se[, 3, i])),
          col = adjustcolor(colist[i], alpha.f = 0.2), border = F
  )
  lines(c(-tp, Tmax + tp), c(alpha[3, i], alpha[3, i]), col = colist[i], lty = 4, lwd = 2)
}

# Panel E
par(mai = c(0.4, 0.6, 0., 0.)) # bottom, left, top, right
plot(NA, ylim = c(0.03, 0.06), xlim = c(-tp, Tmax + tp), ylab = "", xlab = "", 
     cex.axis = labelsize, frame = frame_include)
points(10^out_coexistence$log_Omega_hat, col = "red")
polygon(c(seq(1, Tmax - 1), rev(seq(1, Tmax - 1))),
        c(10^out_coexistence$log_Omega_ci[, 1], 
          rev(10^out_coexistence$log_Omega_ci[, 2])),
        col = adjustcolor("red", alpha.f = 0.2), border = F)
lines(c(-tp, Tmax + tp), c(Omega, Omega), col = "red", lty = 1, lwd = 2)
mtext(TeX(r"($\Omega$)"), 2, line = 2.5, cex = labelsize)

# Panel F
par(mai = c(0.5, 0.6, 0., 0.)) # bottom, left, top, right
plot(NA, ylim = c(0.1, 0.5), xlim = c(-tp, Tmax + tp), ylab = "", xlab = "", 
     cex.axis = labelsize, frame = frame_include)
for (i in 1:S){
  points(out_coexistence$eta_hat[, i], col = colist[i]) 
  polygon(c(seq(1, Tmax - 1), rev(seq(1, Tmax - 1))),
          c(out_coexistence$eta_ci[, i, 1], 
            rev(out_coexistence$eta_ci[, i, 2])),
          col = adjustcolor(colist[i], alpha.f = 0.2), border = F)
  lines(c(-tp, Tmax + tp), c(eta[i], eta[i]), col = colist[i], lty = 1, lwd = 2)
}
mtext(TeX(r"($\eta$)"), 2, line = 2.5, cex = labelsize)
mtext("Time", 1, line = 2.5, cex = labelsize)

# Panel G
par(mai = c(0.4, 0.6, 0.4, 0.2)) # bottom, left, top, right
plot(out_cv_t$theta_v, out_cv_t$RMSE, ylim = limcv, 
     ylab = "", xlab = "", cex.axis = labelsize, frame = frame_include)
lines(out_cv_t$theta_v, out_cv_t$RMSE, col = "red")
lines(c(out_cv_t$theta_o, out_cv_t$theta_o),
      limcv, col = "blue", lty = 2
)
mtext(TeX(r"($\theta$)"), 1, line = 2, cex = labelsize)
mtext("Time weighting kernel", 3, line = 1, cex = 1.2, font = 2)

# Panel H
par(mai = c(0.4, 0.4, 0., 0.2)) # bottom, left, top, right
plot(NA, ylim = c(0.5, 1.5), xlim = c(-tp, Tmax + tp), ylab = "", xlab = "", 
     cex.axis = labelsize, frame = frame_include)
for (i in 1:S) {
  points(out_t$r_hat[, i], col = colist[i], cex = labelsize)
  polygon(c(seq(1, Tmax - 1), rev(seq(1, Tmax - 1))),
          c(out_t$r_hat[, i] + 1.96 * out_t$r_se[, i], 
            rev(out_t$r_hat[, i] - 1.96 * out_t$r_se[, i])),
          col = adjustcolor(colist[i], alpha.f = 0.2), border = F)
  lines(c(-tp, Tmax + tp), c(r[i], r[i]), col = colist[i], lty = 1, lwd = 2)
}


# Panel I
par(mai = c(0.4, 0.4, 0., 0.2)) # bottom, left, top, right
plot(NA,
     ylim = c(-1.5, 0), xlim = c(-tp, Tmax + tp),
     ylab = "", xlab = "", main = "", cex.axis = labelsize, frame = frame_include
)
for (i in 1:S) {
  points(out_t$alpha_hat[, 1, i], col = colist[i])
  polygon(c(seq(1, Tmax - 1), rev(seq(1, Tmax - 1))),
          c(out_t$alpha_hat[, 1, i] + 1.96 * out_t$alpha_se[, 1, i], 
            rev(out_t$alpha_hat[, 1, i] - 1.96 * out_t$alpha_se[, 1, i])),
          col = adjustcolor(colist[i], alpha.f = 0.2), border = F
  )
  lines(c(-tp, Tmax + tp), c(alpha[1, i], alpha[1, i]), col = colist[i], lty = 2, lwd = 2)
}

for (i in 1:S) {
  points(out_t$alpha_hat[, 2, i], col = colist[i])
  polygon(c(seq(1, Tmax - 1), rev(seq(1, Tmax - 1))),
          c(out_t$alpha_hat[, 2, i] + 1.96 * out_t$alpha_se[, 2, i], 
            rev(out_t$alpha_hat[, 2, i] - 1.96 * out_t$alpha_se[, 2, i])),
          col = adjustcolor(colist[i], alpha.f = 0.2), border = F
  )
  lines(c(-tp, Tmax + tp), c(alpha[2, i], alpha[2, i]), col = colist[i], lty = 3, lwd = 2)
}

for (i in 1:S) {
  points(out_t$alpha_hat[, 3, i], col = colist[i])
  polygon(c(seq(1, Tmax - 1), rev(seq(1, Tmax - 1))),
          c(out_t$alpha_hat[, 3, i] + 1.96 * out_t$alpha_se[, 3, i], 
            rev(out_t$alpha_hat[, 3, i] - 1.96 * out_t$alpha_se[, 3, i])),
          col = adjustcolor(colist[i], alpha.f = 0.2), border = F)
  lines(c(-tp, Tmax + tp), c(alpha[3, i], alpha[3, i]), col = colist[i], lty = 4, lwd = 2)
}


# Panel J
par(mai = c(0.4, 0.4, 0., 0.2)) # bottom, left, top, right
plot(NA, ylim =  c(0.03, 0.06), xlim = c(-tp, Tmax + tp), ylab = "", xlab = "", 
     cex.axis = labelsize, frame = frame_include)
points(10^out_coexistence_t$log_Omega_hat, col = "red")
polygon(c(seq(1, Tmax - 1), rev(seq(1, Tmax - 1))),
        c(10^out_coexistence_t$log_Omega_ci[, 1], 
          rev(10^out_coexistence_t$log_Omega_ci[, 2])),
        col = adjustcolor("red", alpha.f = 0.2), border = F)
lines(c(-tp, Tmax + tp), c(Omega, Omega), col = "red", lty = 1, lwd = 2)

# Panel K
par(mai = c(0.5, 0.6, 0., 0.2)) # bottom, left, top, right
plot(NA, ylim = c(0.1, 0.5), xlim = c(-tp, Tmax + tp), ylab = "", xlab = "", 
     cex.axis = labelsize, frame = frame_include)
for (i in 1:S){
  points(out_coexistence_t$eta_hat[, i], col = colist[i]) 
  polygon(c(seq(1, Tmax - 1), rev(seq(1, Tmax - 1))),
          c(out_coexistence_t$eta_ci[, i, 1], 
            rev(out_coexistence_t$eta_ci[, i, 2])),
          col = adjustcolor(colist[i], alpha.f = 0.2), border = F)
  
  lines(c(-tp, Tmax + tp), c(eta[i], eta[i]), col = colist[i], lty = 1, lwd = 2)
}
mtext("Time", 1, line = 2.5, cex = labelsize)

dev.off()





filename <- paste("suppfig_simulation_fixed_parameters_stable_addition_", noise, sep = "")

png(filename = paste(filename, ".png", sep = ""), width = 8, height = 4, 
    units = "in", res = supp_res)

layout(matrix(c(1, 2), nrow = 1, ncol = 2))

point_size <- 1

par(mai = c(0.6, 0.6, 0.5, 0.2)) # bottom, left, top, right
plot(seq(-1.5, 2.5, 1), seq(-1.5, 2.5, 1),
     type = "l", col = "gray", lty = 2, xlim = c(-1.5, 2), ylim = c(-1.5, 2),
     xlab = "", ylab = "", cex.axis = labelsize, frame = frame_include
)
colist2 <- c("indianred", "aquamarine4", "red", "darkorchid4")
for (i in 1:3) {
  points(r_t[, i], out$r_hat[, i], col = colist2[1], cex = point_size, pch = 21)
  arrows(c(r_t[, i], r_t[, i]), c(out$r_hat[, i], out$r_hat[, i]), 
         y1 = c(out$r_hat[, i] + 1.96 * out$r_se[, i], out$r_hat[, i] - 1.96 * out$r_se[, i]),
         length=0.05, angle=90, col = colist2[1])
  points(alpha_t[, 1, i], out$alpha_hat[, 1, i], col = colist2[2], pch = 22, cex = point_size)
  arrows(c(alpha_t[, 1, i], alpha_t[, 1, i]), c(out$alpha_hat[, 1, i], out$alpha_hat[, 1, i]),
         y1 = c(out$alpha_hat[, 1, i] + 1.96 * out$alpha_se[, 1, i], 
                out$alpha_hat[, 1, i] - 1.96 * out$alpha_se[, 1, i]),
         length=0.05, angle=90, col = colist2[2])
  points(alpha_t[, 2, i], out$alpha_hat[, 2, i], col = colist2[2], pch = 22, cex = point_size)
  arrows(c(alpha_t[, 2, i], alpha_t[, 2, i]), c(out$alpha_hat[, 2, i], out$alpha_hat[, 2, i]),
         y1 = c(out$alpha_hat[, 2, i] + 1.96 * out$alpha_se[, 2, i], 
                out$alpha_hat[, 2, i] - 1.96 * out$alpha_se[, 2, i]),
         length=0.05, angle=90, col = colist2[2])
  points(alpha_t[, 3, i], out$alpha_hat[, 3, i], col = colist2[2], pch = 22, cex = point_size)
  arrows(c(alpha_t[, 3, i], alpha_t[, 3, i]), c(out$alpha_hat[, 3, i], out$alpha_hat[, 3, i]),
         y1 = c(out$alpha_hat[, 3, i] + 1.96 * out$alpha_se[, 3, i], 
                out$alpha_hat[, 3, i] - 1.96 * out$alpha_se[, 3, i]),
         length=0.05, angle=90, col = colist2[2])
}
points(rep(10^Omega, Tmax - 1), 10^out_coexistence$log_Omega_hat, pch = 23, 
       cex = point_size, col = colist2[3])
arrows(10^Omega, 10^out_coexistence$log_Omega_hat, 
       y1=c(10^out_coexistence$log_Omega_ci[, 1], 10^out_coexistence$log_Omega_ci[, 2]),
       length=0.05, angle=90, col = colist2[3])
points(rep(eta[1], Tmax - 1), out_coexistence$eta_hat[, 1], pch = 24, cex = point_size, 
       col = colist2[4])
arrows(eta[1], out_coexistence$eta_hat[, 1], 
       y1=c(out_coexistence$eta_ci[, 1, 1], out_coexistence$eta_ci[, 1, 2]),
       length=0.05, angle=90, col = colist2[4])
points(rep(eta[2], Tmax - 1), out_coexistence$eta_hat[, 2], pch = 24, 
       cex = point_size, col = colist2[4])
arrows(eta[2], out_coexistence$eta_hat[, 2], 
       y1=c(out_coexistence$eta_ci[, 2, 1], out_coexistence$eta_ci[, 2, 2]),
       length=0.05, angle=90, col = colist2[4])
points(rep(eta[3], Tmax - 1), out_coexistence$eta_hat[, 3], pch = 24, 
       cex = point_size, col = colist2[4])
arrows(eta[3], out_coexistence$eta_hat[, 3], 
       y1=c(out_coexistence$eta_ci[, 3, 1], out_coexistence$eta_ci[, 3, 2]),
       length=0.05, angle=90, col = colist2[4])
mtext("True values", 1, line = 2, cex = labelsize)
mtext("Inferred values", 2, line = 2, cex = labelsize)
mtext("State-space weighting kernel", 3, line = 1, cex = labelsize, font = 2)
legend(
  x = 0., y = -0.2, legend = c("", "", "", ""), pch = c(21, 22, 23, 24), bty = "n", 
  cex = labelsize/1.5, col = colist2
)

par(mai = c(0.6, 0.6, 0.5, 0.2)) # bottom, left, top, right
plot(seq(-1.5, 2.5, 1), seq(-1.5, 2.5, 1),
     type = "l", col = "gray", lty = 2, xlim = c(-1.5, 2), ylim = c(-1.5, 2),
     xlab = "", ylab = "", cex.axis = labelsize, frame = frame_include
)
colist2 <- c("indianred", "aquamarine4", "red", "darkorchid4")
for (i in 1:3) {
  points(r_t[, i], out_t$r_hat[, i], col = colist2[1], cex = point_size, pch = 21)
  arrows(c(r_t[, i], r_t[, i]), c(out_t$r_hat[, i], out_t$r_hat[, i]), 
         y1 = c(out_t$r_hat[, i] + 1.96 * out_t$r_se[, i], 
                out_t$r_hat[, i] - 1.96 * out_t$r_se[, i]),
         length=0.05, angle=90, col = colist2[1])
  points(alpha_t[, 1, i], out_t$alpha_hat[, 1, i], col = colist2[2],
         pch = 22, cex = point_size)
  arrows(c(alpha_t[, 1, i], alpha_t[, 1, i]), 
         c(out_t$alpha_hat[, 1, i], out_t$alpha_hat[, 1, i]),
         y1 = c(out_t$alpha_hat[, 1, i] + 1.96 * out_t$alpha_se[, 1, i], 
                out_t$alpha_hat[, 1, i] - 1.96 * out_t$alpha_se[, 1, i]),
         length=0.05, angle=90, col = colist2[2])
  points(alpha_t[, 2, i], out_t$alpha_hat[, 2, i], col = colist2[2], 
         pch = 22, cex = point_size)
  arrows(c(alpha_t[, 2, i], alpha_t[, 2, i]), 
         c(out_t$alpha_hat[, 2, i], out_t$alpha_hat[, 2, i]),
         y1 = c(out_t$alpha_hat[, 2, i] + 1.96 * out_t$alpha_se[, 2, i], 
                out_t$alpha_hat[, 2, i] - 1.96 * out_t$alpha_se[, 2, i]),
         length=0.05, angle=90, col = colist2[2])
  points(alpha_t[, 3, i], out_t$alpha_hat[, 3, i], 
         col = colist2[2], pch = 22, cex = point_size)
  arrows(c(alpha_t[, 3, i], alpha_t[, 3, i]), 
         c(out_t$alpha_hat[, 3, i], out_t$alpha_hat[, 3, i]),
         y1 = c(out_t$alpha_hat[, 3, i] + 1.96 * out_t$alpha_se[, 3, i], 
                out_t$alpha_hat[, 3, i] - 1.96 * out_t$alpha_se[, 3, i]),
         length=0.05, angle=90, col = colist2[2])
}
points(rep(10^Omega, Tmax - 1), 10^out_coexistence_t$log_Omega_hat, pch = 23, 
       cex = point_size, col = colist2[3])
arrows(10^Omega, 10^out_coexistence_t$log_Omega_hat, 
       y1=c(10^out_coexistence_t$log_Omega_ci[, 1], 
            10^out_coexistence_t$log_Omega_ci[, 2]),
       length=0.05, angle=90, col = colist2[3])
points(rep(eta[1], Tmax - 1), out_coexistence_t$eta_hat[, 1], pch = 24, cex = point_size, 
       col = colist2[4])
arrows(eta[1], out_coexistence_t$eta_hat[, 1], 
       y1=c(out_coexistence_t$eta_ci[, 1, 1], out_coexistence_t$eta_ci[, 1, 2]),
       length=0.05, angle=90, col = colist2[4])
points(rep(eta[2], Tmax - 1), out_coexistence_t$eta_hat[, 2], pch = 24, 
       cex = point_size, col = colist2[4])
arrows(eta[2], out_coexistence_t$eta_hat[, 2], 
       y1=c(out_coexistence_t$eta_ci[, 2, 1], out_coexistence_t$eta_ci[, 2, 2]),
       length=0.05, angle=90, col = colist2[4])
points(rep(eta[3], Tmax - 1), out_coexistence_t$eta_hat[, 3], pch = 24, 
       cex = point_size, col = colist2[4])
arrows(eta[3], out_coexistence_t$eta_hat[, 3], 
       y1=c(out_coexistence_t$eta_ci[, 3, 1], out_coexistence_t$eta_ci[, 3, 2]),
       length=0.05, angle=90, col = colist2[4])
mtext("True values", 1, line = 2, cex = labelsize)
mtext("Time weighting kernel", 3, line = 1, cex = labelsize, font = 2)


dev.off()

