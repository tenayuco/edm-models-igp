require("scatterplot3d")
require("latex2exp")

plot_feasibility_domain_3D_positive <- function(alpha, r) {
  x_max <- 1
  y_max <- 1
  z_max <- 1
  x_min <- 0
  y_min <- 0
  z_min <- 0
  v1 <- -alpha[, 2]
  v1 <- v1 / sqrt(sum(v1^2))
  v2 <- -alpha[, 1]
  v2 <- v2 / sqrt(sum(v2^2))
  v3 <- -alpha[, 3]
  v3 <- v3 / sqrt(sum(v3^2))
  vr <- r / sqrt(sum(r^2))
  s3d <- scatterplot3d(NA, NA, NA,
    xlim = c(x_min, x_max), ylim = c(-y_max, -y_min), zlim = c(z_min, z_max),
    box = F, angle = 30, axis = F, grid = F
  )
  s3d$points3d(c(0, 0), c(0, 0), c(z_min, z_max), type = "l", col = "black", lwd = 1)
  s3d$points3d(c(0, 0), c(-y_max, -y_min), c(0, 0), type = "l", col = "black", lwd = 1)
  s3d$points3d(c(x_min, x_max), c(0, 0), c(0, 0), type = "l", col = "black", lwd = 1)
  s3d$points3d(v1[1] * c(0, 1), -v1[2] * c(0, 1), v1[3] * c(0, 1), type = "l", col = "mediumseagreen", lwd = 2)
  s3d$points3d(v1[1] * c(1, 10), -v1[2] * c(1, 10), v1[3] * c(1, 10), type = "l", col = "mediumseagreen", lwd = 2, lty = 2)
  s3d$points3d(v2[1] * c(0, 1), -v2[2] * c(0, 1), v2[3] * c(0, 1), type = "l", col = "mediumseagreen", lwd = 2)
  s3d$points3d(v2[1] * c(1, 10), -v2[2] * c(1, 10), v2[3] * c(1, 10), type = "l", col = "mediumseagreen", lwd = 2, lty = 2)
  s3d$points3d(v3[1] * c(0, 1), -v3[2] * c(0, 1), v3[3] * c(0, 1), type = "l", col = "mediumseagreen", lwd = 2)
  s3d$points3d(v3[1] * c(1, 10), -v3[2] * c(1, 10), v3[3] * c(1, 10), type = "l", col = "mediumseagreen", lwd = 2, lty = 2)
  s3d$points3d(vr[1] * c(0, 1), -vr[2] * c(0, 1), vr[3] * c(0, 1), type = "l", col = "tan2", lwd = 2)
  s3d$points3d(vr[1] * c(1, 10), -vr[2] * c(1, 10), vr[3] * c(1, 10), type = "l", col = "tan2", lwd = 2, lty = 2)
  f <- seq(0, 1, 0.01)
  Omega_c1 <- t(t(v2)) %*% f + t(t(v3)) %*% (1 - f)
  Omega_c1 <- Omega_c1 %*% diag(1 / sqrt(colSums(Omega_c1^2)))
  Omega_c2 <- t(t(v3)) %*% f + t(t(v1)) %*% (1 - f)
  Omega_c2 <- Omega_c2 %*% diag(1 / sqrt(colSums(Omega_c2^2)))
  Omega_c3 <- t(t(v1)) %*% f + t(t(v2)) %*% (1 - f)
  Omega_c3 <- Omega_c3 %*% diag(1 / sqrt(colSums(Omega_c3^2)))
  s3d$points3d(Omega_c1[1, ], -Omega_c1[2, ], Omega_c1[3, ], lty = 1, col = "mediumseagreen", type = "l", lwd = 2)
  s3d$points3d(Omega_c2[1, ], -Omega_c2[2, ], Omega_c2[3, ], lty = 1, col = "mediumseagreen", type = "l", lwd = 2)
  s3d$points3d(Omega_c3[1, ], -Omega_c3[2, ], Omega_c3[3, ], lty = 1, col = "mediumseagreen", type = "l", lwd = 2)

  f1 <- seq(-1 / acos(sum(v2 * v3)) * 2 * pi, 1 / acos(sum(v2 * v3)) * 2 * pi, 0.01)
  f2 <- seq(-1 / acos(sum(v1 * v3)) * 2 * pi, 1 / acos(sum(v1 * v3)) * 2 * pi, 0.01)
  f3 <- seq(-1 / acos(sum(v1 * v2)) * 2 * pi, 1 / acos(sum(v1 * v2)) * 2 * pi, 0.01)
  Omega_c1 <- t(t(v2)) %*% f1 + t(t(v3)) %*% (1 - f1)
  Omega_c1 <- Omega_c1 %*% diag(1 / sqrt(colSums(Omega_c1^2)))
  Omega_c2 <- t(t(v3)) %*% f2 + t(t(v1)) %*% (1 - f2)
  Omega_c2 <- Omega_c2 %*% diag(1 / sqrt(colSums(Omega_c2^2)))
  Omega_c3 <- t(t(v1)) %*% f3 + t(t(v2)) %*% (1 - f3)
  Omega_c3 <- Omega_c3 %*% diag(1 / sqrt(colSums(Omega_c3^2)))
  Omega_c1[Omega_c1 < 0] <- NA
  Omega_c2[Omega_c2 < 0] <- NA
  Omega_c3[Omega_c3 < 0] <- NA
  s3d$points3d(Omega_c1[1, ], -Omega_c1[2, ], Omega_c1[3, ], lty = 3, col = "mediumseagreen", type = "l")
  s3d$points3d(Omega_c2[1, ], -Omega_c2[2, ], Omega_c2[3, ], lty = 3, col = "mediumseagreen", type = "l")
  s3d$points3d(Omega_c3[1, ], -Omega_c3[2, ], Omega_c3[3, ], lty = 3, col = "mediumseagreen", type = "l")

  f <- seq(0, 1, 0.01)
  p <- eta_fn(alpha, r)$p
  vp1 <- p[, 2]
  vp1 <- vp1 / sqrt(sum(vp1^2))
  vp2 <- p[, 1]
  vp2 <- vp2 / sqrt(sum(vp2^2))
  vp3 <- p[, 3]
  vp3 <- vp3 / sqrt(sum(vp3^2))
  eta_c1 <- t(t(vp1)) %*% f + t(t(vr)) %*% (1 - f)
  eta_c1 <- eta_c1 %*% diag(1 / sqrt(colSums(eta_c1^2)))
  eta_c2 <- t(t(vp2)) %*% f + t(t(vr)) %*% (1 - f)
  eta_c2 <- eta_c2 %*% diag(1 / sqrt(colSums(eta_c2^2)))
  eta_c3 <- t(t(vp3)) %*% f + t(t(vr)) %*% (1 - f)
  eta_c3 <- eta_c3 %*% diag(1 / sqrt(colSums(eta_c3^2)))
  s3d$points3d(eta_c1[1, ], -eta_c1[2, ], eta_c1[3, ], lty = 1, col = "gray60", type = "l", lwd = 2)
  s3d$points3d(eta_c2[1, ], -eta_c2[2, ], eta_c2[3, ], lty = 1, col = "gray60", type = "l", lwd = 2)
  s3d$points3d(eta_c3[1, ], -eta_c3[2, ], eta_c3[3, ], lty = 1, col = "gray60", type = "l", lwd = 2)

  text(s3d$xyz.convert(0, -0.95, 0), labels = TeX("$r_1$"), pos = 2, cex = 1.5)
  text(s3d$xyz.convert(0.95, 0, 0), labels = TeX("$r_2$"), pos = 1, cex = 1.5)
  text(s3d$xyz.convert(0, 0, 0.95), labels = TeX("$r_3$"), pos = 2, cex = 1.5)
}


plot_feasibility_domain_3D <- function(alpha, r) {
  x_max <- 1
  y_max <- 1
  z_max <- 1
  x_min <- -1
  y_min <- -1
  z_min <- -1
  v1 <- -alpha[, 2]
  v1 <- v1 / sqrt(sum(v1^2))
  v2 <- -alpha[, 1]
  v2 <- v2 / sqrt(sum(v2^2))
  v3 <- -alpha[, 3]
  v3 <- v3 / sqrt(sum(v3^2))
  vr <- r / sqrt(sum(r^2))
  s3d <- scatterplot3d(NA, NA, NA,
    xlim = c(x_min, x_max), ylim = c(-y_max, -y_min), zlim = c(z_min, z_max),
    box = F, angle = 30, axis = F, grid = F
  )
  s3d$points3d(c(0, 0), c(0, 0), c(z_min, z_max), type = "l", col = "black", lwd = 1)
  s3d$points3d(c(0, 0), c(-y_max, -y_min), c(0, 0), type = "l", col = "black", lwd = 1)
  s3d$points3d(c(x_min, x_max), c(0, 0), c(0, 0), type = "l", col = "black", lwd = 1)
  s3d$points3d(v1[1] * c(0, 1), -v1[2] * c(0, 1), v1[3] * c(0, 1), type = "l", col = "mediumseagreen", lwd = 2)
  s3d$points3d(v1[1] * c(1, 10), -v1[2] * c(1, 10), v1[3] * c(1, 10), type = "l", col = "mediumseagreen", lwd = 2, lty = 2)
  s3d$points3d(v2[1] * c(0, 1), -v2[2] * c(0, 1), v2[3] * c(0, 1), type = "l", col = "mediumseagreen", lwd = 2)
  s3d$points3d(v2[1] * c(1, 10), -v2[2] * c(1, 10), v2[3] * c(1, 10), type = "l", col = "mediumseagreen", lwd = 2, lty = 2)
  s3d$points3d(v3[1] * c(0, 1), -v3[2] * c(0, 1), v3[3] * c(0, 1), type = "l", col = "mediumseagreen", lwd = 2)
  s3d$points3d(v3[1] * c(1, 10), -v3[2] * c(1, 10), v3[3] * c(1, 10), type = "l", col = "mediumseagreen", lwd = 2, lty = 2)
  s3d$points3d(vr[1] * c(0, 1), -vr[2] * c(0, 1), vr[3] * c(0, 1), type = "l", col = "tan2", lwd = 2)
  s3d$points3d(vr[1] * c(1, 10), -vr[2] * c(1, 10), vr[3] * c(1, 10), type = "l", col = "tan2", lwd = 2, lty = 2)
  f <- seq(0, 1, 0.01)
  Omega_c1 <- t(t(v2)) %*% f + t(t(v3)) %*% (1 - f)
  Omega_c1 <- Omega_c1 %*% diag(1 / sqrt(colSums(Omega_c1^2)))
  Omega_c2 <- t(t(v3)) %*% f + t(t(v1)) %*% (1 - f)
  Omega_c2 <- Omega_c2 %*% diag(1 / sqrt(colSums(Omega_c2^2)))
  Omega_c3 <- t(t(v1)) %*% f + t(t(v2)) %*% (1 - f)
  Omega_c3 <- Omega_c3 %*% diag(1 / sqrt(colSums(Omega_c3^2)))
  s3d$points3d(Omega_c1[1, ], -Omega_c1[2, ], Omega_c1[3, ], lty = 1, col = "mediumseagreen", type = "l", lwd = 2)
  s3d$points3d(Omega_c2[1, ], -Omega_c2[2, ], Omega_c2[3, ], lty = 1, col = "mediumseagreen", type = "l", lwd = 2)
  s3d$points3d(Omega_c3[1, ], -Omega_c3[2, ], Omega_c3[3, ], lty = 1, col = "mediumseagreen", type = "l", lwd = 2)

  f1 <- seq(-1 / acos(sum(v2 * v3)) * pi, 1 / acos(sum(v2 * v3)) * pi, 0.01)
  f2 <- seq(-1 / acos(sum(v1 * v3)) * pi, 1 / acos(sum(v1 * v3)) * pi, 0.01)
  f3 <- seq(-1 / acos(sum(v1 * v2)) * pi, 1 / acos(sum(v1 * v2)) * pi, 0.01)
  Omega_c1 <- t(t(v2)) %*% f1 + t(t(v3)) %*% (1 - f1)
  Omega_c1 <- Omega_c1 %*% diag(1 / sqrt(colSums(Omega_c1^2)))
  Omega_c2 <- t(t(v3)) %*% f2 + t(t(v1)) %*% (1 - f2)
  Omega_c2 <- Omega_c2 %*% diag(1 / sqrt(colSums(Omega_c2^2)))
  Omega_c3 <- t(t(v1)) %*% f3 + t(t(v2)) %*% (1 - f3)
  Omega_c3 <- Omega_c3 %*% diag(1 / sqrt(colSums(Omega_c3^2)))
  s3d$points3d(Omega_c1[1, ], -Omega_c1[2, ], Omega_c1[3, ], lty = 3, col = "mediumseagreen", type = "l")
  s3d$points3d(Omega_c2[1, ], -Omega_c2[2, ], Omega_c2[3, ], lty = 3, col = "mediumseagreen", type = "l")
  s3d$points3d(Omega_c3[1, ], -Omega_c3[2, ], Omega_c3[3, ], lty = 3, col = "mediumseagreen", type = "l")

  f <- seq(0, 1, 0.01)
  p <- eta_fn(alpha, r)$p
  vp1 <- p[, 2]
  vp1 <- vp1 / sqrt(sum(vp1^2))
  vp2 <- p[, 1]
  vp2 <- vp2 / sqrt(sum(vp2^2))
  vp3 <- p[, 3]
  vp3 <- vp3 / sqrt(sum(vp3^2))
  eta_c1 <- t(t(vp1)) %*% f + t(t(vr)) %*% (1 - f)
  eta_c1 <- eta_c1 %*% diag(1 / sqrt(colSums(eta_c1^2)))
  eta_c2 <- t(t(vp2)) %*% f + t(t(vr)) %*% (1 - f)
  eta_c2 <- eta_c2 %*% diag(1 / sqrt(colSums(eta_c2^2)))
  eta_c3 <- t(t(vp3)) %*% f + t(t(vr)) %*% (1 - f)
  eta_c3 <- eta_c3 %*% diag(1 / sqrt(colSums(eta_c3^2)))
  s3d$points3d(eta_c1[1, ], -eta_c1[2, ], eta_c1[3, ], lty = 1, col = "gray60", type = "l", lwd = 2)
  s3d$points3d(eta_c2[1, ], -eta_c2[2, ], eta_c2[3, ], lty = 1, col = "gray60", type = "l", lwd = 2)
  s3d$points3d(eta_c3[1, ], -eta_c3[2, ], eta_c3[3, ], lty = 1, col = "gray60", type = "l", lwd = 2)

  text(s3d$xyz.convert(0, -0.95, 0), labels = TeX("$r_1$"), pos = 2, cex = 1.5)
  text(s3d$xyz.convert(0.95, 0, 0), labels = TeX("$r_2$"), pos = 1, cex = 1.5)
  text(s3d$xyz.convert(0, 0, 0.95), labels = TeX("$r_3$"), pos = 2, cex = 1.5)
}

plot_projection_domaine_3D_positive <- function(alpha, r) {
  v1 <- -alpha[, 1]
  v1 <- v1 / sum(v1)
  v2 <- -alpha[, 2]
  v2 <- v2 / sum(v2)
  v3 <- -alpha[, 3]
  v3 <- v3 / sum(v3)
  vr <- r / sum(r)

  Xf <- sqrt(2)
  Yf <- sqrt(6) / 2
  XX <- c(-Xf / 2, Xf / 2, 0, -Xf / 2)
  YY <- c(0, 0, Yf, 0)

  plot(-XX, YY, axes = F, xlab = "", ylab = "", xlim = c(-Xf / 2 - 0.1, Xf / 2 + 0.1), ylim = c(0 - 0.1, Yf + 0.1), col = "gray50", type = "l", lwd = 2)

  v1C <- c((0.5 - 0.5 * v1[3] - v1[1]) * Xf, v1[3] * Yf)
  v2C <- c((0.5 - 0.5 * v2[3] - v2[1]) * Xf, v2[3] * Yf)
  v3C <- c((0.5 - 0.5 * v3[3] - v3[1]) * Xf, v3[3] * Yf)
  vrC <- c((0.5 - 0.5 * vr[3] - vr[1]) * Xf, vr[3] * Yf)

  color <- col2rgb("mediumseagreen")

  polygon(-c(v1C[1], v2C[1], v3C[1]), c(v1C[2], v2C[2], v3C[2]), col = rgb(color[1, 1], color[2, 1], color[3, 1], 90, maxColorValue = 255), border = FALSE)

  points(-c(v1C[1], v2C[1], v3C[1], v1C[1]), c(v1C[2], v2C[2], v3C[2], v1C[2]), col = "mediumseagreen", type = "l", lwd = 2)
  points(-c(v1C[1], v2C[1], v3C[1]), c(v1C[2], v2C[2], v3C[2]), col = "mediumseagreen", pch = 16, cex = 1.5)
  points(-vrC[1], vrC[2], col = "tan2", pch = 16, cex = 1.5)

  text(-XX, YY, labels = c("sp1", "sp2", "sp3"), cex = 1.5, pos = c(1, 1, 3))
}

plot_feasibility_domain_2D <- function(alpha, r) {
  scale <- 10
  v1 <- -alpha[, 1]
  v1 <- v1 / sqrt(sum(v1^2))
  v2 <- -alpha[, 2]
  v2 <- v2 / sqrt(sum(v2^2))
  rv <- r / sqrt(sum(r^2))
  plot(NA, type = "n", axes = FALSE, xlim = c(-1.5, 1.5), ylim = c(-1.5, 1.5), xlab = "", ylab = "")
  text(1.45, -0.1, TeX(r"($r_{algae}$)"), cex = 1.5)
  text(-0.2, 1.45, TeX(r"($r_{rotifer}$)"), cex = 1.5)
  abline(h = 0, v = 0, lty = 1, col = "black")
  lines(c(0, v1[1]) * scale, c(0, v1[2]) * scale, col = "mediumseagreen")
  lines(c(0, v2[1]) * scale, c(0, v2[2]) * scale, col = "mediumseagreen")
  arrows(0, 0, rv[1], rv[2], col = "tan2", length = 0.1)
  f <- seq(0, 1, 0.01)
  Omega <- t(t(v1)) %*% f + t(t(v2)) %*% (1 - f)
  Omega <- Omega %*% diag(1 / sqrt(colSums(Omega^2)))
  text(max(Omega[1, ]) + 0.1, mean(Omega[2, ]),
    TeX(r"($\Omega$)"),
    cex = 1.5
  )
  points(Omega[1, ], Omega[2, ], lty = 1, col = "mediumseagreen", type = "l")
  eta1 <- t(t(v1)) %*% f + t(t(r)) %*% (1 - f)
  eta1 <- eta1 %*% diag(1 / sqrt(colSums(eta1^2)))
  text((max(eta1[1, ]) + 0.1) / 2, mean(eta1[2, ]) / 2,
    TeX(r"($\eta_r$)"),
    cex = 1.5
  )
  points(eta1[1, ] / 2, eta1[2, ] / 2, lty = 1, col = "gray", type = "l")
  eta2 <- t(t(v2)) %*% f + t(t(r)) %*% (1 - f)
  eta2 <- eta2 %*% diag(1 / sqrt(colSums(eta2^2)))
  text((max(eta2[1, ]) + 0.1) / 1.5, mean(eta2[2, ]) / 1.5,
    TeX(r"($\eta_a$)"),
    cex = 1.5
  )
  points(eta2[1, ] / 1.5, eta2[2, ] / 1.5, lty = 1, col = "gray", type = "l")
}
