require("mvtnorm")
require("pracma")
###################
## In dimension 2
## Structural niche difference
## log10 of feasibility probability
log_Omega_f2 <- function(alpha) {
  cos_Omega <- sum(alpha[, 1] * alpha[, 2]) / (sqrt(sum(alpha[, 1]^2)) * sqrt(sum(alpha[, 2]^2)))
  Omega <- acos(cos_Omega)
  log_Omega <- log10(Omega / (2 * pi))
  return(log_Omega)
}


###################
## In dimension 3
## Structural niche difference
## log 10 of feasibility probability
log_Omega_f3 <- function(alpha) {
  a <- alpha[, 1]
  b <- alpha[, 2]
  c <- alpha[, 3]
  a <- a / sqrt(sum(a^2))
  b <- b / sqrt(sum(b^2))
  c <- c / sqrt(sum(c^2))
  r <- abs(det(rbind(a, b, c))) / (1 + sum(a * b) + sum(a * c) + sum(b * c))
  log_Omega <- log10(atan(r) / (2 * pi))
  return(log_Omega)
}


###################
## In dimension n
## Structural niche difference
## log10 of feasibility probability
log_Omega_fn <- function(alpha) {
  n <- nrow(alpha)
  alpha <- alpha_standardization(alpha)
  Sigma <- solve(t(alpha) %*% alpha)
  d <- pmvnorm(lower = rep(0, n), upper = rep(Inf, n), mean = rep(0, n), sigma = Sigma)
  log_Omega <- log10(d[1])
  return(log_Omega)
}


alpha_standardization <- function(alpha) {
  S <- dim(alpha)[1]
  f <- sqrt(colSums(alpha^2))
  alpha_s <- (alpha %*% diag(1 / f)) * S
  return(alpha)
}


###################
## Structural fitness difference
## angle between r and the centroide of the feasibility domain
theta_fn <- function(alpha, r) {
  r_c <- centroid_fn(alpha)
  cos_theta <- sum(r_c * r) / sqrt(sum(r^2) * sum(r_c^2))
  theta <- acos(cos_theta)
  return(theta)
}

centroid_fn <- function(alpha) {
  n <- nrow(alpha)
  D <- diag(1 / sqrt(diag(t(alpha) %*% alpha)))
  alpha_n <- alpha %*% D
  r_c <- rowSums(alpha_n)
  r_c / sqrt((sum(r_c^2)))
  r_c <- t(t(r_c))
  return(r_c)
}


###################
## Resistance
## angle between r and the n borders of the feasibility domain
eta_fn <- function(alpha, r) {
  n <- length(r)
  alpha <- alpha_standardization(alpha)
  r <- r / sqrt(sum(r^2)) * n
  p <- matrix(NA, nrow = n, ncol = n)
  h <- matrix(NA, nrow = n, ncol = n)
  eta <- rep(NA, n)
  for (i in 1:n) {
    A <- -alpha[, -i] # get the matrix without column i
    B <- -alpha[, i] # get column vector i
    C <- cbind(A, B)
    # get the orthonormal basis for the interaction keeping column i as the last orthonormal vector
    Q <- gramSchmidt(C)$Q
    # projection of the growth rate vector on the border for species i
    p_t <- Q[, -n] %*% t(Q[, -n]) %*% r
    # get the orthonormal vector that correspond to the vector of species i
    h_t <- Q[, n]
    # get the direction of the orthornormal vector h_t
    h_t <- h_t * sign(sum(B * h_t))
    cos_eta <- sum(r * p_t) / sqrt(sum(r^2) * sum(p_t^2))
    eta[i] <- sign(sum(r * h_t)) * acos(cos_eta) #* 180 / pi
    p[, i] <- p_t
    h[, i] <- h_t
  }
  out <- list(eta = eta, p = p, h = h)
  return(out)
}


###################
## In dimension 2
## Compute coexistence metrics along time-serie
coexistence_metrics_f2 <- function(out_LV_map, times = NA, Nrand = 1000) {
  Tmax <- dim(out_LV_map$r_hat)[1] + 1
  S <- dim(out_LV_map$r_hat)[2]

  if (is.na(times[1])) {
    times <- seq(1, Tmax - 1)
  }

  log_Omega_hat <- rep(NA, Tmax - 1)
  eta_hat <- array(NA, dim = c(Tmax - 1, S))
  log_Omega_rand <- array(NA, dim = c(Tmax - 1, Nrand))
  eta_rand <- array(NA, dim = c(Tmax - 1, S, Nrand))
  log_Omega_ci <- array(NA, dim = c(Tmax - 1, 2))
  eta_ci <- array(NA, dim = c(Tmax - 1, S, 2))

  for (i in times) {
    log_Omega_hat[i] <- log_Omega_f2(out_LV_map$alpha_hat[i, , ])
    eta_hat[i, ] <- eta_fn(out_LV_map$alpha_hat[i, , ], out_LV_map$r_hat[i, ])$eta
    r_rand <- array(NA, dim = c(Nrand, S))
    alpha_rand <- array(NA, dim = c(Nrand, S, S))

    for (j in 1:S) {
      Sigma <- out_LV_map$Sigma_beta[i, j, , ]
      beta <- c(out_LV_map$r_hat[i, j], out_LV_map$alpha_hat[i, j, ])
      out_rand <- rmvnorm(n = Nrand, mean = beta, sigma = Sigma)
      r_rand[, j] <- out_rand[, 1]
      alpha_rand[, j, ] <- out_rand[, -1]
    }

    for (k in 1:Nrand) {
      r <- r_rand[k, ]
      alpha <- alpha_rand[k, , ]
      log_Omega_rand[i, k] <- log_Omega_f2(alpha)
      eta_rand[i, , k] <- eta_fn(alpha, r)$eta
    }

    log_Omega_ci[i, ] <- quantile(x = log_Omega_rand[i, ], prob = c(0.025, 0.975))

    for (j in 1:S) {
      eta_ci[i, j, ] <- quantile(x = eta_rand[i, j, ], prob = c(0.025, 0.975))
    }
  }

  out <- list(
    log_Omega_hat = log_Omega_hat, eta_hat = eta_hat,
    log_Omega_ci = log_Omega_ci, eta_ci = eta_ci,
    log_Omega_rand = log_Omega_rand, eta_rand = eta_rand
  )
}


###################
## In dimension 3
## Compute coexistence metrics along time-serie
coexistence_metrics_f3 <- function(out_LV_map, times = NA, Nrand = 1000) {
  Tmax <- dim(out_LV_map$r_hat)[1] + 1
  S <- dim(out_LV_map$r_hat)[2]

  if (is.na(times[1])) {
    times <- seq(1, Tmax - 1)
  }

  log_Omega_hat <- rep(NA, Tmax - 1)
  eta_hat <- array(NA, dim = c(Tmax - 1, S))
  log_Omega_rand <- array(NA, dim = c(Tmax - 1, Nrand))
  eta_rand <- array(NA, dim = c(Tmax - 1, S, Nrand))
  log_Omega_ci <- array(NA, dim = c(Tmax - 1, 2))
  eta_ci <- array(NA, dim = c(Tmax - 1, S, 2))

  for (i in times) {
    log_Omega_hat[i] <- log_Omega_f3(out_LV_map$alpha_hat[i, , ])
    eta_hat[i, ] <- eta_fn(out_LV_map$alpha_hat[i, , ], out_LV_map$r_hat[i, ])$eta
    r_rand <- array(NA, dim = c(Nrand, S))
    alpha_rand <- array(NA, dim = c(Nrand, S, S))

    for (j in 1:S) {
      Sigma <- out_LV_map$Sigma_beta[i, j, , ]
      beta <- c(out_LV_map$r_hat[i, j], out_LV_map$alpha_hat[i, j, ])
      out_rand <- rmvnorm(n = Nrand, mean = beta, sigma = Sigma)
      r_rand[, j] <- out_rand[, 1]
      alpha_rand[, j, ] <- out_rand[, -1]
    }
    for (k in 1:Nrand) {
      r <- r_rand[k, ]
      alpha <- alpha_rand[k, , ]
      log_Omega_rand[i, k] <- log_Omega_f3(alpha)
      eta_rand[i, , k] <- eta_fn(alpha, r)$eta
    }
    log_Omega_ci[i, ] <- quantile(x = log_Omega_rand[i, ], prob = c(0.025, 0.975),
                                  na.rm = TRUE)

    for (j in 1:S) {
      eta_ci[i, j, ] <- quantile(x = eta_rand[i, j, ], prob = c(0.025, 0.975),
                                 na.rm = TRUE)
    }
  }

  out <- list(
    log_Omega_hat = log_Omega_hat, eta_hat = eta_hat,
    log_Omega_ci = log_Omega_ci, eta_ci = eta_ci,
    log_Omega_rand = log_Omega_rand, eta_rand = eta_rand
  )
}


###################
## In dimension n
## Compute coexistence metrics along time-serie
coexistence_metrics_fn <- function(out_LV_map, times = NA, Nrand = 1000) {
  Tmax <- dim(out_LV_map$r_hat)[1] + 1
  S <- dim(out_LV_map$r_hat)[2]

  if (is.na(times[1])) {
    times <- seq(1, Tmax - 1)
  }


  log_Omega_hat <- rep(NA, Tmax - 1)
  eta_hat <- array(NA, dim = c(Tmax - 1, S))
  log_Omega_rand <- array(NA, dim = c(Tmax - 1, Nrand))
  eta_rand <- array(NA, dim = c(Tmax - 1, S, Nrand))
  log_Omega_ci <- array(NA, dim = c(Tmax - 1, 2))
  eta_ci <- array(NA, dim = c(Tmax - 1, S, 2))

  for (i in times) {
    print(i)
    log_Omega_hat[i] <- log_Omega_fn(out_LV_map$alpha_hat[i, , ])
    eta_hat[i, ] <- eta_fn(out_LV_map$alpha_hat[i, , ], out_LV_map$r_hat[i, ])$eta
    r_rand <- array(NA, dim = c(Nrand, S))
    alpha_rand <- array(NA, dim = c(Nrand, S, S))

    for (j in 1:S) {
      Sigma <- out_LV_map$Sigma_beta[i, j, , ]
      beta <- c(out_LV_map$r_hat[i, j], out_LV_map$alpha_hat[i, j, ])
      out_rand <- rmvnorm(n = Nrand, mean = beta, sigma = Sigma)
      r_rand[, j] <- out_rand[, 1]
      alpha_rand[, j, ] <- out_rand[, -1]
    }

    for (k in 1:Nrand) {
      r <- r_rand[k, ]
      alpha <- alpha_rand[k, , ]
      err_O <- try(log_Omega_rand[i, k] <- log_Omega_fn(alpha))
      err_E <- try(eta_rand[i, , k] <- eta_fn(alpha, r)$eta)
      if (class(err_O) == "try-error" || class(err_E) == "try_error") {
        break
      }
    }

    try(log_Omega_ci[i, ] <- quantile(x = log_Omega_rand[i, ], prob = c(0.025, 0.975)))
    for (j in 1:S) {
      try(eta_ci[i, j, ] <- quantile(x = eta_rand[i, j, ], prob = c(0.025, 0.975)))
    }
  }

  out <- list(
    log_Omega_hat = log_Omega_hat, eta_hat = eta_hat,
    log_Omega_ci = log_Omega_ci, eta_ci = eta_ci,
    log_Omega_rand = log_Omega_rand, eta_rand = eta_rand
  )
}
