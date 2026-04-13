igp_model_LBLB <- function (t, state, parms) {##includes PB, LB as predators and as preys
  
  with(as.list(c(state, parms)), {
    ##the i is the same for both predators 
    
    ###predator IGP P functional responses
    Frp <- Cp*S*R/(Hp + (S*R + (1-S)*Nl))  ##the value of phiP changes according to the model
    Fnp <- Cp*(1-S)*Nl/(Hp + (S*R + (1-S)*Nl))  
    ### prey IGP N functional response
    Frn <- Cn*R/(Hn + R)
    
    #these are the equations 
    dPdt <- Ep*((Frp +Fnp)*P)- mup*P
    dNadt <- mn*Nl- mun*Na
    dNldt <- En*(Frn*(Nl+Na))- Fnp*P-mun*Nl-mn*Nl 
    dRdt <- rho*(K-R)- Frn*(Nl+Na) - Frp*P
    
    # z[R, Nl, Na, P] is the order of the vector
    return(list(c(dRdt, dNldt, dNadt,  dPdt)))        
  })
}



igp_model_LBLB_disc <- function(t, state, parms, dt = 0.1) {
  with(as.list(c(state, parms)), {
    
    ### predator IGP P functional responses
    Frp <- Cp * S * R / (Hp + (S * R + (1 - S) * Nl))
    Fnp <- Cp * (1 - S) * Nl / (Hp + (S * R + (1 - S) * Nl))
    
    ### prey IGP N functional response
    Frn <- Cn * R / (Hn + R)
    
    # Calculate rates of change (derivatives)
    dPdt <- Ep * ((Frp + Fnp) * P) - mup * P
    dNadt <- mn * Nl - mun * Na
    dNldt <- En * (Frn * (Nl + Na)) - Fnp * P - mun * Nl - mn * Nl
    dRdt <- rho * (K - R) - Frn * (Nl + Na) - Frp * P
    
    # Euler update for discrete time step
    R_new  <- R  + dRdt * dt
    Nl_new <- Nl + dNldt * dt
    Na_new <- Na + dNadt * dt
    P_new  <- P  + dPdt * dt
    
    # Return new state (order: R, Nl, Na, P)
    return(list(c(R_new, Nl_new, Na_new, P_new)))
  })
}



igp_model_LBLB_disc_stoc <- function(t, state, parms, dt = 0.001) {
  with(as.list(c(state, parms)), {
    
    # Deterministic rates
    Frp <- Cp * S * R / (Hp + (S * R + (1 - S) * Nl))
    Fnp <- Cp * (1 - S) * Nl / (Hp + (S * R + (1 - S) * Nl))
    Frn <- Cn * R / (Hn + R)
    
   ##

    # Birth and death rates (per capita)
    birth_P <- Ep * (Frp + Fnp)
    death_P <- mup
    
    birth_Nl <- En * Frn
    death_Nl <- mun + mn + Fnp * P / (Nl + 1e-10)  # Avoid division by zero
    
    birth_Na <- 0  # Adults don't reproduce directly (assuming)
    death_Na <- mun
    
    birth_R <- rho * K / (R + 1e-10)  # Density-dependent birth
    death_R <- rho + Frn * (Nl + Na) / (R + 1e-10) + Frp * P / (R + 1e-10)
    
    # Demographic stochasticity (Poisson/ Normal approximation)
    # For large populations, Normal approx; for small, use Poisson
    
    dPdt <- (rpois(1, birth_P * P * dt) - rpois(1, death_P * P * dt)) / dt
  dNldt <- (rpois(1, birth_Nl * Nl * dt) - rpois(1, death_Nl * Nl * dt)) / dt

      dNadt <- rpois(1, death_Na * Na * dt) / dt
      dRdt <- (rpois(1, birth_R * R * dt) - rpois(1, death_R * R * dt)) / dt
    
    
    # Euler update
    R_new  <- R  + dRdt * dt
    Nl_new <- Nl + dNldt * dt
    Na_new <- Na + dNadt * dt
    P_new  <- P  + dPdt * dt
    
    # Prevent negatives
    R_new  <- max(R_new, 0)
    Nl_new <- max(Nl_new, 0)
    Na_new <- max(Na_new, 0)
    P_new  <- max(P_new, 0)
    
    return(list(c(R_new, Nl_new, Na_new, P_new)))
  })
}