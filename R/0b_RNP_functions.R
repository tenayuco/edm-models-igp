

##-------------LV model (simpler)--------------------------------



LBLB_LV_cont_model <- function (t, state, parms) {##includes PB, LB as predators and as preys
  
  with(as.list(c(state, parms)), {
    ##the i is the same for both predators 
  
    ###predator IGP P functional responses
    ### prey IGP N functional response
    
    #these are the equations 
    dPdt <- Ep*((frp*S*R + fnp*N*(1-S))*P)- mup*P
    dNdt <- En*(frn*R*N)- fnp*P*N-mun*N 
    dRdt <- rho*(K-R)- (frn*N- frp*P) * R
    
    # z[R, Nl, Na, P] is the order of the vector
    return(list(c(dRdt, dNdt,  dPdt)))        
  })
}



LBLB_LV_disc_model <- function(t, state, parms, dt = 0.1) {
  with(as.list(c(state, parms)), {
    

    #these are the equations 
    dPdt <- Ep*((frp*S*R + fnp*N*(1-S))*P)- mup*P
    dNdt <- En*(frn*R*N)- fnp*P*N-mun*N 
    dRdt <- rho*(K-R)- (frn*N- frp*P) * R
    
    
    # Euler update for discrete time step
    R_new  <- R  + dRdt * dt
    N_new <- N + dNdt * dt
    P_new  <- P  + dPdt * dt
    
    # Return new state (order: R, Nl, Na, P)
    return(list(c(R_new, N_new, P_new)))
  })
}



## stochastic LV 


LBLB_LV_disc_stoc_model <- function(t, state, parms, dt = 0.1) {
  with(as.list(c(state, parms)), {
    
    rho_s <- rho*exp(rnorm(n=1, sd=0.001))
    mun_s <- mun*exp(rnorm(n=1, sd=0.001))
    mup_s <- mup*exp(rnorm(n=1, sd=0.001))


    #these are the equations 
    dPdt <- Ep*((frp*S*R + fnp*N*(1-S))*P)- mup_s*P
    dNdt <- En*(frn*R*N)- fnp*P*N-mun_s*N 
    dRdt <- rho_s*(K-R)- (frn*N- frp*P) * R
    
    
    # Euler update for discrete time step
    R_new  <- R  + dRdt * dt
    N_new <- N + dNdt * dt
    P_new  <- P  + dPdt * dt
    
    # Return new state (order: R, Nl, Na, P)
    return(list(c(R_new, N_new, P_new)))
  })
}















###OLD ----------------------- (complete age structure)

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
    

    ### linear functional responses 
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




igp_model_LBLB_LV_stoc <- function (t, state, parms) {##includes PB, LB as predators and as preys
  
  with(as.list(c(state, parms)), {
    ##the i is the same for both predators 
  
    ###predator IGP P functional responses
    ### prey IGP N functional response
    
    #these are the equations 
    dPdt <- Ep*((frp*S*R + fnp*N*(1-S))*P)- mup*P
    dNdt <- En*(frn*R*N)- fnp*P*N-mun*N 
    dRdt <- rho*(K-R)- (frn*N- frp*P) * R
    
    # z[R, Nl, Na, P] is the order of the vector
    return(list(c(dRdt, dNdt,  dPdt)))        
  })
}
