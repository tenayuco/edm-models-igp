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
