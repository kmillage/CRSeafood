#===================================================================

# Script with our model functions

# Kat Millage
# Last modified 3/14/16

#====================================================================

#===PART 1=======================================
# Fitting a logistic model to the data
#================================================

fit_schaefer <- function(par,dat,use = 'optim'){
  
  r <- exp(par[1])
  K <- exp(par[2])
  q <- exp(par[3])
  sigma <- exp(par[4])
  initialdepletion = 0.2
  
  dat$biomass <- NA
  dat$pred_cpue <- NA
  
  dat$biomass[1] <- K*(1-initialdepletion)
  dat$pred_cpue[1] <- dat$biomass[1]*q
  
  
  for (i in 1:(dim(dat)[1] - 1)) {
    
    dat$biomass[i+1] <- max(0.00001, dat$biomass[i] + dat$biomass[i] * r * (1 - dat$biomass[i]/K) - dat$catch[i])
    
    dat$pred_cpue[i+1] <- max(0.00001, dat$biomass[i+1]*q)
    
  }
  
  if (any(dat$biomass <=0, na.rm = T) == F){
    #   dat$squared_diff <- (log(dat$pred_cpue) - log(dat$CPUE))^2 / (2*dat$CV^2)
    dat$squared_diff <- -dnorm(log(dat$CPUE), log(dat$pred_cpue), sigma, log = T)
    
  }else{ #penalty for negative biomasses
    dat$squared_diff <- min(dat$biomass, na.rm = T)^2
  }
  
  sum_square <- sum(dat$squared_diff, na.rm = T)
  
  if (use == 'optim')
  {
    return(sum_square)
    
  } else{
    return(dat)
  }
}

# Pella-Tomlinson

fit_pella <- function(par,dat,use = 'optim'){
  
  g <- exp(par[1])
  K <- exp(par[2])
  q <- exp(par[3])
  n <- par[4]
  sigma <- exp(par[5])
  initialdepletion = 0.2
  
  dat$biomass <- NA
  dat$pred_cpue <- NA
  
  dat$biomass[1] <- K*(1-initialdepletion)
  dat$pred_cpue[1] <- dat$biomass[1]*q
  
  for (i in 1:(dim(dat)[1] - 1)) {
    
    dat$biomass[i+1] <- max(0.00001, dat$biomass[i] + ((n+1)/n) * g *dat$biomass[i] * (1 - (dat$biomass[i]/K)^n) - dat$catch[i])
    
    dat$pred_cpue[i+1] <- max(0.00001, dat$biomass[i+1]*q)
    
  }
  
  if (any(dat$biomass <=0, na.rm = T) == F){
    #   dat$squared_diff <- (log(dat$pred_cpue) - log(dat$CPUE))^2 / (2*dat$CV^2)
    dat$squared_diff <- -dnorm(log(dat$CPUE), log(dat$pred_cpue), sigma, log = T)
    
  }else{ #penalty for negative biomasses
    dat$squared_diff <- min(dat$biomass, na.rm = T)^2
  }
  
  sum_square <- sum(dat$squared_diff, na.rm = T)
  if (use == 'optim')
  {
    return(sum_square)
    
  } else{
    return(dat)
  }
}

#===PART 2=======================================
# Making projection model that can handle BAU and scenarios
#================================================

scenario_model <- function(par, dat, effort, scenarios, price, delta){
  
  r <- exp(par[1])
  K <- exp(par[2])
  q <- exp(par[3])
  sigma <- exp(par[4])

  scenario <- scenarios[1]
  gamma_start <- scenarios[2]
  alpha <- scenarios[3]
  lamda <- scenarios[4]
  effortreduction <- scenarios[5]
  
  proj.yr.start <- 2013
  proj.yr.end <- 2050
  proj.years <- proj.yr.start:proj.yr.end
  
  phi <- 0.0004
  phi_neg <- 0.0006
  
  proj <- data.frame(Year = proj.years, 
                     scenario = NA,
                     gamma_start = NA,
                     alpha = NA,
                     lamda = NA, 
                     effortreduction = NA,
                     biomass = NA, 
                     catch = NA, 
                     catch_c = NA, 
                     catch_u = NA,
                     effort = NA,
                     effortchange_c = NA,
                     effort_c = NA,
                     effortchange_u = NA,
                     effort_u = NA,
                     gamma = NA, 
                     pred.cpue = NA, 
                     cost = NA,
                     profit = NA,
                     profit_c = NA, 
                     profit_u = NA, 
                     cdp = NA)
  
  # Pull starting values from the end of the model fit
  fit.dat <- fit_schaefer(par = par, dat = dat, use = "other")
  
  proj$biomass[1] <- fit.dat$biomass[24]
  proj$catch[1] <- fit.dat$catch[24]
  proj$catch_c[1] <- fit.dat$catch[24]*gamma_start
  proj$catch_u[1] <- fit.dat$catch[24]*(1-gamma_start)
  
  # Defining all other starting values
  proj$effort[1] <- effort
  proj$cost[1] <- 45
  proj$effortchange_c[1] <- 0
  proj$effort_c[1] <- effort*gamma_start
  proj$effortchange_u[1] <- 0
  proj$effort_u[1] <- effort*(1-gamma_start)
  proj$gamma[1] <- gamma_start
  proj$pred.cpue[1] <- q*fit.dat$biomass[24]
  proj$profit[1] = (price*q*(proj$effort[1]^alpha)*proj$biomass[1])-(proj$cost[1]*proj$effort[1])
  proj$profit_c[1] = (price*proj$catch_c[1])-(proj$cost[1]*proj$effort_c[1])
  proj$profit_u[1] = (price*proj$catch_u[1])-(proj$cost[1]*proj$effort_u[1])
  proj$cdp[1] = lamda*price*delta*q*proj$effort_c[1]*fit.dat$biomass[24]
  
  # Printing scenario, gamma, alpha, lambda, and effort reduction for each
  proj$scenario[1] = scenario
  proj$gamma_start[1] = gamma_start
  proj$alpha[1] = alpha
  proj$lamda[1] = lamda
  proj$effortreduction[1] = effortreduction
  
  #Logistic growth model fitting. max() ensures it does not go to 0
  for(i in 1:(length(proj.years)-1)){
    
    proj$biomass[i+1] = max(0.001, proj$biomass[i] + r*proj$biomass[i]*(1-proj$biomass[i]/K) - ((q*(proj$effort[i])*proj$biomass[i])))
    
    # Make effort drop off quicker if profits are negative for certified effort (capped)
    if(((price*q*(proj$effort_c[i]^alpha)*proj$biomass[i]) - (proj$cost[i]*proj$effort_c[i])) >= 0){
      proj$effortchange_c[i+1] = phi*((price*q*(proj$effort_c[i]^alpha)*proj$biomass[i]) - (proj$cost[i]*proj$effort_c[i]))
    }
    else{
      proj$effortchange_c[i+1] = phi_neg*((price*q*(proj$effort_c[i]^alpha)*proj$biomass[i]) - (proj$cost[i]*proj$effort_c[i]))
    }
    
    proj$effort_c[i+1] = min((effort*gamma_start)*effortreduction, proj$effort_c[i]+proj$effortchange_c[i+1])
    
    # Make effort drop off quicker if profits are negative for uncertified effort 
    if(((price*q*(proj$effort_u[i]^alpha)*proj$biomass[i]) - (proj$cost[i]*proj$effort_u[i])) >= 0){
      proj$effortchange_u[i+1] = phi*((price*q*(proj$effort_u[i]^alpha)*proj$biomass[i]) - (proj$cost[i]*proj$effort_u[i]))
    }
    else{
      proj$effortchange_u[i+1] = phi_neg*((price*q*(proj$effort_u[i]^alpha)*proj$biomass[i]) - (proj$cost[i]*proj$effort_u[i]))
    }

    proj$effort_u[i+1] = proj$effort_u[i] + proj$effortchange_u[i+1]
    
    proj$effort[i+1] = proj$effort_c[i+1] + proj$effort_u[i+1]
    
    proj$gamma[i+1] = proj$effort_c[i+1]/proj$effort[i+1]
    
    proj$cost[i+1] = 45
    
    proj$catch_c[i+1] = max(0.0001, (q*(proj$effort_c[i+1]^alpha)*proj$biomass[i+1])*proj$gamma[i+1])
    
    proj$catch_u[i+1] = max(0.0001, (q*(proj$effort_u[i+1]^alpha)*proj$biomass[i+1])*(1-proj$gamma[i+1]))
    
    proj$catch[i+1] =  proj$catch_c[i+1] + proj$catch_u[i+1]
    
    proj$pred.cpue[i+1] = q*proj$biomass[i+1]
    
    proj$profit[i+1] = (price*q*(proj$effort[i+1]^alpha)*proj$biomass[i+1])-(proj$cost[i+1]*proj$effort[i+1])
    
    proj$profit_c[i+1] = (price*proj$catch_c[i+1])-(proj$cost[i+1]*proj$effort_c[i+1])
    proj$profit_u[i+1] = (price*proj$catch_u[i+1])-(proj$cost[i+1]*proj$effort_u[i+1])
    
    proj$cdp[i+1] = lamda*price*delta*q*proj$effort_c[i+1]*proj$biomass[i+1]
    
    proj$scenario[i+1] = scenario
    proj$gamma_start[i+1] = gamma_start
    proj$alpha[i+1] = alpha
    proj$lamda[i+1] = lamda
    proj$effortreduction[i+1] = effortreduction
    
  }
  
  return(proj)
}   

  

