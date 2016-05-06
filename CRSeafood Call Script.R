#===================================================================

# Script to call our model functions
# Requires CRSeafood Functions

# Kat Millage
# Last modified 3/14/16

#====================================================================

#===PART 1=======================================
# Loading packages
#================================================
rm(list = ls())
set.seed(543)
library(ggplot2)
library(gridExtra)
library(plyr)
library(dplyr)
library(tidyr)
library(knitr)
library(stats4)
source('CRSeafood Functions.R')

#===PART 2=======================================
# Loading data 
#================================================

# Catch data from INCOPESCA
catches <- read.csv('catches.csv', header=T, stringsAsFactors = F) %>%
  filter(Region == 'Guanacaste', Species %in% c('Pargo', 'Pargo Seda')) %>%
  #   select(-X, -(Jan:Dec)) %>%
  group_by(Year) %>%
  summarize(lowcatch = sum(Total)) %>%
  mutate(catch = lowcatch*2.6)

mean(catches$catch/1000)

# CPUE data from Andy
CPUE <- read.csv('CPUE2.csv', header=T, stringsAsFactors = F) %>%
  ungroup() %>%
  select(-Trip.Num, -Date, -Captain, -Inpsector, -Location) %>%
  #filter(Obs.Type == 'Onboard') %>%
  group_by(Year) %>% 
  summarize(CPUE = mean(Number), SD = sd(Number))

# Dock price data from INCOPESCA
prices <- read.csv('prices.csv', header=T, stringsAsFactors = F) 

# Merging into one data set 
cr_dat <- left_join(catches, CPUE, by = 'Year') 

max_catch <- max(cr_dat$catch)

cr_dat_prices <- cr_dat %>%
  left_join(prices, by = "Year")

cr_dat_fix <- data.matrix(cr_dat_prices)

#===PART 3=======================================
# Making a table of parameter values which we want to run the model on and finding MLE of those. (Schaefer) 
#================================================

possibs_schaefer <- expand.grid(r = seq(1e-2,.5, length.out = 5), 
                       K = seq(2*max_catch,100*max_catch, length.out = 5),
                       q = seq(1e-9,1e-3, length.out = 5), 
                       sigma = seq(1e-3, 20, length.out = 10))

# Try some starting guesses and run the model
for (j in 1:dim(possibs_schaefer)[1]){ 
  
  guess <- fit_schaefer(par = log(as.numeric(possibs_schaefer[j,])), dat = cr_dat)
  
  possibs_schaefer$ss[j] <- guess
}

# Best model scenario of all those we tried (no bounds)
mle_guess <- as.list(log(possibs_schaefer[possibs_schaefer$ss == min(possibs_schaefer$ss), 1:4 ]))

# Running those two things a few times to make sure it agrees
for (i in 1:3){ 
  fitted <- nlminb(start = (log(as.numeric(possibs_schaefer[possibs_schaefer$ss == min(possibs_schaefer$ss), 1:4 ]))),                    objective = fit_schaefer, dat = cr_dat,
                   lower = log(c(1e-3, 2*max_catch, 1e-7)),
                   upper = log(c(0.6, 10000 * max_catch,1e-2)))
  
  par <- fitted$par
  
}

best_pars <-  data.frame(r = exp(par[1]), 
                         k = exp(par[2]), 
                         q = exp(par[3]), 
                         sigma = exp(par[4]))

bestpars <- unlist(best_pars, use.names = F)

kable(best_pars)

#===PART 3.5=======================================
# Making a table of parameter values which we want to run the model on and finding MLE of those (Pella - Tomlinson) 
#================================================

possibs_pella <- expand.grid(g = seq(1e-5,1, length.out = 10), 
                                K = seq(2*max_catch,100*max_catch, length.out = 5),
                                q = seq(1e-9,1e-3, length.out = 5), 
                                n = seq(-1,2, length.out = 10),
                                sigma = seq(1e-3, 20, length.out = 10))

# Try some starting guesses and run the model
for (j in 1:dim(possibs_pella)[1]){ 
  
  guess <- fit_pella(par = as.numeric(c(log(possibs_pella[j,c(1:3,5)]), possibs_pella[j,4])), dat = cr_dat)
  
  possibs_pella$ss[j] <- guess
}

# Best model scenario of all those we tried (no bounds)
mle_guess <- as.numeric(possibs_pella[possibs_pella$ss == min(possibs_pella$ss), 1:5 ])

mle_guess <- c(log(mle_guess[c(1:3,5)]), mle_guess[4])

# Running those two things a few times to make sure it agrees
for (i in 1:4){ 
  fitted <- nlminb(start = mle_guess,                    
                   objective = fit_pella, dat = cr_dat,
                   lower = c(log(1e-9), 
                             log(5*max_catch), 
                             log(1e-7), 
                             -2, 
                             log(1e-3)),
                   upper = c(log(0.5), 
                             log(1000 * max_catch), 
                             log(1e-1), 
                             1, 
                             log(20)))
  # bounds: g, K, q, n, sigma
  
  par_pella <- fitted$par
  
}

best_pars_pella <-  data.frame(g = exp(par_pella[1]), 
                         k = exp(par_pella[2]), 
                         q = exp(par_pella[3]),
                         n = par_pella[4],
                         sigma = exp(par_pella[5]),
                         r = ((par_pella[4]+1)/par_pella[4])*exp(par_pella[1]))

bestpars_pella <- unlist(best_pars_pella, use.names = F)

kable(best_pars_pella)

#corrected AIC -> formula for this type of model. -2*LL*K*#parameters

#===PART 5=======================================
# Model Fit (Schaefer and Pella)
#================================================

######### Running model fit with best parameters

best_fit <- fit_schaefer(par = par, dat = cr_dat, use = 'sim') %>%
  mutate(MSY = (best_pars$r * best_pars$k)/4,
         Fmsy = best_pars$r/2,
         Bmsy = best_pars$k/2,
         effort = catch/(best_pars$q * biomass),
         u = effort * best_pars$q,
         f = -log(1 - u),
         FvFmsy = f/Fmsy,
         BvBmsy = biomass/Bmsy,
         CvMSY = catch/MSY,
         lowerCPUE = exp(qnorm(0.025,log(pred_cpue) - best_pars$sigma^2/2, best_pars$sigma)),
         lowerbiomass = exp(qnorm(0.025, log(biomass) - best_pars$sigma^2/2, best_pars$sigma)), 
         upperbiomass = exp(qnorm(0.975, log(biomass) - best_pars$sigma^2/2, best_pars$sigma)),
         upperCPUE = exp(qnorm(0.975,log(pred_cpue) - best_pars$sigma^2/2, best_pars$sigma)))

write.csv(best_fit, file="Best_Fit_Data_Schaefer.csv")

# Testing random distribution
a = exp(rnorm(2000,log(best_fit$pred_cpue[1]) - best_pars$sigma^2/2 , best_pars$sigma))
mean(a)
hist(a)
abline(v = best_fit$pred_cpue[1])

# Pella - Tomlinson

best_fit_pella <- fit_pella(par = par_pella, dat = cr_dat, use = 'sim') %>%
  mutate(MSY = (best_pars_pella$g * best_pars_pella$k)/(best_pars_pella$n + 1)^(1/best_pars_pella$n),
         Fmsy = best_pars_pella$g,
         Bmsy = best_pars_pella$k/(best_pars_pella$n + 1)^(1/best_pars_pella$n),
         effort = catch/(best_pars_pella$q * biomass),
         u = min(0.99999, effort * best_pars_pella$q),
         f = -log(1 - u),
         FvFmsy = f/Fmsy,
         BvBmsy = biomass/Bmsy,
         CvMSY = catch/MSY,
         lowerCPUE = exp(qnorm(0.025,log(pred_cpue) - best_pars$sigma^2/2, best_pars$sigma)),
         lowerbiomass = exp(qnorm(0.025, log(biomass) - best_pars$sigma^2/2, best_pars$sigma)), 
         upperbiomass = exp(qnorm(0.975, log(biomass) - best_pars$sigma^2/2, best_pars$sigma)),
         upperCPUE = exp(qnorm(0.975,log(pred_cpue) - best_pars$sigma^2/2, best_pars$sigma)))

write.csv(best_fit_pella, file="Best_Fit_Data_Pella.csv")

#Testing distribution
a = exp(rnorm(2000,log(best_fit_pella$pred_cpue[1]) - best_pars_pella$sigma^2/2 , best_pars_pella$sigma))
mean(a)
hist(a)
abline(v = best_fit_pella$pred_cpue[1])

#===PART 6=======================================
# BAU and Fair Trade USA Projections
#================================================

########## Running BAU projection  

scenarios_bau <- c(1,0,0.99,0,1) #Scenario, gamma initial, alpha, lambda, effort reduction

bau <- scenario_model(par = log(bestpars), dat = cr_dat, effort = 10000, scenarios = scenarios_bau, price = 4, delta = 0.8) %>%
  rename(gamma_init = gamma_start)# Why do profit and profit u start off with different values? 

bau$dis <- NA

for(i in 1:nrow(bau)){
  bau$dis[i] <- bau$profit[i]/((1+0.05)^(i-1))
}

bau_bio_profits <-  bau %>%
  select(Year, biomass, profit, dis) %>%
  rename(bau_biomass = biomass, bau_profit = profit, bau_disprofit = dis)

BAU_extend <- bau %>%
  mutate(MSY = (best_pars$r * best_pars$k)/4,
         Fmsy = best_pars$r/2,
         Bmsy = best_pars$k/2,
         u = effort * best_pars$q,
         f = -log(1 - u),
         FvFmsy = f/Fmsy,
         BvBmsy = biomass/Bmsy,
         CvMSY = catch/MSY)

######## Run for all Fair Trade scenarios

scenario_values <- read.csv('scenarios.csv', header=T, stringsAsFactors = F) 

scenario_values2 <- read.csv('scenarios2.csv', header=T, stringsAsFactors = F)

#  create empty list to store outputs of each scenario run
model_results<-list()

for(a in 1:nrow(scenario_values2)){
  
  temp <- scenario_model(par = log(bestpars), dat = cr_dat, effort = 10000, scenarios = as.numeric(scenario_values2[a,]), price = 4, delta = 0.8)
  
  model_results[[a]]<-temp
} 

# Flatten list of data frames into large single data frame

model_results_df <- ldply(model_results)



##### Selecting data for scenario trials (alpha = 0.99)


model_results_0.99 <-  model_results_df %>%
  #filter(gamma_start %in% c(0.1,0.5,1), alpha %in% c(0.99), lamda %in% c(0.03,0.06,0.1)) %>%
  mutate(profit_total = profit_u + profit_c) %>%
  left_join(bau_bio_profits, by = "Year") %>%
  group_by(scenario)

# Discounting 5%
model_results_0.99$dis <- NA
model_results_0.99$dis_cdp <- NA

for(i in 1:nrow(model_results_0.99)){
  model_results_0.99$dis[i] <- model_results_0.99$profit[i]/((1+0.05)^(model_results_0.99$Year[i]-2013))
  
  model_results_0.99$dis_cdp[i] <- model_results_0.99$cdp[i]/((1+0.05)^(model_results_0.99$Year[i]-2013))
  
}

model_results_0.99_10 <- model_results_0.99 %>%
  filter(gamma_start %in% c(0.1))

model_results_0.99_50 <- model_results_0.99 %>%
  filter(gamma_start %in% c(0.5))

model_results_0.99_100 <- model_results_0.99 %>%
  filter(gamma_start %in% c(1))

### Average by Year 

model_results_yearaverage_10 <- model_results_0.99_10 %>%
  group_by(gamma_start,Year) %>%
  summarize(avg_biomass = mean(biomass), 
            min_biomass = min(biomass),
            max_biomass = max(biomass))

model_results_yearaverage_50 <- model_results_0.99_50 %>%
  group_by(gamma_start,Year) %>%
  summarize(avg_biomass = mean(biomass), 
            min_biomass = min(biomass),
            max_biomass = max(biomass))

model_results_yearaverage_100 <- model_results_0.99_100 %>%
  group_by(gamma_start,Year) %>%
  summarize(avg_biomass = mean(biomass), 
            min_biomass = min(biomass),
            max_biomass = max(biomass))

### Summary by scenario 

model_results_summary_0.99 <- model_results_0.99 %>%
  #filter(gamma_start %in% c(0.5,0.1,)) %>%
  group_by(scenario) %>%
  summarize(biomass_change_relativebau = (last(biomass)-last(bau_biomass)), 
            last_biomass = last(biomass),
            last_biomass_bau = last(bau_biomass),
            npv = last(dis), 
            npv_bau = last(bau_disprofit),
            bau_average_profit = mean(bau_disprofit), 
            effortreduc = mean(effortreduction), 
            gamma = mean(gamma_start), 
            lambda = mean(lamda),
            averagecdp = mean(dis_cdp),
            cdp_per_com = averagecdp/(gamma*20),
            cdp_bau = 0)

modsum_filtered <- model_results_0.99 %>%
  filter(gamma_start %in% c(1)) %>%
  group_by(scenario) %>%
  summarize(biomass_change_relativebau = (last(biomass)-last(bau_biomass)), 
            last_biomass = last(biomass),
            last_biomass_bau = last(bau_biomass),
            npv = last(dis), 
            npv_bau = last(bau_disprofit),
            bau_average_profit = mean(bau_disprofit), 
            effortreduc = mean(effortreduction), 
            gamma = mean(gamma_start), 
            lambda = mean(lamda),
            averagecdp = mean(dis_cdp),
            cdp_per_com = averagecdp/(gamma*20),
            cdp_bau = 0)


