###DESCRIPTION Power analysis using the code in AHM V2 4.7.3

###DATE 8/7/2023

#equilibrium occupancy colonization/(colonization + extinction)
#phi = persistence, gamma = colonization
#phi = .9, gamma = .1, equilibrium occ = .1(.1 + .1) = .5

# 4.7.3 A power analysis for occupancy trend estimation
# -----------------------------------------------------
library(AHMbook)
library(unmarked)
library(ggplot2)
library(tidyr)
library(dplyr)
library(ubms)
# Design points of factorial simulation design and number of sims
Msim <- c(50, 100, 150)     # number of sites
Jsim <- c(30, 60, 90)         # number of repeat surveys
detect.low <- c(0.004,0.051,0.101,0.151)
detect.high <- c(0.05,0.1,0.15,0.2)
detect.v <- c("vlow","low","med","high")
# simrep <- 1000          # takes about 70 min

equilibrium.occupancy <- function(phi,gamma){
  return(gamma/(gamma+(1-phi)))
}
equilibrium.occupancy(phi=.9,gamma=.1)
equilibrium.occupancy(phi=.9,gamma=.15)
equilibrium.occupancy(phi=.94,gamma=.155)#.72

####
#psi = 0.9
####

simrep <- 50  # 
# Data structure for results

mean.psi1 = 0.9
#20% decline = end.psi = 0.72


system.time(       # Being Swiss .... and obsessed with time...
  for(d in 1:3){#:length(detect.v)
    results <- array(NA, dim = c(length(Msim), length(Jsim), 2, simrep))
    dimnames(results) <- list(Msim, Jsim, c("Det p", "LRT p"), NULL)
    
  # Loop over settings for# M
  for(i in 1:length(Msim)){#
    # Loop over settings for J
    for(j in 1:length(Jsim)){#
      cat(paste("\n*** detect", detect.v[d], "; nsite:", Msim[i], "; nsurvey:", Jsim[j], "***\n"))
      # Loop over simulation reps
      for(k in 1:simrep){
        cat(paste("survey:", k, "\n"))
        # Simulate data set as a realization of above process
        p.draw <- runif(1, detect.low[d], detect.high[d])#draw value of detection probability
     # p.draw <- 0.2
        # One covariate is allowed to affect each parameter: a site covariate for 
        #psi1, site-by-year covariates for phi and gamma, and an observational 
        #covariate for p. Covariates are generated internally from uniform(-2, 2) 
        #distributions.
        
        #phi = persistence (equals 1-extinction)
        #gamma = colonization
        par(ask=F)
        data <- simDynocc(nsite = Msim[i], nsurvey = Jsim[j], nyear = 10, 
                          mean.psi1 = mean.psi1, beta.Xpsi1 = 0,
                          range.phi = c(0.94, 0.94),
                          range.gamma = c(0.155, 0.155), range.p = c(p.draw, p.draw),
                          show.plot = F)
        
        # Estimate trend using stacked data and static occupancy model
        yrv <- rep(1:data$nyear, each = data$nsite)
        ystack <- array(NA, dim = c(data$nsite * data$nyear, data$nsurvey))
        for(t in 1:data$nyear){
          ystack[((t-1)*data$nsite+1):(data$nsite*t),] <- data$y[,,t]
        }
        umf <- unmarkedFrameOccu(y=ystack, siteCovs=data.frame(year=yrv))
        # (fm0 <- occu(~1 ~ 1, data=umf, se = F))     # Null model
        # (fm0 <- occu(~1 ~ 1, data=umf, engine="R"))     # Null model
        # 
        #str(fm0)
        #ifelse(summary(fm0)$state$Estimate> 40,T,F)
       
        #(fmT <- occu(~1 ~ year, data=umf, se = F))  # Trend model
        #(lrt <- LRT(fm0, fmT))  # likelihood ratio test for trend
        
         (fm.stan.300 <- stan_occu(~1 ~ year, data=umf, iter = 300))  #48 secs for 300iters/chain, n_eff 169 for psi intercept, rhat = 1.01
        output<-summary(fm.stan.300,"state")
        the97.5 <- output$`97.5%`[2]
        
        # Save value of detection probability and LRT result
        results[i,j,1,k] <- p.draw
        results[i,j,2,k] <- ifelse (the97.5 <0, 1,0)
        
      }
    }
  }
    save(results,file=paste0("output/powerSimsStan_",mean.psi1,"psi","_",detect.v[d],".detect",".RData"))
    }
  )  #



power <- array(NA, c(3,3))
rownames(power) <- c('nsite = 50', 'nsite = 100', 'nsite = 150')
colnames(power) <- c('nsurvey = 30', 'nsurvey = 60', ' nsurvey = 90')

for(i in 1:length(Msim)){
  for(j in 1:length(Jsim)){
    power[i,j] <- mean(results.vlow[i,j,2,])
  }
}
power

#for high
power.high <- as.data.frame(power)
power.high$detect <- "0.2"
power.high$nsites <- c(50,100,150)
#for med
power.med <- as.data.frame(power)
power.med$detect <- "0.15"
power.med$nsites <- c(50,100,150)
#for low
power.low <- as.data.frame(power)
power.low$detect <- "0.1"
power.low$nsites <- c(50,100,150)
#for vlow
power.vlow <- as.data.frame(power)
power.vlow$detect <- "0.05"
power.vlow$nsites <- c(50,100,150)

power.psi0.9 <- rbind(power.high,power.med,power.low,power.vlow)
rownames(power.psi0.9) <- NULL
colnames(power.psi0.9) <- c(30,60,90,"detect","Sites")

power.psi0.9
power.psi0.9.l <- pivot_longer(power.psi0.9,cols = 1:3,names_to = "Surveys",values_to = "Power")
power.psi0.9.l$Sites <- ordered(power.psi0.9.l$Sites,c(50,100,150))
save(power.psi0.9.l,file="output/power.sims.psi0.9.NoExtraNoise.RData")
library(stringr)
ggplot(power.psi0.9.l,aes(x=Surveys,y=Power,fill=Sites))+
  geom_bar(stat='identity',position='dodge')+
  geom_hline(yintercept = 0.8, color = "red",linetype = "dashed")+
  facet_wrap(~detect) +
#facet_grid(rows=vars(det),cols=vars(psi))+
  xlab("Surveys (n)")+
  labs(title=str_wrap("Power to detect a 20% decline in occupancy after 10 years by mean initial occupacy (cols) 
                      and detectability (rows)",width=70))


####
#psi = 0.75
####
#20% decline = 0.6
equilibrium.occupancy(phi=.9,gamma=.15)#.6


# Data structure for results
results <- array(NA, dim = c(length(Msim), length(Jsim), 2, simrep))
dimnames(results) <- list(Msim, Jsim, c("Det p", "LRT p"), NULL)

mean.psi1 = 0.75

system.time(       # Being Swiss .... and obsessed with time...
  for(d in 1:3){#:length(detect.v)
    results <- array(NA, dim = c(length(Msim), length(Jsim), 2, simrep))
    dimnames(results) <- list(Msim, Jsim, c("Det p", "LRT p"), NULL)
    
    # Loop over settings for# M
    for(i in 1:length(Msim)){#
      # Loop over settings for J
      for(j in 1:length(Jsim)){#
        cat(paste("\n*** detect", detect.v[d], "; nsite:", Msim[i], "; nsurvey:", Jsim[j], "***\n"))
        # Loop over simulation reps
        for(k in 1:simrep){
          cat(paste("survey:", k, "\n"))
          # Simulate data set as a realization of above process
          p.draw <- runif(1, detect.low[d], detect.high[d])#draw value of detection probability
          # p.draw <- 0.2
          # One covariate is allowed to affect each parameter: a site covariate for 
          #psi1, site-by-year covariates for phi and gamma, and an observational 
          #covariate for p. Covariates are generated internally from uniform(-2, 2) 
          #distributions.
          
          #phi = persistence (equals 1-extinction)
          #gamma = colonization
          par(ask=F)
          data <- simDynocc(nsite = Msim[i], nsurvey = Jsim[j], nyear = 10, 
                            mean.psi1 = mean.psi1, beta.Xpsi1 = 0,
                            range.phi = c(0.94, 0.94),
                            range.gamma = c(0.155, 0.155), range.p = c(p.draw, p.draw),
                            show.plot = F)
          
          # Estimate trend using stacked data and static occupancy model
          yrv <- rep(1:data$nyear, each = data$nsite)
          ystack <- array(NA, dim = c(data$nsite * data$nyear, data$nsurvey))
          for(t in 1:data$nyear){
            ystack[((t-1)*data$nsite+1):(data$nsite*t),] <- data$y[,,t]
          }
          umf <- unmarkedFrameOccu(y=ystack, siteCovs=data.frame(year=yrv))
               
          (fm.stan.300 <- stan_occu(~1 ~ year, data=umf, iter = 300))  #48 secs for 300iters/chain, n_eff 169 for psi intercept, rhat = 1.01
          output<-summary(fm.stan.300,"state")
          the97.5 <- output$`97.5%`[2]
          
          # Save value of detection probability and LRT result
          results[i,j,1,k] <- p.draw
          results[i,j,2,k] <- ifelse (the97.5 <0, 1,0)
          
        }
      }
    }
    save(results,file=paste0("output/powerSimsStan_",mean.psi1,"psi","_",detect.v[d],".detect",".RData"))
  }
)  #

power <- array(NA, c(4,4))
rownames(power) <- c('nsite = 20', 'nsite = 50', 'nsite = 100', 'nsite = 250')
colnames(power) <- c('nsurvey = 2', 'nsurvey = 5', ' nsurvey = 10',
                     ' nsurvey = 25')

results
for(i in 1:length(Msim)){
  for(j in 1:length(Jsim)){
    power[i,j] <- mean(results[i,j,2,] < 0.05)
  }
}
power


####
#psi = 0.5
####
#20% decline = .4
equilibrium.occupancy(phi=.85,gamma=.1)#.4

# Data structure for results
results <- array(NA, dim = c(length(Msim), length(Jsim), 2, simrep))
dimnames(results) <- list(Msim, Jsim, c("Det p", "LRT p"), NULL)

mean.psi1 = 0.5

simrep <- 50

system.time(       # Being Swiss .... and obsessed with time...
  for(d in 1:4){#:length(detect.v)
    results <- array(NA, dim = c(length(Msim), length(Jsim), 2, simrep))
    dimnames(results) <- list(Msim, Jsim, c("Det p", "LRT p"), NULL)
    
    # Loop over settings for# M
    for(i in 1:length(Msim)){#
      # Loop over settings for J
      for(j in 1:length(Jsim)){#
        cat(paste("\n*** detect", detect.v[d], "; nsite:", Msim[i], "; nsurvey:", Jsim[j], "***\n"))
        # Loop over simulation reps
        for(k in 1:simrep){
          cat(paste("survey:", k, "\n"))
          # Simulate data set as a realization of above process
          p.draw <- runif(1, detect.low[d], detect.high[d])#draw value of detection probability
          # p.draw <- 0.2
          # One covariate is allowed to affect each parameter: a site covariate for 
          #psi1, site-by-year covariates for phi and gamma, and an observational 
          #covariate for p. Covariates are generated internally from uniform(-2, 2) 
          #distributions.
          
          #phi = persistence (equals 1-extinction)
          #gamma = colonization
          par(ask=F)
          data <- simDynocc(nsite = Msim[i], nsurvey = Jsim[j], nyear = 10, 
                            mean.psi1 = mean.psi1, beta.Xpsi1 = 0,
                            range.phi = c(0.85, 0.85),
                            range.gamma = c(0.1, 0.1), range.p = c(p.draw, p.draw),
                            show.plot = F)
          
          # Estimate trend using stacked data and static occupancy model
          yrv <- rep(1:data$nyear, each = data$nsite)
          ystack <- array(NA, dim = c(data$nsite * data$nyear, data$nsurvey))
          for(t in 1:data$nyear){
            ystack[((t-1)*data$nsite+1):(data$nsite*t),] <- data$y[,,t]
          }
          umf <- unmarkedFrameOccu(y=ystack, siteCovs=data.frame(year=yrv))
          
          (fm.stan.300 <- stan_occu(~1 ~ year, data=umf, iter = 300))  #48 secs for 300iters/chain, n_eff 169 for psi intercept, rhat = 1.01
          output<-summary(fm.stan.300,"state")
          the97.5 <- output$`97.5%`[2]
          
          # Save value of detection probability and LRT result
          results[i,j,1,k] <- p.draw
          results[i,j,2,k] <- ifelse (the97.5 <0, 1,0)
          
        }
      }
    }
    save(results,file=paste0("output/powerSimsStan_",mean.psi1,"psi","_",detect.v[d],".detect",".RData"))
  }
)  #
power <- array(NA, c(3,3))
rownames(power) <- c('nsite = 50', 'nsite = 100', 'nsite = 150')
colnames(power) <- c('nsurvey = 30', 'nsurvey = 60', ' nsurvey = 90')

for(i in 1:length(Msim)){
  for(j in 1:length(Jsim)){
    power[i,j] <- mean(results[i,j,2,])
  }
}
power

#for high
power.high <- as.data.frame(power)
power.high$detect <- "0.2"
power.high$nsites <- c(50,100,150)
#for med
power.med <- as.data.frame(power)
power.med$detect <- "0.15"
power.med$nsites <- c(50,100,150)
#for low
power.low <- as.data.frame(power)
power.low$detect <- "0.1"
power.low$nsites <- c(50,100,150)
#for vlow
power.vlow <- as.data.frame(power)
power.vlow$detect <- "0.05"
power.vlow$nsites <- c(50,100,150)

power.psi0.5 <- rbind(power.high,power.med,power.low,power.vlow)
rownames(power.psi0.5) <- NULL
colnames(power.psi0.5) <- c(30,60,90,"detect","Sites")

power.psi0.5
power.psi0.5.l <- pivot_longer(power.psi0.5,cols = 1:3,names_to = "Surveys",values_to = "Power")
power.psi0.5.l$Sites <- ordered(power.psi0.5.l$Sites,c(50,100,150))
save(power.psi0.5.l,file="output/power.sims.psi0.5.NoExtraNoise.RData")
library(stringr)
ggplot(power.psi0.5.l,aes(x=Surveys,y=Power,fill=Sites))+
  geom_bar(stat='identity',position='dodge')+
  geom_hline(yintercept = 0.8, color = "red",linetype = "dashed")+
  facet_wrap(~detect) +
  #facet_grid(rows=vars(det),cols=vars(psi))+
  xlab("Surveys (n)")+
  labs(title=str_wrap("Power to detect a 20% decline in occupancy after 10 years by mean initial occupacy (cols) 
                      and detectability (rows)",width=70))

