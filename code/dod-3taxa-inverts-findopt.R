## rm(list=ls())

## Setup -----------------------------------------------------------------------
## Required libraries  
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)

## Geometric mean
gm_mean <- function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}

## -----------------------------------------------------------------------------
## General settings ------------------------------------------------------------
## -----------------------------------------------------------------------------

## Flow settings

## Based on cumecs (m3s-1)
## This model was parameterized using Bill Williams River data (flow-mort rel.)
## etc. Rather than translate to Verde values, I'm using the original Bill
## Will model and values, but translating flow thresholds between the two rivers
## to make it transferrable to the Verde directly. 
## 1 cumec = 35 cfs
## Verde values in cfs. BWR in cms. 

## We're going to simply use the Bill Williams numbers rather than translate
## to the Verde. We know from the fish model that:
## medflood: Q2.5
## highflood: Q4
## These constitute bankfull and large overtopping flood, respectively. 

## Bill Williams numbers
## Q2.5: 400 m3/s
## Q4: 1000 m3/s
## But these aren't realistic of the d/s settings
## The following are from observed events d/s at Rankin Ranch reach.
## The medflood and highflood in the models constitute bankfull and large events
## Thus we can use the following.
## Bankfull: 87 cms
## Observed by Wilcox from pulse event at Rankin. 
## Bigflood: 215 cms - from the graph of Dave's. (7500 cfs)
## 2004: A major flood that completely reworked riparian veg on the BWR.

## Verde numbers (note cfs not cms)
## Q2.5: 220 cfs (i.e. bankfull event)
## Q4: 700 cfs (i.e. large overtopping event)

## So:
## Verde 220cfs = BWR 87cms
## Verde 700cfs = BWR 215cms

## bankfulls
87/220 # actual val
70/220 # matches bigflood ratio
75/220 # intermediate
## bigfloods
215/700 # actual val
270/700 # what matches bankfull ratio
250/700 # intermediate

## Ratio that meets in the middle: 0.35
## I'll work with 0.35 as the scaling factor between the two systems

220*.35
700*.35

## These values aren't HUGELY important as if bug-opt flows were bigger they
## may affect the thresholds for plants and fish, but they're small. Going the
## other way is less critical as bugs don't have a threshold. 

## Minimum threshold of what is considered a flood
Qmin = 5

## Half saturation constant
a = 100

## Maximum flood size to run model to
Qmax = 1000

## Length of full cycle to run each setting over. 
cycle = 10 * 365 # 10 years

## Vector of different cycles
days <- c(365/4, 365/3, 365/2, 365, 365*2)

## -----------------------------------------------------------------------------
## Main function ---------------------------------------------------------------
## -----------------------------------------------------------------------------

## Function to calculate N at time t in relation to flood intensity
MainLogisticSolution <- function(r, t0, t, Q, npre) {
    
    n0 = initialpopsize(Q, npre)
    
    intfunc <- function(y) {
        (r / kfunc(y, Q)) * (exp(r * (y - t0)))
    }
    
    (exp(r * (t - t0)) * n0) /
        (integrate(intfunc, lower = t0, upper = t)$value * n0 + 1)
    
}
    
## Function to calculate the initial population size "N0" after a disturbance
initialpopsize <- function(x, y) y * exp(-h * x) # x is Q, y is npre

## Function to calculate the disturbance magnitude-K relationship.
## Sets to 0 if below the Qmin
QFunction <- function(x)
    ifelse(x < Qmin, 0, (x - Qmin)/(a + (x - Qmin)))

## -----------------------------------------------------------------------------
## Species settings ------------------------------------------------------------
## Note that spp-spec vals keep same notation with different values further down
## e.g. g, h, r, Kd, and Kb. So it's important to run through in sequence
## -----------------------------------------------------------------------------

## -----------------------------------------------------------------------------
## Progomphus ==================================================================
## -----------------------------------------------------------------------------

## Values

## Rate that K returns to pre-disturbance level
g = 0.01

## Strength of disturbance-mortality relationship
h = 0.01
 
## Intrinsic rate of population increase 
r = .08

## Kd is the carrying capacity limit following strong disturbance
Kd = 100

## Kb is the carrying capacity baseline when disturbances are absent
Kb = 40

## -----------------------------------------------------------------------------
## Looping through different frequencies and running mainlogisticsolution ------
## -----------------------------------------------------------------------------

## Creating a list to store each data frame.
## Each df is each of the different frequencies of floods (in days vector).
## 5 separate dfs
## This has overall time from 1 to 3650 days = cycle (10y).
## t = the time in each cycle. Reset to 1 for each flood.
## T = overall time regardless of flood stage. 
## Nt = pop size.
## npre = a value of n in the month prior to flood.
## Kpre = K prior to flood
## round = the round of flood. i.e. flood number in the sequence.

## clearing 'freqlist' it it's already present. 
rm(freqlist)

## Function to calculate K immediately post-flood.
## Same for dragons and mayflies but not ostracods. 
K0func <- function(x, Kpre) {
    Kpre + (Kd - Kpre) * QFunction(x)
} # different from core. works from kpre rather than kb

## Setting up the freqlist list of dfs
## Change Q here for different flows.
freqlist <- list()

for(i in 1:length(days)) {
    
    dat <- data.frame(Time = 1:cycle)
    dat$t <- rep(seq(1, days[i]), length = cycle)
    dat$Nt <- 0
    dat$npre <- 0
    dat$npre[1] <- 50
    dat$Q <- 1000
    dat$round <- rep(1:3650, each = days[i], len = cycle)
    dat$K <- 0
    dat$Kpre <- 0
    dat$Kpre[1] <- Kb
    dat$K0 <- 0
    freqlist[[paste0('freq', i)]] <- dat

}

lapply(freqlist, head)

## Running the model with two loops
for(df in 1:length(freqlist)) {    
    for(k in 1:length(freqlist[[df]]$t)) {

        dat <- freqlist[[df]]
        
        ## setting up kfunc
        kfunc <- function(tau, Q){
            Kb + (K0func(Q, dat$Kpre[k]) - Kb) * exp(-g * tau)
        }

        ## getting npre
        dat$npre[k] <- ifelse(dat$Time[k] == 1,
                              dat$npre[k],
                       ifelse(dat$round[k] ==
                              dat$round[k-1],
                              dat$npre[k-1],
                              dat$Nt[k-1]))

        ## getting Kpre
        dat$Kpre[k] <- ifelse(dat$Time[k] == 1,
                              dat$Kpre[k],
                       ifelse(dat$round[k] ==
                              dat$round[k-1],
                              dat$Kpre[k-1],
                              dat$K[k-1]))

        ## Calculating K0
        dat$K0[k] <- dat$Kpre[k] + (Kd - dat$Kpre[k]) * QFunction(dat$Q[k])

        ## Calculating K
        dat$K[k] <- dat$Kpre[k] + (dat$K0[k] - dat$Kpre[k]) * exp(-g * dat$t[k])

        ## running logistic model
        dat$Nt[k] <-  with(dat, MainLogisticSolution(r = r,
                                                     t0 = 1,
                                                     t = t[k],
                                                     npre = npre[k],
                                                     Q = Q[k]))
        
        freqlist[[df]] <- dat
    }
}

## Combining list into a df. 
fdf <- plyr::ldply(freqlist)

## Taking the mean (geometric and arithmetic) across time for each setting.
## Removing 1st year as started at 50% of K. Not neccessarily stable after that.
means <- fdf %>%
    group_by(`.id`) %>%
    filter(Time > 365) %>%
    summarise(ari.mean = mean(Nt), geo.mean = gm_mean(Nt))

## Plotting Nt, Kt, means, and table of means.
## This is to compare the settings which promote Nt the best. 

nplot <- ggplot(fdf, aes(Time, Nt)) +
    geom_line() +
    expand_limits(y = c(0, 100)) +
    facet_grid(.~`.id`) +
    ggtitle('N') +
    facet_grid(.~`.id`)

kplot <- ggplot(fdf, aes(Time, K)) +
    geom_line() +
    expand_limits(y = c(0, 100)) +
    facet_grid(.~`.id`) +
    ggtitle('K') +
    facet_grid(.~`.id`)

tableplot <- tableGrob(means)

meanplot <- means %>%
    gather(key = key, value = value, -`.id`) %>%
    ggplot(aes(`.id`, value)) +
    geom_bar(stat = 'identity') +
    facet_grid(.~key) +
    ggtitle('Means post Y1') +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

## pdf(paste0('export/flow-exploration/inverts/progomphus_Q', fdf$Q[1], '.pdf'),
##     width = 15, height = 15)

## grid.arrange(arrangeGrob(kplot, nplot, nrow = 2),
##              arrangeGrob(meanplot, tableplot),
##              top = paste('Dragonfly. Q = ', fdf$Q[1], '\n',
##                                        'freq1 =', days[1],
##                                        '; freq2 =', round(days[2], 2),
##                                        '; freq3 =', days[3],
##                                        '; freq4 =', days[4],
##                                        '; freq5 =', days[5]),
##              ncol = 2, widths = c(.7, .3))

## dev.off()

## -----------------------------------------------------------------------------
## Fallceon ====================================================================
## -----------------------------------------------------------------------------
## Note that Fallceon has the same model as Progomphus but different rates

## Values 

## Rate that K returns to pre-disturbance level
g = 0.01

## Strength of disturbance-mortality relationship
h = 0.02
 
## Intrinsic rate of population increase 
r = .23

## Kd is the carrying capacity limit following strong disturbance
Kd = 100

## Kb is the carrying capacity baseline when disturbances are absent
Kb = 40

## -----------------------------------------------------------------------------
## Looping through different frequencies and running mainlogisticsolution ------
## -----------------------------------------------------------------------------

rm(freqlist)

K0func <- function(x, Kpre) {
    Kpre + (Kd - Kpre) * QFunction(x)
}

freqlist <- list()

for(i in 1:length(days)) {
    
    dat <- data.frame(Time = 1:cycle)
    dat$t <- rep(seq(1, days[i]), length = cycle)
    dat$Nt <- 0
    dat$npre <- 0
    dat$npre[1] <- 50
    dat$Q <- 1000
    dat$round <- rep(1:3650, each = days[i], len = cycle)
    dat$K <- 0
    dat$Kpre <- 0
    dat$Kpre[1] <- Kb
    dat$K0 <- 0
    freqlist[[paste0('freq', i)]] <- dat

}

for(df in 1:length(freqlist)) {    
    for(k in 1:length(freqlist[[df]]$t)) {

        dat <- freqlist[[df]]
        
        kfunc <- function(tau, Q){
            Kb + (K0func(Q, dat$Kpre[k]) - Kb) * exp(-g * tau)
        }
        
        dat$npre[k] <- ifelse(dat$Time[k] == 1, 
                              dat$npre[k],
                       ifelse(dat$round[k] ==
                              dat$round[k-1],
                              dat$npre[k-1],
                              dat$Nt[k-1]))

        dat$Kpre[k] <- ifelse(dat$Time[k] == 1,
                              dat$Kpre[k],
                       ifelse(dat$round[k] ==
                              dat$round[k-1],
                              dat$Kpre[k-1],
                              dat$K[k-1]))

        dat$K0[k] <- dat$Kpre[k] + (Kd - dat$Kpre[k]) * QFunction(dat$Q[k])

        dat$K[k] <- dat$Kpre[k] + (dat$K0[k] - dat$Kpre[k]) * exp(-g * dat$t[k])

        dat$Nt[k] <- with(dat, MainLogisticSolution(r = r,
                                                    t0 = 1,
                                                    t = t[k],
                                                    npre = npre[k],
                                                    Q = Q[k]))

        freqlist[[df]] <- dat
    }
}

fdf <- plyr::ldply(freqlist)

means <- fdf %>%
    group_by(`.id`) %>%
    filter(Time > 365) %>%
    summarise(ari.mean = mean(Nt), geo.mean = gm_mean(Nt))

nplot <- ggplot(fdf, aes(Time, Nt)) +
    geom_line() +
    expand_limits(y = c(0, 100)) +
    facet_grid(.~`.id`) +
    ggtitle('N') +
    facet_grid(.~`.id`)

kplot <- ggplot(fdf, aes(Time, K)) +
    geom_line() +
    expand_limits(y = c(0, 100)) +
    facet_grid(.~`.id`) +
    ggtitle('K') +
    facet_grid(.~`.id`)

tableplot <- tableGrob(means)

meanplot <- means %>%
    gather(key = key, value = value, -`.id`) %>%
    ggplot(aes(`.id`, value)) +
    geom_bar(stat = 'identity') +
    facet_grid(.~key) +
    ggtitle('Means post Y1') +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

## pdf(paste0('export/flow-exploration/inverts/fallceon_Q', fdf$Q[1], '.pdf'),
##     width = 15, height = 15)

## grid.arrange(arrangeGrob(kplot, nplot, nrow = 2),
##              arrangeGrob(meanplot, tableplot),
##              top = paste('Mayfly. Q = ', fdf$Q[1], '\n',
##                                        'freq1 =', days[1],
##                                        '; freq2 =', round(days[2], 2),
##                                        '; freq3 =', days[3],
##                                        '; freq4 =', days[4],
##                                        '; freq5 =', days[5]),
##              ncol = 2, widths = c(.7, .3))

## dev.off()

## -----------------------------------------------------------------------------
## Ostracod ====================================================================
## -----------------------------------------------------------------------------

## Note that Ostracod has a different model to the previous two

## Values 

## Rate that K returns to pre-disturbance level
g = 0.01

## Strength of disturbance-mortality relationship
h = 0.05
 
## Intrinsic rate of population increase 
r = .16

## Kd is the carrying capacity limit following strong disturbance
## Note difference to other two spp
Kd = 40

## Kb is the carrying capacity baseline when disturbances are absent
Kb = 100

## -----------------------------------------------------------------------------
## Looping through different frequencies and running mainlogisticsolution ------
## -----------------------------------------------------------------------------

rm(freqlist)

K0func <- function(x, Kpre) {
    Kpre - (Kpre - Kd) * QFunction(x)
} # Note difference to two previous taxa

freqlist <- list()

for(i in 1:length(days)) {
    
    dat <- data.frame(Time = 1:cycle)
    dat$t <- rep(seq(1, days[i]), length = cycle)
    dat$Nt <- 0
    dat$npre <- 0
    dat$npre[1] <- 50
    dat$Q <- 30
    dat$round <- rep(1:3650, each = days[i], len = cycle)
    dat$K <- 0
    dat$Kpre <- 0
    dat$Kpre[1] <- Kb
    dat$K0 <- 0
    freqlist[[paste0('freq', i)]] <- dat

}

for(df in 1:length(freqlist)) {    
    for(k in 1:length(freqlist[[df]]$t)) {

        dat <- freqlist[[df]]
        
        kfunc <- function(tau, Q){
            Kb - (Kb - K0func(Q, dat$Kpre[k])) * exp(-g * tau)
        }

        dat$npre[k] <- ifelse(dat$Time[k] == 1,
                              dat$npre[k],
                       ifelse(dat$round[k] ==
                              dat$round[k-1],
                              dat$npre[k-1],
                              dat$Nt[k-1]))

        dat$Kpre[k] <- ifelse(dat$Time[k] == 1,
                              dat$Kpre[k],
                       ifelse(dat$round[k] ==
                              dat$round[k-1],
                              dat$Kpre[k-1],
                              dat$K[k-1]))

        dat$K0[k] <- dat$Kpre[k] + (Kd - dat$Kpre[k]) * QFunction(dat$Q[k])

        dat$K[k] <- dat$Kpre[k] + (dat$K0[k] - dat$Kpre[k]) * exp(-g * dat$t[k])

        dat$Nt[k] <-  with(dat, MainLogisticSolution(r = r,
                                                     t0 = 1,
                                                     t = t[k],
                                                     npre = npre[k],
                                                     Q = Q[k]))

        freqlist[[df]] <- dat
    }
}

fdf <- plyr::ldply(freqlist)

means <- fdf %>%
    group_by(`.id`) %>%
    filter(Time > 365) %>%
    summarise(ari.mean = mean(Nt), geo.mean = gm_mean(Nt))

nplot <- ggplot(fdf, aes(Time, Nt)) +
    geom_line() +
    expand_limits(y = c(0, 100)) +
    facet_grid(.~`.id`) +
    ggtitle('N') +
    facet_grid(.~`.id`)

kplot <- ggplot(fdf, aes(Time, K)) +
    geom_line() +
    expand_limits(y = c(0, 100)) +
    facet_grid(.~`.id`) +
    ggtitle('K') +
    facet_grid(.~`.id`)

tableplot <- tableGrob(means)

meanplot <- means %>%
    gather(key = key, value = value, -`.id`) %>%
    ggplot(aes(`.id`, value)) +
    geom_bar(stat = 'identity') +
    facet_grid(.~key) +
    ggtitle('Means post Y1') +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

## pdf(paste0('export/flow-exploration/inverts/ostracod_Q', fdf$Q[1], '.pdf'),
##     width = 15, height = 15)

## grid.arrange(arrangeGrob(kplot, nplot, nrow = 2),
##              arrangeGrob(meanplot, tableplot),
##              top = paste('Ostracod. Q = ', fdf$Q[1], '\n',
##                                        'freq1 =', days[1],
##                                        '; freq2 =', round(days[2], 2),
##                                        '; freq3 =', days[3],
##                                        '; freq4 =', days[4],
##                                        '; freq5 =', days[5]),
##              ncol = 2, widths = c(.7, .3))

## dev.off()

## FINAL OPTIMIZATION ----------------------------------------------------------

## 50 cumecs, 4X per year promote both mayflies and dragons.
## Ostracods do poorly, but obviously not as poorly as bigger floods. 

## VERDE CONVERSION
## 1 cfs = 0.028316846592 cumecs 
## 50/0.35 50cumecs in BWR ==
## 143 cfs in Verde
## 4 cumecs in Verde

## Exploration values
## 30/0.35 == 86cfs == 2.4 cumecs in Verde
## 1000/0.35 == 2857cfs == 80.9

## Falls below any important thresholds in fish or riparian plant models.
## Therefore, INV_OPT for them will be non-event years. 

## Creating the INV_OPT df for the scenario running. 
## Length of full cycle to run each setting over. 
cycle = 10 * 365 # 10 years

## Vector cycle
days <- 365/4

INV_OPT <- data.frame(Time = 1:cycle)
INV_OPT$t <- rep(seq(1, days), length = cycle)
INV_OPT$Nt <- 0
INV_OPT$npre <- 0
INV_OPT$npre[1] <- 50
INV_OPT$Q <- 50 ## Will need converting for the Verde models. 
INV_OPT$round <- rep(1:3650, each = days, len = cycle)
INV_OPT$K <- 0
INV_OPT$Kpre <- 0
INV_OPT$Kpre[1] <- Kb
INV_OPT$K0 <- 0

