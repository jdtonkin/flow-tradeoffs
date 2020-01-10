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
cycle = 30 * 365 # 30 years

## Bringing in the natural flow I generated from Verde data
natflow_df <- read.csv("export/natflow_mean.csv")    
## Converting to cms for Bill Will model
natflow_df <- natflow_df %>%
    mutate(cms = Q * 0.35)
head(natflow_df)
tail(natflow_df)

## FLOW DF setup for different scenarios ---------------------------------------
## I'll put these into 'freqlist' to loop over each. 

## Scenarios:
## INV_OPT: Optimizing for inverts. 4 X 50 cms peaks/year
## RIP_OPT: Optimizing for riparian.
## FSH_OPT: Optimizing for fish.
## NAT: Natural flow regime

## Have to place them with each species as I add Kb to the dfs

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

## clearing 'freqlist' if it's already present. 
rm(freqlist)

## Function to calculate K immediately post-flood.
## Same for dragons and mayflies but not ostracods. 
K0func <- function(x, Kpre) {
    Kpre + (Kd - Kpre) * QFunction(x)
} # different from core. works from kpre rather than kb


## INV_OPT ---------------------------------------------------------------------
## Vector cycle
days <- 365/4

INV_OPT <- data.frame(Time = 1:cycle)
INV_OPT$t <- rep(seq(1, days), length = cycle)
INV_OPT$Nt <- 0
INV_OPT$npre <- 0
INV_OPT$npre[1] <- 50
INV_OPT$Q <- 50 ## Will need converting from the Verde models for rip and fish. 
INV_OPT$round <- rep(1:cycle, each = days, len = cycle)
INV_OPT$K <- 0
INV_OPT$Kpre <- 0
INV_OPT$Kpre[1] <- Kb
INV_OPT$K0 <- 0

## FSH_OPT ---------------------------------------------------------------------
## This is one large spring flood per year
## big spring flood in cfs Verde: 700 cfs
## converting to bill williams discharge (cms)
## 700 * .35 = 245
days <- 365

FSH_OPT <- data.frame(Time = 1:cycle)
FSH_OPT$t <- rep(seq(1, days), length = cycle)
FSH_OPT$Nt <- 0
FSH_OPT$npre <- 0
FSH_OPT$npre[1] <- 50
FSH_OPT$Q <- 245 ## Will need converting from the Verde models for rip and fish. 
FSH_OPT$round <- rep(1:cycle, each = days, len = cycle)
FSH_OPT$K <- 0
FSH_OPT$Kpre <- 0
FSH_OPT$Kpre[1] <- Kb
FSH_OPT$K0 <- 0

## RIP_OPT ---------------------------------------------------------------------
## This is one large spring flood every 6 years
## big spring flood in cfs Verde: 700 cfs
## converting to bill williams discharge (cms)
## 700 * .35 = 245
days <- 365 * 6

RIP_OPT <- data.frame(Time = 1:cycle)
RIP_OPT$t <- rep(seq(1, days), length = cycle)
RIP_OPT$Nt <- 0
RIP_OPT$npre <- 0
RIP_OPT$npre[1] <- 50
RIP_OPT$Q <- 245 ## Will need converting from the Verde models for rip and fish. 
RIP_OPT$round <- rep(1:cycle, each = days, len = cycle)
RIP_OPT$K <- 0
RIP_OPT$Kpre <- 0
RIP_OPT$Kpre[1] <- Kb
RIP_OPT$K0 <- 0

## NAT -------------------------------------------------------------------------
## This is one large spring flood and one smaller autumn one.
## Taken from the natural flow regime of the Verde.
## See 'natural-flow-inverts.R'
## This is read in via the natflow_df

head(natflow_df)

## different to others
NAT <- data.frame(Time = 1:cycle)
NAT$t <- rep(natflow_df$t, length = cycle)
NAT$Nt <- 0
NAT$npre <- 0
NAT$npre[1] <- 50
NAT$Q <- rep(natflow_df$cms, length = cycle)
NAT$round <- rep(1:60, times = rep(c(200, 165), len = 60))
NAT$K <- 0
NAT$Kpre <- 0
NAT$Kpre[1] <- Kb
NAT$K0 <- 0

## Setting up the freqlist list of dfs
freqlist <- list(INV_OPT = INV_OPT, FSH_OPT = FSH_OPT, RIP_OPT = RIP_OPT, NAT = NAT)
 
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

## pdf('export/flow-exploration/inverts_scenarios/progomphus.pdf',
##     width = 15, height = 15)

## grid.arrange(arrangeGrob(kplot, nplot, nrow = 2),
##              arrangeGrob(meanplot, tableplot),
##              top = paste('Dragonfly\n'),
##              ncol = 2, widths = c(.7, .3))

## dev.off()

## compiling into new df
fdf_dragon <- fdf %>%
    mutate(taxa = 'Dragonfly') %>%
    rename(scenario = `.id`)

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

## INV_OPT ---------------------------------------------------------------------
## Vector cycle
days <- 365/4

INV_OPT <- data.frame(Time = 1:cycle)
INV_OPT$t <- rep(seq(1, days), length = cycle)
INV_OPT$Nt <- 0
INV_OPT$npre <- 0
INV_OPT$npre[1] <- 50
INV_OPT$Q <- 50 ## Will need converting from the Verde models for rip and fish. 
INV_OPT$round <- rep(1:cycle, each = days, len = cycle)
INV_OPT$K <- 0
INV_OPT$Kpre <- 0
INV_OPT$Kpre[1] <- Kb
INV_OPT$K0 <- 0

## FSH_OPT ---------------------------------------------------------------------
## This is one large spring flood per year
## big spring flood in cfs Verde: 700 cfs
## converting to bill williams discharge (cms)
## 700 * .35 = 245
days <- 365

FSH_OPT <- data.frame(Time = 1:cycle)
FSH_OPT$t <- rep(seq(1, days), length = cycle)
FSH_OPT$Nt <- 0
FSH_OPT$npre <- 0
FSH_OPT$npre[1] <- 50
FSH_OPT$Q <- 245 ## Will need converting from the Verde models for rip and fish. 
FSH_OPT$round <- rep(1:cycle, each = days, len = cycle)
FSH_OPT$K <- 0
FSH_OPT$Kpre <- 0
FSH_OPT$Kpre[1] <- Kb
FSH_OPT$K0 <- 0

## RIP_OPT ---------------------------------------------------------------------
## This is one large spring flood every 6 years
## big spring flood in cfs Verde: 700 cfs
## converting to bill williams discharge (cms)
## 700 * .35 = 245
days <- 365 * 6

RIP_OPT <- data.frame(Time = 1:cycle)
RIP_OPT$t <- rep(seq(1, days), length = cycle)
RIP_OPT$Nt <- 0
RIP_OPT$npre <- 0
RIP_OPT$npre[1] <- 50
RIP_OPT$Q <- 245 ## Will need converting from the Verde models for rip and fish. 
RIP_OPT$round <- rep(1:cycle, each = days, len = cycle)
RIP_OPT$K <- 0
RIP_OPT$Kpre <- 0
RIP_OPT$Kpre[1] <- Kb
RIP_OPT$K0 <- 0

## NAT -------------------------------------------------------------------------
## This is one large spring flood and one smaller autumn one.
## Taken from the natural flow regime of the Verde.
## See 'natural-flow-inverts.R'
## This is read in via the natflow_df

head(natflow_df)

## different to others
NAT <- data.frame(Time = 1:cycle)
NAT$t <- rep(natflow_df$t, length = cycle)
NAT$Nt <- 0
NAT$npre <- 0
NAT$npre[1] <- 50
NAT$Q <- rep(natflow_df$cms, length = cycle)
NAT$round <- rep(1:60, times = rep(c(200, 165), len = 60))
NAT$K <- 0
NAT$Kpre <- 0
NAT$Kpre[1] <- Kb
NAT$K0 <- 0

## Setting up the freqlist list of dfs
freqlist <- list(INV_OPT = INV_OPT, FSH_OPT = FSH_OPT, RIP_OPT = RIP_OPT, NAT = NAT)

lapply(freqlist, head)

## Running the model with two loops
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

## pdf('export/flow-exploration/inverts_scenarios/fallceon.pdf',
##     width = 15, height = 15)

## grid.arrange(arrangeGrob(kplot, nplot, nrow = 2),
##              arrangeGrob(meanplot, tableplot),
##              top = paste('Mayfly'),
##              ncol = 2, widths = c(.7, .3))

## dev.off()

## compiling into new df
fdf_mayfly <- fdf %>%
    mutate(taxa = 'Mayfly') %>%
    rename(scenario = `.id`)

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

## INV_OPT ---------------------------------------------------------------------
## Vector cycle
days <- 365/4

INV_OPT <- data.frame(Time = 1:cycle)
INV_OPT$t <- rep(seq(1, days), length = cycle)
INV_OPT$Nt <- 0
INV_OPT$npre <- 0
INV_OPT$npre[1] <- 50
INV_OPT$Q <- 50 ## Will need converting from the Verde models for rip and fish. 
INV_OPT$round <- rep(1:cycle, each = days, len = cycle)
INV_OPT$K <- 0
INV_OPT$Kpre <- 0
INV_OPT$Kpre[1] <- Kb
INV_OPT$K0 <- 0

## FSH_OPT ---------------------------------------------------------------------
## This is one large spring flood per year
## big spring flood in cfs Verde: 700 cfs
## converting to bill williams discharge (cms)
## 700 * .35 = 245
days <- 365

FSH_OPT <- data.frame(Time = 1:cycle)
FSH_OPT$t <- rep(seq(1, days), length = cycle)
FSH_OPT$Nt <- 0
FSH_OPT$npre <- 0
FSH_OPT$npre[1] <- 50
FSH_OPT$Q <- 245 ## Will need converting from the Verde models for rip and fish. 
FSH_OPT$round <- rep(1:cycle, each = days, len = cycle)
FSH_OPT$K <- 0
FSH_OPT$Kpre <- 0
FSH_OPT$Kpre[1] <- Kb
FSH_OPT$K0 <- 0

## RIP_OPT ---------------------------------------------------------------------
## This is one large spring flood every 6 years
## big spring flood in cfs Verde: 700 cfs
## converting to bill williams discharge (cms)
## 700 * .35 = 245
days <- 365 * 6

RIP_OPT <- data.frame(Time = 1:cycle)
RIP_OPT$t <- rep(seq(1, days), length = cycle)
RIP_OPT$Nt <- 0
RIP_OPT$npre <- 0
RIP_OPT$npre[1] <- 50
RIP_OPT$Q <- 245 ## Will need converting from the Verde models for rip and fish. 
RIP_OPT$round <- rep(1:cycle, each = days, len = cycle)
RIP_OPT$K <- 0
RIP_OPT$Kpre <- 0
RIP_OPT$Kpre[1] <- Kb
RIP_OPT$K0 <- 0

## NAT -------------------------------------------------------------------------
## This is one large spring flood and one smaller autumn one.
## Taken from the natural flow regime of the Verde.
## See 'natural-flow-inverts.R'
## This is read in via the natflow_df

head(natflow_df)

## different to others
NAT <- data.frame(Time = 1:cycle)
NAT$t <- rep(natflow_df$t, length = cycle)
NAT$Nt <- 0
NAT$npre <- 0
NAT$npre[1] <- 50
NAT$Q <- rep(natflow_df$cms, length = cycle)
NAT$round <- rep(1:60, times = rep(c(200, 165), len = 60))
NAT$K <- 0
NAT$Kpre <- 0
NAT$Kpre[1] <- Kb
NAT$K0 <- 0

## Setting up the freqlist list of dfs
freqlist <- list(INV_OPT = INV_OPT, FSH_OPT = FSH_OPT, RIP_OPT = RIP_OPT, NAT = NAT)
 
lapply(freqlist, head)

## Running the model with two loops
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

## pdf('export/flow-exploration/inverts_scenarios/ostracod.pdf',
##     width = 15, height = 15)

## grid.arrange(arrangeGrob(kplot, nplot, nrow = 2),
##              arrangeGrob(meanplot, tableplot),
##              top = paste('Ostracod'),
##              ncol = 2, widths = c(.7, .3))

## dev.off()

## compiling into new df
fdf_ostracod <- fdf %>%
    mutate(taxa = 'Ostracod') %>%
    rename(scenario = `.id`)

## combining
fdf_combined <- rbind(fdf_dragon, fdf_mayfly, fdf_ostracod)

## exporting
write.csv(fdf_combined,
          'export/flow-comp/ALL-SCENARIOS-inverts.csv',
          row.names = FALSE)















