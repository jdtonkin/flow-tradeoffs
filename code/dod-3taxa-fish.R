## Verde Fish Model
## Run through each optimization plus natural flow
## Requires manually setting each scenario
## Change 'fset' to relevant scenario and uncomment code further below.
## Only one setting should be uncommented

## Required libraries
library(ggplot2)
library(plyr)
library(tidyverse)
library(popbio)

## * SETUP ---------------------------------------------------------------------

rm(list = ls()) # clearing the workspace 

## Bringing in flow data
## This came from the DoD fish model code that I simulated on the cluster. 
all.scenarios.list <- readRDS('data/all_scenarios_list.rds')

all.scenarios.list$natural.flow %>%
    summarise_all(mean)

## Setting for simulation - see below detials ----------------------------------
fset <- 'INV_OPT'

## Trial scenarios----------------------
## QTR = 25% of y have springflood, rest are nonevent
## THR and HLF, same as above but 33 and 50%
## FUL_FUL = 100% of yr have both SP and SU highflood
## HLF_HLF_MAT = 50% of yr both SP and SU highflood in same yr. NE in others
## HLF_HLF_UNM = 50% of yr SP and SU highflood in diff yr
## THR_THR_MAT = 33% of yr as both SP and SU highflood in same year. NE in others. 
## TWO_ONE = SPHF, SPHF, SUHF
## FUL_ONE = SPHF + SUHF, SPHF
## FSH_OPT_ = FUL i.e. sp flood every yr. nothing else
## HOM = homogenization. 1 sp flood every 10y. rest are NE. 
## DRT = drought. 1 sp flood/5y. 1 NE. 3 drought.

## Final scenarios ---------------------
## RIP_OPT = 6y return on SP flood, preceded by drt. Rest are NE.
## INV_OPT = nonevent year every year. Peaks are too low. 
## NAT = natural flow extended to 200y
## FUL (FSH_OPT) 100% of y have springflood.

## ## Natural flow (NAT) ----------------------------------------------------------
## ## When using natural flow data, just pull it out from the list here
## flowdata <- all.scenarios.list$natural.flow
## flowdata %>%
##     summarise_all(mean)

## ## repeating the df out to 200 y
## flowdata <- mefa:::rep.data.frame(flowdata, 5)
## flowdata <- flowdata[1:200,]
## flowdata$rep <- row.names(flowdata)


## OTHER SIMS ---------------------------------------------------------------
flowdata <- data.frame(matrix(NA, nrow = 200, ncol = 5))
names(flowdata) <- c('SP_highflood',
                     'SU_highflood',
                     'medflood',
                     'drought',
                     'nonevent')

## ## QTR
## flowdata <- flowdata %>%
##     mutate(SP_highflood = rep(c(rep(0, 3), 1), length.out = 200),
##            SU_highflood = 0,
##            medflood = 0,
##            drought = 0,
##            nonevent = case_when(
##                SP_highflood == 1 ~ 0,
##                TRUE ~ 1
##            ))

## ## THR
## flowdata <- flowdata %>%
##     mutate(SP_highflood = rep(c(rep(0, 2), 1), length.out = 200),
##            SU_highflood = 0,
##            medflood = 0,
##            drought = 0,
##            nonevent = case_when(
##                SP_highflood == 1 ~ 0,
##                TRUE ~ 1
##            ))

## ## HLF
## flowdata <- flowdata %>%
##     mutate(SP_highflood = rep(c(rep(0, 1), 1), length.out = 200),
##            SU_highflood = 0,
##            medflood = 0,
##            drought = 0,
##            nonevent = case_when(
##                SP_highflood == 1 ~ 0,
##                TRUE ~ 1
##            ))

## ## FUL
## ## & FSH_OPT_
## flowdata <- flowdata %>%
##     mutate(SP_highflood = 1,
##            SU_highflood = 0,
##            medflood = 0,
##            drought = 0,
##            nonevent = 0)

## ## FUL_FUL
## flowdata <- flowdata %>%
##     mutate(SP_highflood = 1,
##            SU_highflood = 1,
##            medflood = 0,
##            drought = 0,
##            nonevent = 0)

## ## HLF_HLF_MAT
## flowdata <- flowdata %>%
##     mutate(SP_highflood = rep(c(rep(0, 1), 1), length.out = 200),
##            SU_highflood = case_when(
##                SP_highflood == 1 ~ 1,
##                TRUE ~ 0
##            ),
##            medflood = 0,
##            drought = 0,
##            nonevent = case_when(
##                 SP_highflood == 1 ~ 0,
##                 TRUE ~ 1
##             ))

## ## HLF_HLF_UNM
## flowdata <- flowdata %>%
##     mutate(SP_highflood = rep(c(rep(0, 1), 1), length.out = 200),
##            SU_highflood = case_when(
##                SP_highflood == 1 ~ 0,
##                TRUE ~ 1
##            ),
##            medflood = 0,
##            drought = 0,
##            nonevent = 0)

## ## THR_THR_MAT
## flowdata <- flowdata %>%
##     mutate(SP_highflood = rep(c(rep(0, 2), 1), length.out = 200),
##            SU_highflood = case_when(
##                SP_highflood == 1 ~ 1,
##                TRUE ~ 0
##            ),
##            medflood = 0,
##            drought = 0,
##            nonevent = case_when(
##                SP_highflood == 1 ~ 0,
##                TRUE ~ 1
##            ))

## ## THR_THR_UNM
## flowdata <- flowdata %>%
##     mutate(SP_highflood = rep(c(rep(0, 2), 1), length.out = 200),
##            SU_highflood = rep(c(1, rep(0, 2)), length.out = 200),
##            medflood = 0,
##            drought = 0,
##            nonevent = case_when(
##                SP_highflood == 1 ~ 0,
##                SU_highflood == 1 ~ 0,
##                TRUE ~ 1
##            ))

## ## TWO_ONE
## flowdata <- flowdata %>%
##     mutate(SP_highflood = rep(c(rep(1, 2), 0), length.out = 200),
##            SU_highflood = case_when(
##                SP_highflood == 1 ~ 0,
##                TRUE ~ 1
##            ),
##            medflood = 0,
##            drought = 0,
##            nonevent = 0)

## ## FUL_ONE
## flowdata <- flowdata %>%
##     mutate(SP_highflood = 1,
##            SU_highflood = rep(c(1, 0), length.out = 200), 
##            medflood = 0,
##            drought = 0,
##            nonevent = 0)

## ## HOM
## flowdata <- flowdata %>%
##     mutate(SP_highflood = rep(c(rep(0, 4), 1, rep(0, 5)), length.out = 200),
##            SU_highflood = 0,
##            medflood = 0,
##            drought = 0,
##            nonevent = case_when(
##                SP_highflood == 1 ~ 0,
##                TRUE ~ 1
##            ))

## ## DRT
## flowdata <- flowdata %>%
##     mutate(SP_highflood = rep(c(rep(0, 4), 1), length.out = 200),
##            SU_highflood = 0,
##            medflood = 0,
##            drought = rep(c(1, 0, 1, 1, 0), length.out = 200),
##            nonevent = case_when(
##                SP_highflood == 1 ~ 0,
##                drought == 1 ~ 0,
##                TRUE ~ 1
##            ))

## ## RIP_OPT
## flowdata <- flowdata %>%
##     mutate(SP_highflood = rep(c(rep(0, 5), 1), length.out = 200),
##            SU_highflood = 0,
##            medflood = 0,
##            drought = rep(c(rep(0, 4), 1, 0), length.out = 200),
##            nonevent = case_when(
##                SP_highflood == 1 ~ 0,
##                drought == 1 ~ 0,
##                TRUE ~ 1
##            ))

## INV_OPT
flowdata <- flowdata %>%
    mutate(SP_highflood = 0,
           SU_highflood = 0,
           medflood = 0,
           drought = 0,
           nonevent = 1)


## summarise_all(flowdata, sum)
## apply(flowdata, 1, sum)
## apply(flowdata, 2, sum)

count <- 200 # 54 years in flow record, if count = 45 goes to 2008 
iterations <- 100 # number of replicate projections to run (mid loop)

## Modifiers
modifiers <- read.csv('data/modifiers-all-spp.csv')

## adding 'Modifier' value from csv to 'Code' in csv
for(j in 1:length(modifiers[,1])) {
    nam <- paste(modifiers[j,4])
    assign(nam, modifiers[j,5]) 
}

## Vital rates
## Baseline maturation probability, aCACL3 (adult senescence rate)
## Background mortality
## Initial volume in grams in 100-m reach
## Fecundity based on year type and GSI
## Stage specific densities (ind./g)

vitalrates <- read.csv('data/vital-rates.csv') 

## assigning vital rate values from column 3 to 'code' in column 2
for(k in 1:length(vitalrates[,1])) {
    nam <- paste(vitalrates[k,2])
    assign(nam, vitalrates[k,3]) 
}

## * Key -----------------------------------------------------------------------
## CACL (Catostomus clarki) – desert sucker            **NATIVE**
## GIRO (Gila robusta) – roundtail chub                **NATIVE**
## LECY (Lepomis cyanellus) – green sunfish
## CAIN (Catostomus insignis) – sonora sucker          **NATIVE**
## MIDO (Micropterus dolomieu) - smallmouth bass
## CYLU (Cyprinella lutrensis) – red shiner
## AMNA (Ameiurus natalis) – yellow bullhead

## Average total volume of water per 100 m reach in m3: 307
## Average total fish biomass per 100 m reach in g: 4766
## Average total biomass Bonar 2004 in g/100m2: 606
## Max for a 100 m rech in Gibson samples (excluding GAAF): 6996

## vector of species names
sppnames <- c('CACL', 'GIRO', 'LECY', 'CAIN', 'MIDO', 'CYLU', 'AMNA')

K = 47660 # mean for 1-km reach across 6 replicate reaches

## Loading functions from functions.R file -------------------------------------
source('code/dod-3taxa-fish-functions.R')

## * ITERATION PARAMETERS ------------------------------------------------------
## Setting up arrays/vectors to fill with data from loops

## Mid loop details ------------------------------------------------------------
## 'iterations' - number of replicate flow sequences to run for averaging

## years <- flowdata$year
stages <- as.character(c("S1", "S2", "S3"))

## Total. N of stages 2 and 3 each year ----------------------------------------
## Takes all stages 2 and 3 and sums them for each year and iteration
Total.N <- array(0,
                 dim = c(count, iterations),
                 dimnames = list(1:count, 1:iterations)
                 )

## replist. List of arrays w/ abundance data for each spp ----------------------
## Creating a list of 7 arrays to fill in. One for each spp. 
## Create an array to be repeated
reparray <- array(0,
                  dim = c(count, 3, iterations),
                  dimnames = list(1:count, stages, 1:iterations)
                  )

## Repeating the array 7 times 
replist <- rep(list(reparray), 7)

## Duplicating for biomass. Replist is for N. 
replist_biom <- replist

## Assigning names to each array from sppnames vector
names(replist) <- sppnames
names(replist_biom) <- sppnames

## Inner loop details ----------------------------------------------------------
## 'count' - number of years to project simulations (inner loop)

## N ---------------------------------------------------------------------------
## Output of biomass and no. ind. for each age class for each year projected  
## Array w/ 3 cols (stage classes) and however many rows there are yrs projected
## Creating a list of 7 arrays to fill in. One for each spp. 
## Create an array to be repeated
output.N.array <- array(0, dim = c(count, 3))

## Repeating the array 7 times 
output.N.list <- rep(list(output.N.array), 7)

## Assigning names to each array from sppnames vector
names(output.N.list) <- sppnames

## Create a df to fill in w/ lambda values -------------------------------------
lambda.df <- data.frame(matrix(ncol = 7, nrow = count))
names(lambda.df) <- sppnames

## Biomass ---------------------------------------------------------------------
## Creating a list of 7 arrays to fill in. One for each spp. 
## Create an array to be repeated
output.biom.array <- array(0, dim = c(count, 3))

## Repeating the array 7 times 
output.biom.list <- rep(list(output.biom.array), 7)

## Assigning names to each array from sppnames vector
names(output.biom.list) <- sppnames

## Total biomass as % of K -----------------------------------------------------
## Creating a list of 7 vectors to fill in. One for each spp. 
## Create a vector to be repeated
biomoutput.vector <- numeric(length = count)

## Repeating the vector 7 times 
biomoutput.list <- rep(list(biomoutput.vector), 7)

## Assigning names to each vector from sppnames vector
names(biomoutput.list) <- sppnames

## Flow results ----------------------------------------------------------------
## Flood and drought settings for each yr projected into future (i.e. 0 or 1) 
## Create data frame with 5 cols and 'count' rows to fill in with flow results
flowresults <- data.frame(matrix(ncol = 5, nrow = count))
names(flowresults) <- c('SPhighflood',
                        'SUhighflood',
                        'medflood',
                        'drought',
                        'nonevent')
flowresults

## Fecundities -----------------------------------------------------------------
## Creating a list of 7 vectors to fill in. One for each spp. 
## Create a vector to be repeated
fec.vector <- numeric(length = count)

## Repeating the vector 7 times 
fec.list <- rep(list(fec.vector), 7)

## Assigning names to each vector from sppnames vector
names(fec.list) <- sppnames

### ----------------------------------------------------------------------------
## * Mid loop ##################################################################
### ----------------------------------------------------------------------------
## Middle loop uses iterator "iter" to get "iterations" for suming S2 and S3
for(iter in 1:iterations) {

    ## USE THIS to examine different flow year types +++++++++++++++++++++++++++ 
    ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
                                         # # All 2010 SPflood + SUflood years 
                                         # z <- rep(47, 84)
    
                                         # # All Spflood 1993
                                         # z <- rep(30, 84)
    
                                         # # All drought Y2K
                                         # z <- rep(37, 84)
    
                                         # # Nonevent 1985
                                         # z <- rep(22, 84)
    
                                         # # SUflood
                                         # z <- rep(21, 84)
    
                                         # # Medflood
                                         # z <- rep(25, 84)
    
    ## Need to read in initial biom every time so starting biomass is reset each
    ## iteration
    ## N gives the total number of individuals for each age class.
    ## Initially here, this is found by multiplying the number of g occupied by
    ## a given class by the density per g
    ## biom = g/m3
    ## den = indiv/g
    
    ## To have different initial starting population sizes for each iteration,
    ## taking biom of stage 3 from negative binomial distribution, where the
    ## parameter (lambda = mean) and K (dispersion) is calculated from mean and
    ## variance in abundance across seven sites in Verde River from 94-08, and
    ## scaled to biomass from Gibson 2012 survey in file:
    ## "Rinne Verde River Data 1994-2008-.xlsx"
    
    biomCACL <- c(biomCACL1,
                  biomCACL2,
                  rnbinom(1, size = 1.52, mu = 5284)) 
    
    biomGIRO <- c(biomGIRO1,
                  biomGIRO2,
                  rnbinom(1, 0.44, mu = 2376)) 
    
    biomLECY <- c(biomLECY1,
                  biomLECY2,
                  rnbinom(1, 0.34, mu = 164)) 
    
    biomCAIN <- c(biomCAIN1,
                  biomCAIN2,
                  rnbinom(1, 1.33, mu = 34068)) 
    
    biomMIDO <- c(biomMIDO1,
                  biomMIDO2,
                  rnbinom(1, 0.66, mu = 4202)) 
    
    biomCYLU <- c(biomCYLU1,
                  biomCYLU2,
                  rnbinom(1, 1.78, mu = 238)) 
    
    biomAMNA <- c(biomAMNA1,
                  biomAMNA2,
                  rnbinom(1, 0.36, mu = 1306))

### ----------------------------------------------------------------------------
## * Inner loop ################################################################
### ----------------------------------------------------------------------------
    for(i in 1:count) {

        ## CHANGE WHAT 'y' IS TO SIMULATE DIFFERENT FLOW REGIMES ACROSS THE 54 Y 
        ## y is directly taken from flow vector. 
        y = i # follow flow record sequence

        ## Sampling randomly from the flow record
        #y = sample(nrow(flowdata), 1)


        ## Transition probabilities  -------------------------------------------
        ## G is prob. of transition to next stage
        ## P is prob. of remaining in that stage
        ## Baseline mortality vital rate (from file object: 'vital-rates.csv') 
        ## 'STmort...' is multiplied by modifier (from file object:
        ## 'modifiers-all-species.csv') based on yeartype as specified above
        ## So we have SP_highflood, SU_highflood, medflood, nonevent, drought
        ## and '..._J/2/3_SUHF/SPHF/MF/NE/DR'

        ## Stage 1 - G
        for(nm in sppnames) {
            assign(paste0('G', nm, '1'),
                   
                   get(paste0('a', nm, '1')) *
                   get(paste0('den', nm, 'J')) *
                   (1 /
                    get(paste0('den', nm, '2'))
                   )  *
                   (1 -
                    (flowdata$SU_highflood[y] *
                     get(paste0('STMort', nm)) *
                     get(paste0(nm, '_J_SUHF')))
                   ) *
                   (1 -
                    (flowdata$SP_highflood[y] *
                     get(paste0('STMort', nm)) *
                     get(paste0(nm, '_J_SPHF')))
                   ) *
                   (1 -
                    (flowdata$medflood[y] *
                     get(paste0('STMort', nm)) *
                     get(paste0(nm, '_J_MF')))
                   ) *
                   (1 -
                    (flowdata$nonevent[y] *
                     get(paste0('STMort', nm)) *
                     get(paste0(nm, '_J_NE')))
                   ) *
                   (1 -
                    (flowdata$drought[y] *
                     get(paste0('STMort', nm)) *
                     get(paste0(nm, '_J_DR')))
                   )
                   )          
        }        

        ## Stage 2 - G
        for(nm in sppnames) {
            assign(paste0('G', nm, '2'),

                   get(paste0('a', nm, '2')) *
                   get(paste0('den', nm, '2')) *
                   (1 /
                    get(paste0('den', nm, '3'))
                   ) *
                   (1 -
                    (flowdata$SU_highflood[y] *
                     get(paste0('STMort', nm)) *
                     get(paste0(nm, '_A_SUHF')))
                   ) *
                   (1 -
                    (flowdata$SP_highflood[y] *
                     get(paste0('STMort', nm)) *
                     get(paste0(nm, '_A_SPHF')))
                   ) *
                   (1 -
                    (flowdata$medflood[y] *
                     get(paste0('STMort', nm)) *
                     get(paste0(nm, '_A_MF')))
                   ) *
                   (1 -
                    (flowdata$nonevent[y] *
                     get(paste0('STMort', nm)) *
                     get(paste0(nm, '_A_NE')))
                   ) *
                   (1 -
                    (flowdata$drought[y] *
                     get(paste0('STMort', nm)) *
                     get(paste0(nm, '_A_DR')))
                   )
                   )          
        }        

        ## Stage 3 - P
        for(nm in sppnames) {
            assign(paste0('P', nm, '3'),

            (1 -
             get(paste0('a', nm, '3'))
            ) *
            (1 -
             (flowdata$SU_highflood[y] *
              get(paste0('STMort', nm)) *
              get(paste0(nm, '_A_SUHF')))
            ) *
            (1 -
             (flowdata$SP_highflood[y] *
              get(paste0('STMort', nm)) *
              get(paste0(nm, '_A_SPHF')))
            ) *
            (1 -
             (flowdata$medflood[y] *
              get(paste0('STMort', nm)) *
              get(paste0(nm, '_A_MF')))
            ) *
            (1 -
             (flowdata$nonevent[y] *
              get(paste0('STMort', nm)) *
              get(paste0(nm, '_A_NE')))
            ) *
            (1 -
             (flowdata$drought[y] *
              get(paste0('STMort', nm)) *
              get(paste0(nm, '_A_DR')))
            )
            )          
        }          
        
        ## POTENTIAL FECUNDITY -------------------------------------------------

        ## 1st calculate total grams occupied after year 
        totbiom <- ldply(sppnames, function(x)
            get(paste0('biom', x))[1] +
            get(paste0('biom', x))[2] +
            get(paste0('biom', x))[3]) %>%
            sum
        
        ## Carrying capacity (K) is limiting spawning of all species based on
        ## the total biomass occupied at the end of the previous year. i.e. if
        ## above K, no spp spawn in that year. If spawning occurs, they all do.
        ## Some slight differences in fecund btwn spp so can't loop/lapply

        ## CACL stage 2
        FCACL2 <- ((0.5 * GSI.CACL * (1 - S0MortCACL)) *
                   checkpos((K - totbiom)/K)) *
            denCACL1 *
            (1/denCACLJ)

        ## CACL stage 3
        FCACL3 <- ((0.5 * GSI.CACL * (1 - S0MortCACL)) *
                   checkpos((K - totbiom)/K)) *
            denCACL1 *
            (1/denCACLJ)

        ## GIRO stage 2
        FGIRO2 <- ((0.5 * GSI.GIRO * (1 - S0MortGIRO)) *
                   checkpos((K - totbiom)/K)) * 
            denGIRO1 *
            (1/denGIROJ)

        ## GIRO stage 3
        FGIRO3 <- ((0.5 * GSI.GIRO * (1 - S0MortGIRO)) *
                   checkpos((K - totbiom)/K)) *
            denGIRO1 *
            (1/denGIROJ)

        ## CAIN stage 3
        FCAIN3 <- ((0.5 * GSI.CAIN * (1 - S0MortCAIN)) *
                   checkpos((K - totbiom)/K)) *
            denCAIN1 *
            (1/denCAINJ)

        ## LECY stage 2
        FLECY2 <- ((0.5 * GSI.LECY * (1 - S0MortLECY)) *
                   checkpos((K - totbiom)/K)) *
            denLECY1 *
            (1/denLECYJ)

        ## LECY stage 3
        FLECY3 <- ((0.5 * GSI.LECY * (1 - S0MortLECY)) *
                   checkpos((K - totbiom)/K)) * 
            denLECY1 *
            (1/denLECYJ)
        
        ## MIDO stage 3
        FMIDO3 <- ((0.5 * GSI.MIDO * (1 - S0MortMIDO)) *
                   checkpos((K - totbiom)/K)) *
            denMIDO1 *
            (1/denMIDOJ)

        ## CYLU 
        ## because they are serial spawners, they are allowed to spawn twice a
        ## season in stage 2 and 3
        ## CYLU stage 1
        FCYLUJ <- ((0.5 * GSI.CYLU * (1 - S0MortCYLU)) *
                   checkpos((K - totbiom)/K)) * 
            denCYLU1 *
            (1/denCYLUJ)

        ## CYLU stage 2
        FCYLU2 <- ((0.5 * 2 * GSI.CYLU * (1 - S0MortCYLU)) *
                   checkpos((K - totbiom)/K)) *
            denCYLU1 *
            (1/denCYLUJ)

        ## CYLU stage 3
        FCYLU3 <- ((0.5 * 2 * GSI.CYLU * (1 - S0MortCYLU)) *
                   checkpos((K - totbiom)/K)) *
            denCYLU1 *
            (1/denCYLUJ)

        ## AMNA 
        FAMNA3 <- ((0.5 * GSI.AMNA * (1 - S0MortAMNA)) *
                   checkpos((K - totbiom)/K)) *
            denAMNA1 *
            (1/denAMNAJ)

        ## K -------------------------------------------------------------------
        ## Calculating the percentage of K occupied and adding to KCACL etc
        for(nm in sppnames) {
            assign(paste0('K', nm), 100 * (
                get(paste0('biom', nm))[1] +   
                get(paste0('biom', nm))[2] +
                get(paste0('biom', nm))[3])/K)                                  
        }
        
        ## TRANSITION MATRICES -------------------------------------------------

        ## CACL 
        ACACL1 <- c(0, FCACL2, FCACL3)
        ACACL2 <- c(GCACL1, 0, 0)
        ACACL3 <- c(0, GCACL2, PCACL3)
        
        ## GIRO 
        AGIRO1 <- c(0, FGIRO2, FGIRO3)
        AGIRO2 <- c(GGIRO1, 0, 0)
        AGIRO3 <- c(0, GGIRO2, PGIRO3)
       
        ## LECY 
        ALECY1 <- c(0, FLECY2, FLECY3)
        ALECY2 <- c(GLECY1, 0, 0)
        ALECY3 <- c(0, GLECY2, PLECY3)
       
        ## CAIN 
        ACAIN1 <- c(0, 0, FCAIN3)
        ACAIN2 <- c(GCAIN1, 0, 0)
        ACAIN3 <- c(0, GCAIN2, PCAIN3)
        
        ## MIDO 
        AMIDO1 <- c(0, 0, FMIDO3)
        AMIDO2 <- c(GMIDO1, 0, 0)
        AMIDO3 <- c(0, GMIDO2, PMIDO3)
        
        ## CYLU 
        ACYLU1 <- c(FCYLUJ, FCYLU2, FCYLU3)
        ACYLU2 <- c(GCYLU1, 0, 0)
        ACYLU3 <- c(0, GCYLU2, PCYLU3)
        
        ## AMNA 
        AAMNA1 <- c(0, 0, FAMNA3)
        AAMNA2 <- c(GAMNA1, 0, 0)
        AAMNA3 <- c(0, GAMNA2, PAMNA3)
       
        ## rbinding the vectors from above into transition matrices
        ## Makes ACACL, AGIRO etc.
        for(nm in sppnames) {
            assign(paste0('A', nm), rbind(
                                        get(paste0('A', nm, '1')),
                                        get(paste0('A', nm, '2')),
                                        get(paste0('A', nm, '3'))
                                    ))
        }
        
        ## COMPILING OUTPUTS ---------------------------------------------------

        ## Lambda values
        ## Filling in the df with lambda values for each species and each year
        ## Species as columns, years as rows
        ## This applies 'lambda(ACACL)' etc and adds to correct column each
        ## 'i' value (year)
        lambda.df[i,] <- sapply(mget(paste0('A', names(lambda.df))), lambda)

        ## Fecundity values
        ## Cant loop or anything as different for diff spp
        fec.list$CACL[i] <- FCACL3 + FCACL2
        fec.list$GIRO[i] <- FGIRO3 + FGIRO2
        fec.list$LECY[i] <- FLECY3 + FLECY2
        fec.list$CAIN[i] <- FCAIN3
        fec.list$MIDO[i] <- FMIDO3
        fec.list$CYLU[i] <- FCYLU3 + FCYLU2 + FCYLUJ
        fec.list$AMNA[i] <- FAMNA3

        ## biomass values into each df/array in the list
        for(nm in sppnames) {
            output.biom.list[[nm]][i,1:3] <- get(paste0('biom', nm))
        }
        
        ## N values into each df/array in the list       
        for(nm in sppnames) {
            output.N.list[[nm]][i,1:3] <- c(get(paste0('biom', nm))[1] *
                                            get(paste0('den', nm, 'J')
                                                ),
                                            get(paste0('biom', nm))[2] *
                                            get(paste0('den', nm, '2')
                                                ),
                                            get(paste0('biom', nm))[3] *
                                            get(paste0('den', nm, '3')
                                                ))
        }
        
        ## Flow results --------------------------------------------------------
        ## Records flood settings of each particular projected year
        ## (1 = yes, 0 = no)
        flowresults$SPhighflood[i] <- flowdata$SP_highflood[y]
        flowresults$SUhighflood[i] <- flowdata$SU_highflood[y]
        flowresults$medflood[i] <- flowdata$medflood[y]
        flowresults$nonevent[i] <- flowdata$nonevent[y]
        flowresults$drought[i] <- flowdata$drought[y]
        
        ## MATRIX MULTIPLICATION -----------------------------------------------
        ## can include rescue function for each with 0.5 chance of reach being
        ## colonized by 2 individuals
        ## Loop essentially == biomAMNA <- AAMNA %*% biomAMNA
        ## AAMNA is transit. matrix, biomAMNA = total biomass for each age class
        for(nm in sppnames) {
            assign(paste0('biom', nm),
                   get(paste0('A', nm)) %*% get(paste0('biom', nm)))
        }
               
    }
    
### ----------------------------------------------------------------------------
### End of inner loop ##########################################################
### ----------------------------------------------------------------------------

    ## Mean values for each iteration run over each sequence of years ----------
    ## N
    for(nm in sppnames) {
        replist[[nm]][,,iter] <- output.N.list[[nm]]    
    }
    
    ## Biom
    for(nm in sppnames) {
        replist_biom[[nm]][,,iter] <- output.biom.list[[nm]]    
    }
    
    ## Total.N -----------------------------------------------------------------
    ## Caculating Total.N for each year, and adding it to total.N data frame
    ## with however many iterations run.
    ## Total does not incl. juveniles.
    ## map is purrr version of lapply. Can pass fn using ~ and .x instead of
    ## function(x) x
    ## Gets list output of stages 2:3 for ea spp, then cbinds them all together,
    ## then calcs sum. 
    Total.N[,iter] <- map(output.N.list, ~ .x[,2:3]) %>%
        do.call('cbind', .) %>%
        apply(1, sum)

}

### ----------------------------------------------------------------------------
### End of mid loop ############################################################
### ----------------------------------------------------------------------------

## * OUTPUTS -------------------------------------------------------------------
################################################################################

## N ---------------------------------------------------------------------------
## turning replist into a df
repdf <- ldply(replist, function(x) {
    adply(x, c(1,2,3))
})

names(repdf) <- c('species', 'year', 'stage', 'rep', 'abund')
repdf <- filter(repdf, stage != 'S1')
repdf$year <- as.numeric(as.character(repdf$year))

totn <- adply(Total.N, c(1,2))
names(totn) <- c('year', 'rep', 'tot.abund')
totn$year <- as.numeric(as.character(totn$year))

## joining totn and repdf together
repdf <- left_join(totn, repdf)

## calculating relative abundance
repdf <- mutate(repdf, rel.abund = abund/tot.abund)

write.csv(repdf, file = paste0('export/flow-comp/', fset, '-fish-N.csv')
        , row.names = FALSE)

## Biomass ---------------------------------------------------------------------
## turning replist_biom into a df
repdf_biom <- ldply(replist_biom, function(x) {
    adply(x, c(1,2,3))
})

names(repdf_biom) <- c('species', 'year', 'stage', 'rep', 'biom')
repdf_biom <- filter(repdf_biom, stage != 'S1')
repdf_biom$year <- as.numeric(as.character(repdf_biom$year))

## calculating % of K
repdf_biom <- mutate(repdf_biom, pK = biom/K)

write.csv(repdf_biom, file = paste0('export/flow-comp/', fset, '-fish-biom.csv')
        , row.names = FALSE)

### Local Variables:
### eval: (orgstruct-mode 1)
### orgstruct-heading-prefix-regexp: "## "
### End:

