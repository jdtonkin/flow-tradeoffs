# 5-guild riparian plant flow-population model

# This is a simplified version of the core model that is available here: 
# https://doi.org/10.6084/m9.figshare.4652608.v1
# and is described in: 
# Lytle, D. A., Merritt, D. M., Tonkin, J. D., Olden, J. D., & Reynolds, L. V., Linking
# river flow regimes to riparian plant guilds: a community-wide modeling approach,
# Ecological Applications, 27(4), 1338–1350 (2017).  http://dx.doi.org/10.1002/eap.1528

# In the original paper, we refer to 5 guilds: HT (Hydroriparian Tree), XS (Xeroriparian
# Shrub), HS (Hydroriparian Shrub), MM (Mesoriparian Meadow), and DS (Desert Shrub). 
# Here, we use different names, with local examples as follows: 
# HT: Cottonwood. Includes anything with 'C' or 'Cot'. e.g. 'Cgraph', 'Crep', 'DomC' 
# XS: Tamarisk. Includes anything with 'T' or 'Tam'. e.g. 'Tgraph', 'Trep', 'DomT'
# HS: Willow. Includes anything with 'W'. e.g. 'Wgraph', 'Wrep'
# MM: Meadow. Includes anything with 'M'. e.g. 'Mgraph', 'Mrep'
# DS: Sagebrush. Includes anything with 'S'. e.g. 'Sgraph', 'Srep'

# Selecting the scenario to run is done inside the outer loop
# ----------------------------------------------------------------------------------------

# Required libraries
library(ggplot2)
library(tidyr)
library(dplyr)

# SETUP ----------------------------------------------------------------------------------
rm(list = ls()) # clearing the workspace 
count <- 200 # number of years to project simulations (inner loop)
burnin = 100 # number of years to discard as burn in during long term mean estimation 
outerreps <- 1 # number of iterations for outer loop that alters drought/flood frequency 
replicates <- 1 # number of replicate projections to run (mid loop)

K <- 41770400

# 119344/30

# FLOOD REGIME DEFINITIONS ---------------------------------------------------------------
## Proportion of season available for cot and tam whenever a flood occurs in the model
## Simplified from Lytle et al. (2017): Here flood timing is assumed consistent each time.
cproportion <- .9
tproportion <- .4

# ITERATION PARAMETERS -------------------------------------------------------------------
# Setting up arrays/vectors to fill with data from loops

# Inner loop details ---------------------------------------------------------------------
# 'count' - number of years to project simulations (inner loop)

# Output of no. ind. for each age class for each year projected into the future 
# An array with 6 columns (each age class) and however many rows there are years projected 
Coutput <- array(0, dim = c(count, 6)) # Cottonwood
Toutput <- array(0, dim = c(count, 6)) # Tamarisk
Woutput <- array(0, dim = c(count, 6)) # Willow
Soutput <- array(0, dim = c(count, 6)) # Sagebrush
Moutput <- array(0, dim = c(count, 6)) # Meadow

# Total cottonwood pop. size as % of K 
# This is the total space occupied by this species in cottonwood seedling units 
Cspaceoutput <- numeric(length = count) # Cottonwood
Tspaceoutput <- numeric(length = count) # Tamarisk
Wspaceoutput <- numeric(length = count) # Willow
Sspaceoutput <- numeric(length = count) # Sagebrush
Mspaceoutput <- numeric(length = count) # Meadow

# Flood and drought settings for each year projected into the future (i.e. 0 or 1) 
floodoutput <- numeric(length = count) # flood
droughtoutput <- numeric(length = count) # drought
nonfloodoutput <- numeric(length = count) # nonflood
normaloutput <- numeric(length = count) # normal

# Total pop. size as % of K WITHOUT SEEDLINGS for each year projected into future
# contains %K of each guild except for seedlings for each year of projection
Cnonseedling <- numeric(length = count) 
Tnonseedling <- numeric(length = count)
Wnonseedling <- numeric(length = count)
Snonseedling <- numeric(length = count)
Mnonseedling <- numeric(length = count)

# No. ind. at stages as per Merritt and Poff 2010
# Vector of cot age class 5 for each year projected 
DomC <- numeric(length = count) # these will record the NUMBER of individuals in stg 5,
# which are 5-10 year olds
DomT <- numeric(length = count) # same, for stages 4, although this is 7-15 year olds 

# Mid loop details -----------------------------------------------------------------------
# 'replicates' - number of replicate projections to run (mid loop)

# Mean values for each rep. run over the period specified from burnin to end of projection
# Mean density of each guild WITHOUT seedlings included - each replicate run
Crep <- numeric(length = replicates)
Trep <- numeric(length = replicates)
Wrep <- numeric(length = replicates)
Srep <- numeric(length = replicates)
Mrep <- numeric(length = replicates)

# Mean density of each guild WITH seedlings included
Crep_all <- numeric(length = replicates)
Trep_all <- numeric(length = replicates)
Wrep_all <- numeric(length = replicates)
Srep_all <- numeric(length = replicates)
Mrep_all <- numeric(length = replicates)

# Mean of DomC for each of the replicate runs
DomCrep <- numeric(length = replicates) # to record output of DomC from flow scenarios
DomTrep <- numeric(length = replicates)

# Outer loop details ---------------------------------------------------------------------
# 'outerreps' - number of iterations for outer loop that alters drought/flood frequency 
# Results of flow mod scenarios. Not useful unless simulating changes to flow regime
# This is the mean of each flow mod. setting for the full burnin->end of projection period

# No seedlings for each of the flow mod settings
Cgraph <- numeric(length = outerreps) 
Tgraph <- numeric(length = outerreps)
Mgraph <- numeric(length = outerreps)
Wgraph <- numeric(length = outerreps)
Sgraph <- numeric(length = outerreps)

# All incl. seedlings for each of the flow mod settings
Cgraph_all <- numeric(length = outerreps)
Tgraph_all <- numeric(length = outerreps)
Mgraph_all <- numeric(length = outerreps)
Wgraph_all <- numeric(length = outerreps)
Sgraph_all <- numeric(length = outerreps)

# Mean of DomC for each of the flow mod settings
DomCrep_graph <- numeric(length = outerreps)
DomTrep_graph <- numeric(length = outerreps)

# Proportion of flow years in each flow mod setting
droughtgraph <- numeric(length = outerreps) # droughts
floodgraph <- numeric(length = outerreps) # floods
nonfloodgraph <- numeric(length = outerreps) # nonfloods
normalgraph <- numeric(length = outerreps) # normal years

# Setting flow scenario change to none to begin with 
# The outer loop iterates these one step at a time until it reaches 'outerreps'
droughtchanged = floodchanged = 0

# Checks to see if at least one adult is present
adult_func <- function(x) { 
    ifelse(x > .99999, 1, 0)
}

# Keeps FC6 from dividing by zero by substituting an arbitrary nonzero number that will get
# multiplied by zero later anyway during matrix multiplication
nonind <- function(x) {
    ifelse(x == 0, 666, x)
}

# checkpos makes sure that the K-occupied term is positive, assigns 0 if not
checkpos <- function(x) {
    ifelse(x < 0, 0, x)
}

# Quasi extinction threshold of 1, keeps pop from asymptoting infinitely to zero 
quasi <- function(x) {
    ifelse(x < 1, 0, x)
}

# Quasi rescue function that keeps species from disappearing, 
# e.g. sagebrush that can encroach from uplands 
quasiten <- function(x) {
    ifelse(x < 10, 10, x)
}

# OUTER LOOP #############################################################################

for(zim in 1:outerreps) {

    ## -----------------------------------------------------------------------------------
    ## Simulation choices begin here #####################################################
    ## -----------------------------------------------------------------------------------
    
    ## Comment or uncomment each scenario
    ## (e.g. RIP_OPT is one scenario, including all text down to 'nonflood')
    ## 'DRT' and 'HOM' weren't used in final runs, but remain here as examples
    ## These three are the chosen vectors for optimizing cot. 

    ## ## RIP_OPT
    ## fset <- 'RIP_OPT'
    ## gap <- 5
    ## ## gap 5 (6y return) chosen for plants. Keeps cot high, tam low, and eradicates DS. 
    ## bigflood <- rep(c(rep(0, gap), 1), length.out = 200)
    ## drought <- rep(c(rep(0, gap-1), 1, 0), length.out = 200)
    ## nonflood <- ifelse(bigflood + drought == 1, 0, 1)

    ## ## FSH_OPT
    ## fset <- 'FSH_OPT'
    ## bigflood <- rep(1, length.out = 200)
    ## drought <- rep(0, length.out = 200)
    ## nonflood <- rep(0, length.out = 200)

    ## INV_OPT
    fset <- 'INV_OPT'
    bigflood <- rep(0, length.out = 200)
    drought <- rep(0, length.out = 200)
    nonflood <- rep(1, length.out = 200)
    
    ## ## HOM
    ## fset <- 'HOM'
    ## bigflood <- rep(c(rep(0, 4), 1, rep(0, 5)), length.out = 200)
    ## drought <- rep(0, length.out = 200)
    ## nonflood <- ifelse(bigflood + drought == 1, 0, 1)

    ## ## DRT
    ## fset <- 'DRT'
    ## bigflood <- rep(c(rep(0, 4), 1), length.out = 200)
    ## drought <- rep(c(1, 0, 1, 1, 0), length.out = 200)
    ## nonflood <- ifelse(bigflood + drought == 1, 0, 1)

    ## ## Natural flow (NAT) ----------------------------------------------------------
    ## fset <- 'NAT'
    ## ## Using the same data from fish. Bigflood is SP highflood. 
    ## ## When using natural flow data, just pull it out from the list here
    ## ## Bringing in flow data
    ## all.scenarios.list <- readRDS('data/all_scenarios_list.rds')
   
    ## flowdata <- all.scenarios.list$natural.flow
    
    ## ## repeating the df out to 200 y
    ## flowdata <- mefa:::rep.data.frame(flowdata, 5)
    ## flowdata <- flowdata[1:200,]
    ## flowdata$rep <- row.names(flowdata)
    
    ## bigflood <- flowdata$SP_highflood
    ## drought <- flowdata$drought
    ## nonflood <- ifelse(bigflood + drought == 1, 0, 1)
    
    # ------------------------------------------------------------------------------------

    ## -----------------------------------------------------------------------------------
    ## Simulation choices end here #######################################################
    ## -----------------------------------------------------------------------------------
    
    flooddf <- cbind(bigflood, drought, nonflood)
    head(flooddf, 30)
    apply(flooddf, 1, sum)
    apply(flooddf, 2, sum)
    flooddf
    bigflood + nonflood + drought

# MIDDLE LOOP ############################################################################
# Middle loop uses iterator "rep" to get "replicates" number of runs for averaging
for(rep in 1:replicates) {

# VITAL RATES ----------------------------------------------------------------------------

# K is total area available to cottonwood or tamarisk initially calculated as total area 
# occupied by cottonwood. 

# VITAL RATES - cottonwood ---------------------------------------------------------------

# Stage specific densities
denC1 <- 350 
denC2 <- 10
denC3 <- 1
denC4 <- .91
denC5 <- .6
denC6 <- .12

# "Self thinning" rates, or equivalency rules, for stage transitions 
bC1 <- denC2/denC1
bC2 <- denC3/denC2
bC3 <- denC4/denC3
bC4 <- denC5/denC4
bC5 <- denC6/denC5

# Baseline maturation probabiliity, aC6 (adult senescence rate)
aC1 <- 1
aC2 <- 1 
aC3 <- 1 
aC4 <- 1
aC5 <- .167
aC6 <- .03

# Flood mortality in a flood year
SC1 <- .97
SC2 <- .33
SC3 <- .224
SC4 <- .19
SC5 <- .073
SC6 <- .02

# Drought mortality in a drought year 
DC1 <- .49
DC2 <- .16
DC3 <- .083
DC4 <- .05
DC5 <- .05 
DC6 <- .05

# Initial area in m2. Here, it is calculated based ONLY on cottonwood.
areaC1 <- 3978
areaC2 <- 3978
areaC3 <- 3978
areaC4 <- 3978
areaC5 <- 3978
areaC6 <- 3978 

# K <- (areaC1 + areaC2 + areaC3 + areaC4 + areaC5 + areaC6) * denC1

# VITAL RATES - TAMARISK -----------------------------------------------------------------

# K is common to both cot and tam

# Stage specific densities, number per m2
denT1 <- 400
denT2 <- 29
denT3 <- 4.5
denT4 <- 1.4
denT5 <- 1.3
denT6 <- 1.3

# "Self thinning" rates, or equivalency rules, for stage transitions
bT1 <- denT2/denT1
bT2 <- denT3/denT2
bT3 <- denT4/denT3
bT4 <- denT5/denT4
bT5 <- denT6/denT5

# Baseline maturation probability, aT6 (adult senescence rate)
aT1 <- 1
aT2 <- 1
aT3 <- .25
aT4 <- .11
aT5 <- .07
aT6 <- .05

# Flood mortality in a flood year
ST1 <- .9
ST2 <- .55
ST3 <- .25
ST4 <- .05
ST5 <- .01
ST6 <- .01

# Drought mortality in a drought year
DT1 <- .5
DT2 <- .15
DT3 <- .05
DT4 <- .025
DT5 <- .025
DT6 <- .025

# Initial area in m2. Not included in initial K calculation
areaT1 <- 3978
areaT2 <- 3978
areaT3 <- 3978
areaT4 <- 3978
areaT5 <- 3978
areaT6 <- 3978

# VITAL RATES - willow -------------------------------------------------------------------

# Stage specific densities, number per m2
# THESE NUMBERS ARE ARBITRARY, but reflect a final adult size/spacing of 1 m2 per plant
denW1 <- 350
denW2 <- 35
denW3 <- 1
denW4 <- 1
denW5 <- 1
denW6 <- 1

# "Self thinning" rates, or equivalency rules, for stage transitions
bW1 <- denW2/denW1
bW2 <- denW3/denW2
bW3 <- denW4/denW3
bW4 <- denW5/denW4
bW5 <- denW6/denW5

# Baseline maturation probabiliity, aW6 is adult senescence rate
# Only aW6 is different from cottonwood (faster)
aW1 <- 1
aW2 <- 1
aW3 <- 1
aW4 <- 1
aW5 <- .167
aW6 <- .01

# Flood mortality in a flood year. HALF THAT OF COTTONWOOD
SW1 <- .49
SW2 <- .17
SW3 <- .11
SW4 <- .10
SW5 <- .04
SW6 <- .01

# Drought mortality in a drought year 
# Basically, SURVIVORSHIP rate is half that of cottonwood: convert mortality to 
# survivorship, divide by two, convert back to mortality
DW1 <- .75
DW2 <- .58
DW3 <- .54
DW4 <- .53
DW5 <- .51
DW6 <- .51

# Initial area in m2. Not included in initial K calculation
areaW1 <- 3978
areaW2 <- 3978
areaW3 <- 3978
areaW4 <- 3978
areaW5 <- 3978
areaW6 <- 3978

# VITAL RATES - sagebrush ----------------------------------------------------------------

# Stage specific densities, number per m2
# THESE NUMBERS ARE ARBITRARY, but reflect an adult size/spacing of 1 m2
denS1 <- 350
denS2 <- 35
denS3 <- 1
denS4 <- 1
denS5 <- 1
denS6 <- 1

# "Self thinning" rates, or equivalency rules, for stage transitions
bS1 <- denS2/denS1
bS2 <- denS3/denS2
bS3 <- denS4/denS3
bS4 <- denS5/denS4
bS5 <- denS6/denS5

# Baseline maturation probabiliity, aS6 is adult senescence rate. 
# SAME AS COTTONWOOD
aS1 <- 1
aS2 <- 1
aS3 <- 1
aS4 <- 1
aS5 <- .167
aS6 <- .03

# Flood mortality in a flood year 
# Taken by taking survivorship of cot (1-S), dividing by 2, and coverting back to mort. 
# So, flood SURVIVORSHIP is 1/2 that of cottonwood 
SS1 <- .99
SS2 <- .67
SS3 <- .61
SS4 <- .60
SS5 <- .54
SS6 <- .51

# Drought MORTALITY in a drought year. ONE HALF COTTONWOOD RATES
DS1 <- .24
DS2 <- .08
DS3 <- .042
DS4 <- .025
DS5 <- .005
DS6 <- .005

# Initial area in m2. Not included in initial K calculation
areaS1 <- 3978
areaS2 <- 3978
areaS3 <- 3978
areaS4 <- 3978
areaS5 <- 3978
areaS6 <- 3978

# VITAL RATES - Meadow -------------------------------------------------------------------

# Stage specific densities, number per m2
# THESE NUMBERS ARE ARBITRARY, but reflect an adult size/spacing of 1 m2
denM1 <- 350
denM2 <- 35
denM3 <- 1
denM4 <- 1
denM5 <- 1
denM6 <- 1

# "Self thinning" rates, or equivalency rules, for stage transitions
bM1 <- denM2/denM1
bM2 <- denM3/denM2
bM3 <- denM4/denM3
bM4 <- denM5/denM4
bM5 <- denM6/denM5

# Baseline maturation probabiliity, aM6 is adult senescence rate. SAME AS COTTONWOOD
aM1 <- 1
aM2 <- 1
aM3 <- 1
aM4 <- 1
aM5 <- .167 
aM6 <- .03

# Flood mortality in a flood year 
# Taken by taking survivorship of cot (1-S), dividing by 2, and coverting back to mort. 
# FOR STAGES 1-3 ONLY 
# So, flood SURVIVORSHIP is 1/2 that of cottonwood for those stages, reflecting instablity
# of meadow habitats in highly flood-prone situations.
SM1 <- .99
SM2 <- .67
SM3 <- .61
SM4 <- .19
SM5 <- .073
SM6 <- .02

# Drought MORTALITY in a drought year 
# ONE HALF COTTONWOOD RATES in stages 4-6 only, reflecting greater drought tolerance in
# established meadows and less groundwater dependence 
DM1 <- .49
DM2 <- .16
DM3 <- .083
DM4 <- .025
DM5 <- .005
DM6 <- .005

# Initial area in m2. Not included in initial K calculation
areaM1 <- 3978
areaM2 <- 3978
areaM3 <- 3978
areaM4 <- 3978
areaM5 <- 3978
areaM6 <- 3978

# Maybell area in m2, as established from initial cottonwood occupancy 
# K is the total area available for cot OR tam expressed in cottonwood seedlings per m2 
NC <- c(areaC1 * denC1, 
        areaC2 * denC2, 
        areaC3 * denC3, 
        areaC4 * denC4, 
        areaC5 * denC5, 
        areaC6 * denC6)
# NC gives the total number of individuals for each age class.
# Initially here, this is found by multiplying the number of m2 occupied by a given class
# by the the density per m2 

NT <- c(areaT1 * denT1, 
        areaT2 * denT2, 
        areaT3 * denT3, 
        areaT4 * denT4, 
        areaT5 * denT5, 
        areaT6 * denT6)

NW <- c(areaW1 * denW1, 
        areaW2 * denW2, 
        areaW3 * denW3, 
        areaW4 * denW4, 
        areaW5 * denW5, 
        areaW6 * denW6)

NS <- c(areaS1 * denS1, 
        areaS2 * denS2, 
        areaS3 * denS3, 
        areaS4 * denS4, 
        areaS5 * denS5, 
        areaS6 * denS6)

NM <- c(areaM1 * denM1, 
        areaM2 * denM2, 
        areaM3 * denM3, 
        areaM4 * denM4, 
        areaM5 * denM5, 
        areaM6 * denM6)  

# Inner loop #############################################################################
for(i in 1:count) {

y = i #sample(nrow(flowdata), 1) 
# y is a random number within the length of the flow data to randomly select a year from 
# the 'bigflood' and 'drought' vector 
# in this case anything between 1 and 83 in the maybell data

# VITAL RATE DEFINITIONS: cottonwood -----------------------------------------------------
# G is prob. of transition to next stage
# P is prob. of remaining in that stage
GC1 <- aC1 * bC1 * (1 - bigflood[y] * SC1) * (1 - drought[y] * DC1) 
GC2 <- aC2 * bC2 * (1 - bigflood[y] * SC2) * (1 - drought[y] * DC2) 
GC3 <- aC3 * bC3 * (1 - bigflood[y] * SC3) * (1 - drought[y] * DC3) 
GC4 <- aC4 * bC4 * (1 - bigflood[y] * SC4) * (1 - drought[y] * DC4) 
GC5 <- aC5 * bC5 * (1 - bigflood[y] * SC5) * (1 - drought[y] * DC5) 
PC5 <- (1 - aC5) * (1 - bigflood[y] * SC5) * (1 - drought[y] * DC5) 
PC6 <- (1 - aC6) * (1 - bigflood[y] * SC6) * (1 - drought[y] * DC6) 

# VITAL RATE DEFINITIONS: tamarisk -------------------------------------------------------
GT1 <- aT1 * bT1 * (1 - bigflood[y] * ST1) * (1 - drought[y] * DT1)
GT2 <- aT2 * bT2 * (1 - bigflood[y] * ST2) * (1 - drought[y] * DT2) 
GT3 <- aT3 * bT3 * (1 - bigflood[y] * ST3) * (1 - drought[y] * DT3) 
GT4 <- aT4 * bT4 * (1 - bigflood[y] * ST4) * (1 - drought[y] * DT4) 
GT5 <- aT5 * bT5 * (1 - bigflood[y] * ST5) * (1 - drought[y] * DT5) 
PT3 <- (1 - aT3) * (1 - bigflood[y] * ST3) * (1 - drought[y] * DT3)
PT4 <- (1 - aT4) * (1 - bigflood[y] * ST4) * (1 - drought[y] * DT4)
PT5 <- (1 - aT5) * (1 - bigflood[y] * ST5) * (1 - drought[y] * DT5)
PT6 <- (1 - aT6) * (1 - bigflood[y] * ST6) * (1 - drought[y] * DT6)

# VITAL RATE DEFINITIONS: willow ---------------------------------------------------------
GW1 <- aW1 * bW1 * (1 - bigflood[y] * SW1) * (1 - drought[y] * DW1)
GW2 <- aW2 * bW2 * (1 - bigflood[y] * SW2) * (1 - drought[y] * DW2) 
GW3 <- aW3 * bW3 * (1 - bigflood[y] * SW3) * (1 - drought[y] * DW3) 
GW4 <- aW4 * bW4 * (1 - bigflood[y] * SW4) * (1 - drought[y] * DW4) 
GW5 <- aW5 * bW5 * (1 - bigflood[y] * SW5) * (1 - drought[y] * DW5) 
PW5 <- (1 - aW5) * (1 - bigflood[y] * SW5) * (1 - drought[y] * DW5) 
PW6 <- (1 - aW6) * (1 - bigflood[y] * SW6) * (1 - drought[y] * DW6)

# VITAL RATE DEFINITIONS: sagebrush ------------------------------------------------------
GS1 <- aS1 * bS1 * (1 - bigflood[y] * SS1) * (1 - drought[y] * DS1)
GS2 <- aS2 * bS2 * (1 - bigflood[y] * SS2) * (1 - drought[y] * DS2) 
GS3 <- aS3 * bS3 * (1 - bigflood[y] * SS3) * (1 - drought[y] * DS3) 
GS4 <- aS4 * bS4 * (1 - bigflood[y] * SS4) * (1 - drought[y] * DS4) 
GS5 <- aS5 * bS5 * (1 - bigflood[y] * SS5) * (1 - drought[y] * DS5) 
PS5 <- (1 - aS5) * (1 - bigflood[y] * SS5) * (1 - drought[y] * DS5) 
PS6 <- (1 - aS6) * (1 - bigflood[y] * SS6) * (1 - drought[y] * DS6)

# VITAL RATE DEFINITIONS: meadow ---------------------------------------------------------
GM1 <- aM1 * bM1 * (1 - bigflood[y] * SM1) * (1 - drought[y] * DM1)
GM2 <- aM2 * bM2 * (1 - bigflood[y] * SM2) * (1 - drought[y] * DM2)
GM3 <- aM3 * bM3 * (1 - bigflood[y] * SM3) * (1 - drought[y] * DM3)
GM4 <- aM4 * bM4 * (1 - bigflood[y] * SM4) * (1 - drought[y] * DM4)
GM5 <- aM5 * bM5 * (1 - bigflood[y] * SM5) * (1 - drought[y] * DM5)
PM5 <- (1 - aM5) * (1 - bigflood[y] * SM5) * (1 - drought[y] * DM5)
PM6 <- (1 - aM6) * (1 - bigflood[y] * SM6) * (1 - drought[y] * DM6)

# FECUNDITY ------------------------------------------------------------------------------
# Assumes that if any breeding tam or cot is present, they will seed all recently-scoured 
# substrates, although success is scaled by the lognormal hydrograph drawdown functions.
# If cot and tam seedlings behave entirely independently as modeled here, this means that
# under the right conditions there can be overseeding such that if (K-occupied) is the 
# total amount of scoured habitat remaining, then up to 2*(K-occupied) could be colonized 
# by seedlings, half tam and half cot. One consequence of this is if an extended drought
# is followed by a good flood and then many growth years, populations could exceed K for 
# many years, and when pop > K, recruitment will not occur (although flood-related 
# mortality will keep lowering population sizes). 

# Post flood space occupied --------------------------------------------------------------
# postfloodC gives the amount of space occupied by cottonwood after the flood (AND 
# ACTUALLY AFTER DROUGHT OR NORMAL YEARS AS WELL!!!) IN COTTONWOOD SEEDLING UNITS
postfloodC <- 
    NC[1] * GC1/bC1 + 
    NC[2] * GC2/(bC2 * bC1) + 
    NC[3] * GC3/(bC3 * bC2 * bC1) + 
    NC[4] * GC4/(bC4 * bC3 * bC2 * bC1) + 
    NC[5] * PC5/(bC4 * bC3 * bC2 * bC1) + 
    NC[5] * GC5/(bC5 * bC4 * bC3 * bC2 * bC1) + 
    NC[6] * PC6/(bC5 * bC4 * bC3 * bC2 * bC1)

# postfloodT gives the amount of space occupied by tamarisk after the flood 
# IN TAMARISK SEEDLING UNITS
postfloodT  <-  
    NT[1] * GT1/bT1 + 
    NT[2] * GT2/(bT2 * bT1) + 
    NT[3] * GT3/(bT3 * bT2 * bT1) + 
    NT[3] * PT3/(bT2 * bT1) + 
    NT[4] * GT4/(bT4 * bT3 * bT2 * bT1) + 
    NT[4] * PT4/(bT3 * bT2 * bT1) + 
    NT[5] * GT5/(bT5 * bT4 * bT3 * bT2 * bT1) + 
    NT[5] * PT5/(bT4 * bT3 * bT2 * bT1) + 
    NT[6] * PT6/(bT5 * bT4 * bT3 * bT2 * bT1)

# postfloodW gives the amount of space occupied by willow after the flood 
# IN WILLOW SEEDLING UNITS
postfloodW <-  
    NW[1] * GW1/bW1 + 
    NW[2] * GW2/(bW2 * bW1) + 
    NW[3] * GW3/(bW3 * bW2 * bW1) + 
    NW[4] * GW4/(bW4 * bW3 * bW2 * bW1) + 
    NW[5] * PW5/(bW4 * bW3 * bW2 * bW1) + 
    NW[5] * GW5/(bW5 * bW4 * bW3 * bW2 * bW1) +
    NW[6] * PW6/(bW5 * bW4 * bW3 * bW2 * bW1)

# postfloodS gives the amount of space occupied by SAGEBRUSH after the flood 
# IN SAGEBRUSH SEEDLING UNITS
postfloodS <-  
    NS[1] * GS1/bS1 + 
    NS[2] * GS2/(bS2 * bS1) + 
    NS[3] * GS3/(bS3 * bS2 * bS1) +
    NS[4] * GS4/(bS4 * bS3 * bS2 * bS1) + 
    NS[5] * PS5/(bS4 * bS3 * bS2 * bS1) + 
    NS[5] * GS5/(bS5 * bS4 * bS3 * bS2 * bS1) + 
    NS[6] * PS6/(bS5 * bS4 * bS3 * bS2 * bS1)

# postfloodM gives the amount of space occupied by meadow after the flood 
# IN MEADOW SEEDLING UNITS
postfloodM <-  
    NM[1] * GM1/bM1 + 
    NM[2] * GM2/(bM2 * bM1) + 
    NM[3] * GM3/(bM3 * bM2 * bM1) + 
    NM[4] * GM4/(bM4 * bM3 * bM2 * bM1) + 
    NM[5] * PM5/(bM4 * bM3 * bM2 * bM1) + 
    NM[5] * GM5/(bM5 * bM4 * bM3 * bM2 * bM1) + 
    NM[6] * PM6/(bM5 * bM4 * bM3 * bM2 * bM1)

# POTENTIAL COTTONWOOD FECUNDITY ---------------------------------------------------------
FC6 <- checkpos((adult_func(NC[6])) * # checks to see if at least 1 adult is present
               (1/nonind(NC[6])) * 
               bigflood[y] * 
               cproportion * # no longer need [y] as a fixed proportion 
               ## decline[y] * 
               (K - (postfloodC + 
                     postfloodT * (denC1/denT1) + 
                     postfloodW * (denC1/denW1) + 
                     postfloodS * (denC1/denS1) + 
                     postfloodM * (denC1/denM1))))

# '(1/nonind(NC[6]))' keeps FC6 from dividing by zero by substituting an arbitrary non-0 
# number that will be multiplied by 0 later anyway during matrix multiplication

# This gives POTENTIAL MAX fecundity based on amount of bare substrate available AFTER
# that year's flood. 
# Reproduction is conditional on:
# 1. at least one reproductive cottonwood being present 
# 2. a big flood occurring 
# 3. flood during seedset window. 
# It is independent of # of repro adults, but it is scaled by the rate of flooddecline AND
# BY THE PROPORTION OF SEEDSET DAYS. 
# Number of new seedlings is determined by the total amount of bare substrates; 
# i.e. whatever is not occupied by surviving cottonwood OR OTHER SPECIES. 
# The denC1/denT1 term converts tam to cot seedling units and likewise for other 3 guilds. 
# The 1/nonind[NC[6]] term will cancel out with NC[6] during matrix projection. 
# i.e. number of seedlings is independent of number of mature trees. 

# POTENTIAL TAMARISK FECUNDITY -----------------------------------------------------------
FT <- checkpos((adult_func(NT[3] + NT[4] + NT[5] + NT[6])) *
               bigflood[y] * 
               tproportion * # no longer need [y] as a fixed proportion 
               ## tdecline[y] * 
               (denT1/denC1) * 
               (K - (postfloodC + 
                     postfloodT * (denC1/denT1) + 
                     postfloodW * (denC1/denW1) + 
                     postfloodS * (denC1/denS1) + 
                     postfloodM * (denC1/denM1))))
                 	   
# This gives fecundities of TAMARISK seedlings based on amount of bare substrates 
# available AFTER that year's flood. 
# Note that units are cot seedlings in the (K-occupied) term, but are then converted back 
# to tam seedlings.
# Repro is conditional on 
# 1. at least one repro tamarisk being present, 
# 2. a big flood occurring 
# 3. during seedset. 
# It is independent of # of repro adults, but it is scaled by the rate of flooddecline and
# by prop of seedset days. 
# Note that the independence of FT from breeding pop size is achieved differently here
# than for cot, it is "forced" during matrix iteration, below.

# WILLOW FECUNDITY -----------------------------------------------------------------------
FW <- checkpos((adult_func(NW[3] + NW[4] + NW[5] + NW[6])) *
               bigflood[y] * 
               (denW1/denC1) * 
               (K - (postfloodC + 
                     postfloodT * (denC1/denT1) + 
                     postfloodW * (denC1/denW1) + 
                     postfloodS * (denC1/denS1) + 
                     postfloodM * (denC1/denM1)))) 
# checks to make sure at least one stage 3 to 6 age individual is present, and that a 
# flood occurs. No dependence on drawdown or seedset timing. 

# SAGEBRUSH FECUNDITY --------------------------------------------------------------------
FS <- checkpos((adult_func(NS[2] + NS[3] + NS[4] + NS[5] + NS[6])) * 
               nonflood[y] * 
               (denS1/denC1) * 
               (K - (postfloodC + 
                     postfloodT * (denC1/denT1) + 
                     postfloodW * (denC1/denW1) + 
                     postfloodS * (denC1/denS1) + 
                     postfloodM * (denC1/denM1))))
# Sagebrush fecundity in # of sagebrush seedlings, to be added using "placeholder" to NS1
# during iteration. 
# Here, sagebrush can colonize any empty portion of K, but only during NONFLOOD years. 

# MEADOW FECUNDITY -----------------------------------------------------------------------
FM <- checkpos((adult_func(NM[2] + NM[3] + NM[4] + NM[5] + NM[6])) * 
               bigflood[y] * 
               (denM1/denC1) * 
               (K - (postfloodC + 
                     postfloodT * (denC1/denT1) + 
                     postfloodW * (denC1/denW1) + 
                     postfloodS * (denC1/denS1) + 
                     postfloodM * (denC1/denM1))))
# Meadow fecundity in # of seedlings, to be added using "placeholder" to NM1 during iter. 
# Here, meadow can colonize any empty portion of K.  

# K --------------------------------------------------------------------------------------
# Cottonwood
# gives total cottonwood population size as a percentage of K; 
# this is the total space occupied by this species in cottonwood seedling units
KC <- 100 * (NC[1] + 
             NC[2]/(bC1) + 
             NC[3]/(bC2 * bC1) + 
             NC[4]/(bC3 * bC2 * bC1) + 
             NC[5]/(bC4 * bC3 * bC2 * bC1) + 
             NC[6]/(bC5 * bC4 * bC3 * bC2 * bC1))/K 

# same as above, but without seedlings 
CnonseedK <- 100 * (NC[2]/(bC1) + 
                    NC[3]/(bC2 * bC1) + 
                    NC[4]/(bC3 * bC2 * bC1) + 
                    NC[5]/(bC4 * bC3 * bC2 * bC1) + 
                    NC[6]/(bC5 * bC4 * bC3 * bC2 * bC1))/K 
# Tamarisk
KT <- 100 * (denC1/denT1) * (NT[1] + 
                             NT[2]/(bT1) + 
                             NT[3]/(bT2 * bT1) + 
                             NT[4]/(bT3 * bT2 * bT1) + 
                             NT[5]/(bT4 * bT3 * bT2 * bT1) + 
                             NT[6]/(bT5 * bT4 * bT3 * bT2 * bT1))/K 

TnonseedK <- 100 * (denC1/denT1) * (NT[2]/(bT1) + 
                                    NT[3]/(bT2 * bT1) + 
                                    NT[4]/(bT3 * bT2 * bT1) + 
                                    NT[5]/(bT4 * bT3 * bT2 * bT1) + 
                                    NT[6]/(bT5 * bT4 * bT3 * bT2 * bT1))/K 
# Willow
KW <- 100 * (NW[1] + 
             NW[2]/(bW1) + 
             NW[3]/(bW2 * bW1) + 
             NW[4]/(bW3 * bW2 * bW1) + 
             NW[5]/(bW4 * bW3 * bW2 * bW1) + 
             NW[6]/(bW5 * bW4 * bW3 * bW2 * bW1))/K
    
WnonseedK  <- 100 * (NW[2]/(bW1) + 
                     NW[3]/(bW2 * bW1) + 
                     NW[4]/(bW3 * bW2 * bW1) + 
                     NW[5]/(bW4 * bW3 * bW2 * bW1) + 
                     NW[6]/(bW5 * bW4 * bW3 * bW2 * bW1))/K
# Sagebrush
KS  <- 100 * (NS[1] + 
              NS[2]/(bS1) + 
              NS[3]/(bS2 * bS1) + 
              NS[4]/(bS3 * bS2 * bS1) + 
              NS[5]/(bS4 * bS3 * bS2 * bS1) + 
              NS[6]/(bS5 * bS4 * bS3 * bS2 * bS1))/K
    
SnonseedK <- 100 * (NS[2]/(bS1) + 
                    NS[3]/(bS2 * bS1) + 
                    NS[4]/(bS3 * bS2 * bS1) + 
                    NS[5]/(bS4 * bS3 * bS2 * bS1) + 
                    NS[6]/(bS5 * bS4 * bS3 * bS2 * bS1))/K
# Meadow
KM <- 100 * (NM[1] + 
             NM[2]/(bM1) + 
             NM[3]/(bM2 * bM1) + 
             NM[4]/(bM3 * bM2 * bM1) + 
             NM[5]/(bM4 * bM3 * bM2 * bM1) + 
             NM[6]/(bM5 * bM4 * bM3 * bM2 * bM1))/K
    
MnonseedK <- 100 * (NM[2]/(bM1) + 
                    NM[3]/(bM2 * bM1) + 
                    NM[4]/(bM3 * bM2 * bM1) + 
                    NM[5]/(bM4 * bM3 * bM2 * bM1) + 
                    NM[6]/(bM5 * bM4 * bM3 * bM2 * bM1))/K

# TRANSITION MATRICES --------------------------------------------------------------------

# TRANSITION MATRIX FOR cottonwood -------------------------------------------------------
AC1 <- c(0, 0, 0, 0, 0, FC6)
AC2 <- c(GC1, 0, 0, 0, 0, 0)
AC3 <- c(0, GC2, 0, 0, 0, 0)
AC4 <- c(0, 0, GC3, 0, 0, 0)
AC5 <- c(0, 0, 0, GC4, PC5, 0)
AC6 <- c(0, 0, 0, 0, GC5, PC6)
# Matrix
AC <- rbind(AC1, AC2, AC3, AC4, AC5, AC6)

# TRANSITION MATRIX FOR tamarisk ---------------------------------------------------------
# Note: fecundity is not included here, since it is assigned directly during iteration
AT1 <- c(0, 0, 0, 0, 0, 0)
AT2 <- c(GT1, 0, 0, 0, 0, 0)
AT3 <- c(0, GT2, PT3, 0, 0, 0)
AT4 <- c(0, 0, GT3, PT4, 0, 0)
AT5 <- c(0, 0, 0, GT4, PT5, 0)
AT6 <- c(0, 0, 0, 0, GT5, PT6)
# Matrix
AT <- rbind(AT1, AT2, AT3, AT4, AT5, AT6)

# TRANSITION MATRIX FOR dynamic riverbank specialist, willow -----------------------------
# Similar stage structure to cot, except that reproduction can occur in all but 1st yr
AW1 <- c(0, 0, 0, 0, 0, 0)
AW2 <- c(GW1, 0, 0, 0, 0, 0)
AW3 <- c(0, GW2, 0, 0, 0, 0)
AW4 <- c(0, 0, GW3, 0, 0, 0)
AW5 <- c(0, 0, 0, GW4, PW5, 0)
AW6 <- c(0, 0, 0, 0, GW5, PW6)
# Matrix
AW <- rbind(AW1, AW2, AW3, AW4, AW5, AW6)

# TRANSITION MATRIX FOR sagebrush --------------------------------------------------------
# Arid shrubland indicator big sagebrush.
# Similar stage structure to cottonwood, fecundity is assigned directly as with tamarisk
# during iteration.
AS1 <- c(0, 0, 0, 0, 0, 0)
AS2 <- c(GS1, 0, 0, 0, 0, 0)
AS3 <- c(0, GS2, 0, 0, 0, 0)
AS4 <- c(0, 0, GS3, 0, 0, 0)
AS5 <- c(0, 0, 0, GS4, 0, 0)
AS6 <- c(0, 0, 0, 0, GS5, PS6)
# Matrix
AS <- rbind(AS1, AS2, AS3, AS4, AS5, AS6)

# TRANSITION MATRIX FOR 5th species, xeric meadow ----------------------------------------
# Includes grasses such as wheatgrass.
# Similar stage structure to cottonwood, fecundity is assigned directly as with tamarisk
# during iteration.
AM1 <- c(0, 0, 0, 0, 0, 0)
AM2 <- c(GM1, 0, 0, 0, 0, 0)
AM3 <- c(0, GM2, 0, 0, 0, 0)
AM4 <- c(0, 0, GM3, 0, 0, 0)
AM5 <- c(0, 0, 0, GM4, 0, 0)
AM6 <- c(0, 0, 0, 0, GM5, PM6)
# Matrix
AM <- rbind(AM1, AM2, AM3, AM4, AM5, AM6)

# COMPILING OUTPUTS ----------------------------------------------------------------------
# Cottonwood
Coutput[i,1:6] <- log(NC + 1) # array of no. ind. of each age class for each yr 
# projected. NC = total no. ind. for each age class
DomC[i] <- NC[5] # vector of cot age class 5 for each year projected 
Cnonseedling[i] <- CnonseedK # total cottonwood pop. size as % of K WITHOUT SEEDLINGS for 
# each projected year
Cspaceoutput[i] <- KC # total cottonwood pop. size as % of K; this is the total space 
# occupied by this species in cottonwood seedling units

# Tamarisk - same as cottonwood
Toutput[i,1:6] <- log(NT + 1) 
DomT[i] <- NT[4]
Tnonseedling[i] <- TnonseedK
Tspaceoutput[i] <- KT

# Willow
Woutput[i,1:6] <- log(NW + 1)
Wnonseedling[i] <- WnonseedK
Wspaceoutput[i] <- KW

# Sagebrush
Soutput[i,1:6] <- log(NS + 1)
Snonseedling[i] <- SnonseedK 
Sspaceoutput[i] <- KS

# Meadow
Moutput[i,1:6] <- log(NM + 1)
Mnonseedling[i] <- MnonseedK
Mspaceoutput[i] <- KM

# Records flood settings of each particular projected year (0 for nonflood, 1 for flood)
floodoutput[i] <- bigflood[y]

# Same for drought
droughtoutput[i] <- drought[y]

# Same for nonflood
nonfloodoutput[i] <- nonflood[y]

# Same for normal
normaloutput[i] <- ifelse(bigflood[y] == 0 & drought[y] == 0 & nonflood[y] == 1, 1, 0)

# Fecundity of all but cottonwood to put into matrix projection. 
# Cottonwood is already in matrix
Tplaceholder <- FT
Splaceholder <- FS
Wplaceholder <- FW
Mplaceholder <- FW

# MATRIX MULTIPLICATION ------------------------------------------------------------------
# Cottonwood
NC <- AC %*% NC # AC is transition matrix, NC = total no. ind. for each age class
NC <- quasi(NC) # quasi extinction threshold of 1: below 1 go to 0

# Tamarisk
# Note the use of 'placeholders' for fecundity in the following guilds
NT <- AT %*% NT
NT[1] <- Tplaceholder
NT <- quasi(NT)

# Willow
NW <- AW %*% NW
NW[1] <- Wplaceholder
NW <- quasi(NW)

# Sagebrush
NS <- AS %*% NS 
NS[1] <- Splaceholder
NS <- quasiten(NS)

# Meadow
NM <- AM %*% NM
NM[1] <- Mplaceholder
NM <- quasi(NM)
} # End of inner loop ####################################################################

# Mean vals for each replicate run over period specified from burning to end of projection
# No seedlings
Crep[rep] <- mean(Cnonseedling[seq(burnin + 1, count)])
Trep[rep] <- mean(Tnonseedling[seq(burnin + 1, count)])
Wrep[rep] <- mean(Wnonseedling[seq(burnin + 1, count)])
Srep[rep] <- mean(Snonseedling[seq(burnin + 1, count)])
Mrep[rep] <- mean(Mnonseedling[seq(burnin + 1, count)])

# All incl. seedlings
Crep_all[rep] <- mean(Cspaceoutput[seq(burnin + 1, count)])
Trep_all[rep] <- mean(Tspaceoutput[seq(burnin + 1, count)])
Wrep_all[rep] <- mean(Wspaceoutput[seq(burnin + 1, count)])
Srep_all[rep] <- mean(Sspaceoutput[seq(burnin + 1, count)])
Mrep_all[rep] <- mean(Mspaceoutput[seq(burnin + 1, count)])

# Stage 5 cot, stage 4 tam - as per Merritt and Poff 2010
DomCrep[rep] <- mean(DomC[seq(burnin + 1, count)]) 
DomTrep[rep] <- mean(DomT[seq(burnin + 1, count)]) 

} # End of mid loop ######################################################################

# Outer loop compilation - results of flow mod scenarios. Not useful unless simulating 
# changes to flow regime
# Adults - mean of Crep (Mean values for each replicate run over the period specified from
# burnin to end of projection)
# This is then the mean of each flow modification setting for the full burnin -> end of 
# projection period. 

# No seedlings
Cgraph[zim] <- mean(Crep)
Tgraph[zim] <- mean(Trep)
Wgraph[zim] <- mean(Wrep)
Sgraph[zim] <- mean(Srep)
Mgraph[zim] <- mean(Mrep)

# All incl. seedlings
Cgraph_all[zim] <- mean(Crep_all)
Tgraph_all[zim] <- mean(Trep_all)
Wgraph_all[zim] <- mean(Wrep_all)
Sgraph_all[zim] <- mean(Srep_all)
Mgraph_all[zim] <- mean(Mrep_all)

# Stage 5 cot, stage 4 tam - as per Merritt and Poff 2010
DomCrep_graph[zim] <- mean(DomCrep)
DomTrep_graph[zim] <- mean(DomTrep)

# Proportion of drought years in model run
droughtgraph[zim] <- sum(drought)/length(bigflood)
    
# Proportion of flood years in model run
floodgraph[zim] <- sum(bigflood)/length(bigflood)
    
# Proportion of nonflood years in model run
nonfloodgraph[zim] <- sum(nonflood)/length(bigflood)
    
# Proportion of normal years in model run (not floods and not droughts)
normalgraph[zim] <- 
    sum(ifelse(bigflood == 0 & drought == 0 & nonflood == 1, 1, 0))/length(bigflood)

# Adding 1 to droughtchanged and floodchanged - this keeps going until reaching the number
# specified in outerreps (i.e. 84 mods all years)
droughtchanged = floodchanged = droughtchanged + 1

} # End outer loop #######################################################################

# ########################################################################################
# OUTPUT ---------------------------------------------------------------------------------
# ########################################################################################

# Exporting single replicate which does NOT incl. seedlings
# 5 taxa, 200 years, that's all - no need for the 'rep' loop, but remains for consistency
# with the original model. 
space_df_ns <- as.data.frame(cbind(Cnonseedling, Tnonseedling, Wnonseedling, Snonseedling,
                                Mnonseedling))
# Adding a year column
space_df_ns$year <- as.numeric(as.character(row.names(space_df_ns)))

write.csv(space_df_ns, paste0('export/flow-comp/', fset, '-rip-ns.csv'),
          row.names = FALSE)

# END ------------------------------------------------------------------------------------
