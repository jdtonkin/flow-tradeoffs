## Compiling results from the various simulations into figures

## -----------------------------------------------------------------------------
## * Header stuff --------------------------------------------------------------
## -----------------------------------------------------------------------------

## Required libraries
library(ggplot2)
library(plyr)
library(tidyverse)
library(ggsci) # colour themes
library(circlize) # chord diagram
library(png) # for adding images to plots
library(grid)

rm(list = ls())

## Colours
## viridis with black for natural 
col <- c("Invertebrates" = "#440154FF",
        "Fish" = "#31688EFF",
        "Plants" = "#35B779FF",
        "Natural flow" = "grey")

## Loading functions from functions.R file -------------------------------------
source('code/dod-3taxa-fish-functions.R')

## read all files in the folder ------------------------------------------------
## Getting the filenames
filenames <- list.files('export/flow-comp/',
                        pattern = '*.csv',
                        full.names = TRUE)
filenames.s <- list.files('export/flow-comp/',
                          pattern = '*.csv',
                          full.names = FALSE)

## removing the .csv at the end
basenames <- gsub("\\.csv$","", filenames.s)

# reading all files in through lapply on the list of filenames
all.sims <- lapply(filenames, function(x) read.csv(x))

## adding names to dataframes as need them later
names(all.sims) <- basenames

## -----------------------------------------------------------------------------
## * Fish biom -----------------------------------------------------------------
## -----------------------------------------------------------------------------

## selecting fish biomass sims
fish.biom.sims <- all.sims[grep("-fish-biom$", names(all.sims))]

## combining into a single dataframe
fish.biom.sims.df <- plyr::ldply(fish.biom.sims)

## adding new column for scenario without 'fish-biom'
fish.biom.sims.df <- fish.biom.sims.df %>%
    separate(`.id`,
             c('scenario', 'org', 'biom_n'),
             sep = '-',
             remove = FALSE)

## Fish biom compilation -------------------------------------------------------

## Taking mean results
fish.means.biom <- fish.biom.sims.df %>%
    group_by(scenario, year, rep, species) %>% # combining stages
    summarise(biom = sum(biom),
              pK = sum(pK)) %>%
    ungroup() %>%
    group_by(scenario, species, year) %>%
    summarise(mean.biom = mean(biom),
              sd.biom = sd(biom),
              se.biom = sd(biom)/sqrt(max(rep)),
              mean.pK = mean(pK),
              sd.pK = sd(pK),
              se.pK = sd(pK)/sqrt(max(rep))) %>%
    ungroup()

## native vs. nonnative
fish.means.biom.NN <- fish.biom.sims.df %>%
    mutate(
        NN = case_when(
            species == 'CACL' |
            species == 'CAIN' |
            species == 'GIRO' ~ 'Native',
            TRUE ~ 'Non-native'
        )
    ) %>%
    group_by(scenario, year, rep, NN) %>% # combining stages
    summarise(biom = sum(biom),
              pK = sum(pK)) %>%
    ungroup() %>%
    group_by(scenario, NN, year) %>%
    summarise(mean.biom = mean(biom),
              sd.biom = sd(biom),
              se.biom = sd(biom)/sqrt(max(rep)),
              mean.pK = mean(pK),
              sd.pK = sd(pK),
              se.pK = sd(pK)/sqrt(max(rep))) %>%
    ungroup()

fish.means.biom.mod <- fish.means.biom %>%
    mutate(scenario = case_when(
               scenario == 'RIP_OPT' ~ 'Plant\nprescription',
               scenario == 'FSH_OPT' ~ 'Fish\nprescription',
               scenario == 'INV_OPT' ~ 'Invertebrate\nprescription',
               scenario == 'NAT' ~ 'Natural flow'))

    
fish.means.biom.mod$scenario <- factor(fish.means.biom.mod$scenario,
                                       levels = c('Plant\nprescription',
                                                  'Fish\nprescription',
                                                  'Invertebrate\nprescription',
                                                  'Natural flow'))
fish.means.biom.mod$species <- factor(fish.means.biom.mod$species,
                                      levels = c('CACL',
                                                 'CAIN',
                                                 'GIRO',
                                                 'AMNA',
                                                 'CYLU',
                                                 'LECY',
                                                 'MIDO'))

fish.pK.trends <- ggplot(fish.means.biom.mod,
                         aes(year,
                             mean.pK,
                             colour = species,
                             fill = species)) +
    geom_ribbon(aes(ymin = mean.pK - 2 * se.pK,
                    ymax = mean.pK + 2 * se.pK),
                colour = 'transparent',
                alpha = .5,
                show.legend = FALSE) +
    geom_line(show.legend = FALSE) +
    facet_grid(scenario~species) +
    theme_classic_facet() +
    coord_cartesian(ylim = c(0,1)) +
    ylab('Percent of community carrying capacity') +
    xlab('Year') +
    scale_color_futurama() +
    scale_fill_futurama() 
fish.pK.trends

ggsave('export/flow-comp-figs/fish-200trend-pK-spp.pdf',
       width = 9, height = 7)

## -----------------------------------------------------------------------------
## * Riparian plants -----------------------------------------------------------
## -----------------------------------------------------------------------------

## selecting rip ns sims
rip.ns.sims <- all.sims[grep("-rip-ns$", names(all.sims))]

## combining into a single dataframe
rip.ns.sims.df <- plyr::ldply(rip.ns.sims)

## adding new column for scenario without 'rip-ns'
rip.ns.sims.df <- rip.ns.sims.df %>%
    separate(`.id`,
             c('scenario', 'org', 'ad_ns'),
             sep = '-',
             remove = FALSE)

## Riparian plants compilation -------------------------------------------------

rip.ns.sims.df.g <- gather(rip.ns.sims.df,
                           key,
                           value,
                           Cnonseedling:Mnonseedling)

rip.ns.sims.df.g.mod <- rip.ns.sims.df.g %>%
    mutate(scenario = case_when(
               scenario == 'RIP_OPT' ~ 'Plant\nprescription',
               scenario == 'FSH_OPT' ~ 'Fish\nprescription',
               scenario == 'INV_OPT' ~ 'Invertebrate\nprescription',
               scenario == 'NAT' ~ 'Natural flow')) %>%
    mutate(key = case_when(
               key == 'Cnonseedling' ~ 'Cottonwood',
               key == 'Tnonseedling' ~ 'Tamarisk',
               key == 'Wnonseedling' ~ 'Willow',
               key == 'Snonseedling' ~ 'Sagebrush',
               key == 'Mnonseedling' ~ 'Meadow'))

    
rip.ns.sims.df.g.mod$scenario <- factor(rip.ns.sims.df.g.mod$scenario,
                                        levels = c('Plant\nprescription',
                                                   'Fish\nprescription',
                                                   'Invertebrate\nprescription',
                                                   'Natural flow'))

rip.ns.sims.df.g.mod$key <- factor(rip.ns.sims.df.g.mod$key,
                                   levels = c('Cottonwood',
                                              'Tamarisk',
                                              'Willow',
                                              'Meadow',
                                              'Sagebrush'))

ggplot(rip.ns.sims.df.g.mod, aes(year, value, colour = key)) +
    geom_path(show.legend = FALSE) +
    facet_grid(scenario~key) +
    theme_classic_facet() +
    scale_colour_futurama() +
    xlab('Year') + ylab('Percent of community carrying capacity')
ggsave('export/flow-comp-figs/rip-200trend-ns.pdf',
       width = 9,
       height = 7)

## -----------------------------------------------------------------------------
## * Inverts -------------------------------------------------------------------
## -----------------------------------------------------------------------------

## selecting inverts sims
invert.sims <- all.sims$`ALL-SCENARIOS-inverts`

## reducing down to last 20 years to summarize on
invert.sims.20y <- invert.sims %>%
    filter(Time > 3650)

## getting means for dragons and mayflies only
inverts.2taxa.means <- invert.sims.20y %>%
    filter(taxa != 'Ostracod') %>%
    group_by(scenario, Time) %>%
    summarise(Nt = mean(Nt)) 

## Inverts compilation ---------------------------------------------------------

invert.sims.mod <- invert.sims %>%
    mutate(scenario = case_when(
               scenario == 'RIP_OPT' ~ 'Plant\nprescription',
               scenario == 'FSH_OPT' ~ 'Fish\nprescription',
               scenario == 'INV_OPT' ~ 'Invertebrate\nprescription',
               scenario == 'NAT' ~ 'Natural flow')) 

    
invert.sims.mod$scenario <-
    factor(invert.sims.mod$scenario,
           levels = c('Plant\nprescription',
                      'Fish\nprescription',
                      'Invertebrate\nprescription',
                      'Natural flow'))

invert.sims.mod$taxa <-
    factor(invert.sims.mod$taxa,
           levels = c('Mayfly',
                      'Dragonfly',
                      'Ostracod'))

## plotting all
ggplot(invert.sims.mod, aes(Time/365, Nt, colour = taxa)) +
    geom_line(show.legend = FALSE) +
    facet_grid(scenario~taxa) +
    theme_classic_facet() +
    scale_colour_futurama() +
    xlab('Year') + ylab('Percent of population carrying capacity')
ggsave('export/flow-comp-figs/inv-3taxa-20trend.pdf',
       width = 9, height = 7)

## -----------------------------------------------------------------------------
## * Summary sims plot ---------------------------------------------------------
## -----------------------------------------------------------------------------

fish.sims.perc <- fish.means.biom.NN %>%
    mutate(group = 'Fish') %>%
    mutate(percent = mean.pK * 100) %>%
    mutate(se.perc = se.pK * 100) %>%
    mutate(colour = ifelse(NN == 'Native',
                           col[['Fish']],
                           alpha('grey20', 0.15))) %>%
    select(scenario, taxa = NN, Time = year, group, percent, se.perc, colour)

invert.sims.perc <- invert.sims %>%
    mutate(taxa = ifelse(taxa == 'Ostracod', 'B', 'A')) %>%
    group_by(taxa, scenario, Time) %>%
    summarise(Nt = mean(Nt)) %>%
    ungroup() %>%
    mutate(group = 'Invertebrates') %>%
    mutate(se.perc = 0) %>%
    mutate(Time = Time/365) %>%
    mutate(colour = ifelse(taxa == 'A',
                           col[['Invertebrates']],
                           alpha('grey20', 0.15))) %>%
    select(scenario, taxa, Time, group, percent = Nt, se.perc, colour)
    
rip.sims.perc <- rip.ns.sims.df.g %>%
    mutate(group = 'Plants') %>%
    mutate(se.perc = 0) %>%
    mutate(colour = ifelse(key == 'Cnonseedling',
                           col[['Plants']],
                           alpha('grey20', 0.15))) %>%
    select(scenario,
           taxa = key,
           Time = year,
           group,
           percent = value,
           se.perc,
           colour)
    
groups.sims.perc <- rbind(rip.sims.perc,
                          fish.sims.perc,
                          invert.sims.perc)

groups.sims.perc <- groups.sims.perc %>%
    mutate(scenario = case_when(
               scenario == 'RIP_OPT' ~ 'Plant\nprescription',
               scenario == 'FSH_OPT' ~ 'Fish\nprescription',
               scenario == 'INV_OPT' ~ 'Invertebrate\nprescription',
               scenario == 'NAT' ~ 'Natural flow'))
    
groups.sims.perc$scenario <- factor(groups.sims.perc$scenario,
                                    levels = c('Plant\nprescription',
                                               'Fish\nprescription',
                                               'Invertebrate\nprescription',
                                               'Natural flow'))

cols <- as.character(groups.sims.perc$colour)
names(cols) <- as.character(groups.sims.perc$taxa)

groups.sims.perc$group <- factor(groups.sims.perc$group,
                                 levels = c('Plants', 'Fish', 'Invertebrates'))
    
## adding images
plant.img <- readPNG('data/tree_viridis_small.png')
fish.img <- readPNG('data/fish_viridis_small.png')
bug.img <- readPNG('data/bug_viridis_small.png')


pdf('export/flow-comp-figs/multi-sim-facet-img.pdf', width = 9, height = 7)

ggplot(groups.sims.perc, aes(Time, percent, colour = taxa, fill = taxa)) +
    geom_line(show.legend = FALSE) +
    scale_colour_manual(values = cols) +
    scale_fill_manual(values = cols) +
    facet_grid(scenario~group, scales = 'free_x') +
    theme_classic_facet() +
    geom_ribbon(aes(ymin = percent - 2 * se.perc,
                    ymax = percent + 2 * se.perc),
                colour = 'transparent',
                show.legend = FALSE) +
    xlab('Year') + ylab('Percent of carrying capacity') + 
    theme(plot.margin = unit(c(6, 1, 1, 1), "lines"),
          strip.text.x = element_blank())

grid.raster(plant.img, x=.2, y=.9, width = .13)
grid.raster(fish.img, x=.5, y=.9, width = .13)
grid.raster(bug.img, x=.8, y=.9, width = .13)

dev.off()

## -----------------------------------------------------------------------------
## * Trade-off plots -----------------------------------------------------------
## -----------------------------------------------------------------------------

## For fish and riparian, taking mean across final 100 years of the 200y run
## Or just discarding first 10 y (e.g. 190 y)
## Need to do this here

rip.ns.meantarget <- rip.ns.sims.df.g %>%
    filter(year > 10) %>% ### KEY ADDITION
    group_by(scenario, key) %>%
    summarise(mean = mean(value)) %>%
    filter(key == 'Cnonseedling') %>%
    select(-key) %>%
    mutate(org = 'rip')
rip.ns.meantarget

fish.biom.meantarget <- fish.means.biom.NN %>%
    filter(year > 10) %>% ### KEY ADDITION
    group_by(scenario, NN) %>%
    summarise(mean = mean(mean.pK * 100)) %>%
    filter(NN == 'Native') %>%
    select(-NN) %>%
    mutate(org = 'fish') 
fish.biom.meantarget    

## For inverts, taking mean of final 20y of 30y run
## Already present in the inverts.2taxa.means df
inverts.2taxa.meantarget <- inverts.2taxa.means %>%
    group_by(scenario) %>%
    summarise(mean = mean(Nt)) %>%
    mutate(org = 'inverts')
inverts.2taxa.meantarget

target.df <- bind_rows(rip.ns.meantarget,
                   fish.biom.meantarget,
                   inverts.2taxa.meantarget) %>%
    ungroup() 
target.df

target.df$scenario <- as.factor(target.df$scenario)
target.df$org <- as.factor(target.df$org)

target.df.rename <- target.df %>%
     mutate(Scenario = case_when(
               scenario == 'FSH_OPT' ~ 'Fish prescription',
               scenario == 'INV_OPT' ~ 'Invertebrate prescription',
               scenario == 'RIP_OPT' ~ 'Plant prescription',
               scenario == 'NAT' ~ 'Natural flow'),
            org = case_when(
                org == 'fish' ~ 'Fish',
                org == 'inverts' ~ 'Invertebrates',
                org == 'rip' ~ 'Plants')
            )

target.df.rename$org <- factor(target.df.rename$org,
                               levels = c('Plants', 'Fish', 'Invertebrates'))

target.df.rename$Scenario <- factor(target.df.rename$Scenario,
                                    levels = c('Plant prescription',
                                               'Fish prescription',
                                               'Invertebrate prescription',
                                               'Natural flow'))

target.df.rename <- target.df.rename %>%
    mutate(col = case_when(
               scenario == 'FSH_OPT' ~ col[['Fish']],
               scenario == 'INV_OPT' ~ col[['Invertebrates']],
               scenario == 'RIP_OPT' ~ col[['Plants']],
    scenario == 'NAT' ~ col[['Natural flow']]))

target.bar <- ggplot(target.df.rename, aes(org, mean, fill = Scenario)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values = c(col[['Plants']],
                                 col[['Fish']],
                                 col[['Invertebrates']],
                                 col[['Natural flow']])) +
    theme_classic_facet() +
    xlab('Ecosystem compartment') +
    ylab('Percent of carrying capacity')
target.bar

ggsave('export/flow-comp-figs/tradeoff.pdf', width = 6, height = 3)

## -----------------------------------------------------------------------------
## * Chord diagram -------------------------------------------------------------
## -----------------------------------------------------------------------------

chord.df <- target.df %>%
    mutate(from = case_when(
               scenario == 'FSH_OPT' ~ 'Fish',
               scenario == 'INV_OPT' ~ 'Invertebrates',
               scenario == 'RIP_OPT' ~ 'Plants',
               scenario == 'NAT' ~ 'Natural flow'),
           to = case_when(
               org == 'fish' ~ 'Fish',
               org == 'inverts' ~ 'Invertebrates',
               org == 'rip' ~ 'Plants'),
           
           direc = case_when(
               from == to ~ 'DOM',
               TRUE ~ 'OB'),
           col = case_when(
               scenario == 'FSH_OPT' ~ col[['Fish']],
               scenario == 'INV_OPT' ~ col[['Invertebrates']],
               scenario == 'RIP_OPT' ~ col[['Plants']],
               scenario == 'NAT' ~ col[['Natural flow']]),
           meaninv = 100-mean,
           logmean = log(mean))
chord.df

chord.df <- chord.df %>%
    arrange(mean)

directional = c("IB" = -1,
                "OB" = 1,
                "RETURN" = 2,
                "DOM" = 0)
arrtyp = c("IB" = NA,
           "OB" = "big.arrow",
           "RETURN" = NA,
           "DOM" = FALSE)

diff.df <- chord.df %>%
    filter(scenario == 'NAT' | from == to) %>%
    group_by(org) %>%
    summarise(mean = diff(mean))

prop.df <- chord.df %>%
    filter(scenario == 'NAT' | from == to) %>%
    select(to, mean, direc) %>%
    spread(direc, mean) %>%
    mutate(prop = OB/DOM)

optval <- chord.df %>%
    filter(from == to)

natval <- chord.df %>%
    filter(scenario == 'NAT')

pdf('export/flow-comp-figs/chorddiag-bar-bar.pdf', width = 7, height = 7)
circos.clear()
circos.par(gap.degree = 8)
cd <- chordDiagram(select(chord.df, from, to, mean),
                   col = chord.df$col,
                   transparency = 0.3,
                   grid.col = col,
                   directional = 1, #directional[chord.df$direc],
                   direction.type = c("diffHeight", "arrows"),
                   diffHeight = -.05,
                   link.arr.type = 'big.arrow', #arrtyp[chord.df$direc],
                   link.border = chord.df$col,
                   ## annotationTrack = c("grid"), #"name","grid"
                   annotationTrack = NULL,
                   ## annotationTrackHeight = c(.025),
                   preAllocateTracks = list(list(track.height = .35),
                                            list(track.height = .05),
                                            list(track.height = .05),
                                            list(track.height = .05)))

cd

maxvals <- cd %>%
    group_by(rn) %>%
    summarise(max = round(max(value1))/100) %>%
    mutate(hmax = round(max/2,2)) %>%
    ungroup()

circos.info()
## With xlim scale
circos.track(track.index = 4, panel.fun = function(x, y) {
    xlim = get.cell.meta.data("xlim")
    ylim = get.cell.meta.data("ylim")
    sect = get.cell.meta.data("sector.index")

  circos.rect(0,
              0,
              xlim[2]*filter(prop.df, to == sect)$prop,
              1, 
              sector.index = sect,
              track.index = 4,
              col = col[match(sect, names(col))]
              )
},
bg.col = 'lightgrey', bg.border = NA)

circos.track(track.index = 3, panel.fun = function(x, y) {
    xlim = get.cell.meta.data("xlim")
    ylim = get.cell.meta.data("ylim")
    sector.name = get.cell.meta.data("sector.index")
    xplot = get.cell.meta.data("xplot")


    if(sector.name != 'Natural flow'){
    circos.lines(xlim, c(ylim[1], ylim[1]), lty = 3) # dotted line
    by = ifelse(abs(xplot[2] - xplot[1]) > 30, 0.2, 0.5)
    for(p in seq(by, 1, by = by)) {
        circos.text(p*(xlim[2] - xlim[1]) + xlim[1], ylim[1] + 0.2, 
            paste0(p*100, "%"), cex = 0.6, adj = c(0.5, 0), niceFacing = TRUE)
    }
    }
}, bg.border = NA)
    
circos.info()

circos.trackPlotRegion(track.index = 2, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  circos.text(mean(xlim), ylim[1] + .2, sector.name, facing = "bending.inside", niceFacing = TRUE,  col = "black", cex = .8, font = 2)
}, bg.border = NA)

circos.info()

circos.trackPlotRegion(track.index = 1, 
for(i in seq_len(nrow(cd))) {

    circos.rect(cd[i, "x1"], 0, cd[i, "x1"] - abs(cd[i, "value1"]), cd[i, "value1"]*.01, 
                sector.index = cd$rn[i], track.index = 1,
                col = col[match(cd[i, "cn"], names(col))])
        
}, bg.border = NA)

circos.info()

circos.yaxis(side = "left",
             ## at = c(0,
             ##        filter(maxvals,rn == get.all.sector.index()[1])$hmax,
             ##        filter(maxvals,rn == get.all.sector.index()[1])$max),
             track.index = 1, 
             sector.index = get.all.sector.index()[1],
             labels.cex = .6)

circos.yaxis(side = "left",
             ## at = c(0,
             ##        filter(maxvals,rn == get.all.sector.index()[2])$hmax,
             ##        filter(maxvals,rn == get.all.sector.index()[2])$max),
             track.index = 1, 
             sector.index = get.all.sector.index()[2],
             labels.cex = .6)

circos.yaxis(side = "left",
             ## at = c(0,
             ##        filter(maxvals,rn == get.all.sector.index()[3])$hmax,
             ##        filter(maxvals,rn == get.all.sector.index()[3])$max),
             track.index = 1, 
             sector.index = get.all.sector.index()[3],
             labels.cex = .6)

circos.yaxis(side = "left",
             ## at = c(0,
             ##        filter(maxvals,rn == get.all.sector.index()[4])$hmax,
             ##        filter(maxvals,rn == get.all.sector.index()[4])$max),
             track.index = 1, 
             sector.index = get.all.sector.index()[4],
             labels.cex = .6)

circos.clear()
dev.off()

### Local Variables:
### eval: (orgstruct-mode 1)
### orgstruct-heading-prefix-regexp: "## "
### End:
