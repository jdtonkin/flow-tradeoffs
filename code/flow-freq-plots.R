## Code used to create schematic hydrographs
## rm(list=ls())

library(tidyverse)
library(patchwork)
library(png)
library(grid)
library(gridExtra)

## Plotting theme
theme_classic_facet <- function() {
    theme_classic() +
        theme(strip.background = element_rect(colour = NA, fill = NA))
}

## viridis with grey for nat
col <- c("Invertebrates" = "#440154FF",
        "Fish" = "#31688EFF",
        "Plants" = "#35B779FF",
        "Natural flow" = "grey")

## Making hydrographs ----------------------------------------------------------
## Taken from here: https://gist.github.com/TonyLadson/b4f34034920d339ddf09
Hydro <- function(tt, t.peak = 1, Qmin = 1, Qmax = 10, beta = 5) {
  Qmin + (Qmax - Qmin)*( (tt/t.peak) * (exp(1 - tt/t.peak)))^beta  
}

## Riparian and fish optimized flows
Qmin = 50
tt.seq <- seq(0,(365),1)
flow.seq <- Hydro(tt.seq,
                  beta = .5,
                  Qmin = Qmin,
                  Qmax = 700,
                  t.peak = 10)

base.seq <- rep(Qmin, 365)

drought.seq <- c(seq(Qmin, 20, length.out = 110),
                 rep(20, 145),
                 seq(20, Qmin, length.out = 110))

rip.flow <- rep(c(rep(base.seq, 4),
             drought.seq,
             flow.seq), length.out = 7300)
time = 1:7300

rip.flows <- as.data.frame(cbind(time, flow = rip.flow))
rip.flows <- rip.flows %>%
    mutate(opt = 'Plant\nprescription')

fish.flow <- rep(flow.seq, length.out = 7300)
fish.flows <- as.data.frame(cbind(time, flow = fish.flow))
fish.flows <- fish.flows %>%
    mutate(opt = 'Fish\nprescription')

## Invertebrate optimiztion = 143 cfs 4X per y
inv.tt.seq <- seq(0,(365/4),1)

inv.flow.seq <- Hydro(inv.tt.seq,
                  beta = 1,
                  Qmin = Qmin,
                  Qmax = 143,
                  t.peak = 10)

inv.flow <- rep(inv.flow.seq, length.out = 7300)
time <- 1:7300

inv.flows <- as.data.frame(cbind(time, flow = inv.flow))
inv.flows <- inv.flows %>%
    mutate(opt = 'Invertebrate\nprescription')

## Merging with natural flow ---------------------------------------------------
## Read gage data in
gagedata <- read.csv("data/Paulden_USGS_gage1963-2017.csv", header = T)
head(gagedata)

## Remove 1963
gagedata <- gagedata %>%
    filter(year >= 1981 & year <= 2000) %>%
    mutate(time = 1:nrow(.)) %>%
    mutate(opt = 'Natural flow') %>%
    select(time, flow = cfs, opt)

## joining all flows
all.flows <- rbind(rip.flows, fish.flows, inv.flows, gagedata)

all.flows$opt <- factor(all.flows$opt,
                        levels = c('Plant\nprescription',
                                   'Fish\nprescription',
                                   'Invertebrate\nprescription',
                                   'Natural flow'))

### Flow frequencies plus cartoon ----------------------------------------------
## adding images
cartoon.img <- readPNG('data/cartoon_viridis_small.png')
cartoon.g <- rasterGrob(cartoon.img, interpolate=TRUE)

## convert to cumecs
all.flows <- all.flows %>%
    mutate(cumecs = flow * 0.028316847)

## plot
freq.nat.log <- ggplot(all.flows, aes(time/365, cumecs, colour = opt, fill = opt)) +
    geom_line(show.legend = FALSE) +
    facet_grid(opt~.) + # , scales = 'free_y'
    theme_classic_facet() +
    xlab('Year') +
    ylab('Discharge (cfs)') +
    scale_color_manual(values = c("#35B779FF",
                                  "#31688EFF",
                                  "#440154FF",
                                  "grey")) +
    scale_fill_manual(values = c("#35B779FF",
                                 "#31688EFF",
                                 "#440154FF",
                                 "grey")) +
    expand_limits(y = 0) +
    scale_y_log10() +
    ylab(bquote('Discharge ('*m^3~s^-1*')'))
freq.nat.log

pdf('export/flow-comp-figs/flow-freqs-nat-log-cartoon.pdf', width = 10, height = 5)
grid.arrange(cartoon.g, freq.nat.log, nrow = 1, widths = c(.4, .6))
grid.text('A.', x=.02, y=.95, gp = gpar(fontsize = 20))
grid.text('B.', x=.4, y=.95, gp = gpar(fontsize = 20))
dev.off()


