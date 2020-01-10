## Making a natural flow regime to run for the inverts
## The invert model needs single flood events, not full hydrographs
## Process
## 1. Take the full sequence from the Verde @ Paulden 1964-2008
## 2. Look at mean max values for each month across full series.
## 3. Identify seasonal peaks.
## 4. These occur in spring (big) and fall/late summer (small).
## 5. Identify seasonal windows where these peaks fall as they vary annually.
## 6. Make that the window to search for each of those 2 peaks.
## 7. Take the mean max flow across the full series that falls in those windows.
## 8. That's the natural flow; then repeat for 200 y. 

library(plyr)
library(tidyverse)

## Plotting theme
theme_classic_facet <- function() {
    theme_classic() +
        theme(strip.background = element_rect(colour = NA, fill = NA))
}

## Read data in
gagedata <- read.csv("data/Paulden_USGS_gage1963-2017.csv", header = T)

## Remove 1963
gagedata <- gagedata %>%
    filter(year != 1963)

## -------------------------------------------------------------------------- ##
## explore                                                                    ##
## -------------------------------------------------------------------------- ##
str(gagedata)
ggplot(gagedata, aes(1:nrow(gagedata), cfs)) +
    geom_line()

gagedata %>%
    filter(year > 1990 & year <1996) %>%
    ggplot(aes(1:nrow(.), cfs)) +
    geom_line()

gagedata %>%
    group_by(year) %>%
    summarise(cfs = max(cfs))

## mean daily
gagedata %>%
    filter(year < 2009) %>%
    group_by(day_of_year) %>%
    summarise(cfs = mean(cfs)) %>%
    filter(day_of_year != 366) %>%
    ggplot(aes(day_of_year, cfs)) +
    geom_line() 

## mean monthly
gagedata %>%
    filter(year < 2009) %>%
    group_by(year, month) %>%
    summarise(maxcfs = max(cfs)) %>%
    ungroup() %>%
    group_by(month) %>%
    summarise(cfs = mean(maxcfs)) %>%
    ggplot(aes(month, cfs)) +
    geom_bar(stat = 'identity')

## grouping into seasons and then taking the max in each year in each season
## taking the mean of this
natflow_mean <- gagedata %>%
    filter(year < 2009) %>%
    mutate(season = ifelse(month >= 1 & month <=4, 'spring',
                    ifelse(month >= 8 & month <=10, 'fall', 'other'))) %>%
    group_by(year, season) %>%
    summarise(maxcfs = max(cfs)) %>%
    ungroup() %>%
    group_by(season) %>%
    summarise(cfs = mean(maxcfs)) 

head(natflow_mean)

ggplot(natflow_mean, aes(season, cfs)) +
    geom_bar(stat = 'identity')

## -------------------------------------------------------------------------- ##
## Final vector                                                               ##
## -------------------------------------------------------------------------- ##

## values
## spring: 1134 cfs
## fall: 459 cfs

## spring value ca. day 50 (corresponds to 19 Feb)
## fall value ca. day 251 (corresponds to 7 Sept)

## making the vector to use
## doesn't matter exactly when, just how far apart as we're looking at long-term
## trends. i.e. no need to put the first flood at day 50. I'll put it at day 1,
## then have the fall flood fall on day 200. i.e. repeat the spring value 200
## times and the fall value 115 + 50. That is through the end of the year and to
## day 49

t <- c(seq(1:200), seq(1:165))
Q <- c(rep(1134, 200), rep(459, 165))

natflow_df <- as.data.frame(cbind(t, Q))
str(natflow_df)
head(natflow_df)

## Exporting
write.csv(natflow_df, file = "export/natflow_mean.csv", row.names = FALSE)    


