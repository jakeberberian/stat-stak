library(tidyverse)
library(lubridate)
library(lattice)

# Read data
data <- read_csv("~/stat-stak/data/duke_games_trackman.csv")

# Set parameters for scrape
pitcher <- "Jarvis, Bryce"
year <- 2019

# Set strike zone parameters
topKzone <- 3.5
botKzone <- 1.6
inKzone <- -0.95
outKzone <- 0.95

# Clean function. Input Trackman data, pitcher, and year.
clean_data <- function(df, pitcher, year, batterside) {
  df %>% 
    mutate(pitch_type = case_when(
      TaggedPitchType == "Fastball" ~ "FB",
      TaggedPitchType == "Cutter" ~ "FB",
      TaggedPitchType == "Sinker" ~ "FB",
      TRUE ~ "BB")) %>% 
    filter(Pitcher == pitcher, year(Date) == year, 
           TaggedPitchType != "Undefined", BatterSide != "Undefined")
}

# Plot pitch locations for the variations of FB/BB and batter handedness. 
xyplot(PlateLocHeight ~ -PlateLocSide | BatterSide * pitch_type, data = clean_data(data, pitcher, year),
       col = "black",
       pch = 20,
       aspect = "iso",
       xlim = c(-3, 3),
       ylim = c(0, 5),
       # main = paste0("Pitch Locations for ", pitcher, " in ", year), 
       # xlab = "Distance from Center\nCatcher's Perspective (ft.)",
       # ylab = "Height (ft.)",
       xlab = NULL,
       ylab = NULL,
       strip = FALSE, 
       layout = c(4, 1), 
       panel = function(...){
         panel.xyplot(...)
         panel.rect(inKzone, botKzone, outKzone, topKzone,
                    border = "red", lty = 5)})

# Release point plot
xyplot(RelHeight ~ -RelSide, data = clean_data(data, pitcher, year), groups = TaggedPitchType,
       auto.key = list(space = "right",
                       border = TRUE,
                       cex.title = 0.8,
                       title = "Pitch Types",
                       padding.text = 3),
       aspect = "iso",
       # xlim = c(min(data$RelSide), max(data$RelSide)),
       # ylim = c(min(data$RelHeight), max(data$RelHeight)),
       xlim = c(-3, 3),
       ylim = c(0, 7.5),
       # pch = 20, 
       # main = paste0("Release Points for ", pitcher, " in ", year), 
       # xlab = "Distance from Center\nCatcher's Perspective (ft.)",
       # ylab = "Height (ft.)",
       xlab = NULL,
       ylab = NULL, 
       panel = function(...){
         panel.xyplot(...)
         panel.rect(inKzone, botKzone, outKzone, topKzone,
                    border = "black", lty = 3)})
