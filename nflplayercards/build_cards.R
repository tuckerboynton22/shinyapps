library(magick)
library(cowplot)
library(ggridges)
library(ggtext)
library(ggplot2)
library(tidyverse)
library(readxl)
library(gt)
library(nflfastR)
library(espnscrapeR)
library(sysfonts)
library(jsonlite)
library(curl)
library(showtext)
library(ggrepel)
library(nflreadr)

options(remove(list=ls()))

passers <- read_csv("passers.csv") %>%
  filter(season == 2021)

for (i in 1:nrow(passers)){
  
  player <- passers[[i,1]]
  year <- passers[[i,2]]
  
  print(player)
  print(year)
  
  epa_breakdown <- ggdraw() + draw_image(paste0("Card Components/", year, "/", player, " EPA.png"))
  heatmap <- ggdraw() + draw_image(paste0("Card Components/", year, "/", player, " Heatmap.png"))
  basic <- ggdraw() + draw_image(paste0("Card Components/", year, "/", player, " Basic.png"))
  timeline <- ggdraw() + draw_image(paste0("Card Components/", year, "/", player, " EPA Timeline.png"))
  gamelog <- ggdraw() + draw_image(paste0("Card Components/", year, "/", player, " Gamelog.png"))
  headshot <- ggdraw() + draw_image(paste0("Card Components/", year, "/", player, " Headshot.png"))
  info <- ggdraw() + draw_image(paste0("Card Components/", year, "/", player, " Info.png"))
  credits <- ggdraw() + draw_image("Card Components/Credits.png")
  
  col_3 <- plot_grid(timeline, gamelog, credits, rel_heights = c(3,2,1.5), ncol = 1)
  col_2 <- plot_grid(basic, heatmap, epa_breakdown, rel_heights = c(324,537,324), ncol = 1)
  col_1 <- plot_grid(headshot, info, rel_heights = c(1,2), ncol = 1)
  plot_grid(col_1, col_2, col_3, rel_widths = c(1,2,2), nrow = 1)
  
  ggsave(as.character(paste("Cards/", year, "/", player, ".png", sep = "")), height = 6, width = 11)
  
}
