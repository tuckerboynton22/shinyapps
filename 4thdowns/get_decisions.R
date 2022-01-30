library(ggplot2)
library(tidyverse)
library(readxl)
library(ggimage)
library(nflfastR)
library(ggtext)
library(statar)
library(nfl4th)

for (i in 2021:2021){
  
  load_4th_pbp(i) %>%
    filter(!is.na(go_boost)) %>%
    mutate(xwp_boost = if_else(go == 100, go_boost, -go_boost),
           fourth_success = if_else(xwp_boost > 0, 1, 0)) %>%
    select(posteam, defteam, posteam_score, defteam_score, week, qtr, desc, wp, ydstogo,
           go_boost:go, xwp_boost, fourth_success, yrdln, game_seconds_remaining, first_down) %>%
    write_csv(paste0("/Users/tuckerboynton/Desktop/R/Shiny/4thdowns/fourth_", i, ".csv"))

}