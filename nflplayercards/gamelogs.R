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

font_add_google("Chivo", "Chivo")
showtext_auto()

color_palette <- c("#FF1100", "#FFFFFF", "#00690E")
color_palette_reversed <- c("#00690E", "#FFFFFF", "#FF1100")

## READ IN ALL DATA
setwd("/Users/tuckerboynton/Desktop/R/Shiny/nflplayercards")

seasons <- 1999:2021

weekly_stats <- load_player_stats(seasons) %>%
  mutate(player_name = case_when(
    player_name == "Aa.Rodgers" ~ "A.Rodgers",
    player_name == "Ty.Taylor" ~ "T.Taylor",
    TRUE ~ player_name))

pbp <- nflreadr::load_pbp(1999:2021) %>%
  decode_player_ids() %>%
  mutate(name = case_when(
    name == "Aa.Rodgers" ~ "A.Rodgers",
    name == "Ty.Taylor" ~ "T.Taylor",
    TRUE ~ name))

passers <- weekly_stats %>%
  select(player_name,
         player_id,
         season,
         week,
         dakota) %>%
  mutate(dakota_pct = percent_rank(dakota)*100)

team_logos <- nflfastR::teams_colors_logos %>%
  select(team_abbr, team_wordmark)

games <- pbp %>%
  filter(pass == 1 | rush == 1, !is.na(defteam)) %>%
  group_by(name, id, season, week, defteam) %>%
  summarize(
    plays = n(),
    dropbacks = sum(pass, na.rm = T),
    total_epa = sum(qb_epa, na.rm = T),
    epa_play = mean(qb_epa, na.rm = T),
    cpoe = mean(cpoe, na.rm = T),
    pass_att = sum(pass - sack - qb_scramble, na.rm = T),
    pass_epa = sum(qb_epa * pass - qb_epa * qb_scramble - qb_epa * sack, na.rm = T),
    rushes = sum(rush, na.rm = T),
    rush_epa = sum(qb_epa * rush, na.rm = T),
    scrambles = sum(qb_scramble, na.rm = T),
    scramble_epa = sum(qb_epa * qb_scramble, na.rm = T),
    sacks = sum(sack, na.rm = T),
    sack_epa = sum(qb_epa * sack, na.rm = T),
  ) %>%
  filter(dropbacks >= 5) %>%
  ungroup() %>%
  mutate(
    plays_pct = percent_rank(plays)*100,
    dropbacks_pct = percent_rank(dropbacks)*100,
    total_epa_pct = percent_rank(total_epa)*100,
    epa_play_pct = percent_rank(epa_play)*100,
    cpoe_pct = percent_rank(cpoe)*100,
    pass_att_pct = percent_rank(pass_att)*100,
    pass_epa_pct = percent_rank(pass_epa)*100,
    rushes_pct = percent_rank(rushes)*100,
    rush_epa_pct = percent_rank(rush_epa)*100,
    scrambles_pct = percent_rank(scrambles)*100,
    scramble_epa_pct = percent_rank(scramble_epa)*100,
    sacks_pct = percent_rank(sacks)*100,
    sack_epa_pct = percent_rank(sack_epa)*100
  ) %>%
  merge(passers, by.x = c("name","id","season","week"),
        by.y = c("player_name","player_id","season","week"),
        all.x = TRUE) %>%
  merge(team_logos, by.x = "defteam", by.y = "team_abbr") %>%
  arrange(name, season, week)

games <- games %>% mutate(cpoe = ifelse(is.nan(cpoe), NA_real_, cpoe))

write.csv(games, "qb_gamelogs.csv", row.names = F)
 