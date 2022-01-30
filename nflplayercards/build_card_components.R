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

min_plays <- 300

color_palette <- c("#FF1100", "#FFFFFF", "#00690E")
color_palette_reversed <- c("#00690E", "#FFFFFF", "#FF1100")

gt_theme_538 <- function(data,...) {
  data %>%
    opt_all_caps()  %>%
    opt_table_font(
      font = list(
        google_font("Chivo"),
        default_fonts()
      )
    ) %>%
    tab_style(
      style = cell_borders(
        sides = "bottom", color = "black", weight = px(3)
      ),
      locations = cells_body(
        columns = TRUE,
        # This is a relatively sneaky way of changing the bottom border
        # Regardless of data size
        rows = nrow(data$`_data`)
      )
    )  %>% 
    tab_options(
      column_labels.background.color = "white",
      table.border.top.width = px(3),
      table.border.top.color = "white",
      table.border.bottom.color = "white",
      table.border.bottom.width = px(3),
      table_body.border.bottom.color = "black",
      column_labels.border.top.width = px(3),
      column_labels.border.top.color = "black",
      column_labels.border.lr.color = "grey",
      column_labels.vlines.style = "solid",
      table_body.vlines.style = "solid",
      table_body.vlines.color = "lightgrey",
      column_labels.border.bottom.width = px(3),
      column_labels.border.bottom.color = "black",
      data_row.padding = px(3),
      source_notes.font.size = 16,
      table.font.size = 24,
      heading.align = "left",
      ...
    ) 
}

## READ IN ALL DATA
setwd("/Users/tuckerboynton/Desktop/R/Shiny/nflplayercards")

seasons <- 2001:2021

ff_player_ids <- load_ff_playerids() %>%
  select(espn_id, name, gsis_id) %>%
  filter(!is.na(espn_id), !is.na(gsis_id))

caphits <- readxl::read_excel("Data/caphits.xlsx")

pff_teams <- readxl::read_excel("Data/pff_teams.xlsx")

pff <- readxl::read_excel("Data/pff_qbs.xlsx")

dvoa <- readxl::read_excel("Data/dvoa.xlsx") %>%
  select(-QBR)

weekly_stats <- load_player_stats(seasons) %>%
  mutate(player_name = case_when(
    player_name == "Aa.Rodgers" ~ "A.Rodgers",
    player_name == "Ty.Taylor" ~ "T.Taylor",
    TRUE ~ player_name))

roster <- load_rosters(2001:2021) %>%
  filter(position == "QB") %>%
  rename(espn_id_roster = espn_id) %>%
  left_join(ff_player_ids, by="gsis_id") %>%
  mutate(
    espn_id = case_when(
      !is.na(espn_id) ~ as.character(espn_id),
      !is.na(espn_id_roster) ~ as.character(espn_id_roster),
      TRUE ~ NA_character_),
    qbr_join = ifelse(is.na(espn_id), full_name, espn_id),
    pff_id = case_when(
      full_name == "Mac Jones" ~ as.integer(60323),
      full_name == "Trevor Lawrence" ~ as.integer(77632),
      full_name == "Zach Wilson" ~ as.integer(82096),
      TRUE ~ pff_id
    ))

no_espn_id <- roster %>%
  filter(is.na(espn_id)) %>%
  select(full_name, gsis_id) %>%
  rename(name_display = full_name) %>%
  mutate(missing_espn = 1) %>%
  distinct()

qbrs <- load_espn_qbr(seasons = 2006:2021) %>%
  filter(season_type == "Regular") %>%
  left_join(no_espn_id, by="name_display") %>%
  mutate(qbr_join = ifelse(is.na(missing_espn), player_id, name_display)) %>%
  left_join(read_excel("Data/manual_qbr.xlsx")) %>%
  mutate(qbr_total = ifelse(is.na(qbr_manual), qbr_total, qbr_manual)) %>%
  select(-qbr_manual)

pbp <- nflreadr::load_pbp(2001:2021) %>%
  decode_player_ids() %>%
  mutate(name = case_when(
    name == "Aa.Rodgers" ~ "A.Rodgers",
    name == "Ty.Taylor" ~ "T.Taylor",
    TRUE ~ name))

## LEAGUE AVG HEATMAP
get_heatmap_averages <- function(pbp, year){
  
  data <- pbp %>% filter(season == year, season_type == "REG")
  
  left <- data %>%
    filter(!is.na(epa), !is.na(air_yards), pass_location == "left") %>%
    mutate(depth = case_when(
      air_yards <= 0 ~ "< 1",
      air_yards >= 1 & air_yards < 11 ~ "1-10",
      air_yards >= 11 & air_yards < 21 ~ "11-20",
      air_yards >= 21 ~ "> 20"
    )) %>%
    group_by(depth) %>%
    summarize(EPA = mean(qb_epa))
  
  right <- data %>%
    filter(!is.na(epa), !is.na(air_yards), pass_location == "right") %>%
    mutate(depth = case_when(
      air_yards <= 0 ~ "< 1",
      air_yards >= 1 & air_yards < 11 ~ "1-10",
      air_yards >= 11 & air_yards < 21 ~ "11-20",
      air_yards >= 21 ~ "> 20"
    )) %>%
    group_by(depth) %>%
    summarize(EPA = mean(qb_epa))
  
  middle <- data %>%
    filter(!is.na(epa), !is.na(air_yards), pass_location == "middle") %>%
    mutate(depth = case_when(
      air_yards <= 0 ~ "< 1",
      air_yards >= 1 & air_yards < 11 ~ "1-10",
      air_yards >= 11 & air_yards < 21 ~ "11-20",
      air_yards >= 21 ~ "> 20"
    )) %>%
    group_by(depth) %>%
    summarize(EPA = mean(qb_epa))
  
  left_middle <- left_join(left, middle, by = "depth")
  averages <- left_join(left_middle, right, by = "depth") %>%
    arrange(match(depth, c("> 20", "11-20", "1-10", "< 1")))
  
  return(averages)
}

## GET PASSERS
get_passers <- function(pbp, year, type){
  
  if (type == "season"){

    passers <- pbp %>%
      filter(season == year, down < 5, season_type == "REG", !is.na(qb_epa), pass == 1 | rush == 1) %>%
      mutate(no_play = ifelse(play_type == "no_play", 1, 0)) %>%
      group_by(name, id, season) %>%
      summarize(
        espn_plays = n() - sum(no_play),
        Plays = n(),
        Dropbacks = sum(pass),
        Sacks = mean(qb_epa * sack, na.rm = T),
        Scrambles = mean(qb_epa * qb_scramble, na.rm = T),
        Rushes = mean(qb_epa * rush, na.rm = T),
        Passes = mean(qb_epa * pass - qb_epa * qb_scramble - qb_epa * sack, na.rm = T),
        EPA = mean(qb_epa, na.rm = T),
        CPOE = mean(cpoe, na.rm = T)
      ) %>%
      ungroup() %>%
      mutate(is_eligible = case_when(
        # season == 2021 & espn_plays >= min_plays & Dropbacks >= min_plays ~ 1,
        espn_plays >= 300 & Dropbacks >= 250 ~ 1,
        TRUE ~ 0
      )) %>%
      filter(is_eligible == 1) %>%
      mutate(
        Sacks = rank(-Sacks, ties.method = "min"),
        Scrambles = rank(-Scrambles, ties.method = "min"),
        Rushes = rank(-Rushes, ties.method = "min"),
        Passes = rank(-Passes, ties.method = "min"),
        EPA = rank(-EPA, ties.method = "min"),
        CPOE = rank(-CPOE, ties.method = "min")
      ) %>%
      merge(roster, by.x=c("id","season"), by.y=c("gsis_id","season")) %>%
      merge(qbrs, by.x=c("qbr_join","season"),by.y=c("qbr_join","season")) %>%
      merge(pff, by.x=c("pff_id","season"), by.y=c("player_id","season")) %>%
      merge(teams_colors_logos, by.x=c("team.x"), by.y=c("team_abbr")) %>%
      merge(dvoa, by.x=c("name.x","season"), by.y=c("Player","Year")) %>%
      left_join(caphits, by=c("full_name"="Player","season"="Year")) %>%
      group_by(season) %>%
      mutate(
        DVOA = rank(-DVOA, ties.method = "min"),
        QBR = rank(-qbr_total, ties.method = "min"),
        PFF = rank(-grades_offense, ties.method = "min"),
        DYAR = rank(-DYAR, ties.method = "min")) %>%
      rename(
        name = name.x,
        team = team.x,
        position = position.x,
        team_name = team.y,
        team_name_alt = team_name.x,
        full_team_name = team_name.y
      ) %>%
      ungroup() %>%
      select(
        name:team,
        position,
        team_abb,
        team_name,
        name_short,
        qbr_total:sack,
        player_game_count:ypa,
        DYAR, YAR, DVOA, VOA,
        EYds, DPI, ALEX,
        espn_plays:CPOE,
        QBR, PFF,
        position,
        jersey_number,
        full_name:high_school,
        headshot_url,
        headshot_href,
        full_team_name:team_wordmark,
        `Cap Number`, `Cash Spent`,
        pff_id:id,
        yahoo_id:rotowire_id,
        fantasy_data_id:sleeper_id,
        sportradar_id:espn_id
      ) %>%
      distinct()
  }
  
  else if (type == "seasons"){
    
    passers <- pbp %>%
      filter(down < 5, season_type == "REG", !is.na(qb_epa), pass == 1 | rush == 1) %>%
      mutate(no_play = ifelse(play_type == "no_play", 1, 0)) %>%
      group_by(name, id, season) %>%
      summarize(
        espn_plays = n() - sum(no_play),
        Dropbacks = sum(pass),
        plays = n(),
        EPA = mean(qb_epa, na.rm = T),
        CPOE = mean(cpoe, na.rm = T)
      ) %>%
      ungroup() %>%
      mutate(is_eligible = case_when(
        # season == 2021 & espn_plays >= min_plays & Dropbacks >= min_plays ~ 1,
        espn_plays >= 300 & Dropbacks >= 250 ~ 1,
        TRUE ~ 0
      )) %>%
      filter(is_eligible == 1) %>%
      merge(roster, by.x=c("id","season"), by.y=c("gsis_id","season")) %>%
      mutate(
        epa_pct = percent_rank(EPA)*100,
        cpoe_pct = percent_rank(CPOE)*100
      ) %>%
      group_by(season) %>%
      mutate(
        epa_rank = rank(-EPA, ties.method = "min"),
        cpoe_rank = rank(-CPOE, ties.method = "min")
      ) %>%
      ungroup() %>%
      merge(teams_colors_logos, by.x=c("team"), by.y=c("team_abbr")) %>%
      rename(name = name.x) %>%
      select(-name.y)
  }
  
  else {
    
    passers <- weekly_stats %>%
      filter(season_type == "REG", attempts >= 10) %>%
      mutate(dakota_pct = percent_rank(dakota)*100) %>%
      as.data.frame() %>%
      merge(roster, by.x=c("player_id","season"), by.y=c("gsis_id","season")) %>%
      merge(teams_colors_logos, by.x=c("team"), by.y=c("team_abbr"))

  }
  
  return(passers)
  
}

## HEATMAPS
make_heatmap <- function(pbp, year, player, averages){
  
  data <- pbp %>% filter(season == year, season_type == "REG")
  
  left_player <- data %>%
    filter(!is.na(epa), !is.na(air_yards), pass_location == "left", name == player) %>%
    mutate(depth = case_when(
      air_yards <= 0 ~ "< 1",
      air_yards >= 1 & air_yards < 11 ~ "1-10",
      air_yards >= 11 & air_yards < 21 ~ "11-20",
      air_yards >= 21 ~ "> 20"
    )) %>%
    group_by(depth) %>%
    summarize(EPA = mean(qb_epa)) %>%
    mutate(lg_avg = case_when(
      depth == "< 1" ~ averages[[2]][4],
      depth == "1-10" ~ averages[[2]][3],
      depth == "11-20" ~ averages[[2]][2],
      depth == "> 20" ~ averages[[2]][1]),
      EPA_pct = (EPA - lg_avg)) %>%
    select(depth, EPA_pct)
  
  middle_player <- data %>%
    filter(!is.na(epa), !is.na(air_yards), pass_location == "middle", name == player) %>%
    mutate(depth = case_when(
      air_yards <= 0 ~ "< 1",
      air_yards >= 1 & air_yards < 11 ~ "1-10",
      air_yards >= 11 & air_yards < 21 ~ "11-20",
      air_yards >= 21 ~ "> 20"
    )) %>%
    group_by(depth) %>%
    summarize(EPA = mean(qb_epa)) %>%
    mutate(lg_avg = case_when(
      depth == "< 1" ~ averages[[3]][4],
      depth == "1-10" ~ averages[[3]][3],
      depth == "11-20" ~ averages[[3]][2],
      depth == "> 20" ~ averages[[3]][1]),
      EPA_pct = (EPA - lg_avg)) %>%
    select(depth, EPA_pct)
  
  right_player <- data %>%
    filter(!is.na(epa), !is.na(air_yards), pass_location == "right", name == player) %>%
    mutate(depth = case_when(
      air_yards <= 0 ~ "< 1",
      air_yards >= 1 & air_yards < 11 ~ "1-10",
      air_yards >= 11 & air_yards < 21 ~ "11-20",
      air_yards >= 21 ~ "> 20"
    )) %>%
    group_by(depth) %>%
    summarize(EPA = mean(qb_epa)) %>%
    mutate(lg_avg = case_when(
      depth == "< 1" ~ averages[[4]][4],
      depth == "1-10" ~ averages[[4]][3],
      depth == "11-20" ~ averages[[4]][2],
      depth == "> 20" ~ averages[[4]][1]),
      EPA_pct = (EPA - lg_avg)) %>%
    select(depth, EPA_pct)
  
  left_middle_player <- left_join(left_player, middle_player, by = "depth")
  total <- left_join(left_middle_player, right_player, by = "depth") %>%
    arrange(match(depth, c("> 20", "11-20", "1-10", "< 1")))
  
  total %>%
    gt() %>%
    cols_label(
      EPA_pct.x = "Left",
      EPA_pct.y = "Middle",
      EPA_pct = "Right",
      depth = "Air Yards"
    ) %>%
    tab_header(
      title = paste0(year, " Passing Heatmap"),
      subtitle = "EPA/play above/below league average"
    ) %>%
    gt_theme_538() %>%
    fmt_number(columns = 2:4, decimals = 2) %>%
    cols_align(align = "center", columns = c(1:4)) %>%
    data_color(
      columns = 2:4, 
      colors =
        scales::col_numeric(
          palette = (
            palette = color_palette
          ) %>% as.character(),
          domain = c(-1, 1)
        )
    ) %>%
    tab_style(cell_fill(color = color_palette[3]), locations = list(
      cells_body(columns = 2, rows = EPA_pct.x > 1),
      cells_body(columns = 3, rows = EPA_pct.y > 1),
      cells_body(columns = 4, rows = EPA_pct > 1))) %>%
    tab_style(cell_fill(color = color_palette[1]), locations = list(
      cells_body(columns = 2, rows = EPA_pct.x < -1),
      cells_body(columns = 3, rows = EPA_pct.y < -1),
      cells_body(columns = 4, rows = EPA_pct < -1))) %>%
    tab_style(cell_text(color = "white"), locations = list(
      cells_body(columns = 2, rows = EPA_pct.x > 1),
      cells_body(columns = 3, rows = EPA_pct.y > 1),
      cells_body(columns = 4, rows = EPA_pct > 1))) %>%
    tab_style(cell_text(color = "white"), locations = list(
      cells_body(columns = 2, rows = EPA_pct.x < -1),
      cells_body(columns = 3, rows = EPA_pct.y < -1),
      cells_body(columns = 4, rows = EPA_pct < -1))) %>%
    gtsave(paste("Card Components/", year, "/", player, " Heatmap.png", sep = ""))
  
  print("Heatmap")
}

## EPA BREAKDOWN
make_epa <- function(passers, year, player){
  
  passers %>%
    select(name, Sacks:Passes) %>%
    filter(name == player) %>%
    select(-name) %>%
    gt() %>%
    tab_header(
      title = paste0(year, " Efficiency by Play Type"),
      subtitle = paste0("EPA/play rank among ", nrow(passers), " QBs w/ ", min_plays,  "+ plays")
    ) %>% 
    gt_theme_538() %>%
    cols_align(align = "center", columns = 1:4) %>%
    data_color(
      columns = 1:4, 
      colors = scales::col_numeric(
        palette = (
          palette = color_palette_reversed
        ) %>% as.character(),
        domain = 1:nrow(passers)
      )
    ) %>%
    gtsave(paste("Card Components/", year, "/", player, " EPA.png", sep = ""))

  print("EPA")
}

## EPA/CPOE/QBR/PFF/DVOA
make_basic <- function(passers, year, player){

  passers %>%
    filter(name == player) %>%
    select(EPA, CPOE, PFF, QBR, DVOA) %>%
    gt() %>%
    tab_header(
      title = paste0(year, " Overall"),
      subtitle = paste0("Rank among ", nrow(passers), " QBs w/ ", min_plays, "+ plays")
    ) %>%
    gt_theme_538() %>%
    cols_align(align = "center", columns = 1:5) %>%
    data_color(
      columns = 1:5,
      colors = scales::col_numeric(
        palette = (
          palette = color_palette_reversed
        ) %>% as.character(),
        domain = 1:nrow(passers)
      )
    ) %>%
    gtsave(paste("Card Components/", year, "/", player, " Basic.png", sep = ""))
  
  print("Basic")
}

## LAST FIVE YEARS EPA
make_epa_timeline <- function(passers, year, player){
  
  passers <- passers %>%
    filter(between(season, year-4, year))
  
  passers %>%
    filter(name == player) %>%
    arrange(season) %>%
    ggplot(aes(season, epa_pct)) +
    geom_path(aes(y = cpoe_pct), color = color_palette[3], size = 4) +
    geom_path(color = color_palette[1], size = 4) +
    geom_label(aes(y = cpoe_pct, label = cpoe_rank), size = 6, fill = "white", color = color_palette[3]) +
    geom_label(aes(label = epa_rank), size = 6, fill = "white", color = color_palette[1]) +
    ggthemes::theme_fivethirtyeight() +
    labs(title = paste0("<span style='color: ", color_palette[1],
                        ";'>EPA/Play</span> vs. <span style='color: ", color_palette[3],
                        ";'>CPOE</span> Percentile Timeline"), subtitle = "League rank in box") +
    ggthemes::theme_fivethirtyeight() +
    theme(
      text = element_text(family = "Chivo"),
      legend.position = "none",
      plot.title = element_markdown(size = 18, hjust = 0.5, face = "plain"),
      plot.subtitle = element_markdown(size = 14, hjust = 0.5),
      axis.text.x = element_text(size = 15),
      axis.text.y = element_text(size = 15),
      plot.background = element_rect(fill = "white"),
      panel.background = element_rect(fill = "white")
    ) +
    scale_x_continuous(breaks = (year-4):year) +
    ylim(0,100)
  
  ggsave(paste("Card Components/", year, "/", player, " EPA Timeline.png", sep = ""), width = 5.77, height = 4.13, units = "in")
  
  print("Timeline")
  
}

## MAKE GAMELOG
make_gamelog <- function(passers, year, player){
  
  passers <- passers %>%
    filter(season == year, player_name == player)
  
  # median_epa <- quantile(passers$dakota, probs = 0.5, na.rm = T)
  # scaleFUN <- function(x) sprintf("%.2f", x)
  
  color1 <- passers$team_color2[1]
  
  avg_dakota <- mean(passers$dakota_pct)
  
  passers %>%
    arrange(week) %>%
    ggplot(aes(week, dakota_pct)) +
    # geom_hline(yintercept = avg_dakota, color = passers$team_color[1], linetype = "dashed", alpha = 0.75) +
    geom_path(color = color_palette[1], size = 2) +
    geom_point(color = color_palette[3], size = 2) +
    ggthemes::theme_fivethirtyeight() +
    labs(title = "DAKOTA Percentile Game Log") +
    ggthemes::theme_fivethirtyeight() +
    theme(
      text = element_text(family = "Chivo"),
      legend.position = "none",
      plot.title = element_markdown(size = 22, hjust = 0.5, face = "plain"),
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size = 10),
      plot.background = element_rect(fill = "white"),
      panel.background = element_rect(fill = "white"),
      plot.subtitle = element_markdown(size = 12, hjust = 0.5, face = "plain")
    ) +
    ylim(0,100) +
    scale_x_continuous(breaks = c(5,10,15))

  ggsave(paste("Card Components/", year, "/", player, " Gamelog.png", sep = ""), width = 5.77, height = 2.08, units = "in")
  
  print("Gamelog")
}

## MAKE PLAYER INFO TABLE
make_info <- function(passers, year, player){
  
  passers <- passers %>%
    filter(season == year)
  
  info <- passers %>%
    filter(season == year) %>%
    mutate(age = round((as.numeric(as.Date(paste0(year, "-12-31")) - birth_date))/365),
           caphit_rank = rank(`Cap Number`, ties.method = "min"),
           plays_rank = rank(Plays, ties.method = "min"),
           ypa_rank = rank(ypa, ties.method = "min"),
           sack_rank = rank(sack_percent, ties.method = "min"),
           adot_rank = rank(avg_depth_of_target, ties.method = "min")
           ) %>%
    filter(name == player) %>%
    select(full_name,
           team_wordmark,
           position,
           jersey_number,
           college,
           age,
           height,
           weight,
           player_game_count,
           `Cap Number`,
           caphit_rank,
           Plays,
           plays_rank,
           ypa,
           ypa_rank,
           sack_percent,
           sack_rank,
           avg_depth_of_target,
           adot_rank,
           headshot_href) %>%
    mutate(`Cap Number` = paste0("$", round(`Cap Number`/1000000, 1), "M")) %>%
    select(-c(caphit_rank, plays_rank, ypa_rank, sack_rank, adot_rank)) %>%
    dplyr::slice(1)
  
  if (!is.na(info$headshot_href)){
    download.file(as.character(info$headshot_href), paste("Card Components/", year, "/", player, " Headshot.png", sep = ""), mode = "wb")
  }
  else{
    download.file("https://upload.wikimedia.org/wikipedia/en/thumb/a/a2/National_Football_League_logo.svg/1200px-National_Football_League_logo.svg.png", paste("Card Components/", year, "/", player, " Headshot.png", sep = ""), mode = "wb")
  }
  
  info <- info %>%
    select(-headshot_href) %>%
    t() %>%
    as.data.frame()
  
  rownames(info) <- c("Player",
                      "Team",
                      "Pos",
                      "Number",
                      "College",
                      "Age",
                      "Height",
                      "Weight",
                      "Games",
                      "Cap Hit",
                      "Plays",
                      "YPA",
                      "Sack%",
                      "ADOT")
  
  colnames(info) <- " "

  info %>%
    gt(rownames_to_stub = TRUE) %>%
    gt_theme_538() %>%
    cols_align(align = "center", columns = 2) %>%
    text_transform(
      locations = cells_body(columns = 2, rows = 2),
      fn = function(x){
        map(x, ~web_image(
          url = .x,
          height = 25
        ))
      }
    ) %>%
    tab_options(table.border.bottom.color = "black",
                table.border.bottom.width = px(3),
                table.border.bottom.style = "solid") %>%
    gtsave(paste("Card Components/", year, "/", player, " Info.png", sep = ""))
  
}

## MAKE CARDS
make_cards <- function(year){
  
  passers <- get_passers(pbp, year, "season")
  multiseason_passers <- get_passers(pbp, year, "seasons")
  game_passers <- get_passers(pbp, year, "games")
  averages <- get_heatmap_averages(pbp, year)
  
  for (i in 1:nrow(passers)){
    
    player <- passers[[i,1]]
    print(player)
    
    make_heatmap(pbp, year, player, averages)
    make_epa(passers, year, player)
    make_basic(passers, year, player)
    make_epa_timeline(multiseason_passers, year, player)
    make_gamelog(game_passers, year, player)
    make_info(passers, year, player)
  
  } 
}

for (i in 2021:2021){
  make_cards(i)
}

## MAKE CREDITS
notes <- "Notes: EPA/play, CPOE, and DAKOTA expressed as"
notes2 <- "percentile among single games/seasons. DAKOTA is"
notes3 <- "EPA/play + CPOE composite statistic."
graphic <- "Graphic: @Tucker_TnL"
data <- "Data: nflfastR, PFF, ESPN, OTC, Football Outsiders"
inspiration <- "Inspiration: JFresh Hockey"

credits <- data.frame(notes, notes2, notes3, graphic, data, inspiration) %>%
  t() %>%
  as.data.frame()

colnames(credits) <- " "

credits %>%
  gt() %>%
  gt_theme_538() %>%
  gtsave("Card Components/Credits.png")

## GET LIST OF PASSERS FOR DROPDOWN
pbp %>%
  filter(down < 5, season_type == "REG", !is.na(qb_epa), pass == 1 | rush == 1) %>%
  mutate(no_play = ifelse(play_type == "no_play", 1, 0)) %>%
  group_by(name, id, season) %>%
  summarize(
    espn_plays = n() - sum(no_play),
    Plays = n(),
    Dropbacks = sum(pass)
  ) %>%
  ungroup() %>%
  mutate(is_eligible = case_when(
    # season == 2021 & espn_plays >= min_plays & Dropbacks >= min_plays ~ 1,
    espn_plays >= 300 & Dropbacks >= 250 ~ 1,
    TRUE ~ 0
  )) %>%
  filter(is_eligible == 1, season > 2005) %>%
  merge(roster, by.x=c("id","season"), by.y=c("gsis_id","season")) %>%
  group_by(season) %>%
  rename(name = name.x) %>%
  ungroup() %>%
  mutate(
    name = case_when(
      name == "Aa.Rodgers" ~ "A.Rodgers",
      name == "Ty.Taylor" ~ "T.Taylor",
      full_name == "Derek Carr" ~ "De.Carr",
      full_name == "David Carr" ~ "Da.Carr",
      TRUE ~ name
      )
  ) %>%
  select(name, season) %>%
  distinct() %>%
  arrange(name) %>%
  write.csv("passers.csv", row.names = F)

## MAKE RANKS OF PASSERS OVERALL
passers %>%
  select(headshot_url, name, team_wordmark, EPA, CPOE, DVOA, DYAR, QBR, PFF) %>%
  mutate(Avg = (EPA+CPOE+DVOA+QBR+PFF+DYAR)/6) %>%
  arrange(Avg) %>%
  select(-Avg) %>%
  gt() %>%
  cols_label(
    headshot_url = "",
    team_wordmark = "Team"
  ) %>%
  tab_header(
    title = "2021 Quarterback Ranks in Various Categories",
    subtitle = paste0("Rank out of ", nrow(passers), " passers with ", min_plays, "+ plays")
  ) %>%
  tab_source_note("Table: @Tucker_TnL || Data: #nflverse, PFF, ESPN, FO") %>%
  gtExtras::gt_theme_538() %>%
  cols_align(align = "center", columns = c(1:9)) %>%
  data_color(
    columns = 4:9, 
    colors =
      scales::col_numeric(
        palette = (
          palette = color_palette_reversed
        ) %>% as.character(),
        domain = NULL
      )
  ) %>%
  text_transform(
    locations = cells_body(columns = 1),
    fn = function(x){
      map(x, ~web_image(
        url = .x,
        height = 75
      ))
    }
  ) %>%
  text_transform(
    locations = cells_body(columns = 3),
    fn = function(x){
      map(x, ~web_image(
        url = .x,
        height = 25
      ))
    }
  ) %>%
  # fmt_number(columns = 10, decimals = 1) %>%
  gtsave("qb_ranks.png")

pbp$game

num_games <- pbp %>%
  filter(!is.na(posteam), season >= 2006, season_type == "REG") %>%
  group_by(posteam, season) %>%
  summarize(team_games = n_distinct(game_id))

## GET LIST OF PASSERS FOR PLAYER COMPARISONS/PAGES
passers <- pbp %>%
  filter(down < 5, season_type == "REG", !is.na(qb_epa), pass == 1 | rush == 1) %>%
  mutate(no_play = ifelse(play_type == "no_play", 1, 0)) %>%
  group_by(name, id, season) %>%
  summarize(
    espn_plays = n() - sum(no_play),
    Plays = n(),
    Dropbacks = sum(pass),
    EPA = mean(qb_epa, na.rm = T),
    CPOE = mean(cpoe, na.rm = T),
    air_yards = mean(air_yards, na.rm = T),
    sack_rate = sum(sack, na.rm = T) / Dropbacks,
    cp = mean(cp, na.rm = T),
    wpa = mean(wpa, na.rm = T),
    xpass = mean(xpass, na.rm = T)
  ) %>%
  ungroup() %>%
  mutate(is_eligible = case_when(
    # season == 2021 & espn_plays >= min_plays & Dropbacks >= min_plays ~ 1,
    espn_plays >= 300 & Dropbacks >= 250 ~ 1,
    TRUE ~ 0
  )) %>%
  filter(is_eligible == 1) %>%
  mutate(
    EPA_pct = percent_rank(EPA)*100,
    CPOE_pct = percent_rank(CPOE)*100,
    air_yards_pct = percent_rank(air_yards)*100,
    sack_rate_pct = percent_rank(sack_rate)*100,
    cp_pct = percent_rank(cp)*100,
    wpa_pct = percent_rank(wpa)*100,
    xpass_pct = percent_rank(xpass)*100
  ) %>%
  merge(roster, by.x=c("id","season"), by.y=c("gsis_id","season")) %>%
  merge(qbrs, by.x=c("qbr_join","season"),by.y=c("qbr_join","season")) %>%
  merge(pff, by.x=c("pff_id","season"), by.y=c("player_id","season")) %>%
  merge(teams_colors_logos, by.x=c("team.x"), by.y=c("team_abbr")) %>%
  merge(dvoa, by.x=c("name.x","season"), by.y=c("Player","Year")) %>%
  left_join(caphits, by=c("full_name"="Player","season"="Year")) %>%
  merge(pff_teams, by.x=c("team_name.y","season"), by.y=c("team","season"), all.x = T) %>%
  left_join(num_games, by=c("team.x"="posteam","season"="season")) %>%
  mutate(
    pa = ifelse(is.na(team_games), team_pa / 16, team_pa / team_games),
    PFF = grades_offense,
    qbr_pct = percent_rank(qbr_total)*100,
    DVOA_pct = percent_rank(DVOA)*100,
    DYAR_pct = percent_rank(DYAR)*100,
    PFF_pct = percent_rank(grades_offense)*100,
    pa_pct = percent_rank(team_pa)*100,
    pblk_pct = percent_rank(grades_pblk)*100,
    recv_pct = percent_rank(grades_recv)*100
    ) %>%
  rename(
    name = name.x,
    team = team.x,
    position = position.x,
    team_name = team.y,
    team_name_alt = team_name.x,
    full_team_name = team_name.y
  ) %>%
  select(
    headshot_url,
    full_name,
    season,
    team_logo_espn,
    Plays,
    EPA_pct,
    CPOE_pct,
    DVOA_pct,
    DYAR_pct,
    PFF_pct,
    qbr_pct,
    sack_rate_pct,
    air_yards_pct,
    cp_pct,
    recv_pct,
    pblk_pct,
    pa_pct,
    EPA,
    CPOE,
    DVOA,
    DYAR,
    PFF,
    qbr_total,
    sack_rate,
    air_yards,
    cp,
    grades_recv,
    grades_pblk,
    pa,
    team_wordmark
  ) %>%
  distinct() %>%
  arrange(full_name, season)

(0.257-0.216)/sd(passers$EPA)
1.1/sd(passers$qbr_total)
(0.278-0.266)/sd(passers$DVOA)
(1892-1511)/sd(passers$DYAR)

empty_row = c("","","","","","","","","","","","","","","","","")

passers <- rbind(empty_row, passers)

write.csv(passers, "qb_comps.csv", row.names = F)
