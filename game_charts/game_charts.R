library(nflreadr)
library(tidyverse)
library(ggtext)
library(plotly)

options(remove(list=ls()))

pbp <- load_pbp(seasons = 2017:2021) %>%
  select(down, ydstogo, qtr, desc, posteam, week, season, play_type_nfl, epa)
write.csv(pbp, "/Users/tuckerboynton/Desktop/R/Shiny/game_charts/game_charts/game_charts.csv")

pbp <- read_csv("game_charts.csv") %>%
  filter(posteam == team_entered, week == week_entered, season == season_entered)

teams <- load_teams()

games <- load_schedules()

game <- games %>%
  filter(week == week_entered, season == season_entered,
         home_team == team_entered | away_team == team_entered)

game_breakdown <- pbp %>%
  filter(!is.na(posteam), play_type_nfl != "END_QUARTER", play_type_nfl != "GAME_START",
         play_type_nfl != "END_GAME", play_type_nfl != "TIMEOUT") %>%
  mutate(
    play_type_nfl = case_when(
      play_type_nfl == "PUNT" | play_type_nfl == "KICK_OFF" ~ "PUNT/KO",
      play_type_nfl == "FIELD_GOAL" | play_type_nfl == "XP_KICK" | play_type_nfl == "PAT2" ~ "FG/PAT",
      TRUE ~ play_type_nfl
    ),
    epa = round(epa, 2)
  ) %>%
  arrange(abs(epa))

game_breakdown$play_type_nfl <- factor(game_breakdown$play_type_nfl,
                                       levels = c("PASS","RUSH","SACK","PENALTY","FG/PAT","PUNT/KO"))

team_name <- teams %>%
  filter(team_abbr == team_entered) %>%
  select(team_name) %>%
  as.character()



x <- game_breakdown$play_type_nfl
y <- game_breakdown$epa
text <- paste0("EPA: ", game_breakdown$epa, "\n",
               "Down: ", game_breakdown$down, ", Distance: ", game_breakdown$ydstogo, "\n",
               "Q", game_breakdown$qtr, " ",
               game_breakdown$desc)
data <- data.frame(x, y, text)

fig <- plot_ly(data, x = ~x, y = ~y, type = 'bar', text = text,
               hoverinfo = "text",
               marker = list(color = ~game_breakdown$epa,
                             showscale = TRUE,
                             line = list(color = "black",
                                         width = 1.5)))
fig <- fig %>% layout(title = paste0(team_name, " Possession Report - Week ",
                                     week_entered, ", ", season_entered, "\n",
                                     game$home_team[1], ": ", game$home_score[1],
                                     ", ", game$away_team[1], ": ", game$away_score[1]),
                      xaxis = list(title = ""),
                      yaxis = list(title = "EPA"))

fig

