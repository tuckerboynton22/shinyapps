library(ggridges)
library(ggtext)
library(ggplot2)
library(tidyverse)
library(nflreadr)

seasons <- 2006:2021
pbp <- load_pbp(seasons = seasons)

# pbp <- pbp_full %>% filter(season >= 2006)

pbp_trimmed <- pbp %>%
  filter(!is.na(air_yards), !is.na(name)) %>%
  mutate(name = ifelse(name == "Aa.Rodgers", "A.Rodgers", name)) %>%
  select(name, id, season, air_yards) %>%
  group_by(name, id) %>%
  mutate(total_passes = n()) %>%
  filter(total_passes >= 100) %>%
  ungroup()

write.csv(pbp_trimmed, "/Users/tuckerboynton/Desktop/R/Shiny/airyards/passes.csv")

pbp %>%
  filter(!is.na(air_yards), season == 2020) %>%
  mutate(
    color = case_when(
      name == "T.Brady" ~ "#D50A0A",
      TRUE ~ "#04328C"
    )
  ) %>%
  ggplot(aes(x = air_yards, color = color, fill = color)) +
  geom_density(alpha = 0.5, show.legend = FALSE) +
  scale_color_identity(aesthetics = c("fill", "color"))+
  ggthemes::theme_fivethirtyeight() +
  labs(x = "Air yards",
       y = "Density",
       caption = "Figure: @Tucker_TnL | Data: @nflfastR",
       subtitle = "<span style='color:#04328C'>League Avg</span>
                  vs.
                  <span style='color:#D50A0A'>T.Brady</span>",
       title = "Air Yard Distribution") +
  ggthemes::theme_fivethirtyeight()+
  theme(
    legend.position = "none",
    plot.title = element_markdown(size = 22, hjust = 0.5),
    plot.subtitle = element_markdown(size = 12, hjust = 0.5),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
  ) +
  xlim(-15, 60)
