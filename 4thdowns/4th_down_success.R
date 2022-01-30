library(nfl4th)
library(nflreadr)

fourth_downs <- read_csv("fourth_2014.csv") %>%
  mutate(season = 2014) %>%
  rbind(read_csv("fourth_2015.csv") %>% mutate(season = 2015)) %>%
  rbind(read_csv("fourth_2016.csv") %>% mutate(season = 2016)) %>%
  rbind(read_csv("fourth_2017.csv") %>% mutate(season = 2017)) %>%
  rbind(read_csv("fourth_2018.csv") %>% mutate(season = 2018)) %>%
  rbind(read_csv("fourth_2019.csv") %>% mutate(season = 2019)) %>%
  rbind(read_csv("fourth_2020.csv") %>% mutate(season = 2020)) %>%
  rbind(read_csv("fourth_2021.csv") %>% mutate(season = 2021)) %>%
  mutate(rownum = row_number())

fourth_downs_kicks <- fourth_downs %>%
  filter(go_boost >= 2) %>%
  mutate(
    success = zoo::rollmeanr(fourth_success, 200, na.pad=TRUE, align="right")
  )

fourth_downs_kicks %>%
  ggplot(aes(rownum, success)) +
  geom_point() +
  geom_smooth() +
  labs(
    x = "Decisions Since the First Rec of Wk 1, 2014",
    y = "Go-for-it Rate in Last 200 Strong 'Go' Recommendations",
    title = "Teams Are Listening to the 4th Down Data",
    subtitle = "Min. strength of rec = 2 percentage points",
    caption = "Figure: @Tucker_TnL\nData: @benbbaldwin's 4th down model"
  ) +
  ggthemes::theme_fivethirtyeight() +
  theme(
    legend.position = "none",
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )

league_performance <- fourth_downs %>%
  filter(go_boost > 3, week <= 17) %>%
  group_by(season, week) %>%
  summarize(fourth_success = mean(fourth_success),
            xwp_boost = mean(xwp_boost),
            n = n(),
            date = paste0("Week ", week, ", ", season)) %>%
  distinct() %>%
  arrange(desc(fourth_success)) %>%
  head(10)

team_conversion <- fourth_downs %>%
  filter(go > 0, !is.na(first_down)) %>%
  group_by(posteam) %>%
  summarize(conversion_rate = mean(first_down, na.rm = T),
            x_conversion_rate = mean(first_down_prob, na.rm = T),
            n = n()) %>%
  merge(nflfastR::teams_colors_logos, by.x = "posteam", by.y = "team_abbr")

team_conversion %>%
  ggplot(aes(x_conversion_rate, conversion_rate)) +
  geom_abline() +
  ggimage::geom_image(aes(image = team_logo_espn), asp = 14/9) +
  ggrepel::geom_text_repel(aes(label = n), size = 6) +
  labs(x = "Expected Conversion Rate",
       y = "Actual Conversion Rate",
       title = "Not All 4th Down 'Go-for-it's Are Created Equal",
       subtitle = "2014-21 4th Down conversion rate vs. expected; Number of team attempts labeled",
       caption = "Figure: @Tucker_TnL\nData: @benbbaldwin's nfl4th"
  ) +
  annotate("label", x = max(team_conversion$x_conversion_rate) - 0.01,
           y = min(team_conversion$conversion_rate), label = "Convert less\nthan expected") +
  annotate("label", x = min(team_conversion$x_conversion_rate) + 0.01,
           y = max(team_conversion$conversion_rate), label = "Convert more\nthan expected") +
  ggthemes::theme_fivethirtyeight() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
  ) +
  scale_x_continuous(breaks = seq(0.4,0.6,0.1))

