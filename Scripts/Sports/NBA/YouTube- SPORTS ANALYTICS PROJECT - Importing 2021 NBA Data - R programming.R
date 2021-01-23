library(readr)
library(dplyr)

games_2021 <- read_csv("https://raw.githubusercontent.com/MattC137/Open_Data/master/Data/Sports/NBA/NBA_2021_Games.csv")
games_2020 <- read_csv("https://raw.githubusercontent.com/MattC137/Open_Data/master/Data/Sports/NBA/NBA_2020_Games.csv")
games_2019 <- read_csv("https://raw.githubusercontent.com/MattC137/Open_Data/master/Data/Sports/NBA/NBA_2019_Games.csv")
games_2018 <- read_csv("https://raw.githubusercontent.com/MattC137/Open_Data/master/Data/Sports/NBA/NBA_2018_Games.csv")
# games_2021 <- read_csv("https://raw.githubusercontent.com/MattC137/Open_Data/master/Data/Sports/NBA/NBA_2017_Games.csv")

games <- rbind(games_2021, games_2020, games_2019, games_2018)

View(games_2021 %>% filter(Result == "TBD"))

#### Standings Table ####

View(
  games_2021 %>% filter(Season_Type == "Regular-Season") %>% group_by(Team) %>% summarize(
    Wins = sum(Result == "W"),
    Losses = sum(Result == "L"),
    PCT = Wins/(Wins + Losses)
  ) %>% arrange(desc(PCT))
)

#### Stat Leaders ####



































