library(readr)
library(dplyr)

games_2021 <- read_csv("https://raw.githubusercontent.com/MattC137/Open_Data/master/Data/Sports/NBA/NBA_2021_Games.csv")
games_2020 <- read_csv("https://raw.githubusercontent.com/MattC137/Open_Data/master/Data/Sports/NBA/NBA_2020_Games.csv")
games_2019 <- read_csv("https://raw.githubusercontent.com/MattC137/Open_Data/master/Data/Sports/NBA/NBA_2019_Games.csv")
games_2018 <- read_csv("https://raw.githubusercontent.com/MattC137/Open_Data/master/Data/Sports/NBA/NBA_2018_Games.csv")
# games_2021 <- read_csv("https://raw.githubusercontent.com/MattC137/Open_Data/master/Data/Sports/NBA/NBA_2017_Games.csv")

games <- rbind(games_2021, games_2020, games_2019, games_2018)

View(games_2021 %>% filter(Result == "TBD"))
apply(is.na(games_2021 %>% filter(Result == "TBD")), 2, sum)

games_2021[games_2021$Result == "TBD", c("Threes_Made_For", "Threes_Att_For", "FG_Made_For", "FG_Att_For", "FT_Made_For", "FT_Att_For", 
                                         "FG_Made_Against", "FG_Att_Against", "Threes_Made_Against", "Threes_Att_Against", "FT_Made_Against", 
                                         "FT_Att_Against")] <- 0


#### Standings Table ####

View(
  games_2021 %>% filter(Season_Type == "Regular-Season") %>% group_by(Team) %>% summarize(
    Wins = sum(Result == "W"),
    Losses = sum(Result == "L"),
    PCT = Wins/(Wins + Losses)
  ) %>% arrange(desc(PCT))
)

#### Stat Leaders ####



































