library(readr)
library(dplyr)

#### Game Summaries ####

games_2021 <- read_csv("https://raw.githubusercontent.com/MattC137/Open_Data/master/Data/Sports/NBA/NBA_2021_Games.csv")
glimpse(games_2021)

View(games_2021 %>% filter(Season_Type == "Regular-Season", Game_Id == "401267173"))

View(games_2021 %>% filter(Result == "TBD", !is.na(Money_Line)) %>% select(Date, Team, Opponent, Line_Favored, Line_Amount, Over_Under, Money_Line, Implied_Odds, Implied_Odds_Opp))

#### Box Score Summaries ####

box_2021 <- read.csv("https://raw.githubusercontent.com/MattC137/Open_Data/master/Data/Sports/NBA/NBA_2021_Box_Score.csv")

View(box_2021 %>% filter(Season_Type == "Regular-Season", Game_Id == "401267173"))

#### Box Score Summaries ####

box_2021 <- read.csv("https://raw.githubusercontent.com/MattC137/Open_Data/master/Data/Sports/NBA/NBA_2021_Box_Score.csv")

View(box_2021 %>% filter(Season_Type == "Regular-Season", Game_Id == "401267173"))

#### Play_by_Play ####

play_by_play_2021 <- read.csv("https://raw.githubusercontent.com/MattC137/Open_Data/master/Data/Sports/NBA/NBA_2021_Play_by_Play.csv")

View(play_by_play_2021 %>% filter(Game_Id == "401267173"))

#### Players ####

players_2021 <- read.csv("https://raw.githubusercontent.com/MattC137/Open_Data/master/Data/Sports/NBA/NBA_2021_Players.csv")
View(players_2021 %>% filter(Short_Name == "PHI"))

#### Shots ####
 
shots_2021 <- read.csv("https://raw.githubusercontent.com/MattC137/Open_Data/master/Data/Sports/NBA/NBA_2021_Shots.csv")
View(shots_2021 %>% filter(Game_Id == "401267173"))


#### Delete Me ####

players_team_recon <- players_2021 %>% group_by(Team_Id, Team) %>% tally()
players_team_recon_max <- players_team_recon %>% group_by(Team_Id) %>% summarize(n_max = max(n))
players_team_recon <- players_team_recon %>% left_join(players_team_recon_max) %>% filter(n == n_max)
players_2021 <- players_2021 %>% select(-Team) %>% left_join(players_team_recon %>% select(Team_Id, Team))

























