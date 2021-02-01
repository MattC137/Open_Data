library(rvest)
library(dplyr)

games_2021 <- read.csv("https://raw.githubusercontent.com/MattC137/Open_Data/master/Data/Sports/NBA/NBA_2021_Games.csv")
box_2021 <- read.csv("https://raw.githubusercontent.com/MattC137/Open_Data/master/Data/Sports/NBA/NBA_2021_Box_Score.csv")
recap_2021 <- read.csv("https://raw.githubusercontent.com/MattC137/Open_Data/master/Data/Sports/NBA/NBA_2021_Game_Recaps.csv")
pbp_2021 <- read.csv("https://raw.githubusercontent.com/MattC137/Open_Data/master/Data/Sports/NBA/NBA_2021_Play_by_Play.csv")
players_2021 <- read.csv("https://raw.githubusercontent.com/MattC137/Open_Data/master/Data/Sports/NBA/NBA_2021_Players.csv")
shots_2021 <- read.csv("https://raw.githubusercontent.com/MattC137/Open_Data/master/Data/Sports/NBA/NBA_2021_Shots.csv")
line_log_2021 <- read.csv("https://raw.githubusercontent.com/MattC137/Open_Data/master/Data/Sports/NBA/NBA_2021_Line_Log.csv")

upcoming_games <- games_2021 %>% filter(Result == "TBD" & !is.na(Money_Line))

View(games_2021 %>% filter(Result == "TBD"))

#### Current ####

#### Standings Table ####

View(
  games_2021 %>% filter(Season_Type == "Regular-Season", Result != "TBD") %>% group_by(Team) %>% summarize(
    Wins = sum(Result == "W"),
    Losses = sum(Result == "L"),
    Total_Games = Wins + Losses,
    PCT = Wins/(Wins + Losses),
    
    Total_Points_For = sum(Points_For, na.rm = T),
    Total_Points_Against = sum(Points_Against, na.rm = T),
    Points_For = mean(Points_For, na.rm = T),
    Points_Against = mean(Points_Against, na.rm = T),
    Point_Diff = Points_For - Points_Against,
    
    FG_Made_For = sum(FG_Made_For, na.rm = T),
    FG_Made_Against = sum(FG_Made_Against, na.rm = T),
    FG_Made_For = mean(FG_Made_For, na.rm = T),
    FG_Made_Against = mean(FG_Made_Against, na.rm = T),
    FG_Made_Diff = FG_Made_For - FG_Made_Against,
    
    FG_Att_For = sum(FG_Att_For, na.rm = T),
    FG_Att_Against = sum(FG_Att_Against, na.rm = T),
    FG_Att_For = mean(FG_Att_For, na.rm = T),
    FG_Att_Against = mean(FG_Att_Against, na.rm = T),
    FG_Att_Diff = FG_Att_For - FG_Att_Against,
    
    Total_Assists_For = sum(Assists_For, na.rm = T),
    Total_Assists_Against = sum(Assists_Against, na.rm = T),
    Assists_For = mean(Assists_For, na.rm = T),
    Assists_Against = mean(Assists_Against, na.rm = T),
    Assists_Diff = Assists_For - Assists_Against,
    
    Total_Assists_For = sum(Assists_For, na.rm = T),
    Total_Assists_Against = sum(Assists_Against, na.rm = T),
    Assists_For = mean(Assists_For, na.rm = T),
    Assists_Against = mean(Assists_Against, na.rm = T),
    Assists_Diff = Assists_For - Assists_Against,
    
    Total_Rebounds_For = sum(Rebounds_For, na.rm = T),
    Total_Rebounds_Against = sum(Rebounds_Against, na.rm = T),
    Rebounds_For = mean(Rebounds_For, na.rm = T),
    Rebounds_Against = mean(Rebounds_Against, na.rm = T),
    Rebounds_Diff = Rebounds_For - Rebounds_Against,
    
    Total_Offensive_Rebounds_For = sum(Offensive_Rebounds_For, na.rm = T),
    Total_Offensive_Rebounds_Against = sum(Offensive_Rebounds_Against, na.rm = T),
    Offensive_Rebounds_For = mean(Offensive_Rebounds_For, na.rm = T),
    Offensive_Rebounds_Against = mean(Offensive_Rebounds_Against, na.rm = T),
    Offensive_Rebounds_Diff = Offensive_Rebounds_For - Offensive_Rebounds_Against,
    
    Total_Defensive_Rebounds_For = sum(Defensive_Rebounds_For, na.rm = T),
    Total_Defensive_Rebounds_Against = sum(Defensive_Rebounds_Against, na.rm = T),
    Defensive_Rebounds_For = mean(Defensive_Rebounds_For, na.rm = T),
    Defensive_Rebounds_Against = mean(Defensive_Rebounds_Against, na.rm = T),
    Defensive_Rebounds_Diff = Defensive_Rebounds_For - Defensive_Rebounds_Against,
    
    Total_Threes_Made_For = sum(Threes_Made_For, na.rm = T),
    Total_Threes_Made_Against = sum(Threes_Made_Against, na.rm = T),
    Threes_Made_For = mean(Threes_Made_For, na.rm = T),
    Threes_Made_Against = mean(Threes_Made_Against, na.rm = T),
    Threes_Made_Diff = Threes_Made_For - Threes_Made_Against,
    
    Total_Threes_Att_For = sum(Threes_Att_For, na.rm = T),
    Total_Threes_Att_Against = sum(Threes_Att_Against, na.rm = T),
    Threes_Att_For = mean(Threes_Att_For, na.rm = T),
    Threes_Att_Against = mean(Threes_Att_Against, na.rm = T),
    Threes_Att_Diff = Threes_Att_For - Threes_Att_Against,
    
    Total_Steals_For = sum(Steals_For, na.rm = T),
    Total_Steals_Against = sum(Steals_Against, na.rm = T),
    Steals_For = mean(Steals_For, na.rm = T),
    Steals_Against = mean(Steals_Against, na.rm = T),
    Steals_Diff = Steals_For - Steals_Against,

    Total_Rebounds_For = sum(Rebounds_For, na.rm = T),
    Total_Rebounds_Against = sum(Rebounds_Against, na.rm = T),
    Rebounds_For = mean(Rebounds_For, na.rm = T),
    Rebounds_Against = mean(Rebounds_Against, na.rm = T),
    Rebounds_Diff = Rebounds_For - Rebounds_Against,
    
    Total_FT_Made_For = sum(FT_Made_For, na.rm = T),
    Total_FT_Made_Against = sum(FT_Made_Against, na.rm = T),
    FT_Made_For = mean(FT_Made_For, na.rm = T),
    FT_Made_Against = mean(FT_Made_Against, na.rm = T),
    FT_Made_Diff = FT_Made_For - FT_Made_Against,
    
    Total_FT_Att_For = sum(FT_Att_For, na.rm = T),
    Total_FT_Att_Against = sum(FT_Att_Against, na.rm = T),
    FT_Att_For = mean(FT_Att_For, na.rm = T),
    FT_Att_Against = mean(FT_Att_Against, na.rm = T),
    FT_Att_Diff = FT_Att_For - FT_Att_Against,
    
    Total_Blocks_For = sum(Blocks_For, na.rm = T),
    Total_Blocks_Against = sum(Blocks_Against, na.rm = T),
    Blocks_For = mean(Blocks_For, na.rm = T),
    Blocks_Against = mean(Blocks_Against, na.rm = T),
    Blocks_Diff = Blocks_For - Blocks_Against,
    
    Total_Total_Turnovers_For = sum(Total_Turnovers_For, na.rm = T),
    Total_Total_Turnovers_Against = sum(Total_Turnovers_Against, na.rm = T),
    Total_Turnovers_For = mean(Total_Turnovers_For, na.rm = T),
    Total_Turnovers_Against = mean(Total_Turnovers_Against, na.rm = T),
    Total_Turnovers_Diff = Total_Turnovers_For - Total_Turnovers_Against,
    
    Total_Fouls_For = sum(Fouls_For, na.rm = T),
    Total_Fouls_Against = sum(Fouls_Against, na.rm = T),
    Fouls_For = mean(Fouls_For, na.rm = T),
    Fouls_Against = mean(Fouls_Against, na.rm = T),
    Fouls_Diff = Fouls_For - Fouls_Against,
    
  ) %>% arrange(desc(PCT))
)

#### Stat Leaders ####

View(
  box_2021 %>% filter(Season_Type == "Regular-Season") %>% group_by(Team) %>% summarize(
    Wins = sum(Result == "W"),
    Losses = sum(Result == "L"),
    PCT = Wins/(Wins + Losses)
  ) %>% arrange(desc(PCT))
)






