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
  
  team_summary <- games %>% filter(Season_Type == "Regular-Season", Result != "TBD") %>% group_by(Team = Short_Name) %>% summarize(
    Wins = mean(Result == "W"),
    Losses = mean(Result == "L"),
    Total_Games = Wins + Losses,
    PCT = Wins/(Wins + Losses),
    
    Total_Points_For = mean(Points_For, na.rm = T),
    Total_Points_Against = mean(Points_Against, na.rm = T),
    Points_For = mean(Points_For, na.rm = T),
    Points_Against = mean(Points_Against, na.rm = T),
    Point_Diff = Points_For - Points_Against,
    
    FG_Made_For = mean(FG_Made_For, na.rm = T),
    FG_Made_Against = mean(FG_Made_Against, na.rm = T),
    FG_Made_For = mean(FG_Made_For, na.rm = T),
    FG_Made_Against = mean(FG_Made_Against, na.rm = T),
    FG_Made_Diff = FG_Made_For - FG_Made_Against,
    
    FG_Att_For = mean(FG_Att_For, na.rm = T),
    FG_Att_Against = mean(FG_Att_Against, na.rm = T),
    FG_Att_For = mean(FG_Att_For, na.rm = T),
    FG_Att_Against = mean(FG_Att_Against, na.rm = T),
    FG_Att_Diff = FG_Att_For - FG_Att_Against,
    
    Total_Assists_For = mean(Assists_For, na.rm = T),
    Total_Assists_Against = mean(Assists_Against, na.rm = T),
    Assists_For = mean(Assists_For, na.rm = T),
    Assists_Against = mean(Assists_Against, na.rm = T),
    Assists_Diff = Assists_For - Assists_Against,
    
    Total_Assists_For = mean(Assists_For, na.rm = T),
    Total_Assists_Against = mean(Assists_Against, na.rm = T),
    Assists_For = mean(Assists_For, na.rm = T),
    Assists_Against = mean(Assists_Against, na.rm = T),
    Assists_Diff = Assists_For - Assists_Against,
    
    Total_Rebounds_For = mean(Rebounds_For, na.rm = T),
    Total_Rebounds_Against = mean(Rebounds_Against, na.rm = T),
    Rebounds_For = mean(Rebounds_For, na.rm = T),
    Rebounds_Against = mean(Rebounds_Against, na.rm = T),
    Rebounds_Diff = Rebounds_For - Rebounds_Against,
    
    Total_Offensive_Rebounds_For = mean(Offensive_Rebounds_For, na.rm = T),
    Total_Offensive_Rebounds_Against = mean(Offensive_Rebounds_Against, na.rm = T),
    Offensive_Rebounds_For = mean(Offensive_Rebounds_For, na.rm = T),
    Offensive_Rebounds_Against = mean(Offensive_Rebounds_Against, na.rm = T),
    Offensive_Rebounds_Diff = Offensive_Rebounds_For - Offensive_Rebounds_Against,
    
    Total_Defensive_Rebounds_For = mean(Defensive_Rebounds_For, na.rm = T),
    Total_Defensive_Rebounds_Against = mean(Defensive_Rebounds_Against, na.rm = T),
    Defensive_Rebounds_For = mean(Defensive_Rebounds_For, na.rm = T),
    Defensive_Rebounds_Against = mean(Defensive_Rebounds_Against, na.rm = T),
    Defensive_Rebounds_Diff = Defensive_Rebounds_For - Defensive_Rebounds_Against,
    
    Total_Threes_Made_For = mean(Threes_Made_For, na.rm = T),
    Total_Threes_Made_Against = mean(Threes_Made_Against, na.rm = T),
    Threes_Made_For = mean(Threes_Made_For, na.rm = T),
    Threes_Made_Against = mean(Threes_Made_Against, na.rm = T),
    Threes_Made_Diff = Threes_Made_For - Threes_Made_Against,
    
    Total_Threes_Att_For = mean(Threes_Att_For, na.rm = T),
    Total_Threes_Att_Against = mean(Threes_Att_Against, na.rm = T),
    Threes_Att_For = mean(Threes_Att_For, na.rm = T),
    Threes_Att_Against = mean(Threes_Att_Against, na.rm = T),
    Threes_Att_Diff = Threes_Att_For - Threes_Att_Against,
    
    Total_Steals_For = mean(Steals_For, na.rm = T),
    Total_Steals_Against = mean(Steals_Against, na.rm = T),
    Steals_For = mean(Steals_For, na.rm = T),
    Steals_Against = mean(Steals_Against, na.rm = T),
    Steals_Diff = Steals_For - Steals_Against,
  
    Total_Rebounds_For = mean(Rebounds_For, na.rm = T),
    Total_Rebounds_Against = mean(Rebounds_Against, na.rm = T),
    Rebounds_For = mean(Rebounds_For, na.rm = T),
    Rebounds_Against = mean(Rebounds_Against, na.rm = T),
    Rebounds_Diff = Rebounds_For - Rebounds_Against,
    
    Total_FT_Made_For = mean(FT_Made_For, na.rm = T),
    Total_FT_Made_Against = mean(FT_Made_Against, na.rm = T),
    FT_Made_For = mean(FT_Made_For, na.rm = T),
    FT_Made_Against = mean(FT_Made_Against, na.rm = T),
    FT_Made_Diff = FT_Made_For - FT_Made_Against,
    
    Total_FT_Att_For = mean(FT_Att_For, na.rm = T),
    Total_FT_Att_Against = mean(FT_Att_Against, na.rm = T),
    FT_Att_For = mean(FT_Att_For, na.rm = T),
    FT_Att_Against = mean(FT_Att_Against, na.rm = T),
    FT_Att_Diff = FT_Att_For - FT_Att_Against,
    
    Total_Blocks_For = mean(Blocks_For, na.rm = T),
    Total_Blocks_Against = mean(Blocks_Against, na.rm = T),
    Blocks_For = mean(Blocks_For, na.rm = T),
    Blocks_Against = mean(Blocks_Against, na.rm = T),
    Blocks_Diff = Blocks_For - Blocks_Against,
    
    Total_Total_Turnovers_For = mean(Total_Turnovers_For, na.rm = T),
    Total_Total_Turnovers_Against = mean(Total_Turnovers_Against, na.rm = T),
    Total_Turnovers_For = mean(Total_Turnovers_For, na.rm = T),
    Total_Turnovers_Against = mean(Total_Turnovers_Against, na.rm = T),
    Total_Turnovers_Diff = Total_Turnovers_For - Total_Turnovers_Against,
    
    Total_Fouls_For = mean(Fouls_For, na.rm = T),
    Total_Fouls_Against = mean(Fouls_Against, na.rm = T),
    Fouls_For = mean(Fouls_For, na.rm = T),
    Fouls_Against = mean(Fouls_Against, na.rm = T),
    Fouls_Diff = Fouls_For - Fouls_Against
    
  ) %>% arrange(desc(PCT))

qc <- function(games, box_scores, season_type){
  
  #### Stat Leaders ####
  
  games_summary <- games %>% filter(Season_Type == season_type, Result != "TBD") %>% group_by(Team = Short_Name) %>% summarize(
    games_Total_Points = sum(Points_For, na.rm = T),
    games_Assists = sum(Assists_For, na.rm = T),
    games_Steals = sum(Steals_For, na.rm = T),
  )
  
  ## ADD TO CORE CODE ##
  bs_summary <- box_scores %>% filter(Season_Type == season_type) %>% group_by(Team) %>% summarize(
      bs_Total_Points = sum(Points, na.rm = T),
      bs_Assists = sum(Assists, na.rm = T),
      bs_Steals = sum(Steals, na.rm = T)
    ) %>% arrange(desc(bs_Total_Points))
  
  qc <- games_summary %>%
    left_join(bs_summary, by = c("Team" = "Team")) %>% mutate(
      tf_Total_Points = bs_Total_Points == games_Total_Points,
      tf_Assists = bs_Assists == games_Assists,
      tf_Steals = bs_Steals == games_Steals
    )
  
  return(qc)
}

View(qc(games_2021, box_2021, "Regular-Season"))

games_2020 <- read.csv("https://raw.githubusercontent.com/MattC137/Open_Data/master/Data/Sports/NBA/NBA_2020_Games.csv")
box_2020 <- read.csv("https://raw.githubusercontent.com/MattC137/Open_Data/master/Data/Sports/NBA/NBA_2020_Box_Score.csv")
View(qc(games_2020, box_2020, "Regular-Season"))

games_2019 <- read.csv("https://raw.githubusercontent.com/MattC137/Open_Data/master/Data/Sports/NBA/NBA_2019_Games.csv")
box_2019 <- read.csv("https://raw.githubusercontent.com/MattC137/Open_Data/master/Data/Sports/NBA/NBA_2019_Box_Score.csv")
View(qc(games_2019, box_2019, "Playoffs"))
