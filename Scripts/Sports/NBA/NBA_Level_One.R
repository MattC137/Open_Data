library(dplyr)
library(rvest)
library(readr)
library(tidyr)
library(stringr)
library(lubridate)

#### Get Teams ####

## Only run to update and generate .csv the rest need to be complete by hand 

# team_html <- read_html("https://www.espn.com/nba/teams")
# team_ids <- team_html %>% str_extract_all('(?<=nba/team/_/name/)(.*?)(?=")') %>% unlist() %>% data.frame()
# names(team_ids) <- c("ids_raw")
# team_ids <- team_ids %>%
#   separate("ids_raw", c("Short_Name", "Team_ID") ,sep = "/") %>%
#   arrange(Short_Name) %>%
#   mutate(
#     dup = ifelse(Short_Name == lag(Short_Name, 1), 1, 0)
#   )
# 
# team_ids[1, "dup"] <- 0
# 
# team_ids <- team_ids %>% filter(dup == 0) %>% select(-dup)
# 
# write.csv(team_ids, "~/GitHub/Open_Data/Scripts/Sports/NBA/team_ids_TO_UPDATE.csv", row.names = F)

## End .csv Update

team_ids <- read_csv("~/GitHub/Open_Data/Scripts/Sports/NBA/team_ids.csv")

Schedule <- as.data.frame(matrix(nrow = 0, ncol = 9))
names(Schedule) <- c("Date", "Team", "Opponent", "Result", "W_L", "Season", "Season_Type", "Playoff_Round", "Game_Id")

for(i in 1:nrow(team_ids)){
  # i = 1
  team_name <- as.character(team_ids[i, "Team_Name"])
  short_name <- as.character(team_ids[i, "Short_Name"])
  team_id <- as.character(team_ids[i, "Team_ID"])
  team_city <- as.character(team_ids[i, "City"])
  
  #### Update Seasons ####
  # current_year <- year(Sys.Date())
  
  for(season in 2020:2020){
    # season = 2020
    
    Schedule_Season <- as.data.frame(matrix(nrow = 0, ncol = 9))
    names(Schedule_Season) <- c("Date", "Team", "Opponent", "Result", "W_L", "Season", "Season_Type", "Playoff_Round", "Game_Id")
    
    
    print(paste(team_name, season))
    
    for(season_type in 1:3){
      # season_type = 3
      
      if(season_type != 3){
        team_schedule_url <- try({read_html(paste0("https://www.espn.com/nba/team/schedule/_/name/", short_name, "/season/", season, "/seasontype/", season_type))})
        schedule <- try({team_schedule_url %>% html_node("table") %>% html_table()}, silent = T)
        game_ids <- team_schedule_url %>% str_extract_all('(?<=gameId=)(.*?)(?=")') %>% unlist() %>% unique()
        
        if(class(schedule) != 'try-error'){
          names(schedule) <- c("Date", "Opponent", "Result", "W_L", "Hi_Points", "Hi_Rebounds", "Hi_Assists")
          schedule <- schedule %>% 
            select(Date, Opponent, Result, W_L) %>% filter(Date != "DATE") %>% 
            mutate(
              Team = team_city,
              Season = season,
              Season_Type = case_when(
                season_type == 1 ~ "Pre-Season",
                season_type == 2 ~ "Regular-Season",
                season_type == 3 ~ "Post-Season"
              ),
              Playoff_Round = "Non-Playoff"
            )
          
          schedule$Game_Id <- game_ids
          # schedule$Order <- 1:nrow(schedule) DELETE ME
          
          Schedule_Season <- rbind(Schedule_Season, schedule %>% select(Date, Team, Opponent, Result, W_L, Season, Season_Type, Playoff_Round, Game_Id))
        }
        
      }else{
        
        team_schedule_url <- try({read_html(paste0("https://www.espn.com/nba/team/schedule/_/name/", short_name, "/season/", season, "/seasontype/", season_type))})
        schedule <- try({team_schedule_url %>% html_node("table") %>% html_table()}, silent = T)

        if(class(schedule) != 'try-error'){
          
          game_ids <- team_schedule_url %>% str_extract_all('(?<=gameId=)(.*?)(?=")') %>% unlist() %>% unique()
          
          round <- schedule[str_detect(schedule$X1, "Conference") | 
                              str_detect(schedule$X1, "final") |
                              str_detect(schedule$X1, "Final"), 1]
          
          schedule <- schedule[!(str_detect(schedule$X1, "Conference") | 
                                 str_detect(schedule$X1, "final") |
                                 str_detect(schedule$X1, "Final") |
                                 str_detect(schedule$X1, "DATE")), ]
          
          names(schedule) <- c("Date", "Opponent", "Result", "Hi_Points", "Hi_Rebounds", "Hi_Assists")
          
          #### Detect playoff round ####
          
          Playoff_Round_Table <- data.frame(Opp_Raw = unique(schedule$Opponent))
          
          Playoff_Round_Table <- Playoff_Round_Table %>% mutate(
            Opp = str_replace(Opp_Raw, "vs", ""),
            Opp = str_replace(Opp, "@", "")
          )
          
          Round_mapping <- data.frame(Opp = unique(Playoff_Round_Table$Opp))
          Round_mapping$Playoff_Round <- round
          
          Playoff_Round_Table <- Playoff_Round_Table %>% left_join(Round_mapping, by = c("Opp" = "Opp"))
          
          schedule <- schedule %>% 
            select(Date, Opponent, Result) %>% 
            mutate(
              Team = team_name,
              W_L = "NA-NA",
              Season = season,
              Season_Type = case_when(
                season_type == 1 ~ "Pre-Season",
                season_type == 2 ~ "Regular-Season",
                season_type == 3 ~ "Post-Season"
              )
            )
          
          schedule <- schedule %>% left_join(Playoff_Round_Table, by = c("Opponent" = "Opp_Raw"))
          
          schedule$Game_Id <- game_ids
          
          Schedule_Season <- rbind(Schedule_Season, schedule %>% select(Date, Team, Opponent, Result, W_L, Season, Season_Type, Playoff_Round, Game_Id))
        }
      }
    }
    
    #Schedule_Season <- Schedule_Season %>% arrange(Game_Id)
    
    Schedule_Season <- Schedule_Season %>% mutate(
      Date = str_replace(Date, "Sat, ", ""),
      Date = str_replace(Date, "Sun, ", ""),
      Date = str_replace(Date, "Mon, ", ""),
      Date = str_replace(Date, "Tue, ", ""),
      Date = str_replace(Date, "Wed, ", ""),
      Date = str_replace(Date, "Thu, ", ""),
      Date = str_replace(Date, "Fri, ", ""),
      Date = paste0(Date, ", ", season),
      Date = as.Date(Date, format = "%b %d, %Y")
    )
    
    if(month(Schedule_Season$Date[1]) > 4){
      i <- 2
      update_date = TRUE
      while(update_date){
        
        update_date = Schedule_Season[i-1, "Date"] < Schedule_Season[i, "Date"]
        Schedule_Season[i-1, "Date"] <- as.Date(Schedule_Season[i-1, "Date"] - years(1))
        # schedule[i, Date] = ifelse(schedule[i, Date] <= schedule[i-1, Date], as.Date(schedule[i, Date] - years(1)), as.Date(schedule[i, Date]))

        i = i + 1
      }
    }
    Schedule <- rbind(Schedule, Schedule_Season)
  }
}

Schedule_Raw <- Schedule

#### Cleaning Schedule ####

Schedule <- Schedule_Raw

Schedule <- Schedule %>% mutate(
  Home = ifelse(str_detect(Opponent, "vs"), 1, 0),
  Neutral = ifelse(str_detect(Opponent, " \\*"), 1, 0),
  Opponent = str_remove(Opponent, "vs"),
  Opponent = str_remove(Opponent, "@"),
  Opponent = str_remove(Opponent, " \\*"),
  OT_Rounds = case_when(
    str_detect(Result, " OT") ~ 1,
    str_detect(Result, " 2OT") ~ 2,
    str_detect(Result, " 3OT") ~ 3,
    str_detect(Result, " 4OT") ~ 4,
    str_detect(Result, " 5OT") ~ 5,
    str_detect(Result, " 6OT") ~ 6,
    str_detect(Result, " 7OT") ~ 7,
    str_detect(Result, " 8OT") ~ 8,
    str_detect(Result, " 9OT") ~ 9,
    str_detect(Result, " 10OT") ~ 10,
    str_detect(Result, " 11OT") ~ 11,
    str_detect(Result, " 12OT") ~ 12,
    TRUE ~ 0
    ),
  Score = Result,
  Score = ifelse(Score == "Postponed", "0-0", Score),
  Score = str_replace(Score, " OT", ""),
  Score = str_replace(Score, " 2OT", ""),
  Score = str_replace(Score, " 3OT", ""),
  Score = str_replace(Score, " 4OT", ""),
  Score = str_replace(Score, " 5OT", ""),
  Score = str_replace(Score, " 6OT", ""),
  Score = str_replace(Score, " 7OT", ""),
  Score = str_replace(Score, " 8OT", ""),
  Score = str_replace(Score, " 9OT", ""),
  Score = str_replace(Score, " 10OT", ""),
  Score = str_replace(Score, " 11OT", ""),
  Score = str_replace(Score, " 12OT", ""),
  Score = str_replace(Score, "L", ""),
  Score = str_replace(Score, "W", ""),
  Score = str_replace(Score, "P", ""),
  Score = str_replace(Score, "D", ""),
  Score = str_replace(Score, "T", ""),
  Score = ifelse(Score == "Postponed", "0-0", Score),
  Result = substr(Result, 1, 1)
)

Schedule <- Schedule %>% separate(Score, c("Points_Winner", "Points_Loser"), sep = "-")
Schedule <- Schedule %>% mutate(
  Points_For = ifelse(Result == "W", Points_Winner, Points_Loser),
  Points_Against = ifelse(Result == "W", Points_Loser, Points_Winner)
)

Schedule <- Schedule %>% select(
  Date, Team, Opponent, Result, Points_For, Points_Against, Home, Neutral, OT_Rounds, Season, Season_Type, Playoff_Round, Game_Id
)

opp_team_ids <- team_ids
names(opp_team_ids) <- paste0("Opp_", names(opp_team_ids))

Schedule <- Schedule %>% 
  left_join(team_ids, by = c("Team" = "City")) %>% 
  left_join(opp_team_ids, by = c("Opponent" = "Opp_City")) %>% 
  filter(complete.cases(.))
  arrange(Game_Id, Date)

#### Update Level Two Data ####

Schedule <- Schedule %>% mutate(
  Q1_Points_For = 0,
  Q2_Points_For = 0,
  Q3_Points_For = 0,
  Q4_Points_For = 0,
  OT_Points_For = 0,
  Q1_Points_Against = 0,
  Q2_Points_Against = 0,
  Q3_Points_Against = 0,
  Q4_Points_Against = 0,
  OT_Points_Against = 0
)

for(i in 1:nrow(Schedule)){
  # i = 1
  if(i %% 2 != 0){
    game_id <- Schedule$Game_Id[i]
    team_stats_url <- read_html(paste0("https://www.espn.com/nba/matchup?gameId=", game_id))
    team_stats <- team_stats_url %>% html_nodes("table") %>% html_table()
    
    #### Quarter Points ####
    
    quarters <- team_stats[[1]]
    home_team <- quarters[2, 1]
    away_team <- quarters[1, 1]
    
    # First solve which team is home on Neutral games
    
    if(Schedule$Neutral[i] == TRUE){
      Schedule$Home[i] <- ifelse(Schedule$Short_Name[i] == tolower(home_team), 1, 0)
      Schedule$Home[i+1] <- ifelse(Schedule$Short_Name[i+1] == tolower(home_team), 1, 0)
    }
    
    quarters <- quarters[ , -ncol(quarters)]
    
    Schedule$Q1_Points_For[i] <- ifelse(Schedule$Home[i] == TRUE, quarters[2, 2], quarters[1, 2])
    Schedule$Q2_Points_For[i] <- ifelse(Schedule$Home[i] == TRUE, quarters[2, 3], quarters[1, 3])
    Schedule$Q3_Points_For[i] <- ifelse(Schedule$Home[i] == TRUE, quarters[2, 4], quarters[1, 4])
    Schedule$Q4_Points_For[i] <- ifelse(Schedule$Home[i] == TRUE, quarters[2, 5], quarters[1, 5])
    Schedule$Q1_Points_Against[i] <- ifelse(Schedule$Home[i] == FALSE, quarters[2, 2], quarters[1, 2])
    Schedule$Q2_Points_Against[i] <- ifelse(Schedule$Home[i] == FALSE, quarters[2, 3], quarters[1, 3])
    Schedule$Q3_Points_Against[i] <- ifelse(Schedule$Home[i] == FALSE, quarters[2, 4], quarters[1, 4])
    Schedule$Q4_Points_Against[i] <- ifelse(Schedule$Home[i] == FALSE, quarters[2, 5], quarters[1, 5])
    
    if(ncol(quarters) > 5){
      Schedule$OT_Points_For[i] <- ifelse(Schedule$Home[i] == TRUE, sum(quarters[2, c(6:ncol(quarters))]), sum(quarters[1, c(6:ncol(quarters))]))
      Schedule$OT_Points_Against[i] <- ifelse(Schedule$Home[i] == FALSE, sum(quarters[2, c(6:ncol(quarters))]), sum(quarters[1, c(6:ncol(quarters))]))
    }

    Schedule$Q1_Points_For[i+1] <- ifelse(Schedule$Home[i+1] == TRUE, quarters[2, 2], quarters[1, 2])
    Schedule$Q2_Points_For[i+1] <- ifelse(Schedule$Home[i+1] == TRUE, quarters[2, 3], quarters[1, 3])
    Schedule$Q3_Points_For[i+1] <- ifelse(Schedule$Home[i+1] == TRUE, quarters[2, 4], quarters[1, 4])
    Schedule$Q4_Points_For[i+1] <- ifelse(Schedule$Home[i+1] == TRUE, quarters[2, 5], quarters[1, 5])
    Schedule$Q1_Points_Against[i+1] <- ifelse(Schedule$Home[i+1] == FALSE, quarters[2, 2], quarters[1, 2])
    Schedule$Q2_Points_Against[i+1] <- ifelse(Schedule$Home[i+1] == FALSE, quarters[2, 3], quarters[1, 3])
    Schedule$Q3_Points_Against[i+1] <- ifelse(Schedule$Home[i+1] == FALSE, quarters[2, 4], quarters[1, 4])
    Schedule$Q4_Points_Against[i+1] <- ifelse(Schedule$Home[i+1] == FALSE, quarters[2, 5], quarters[1, 5])
    
    if(ncol(quarters) > 5){
      Schedule$OT_Points_For[i+1] <- ifelse(Schedule$Home[i+1] == TRUE, sum(quarters[2, c(6:ncol(quarters))]), sum(quarters[1, c(6:ncol(quarters))]))
      Schedule$OT_Points_Against[i+1] <- ifelse(Schedule$Home[i+1] == FALSE, sum(quarters[2, c(6:ncol(quarters))]), sum(quarters[1, c(6:ncol(quarters))]))
    }
    
  }
}
