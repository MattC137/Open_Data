library(tidyr)
library(dplyr)
library(stringr)
library(rvest)
library(lubridate)

setwd("~/GitHub/Open_Data/Scripts/Sports")

ncaaf <- read_html("https://www.espn.com/college-football/teams")

ids <- ncaaf %>% str_extract_all('(?<=href="/college-football/team/_/id/)(.*?)(?="><img class=")') %>% unlist()
ids <- ids %>% str_split("/", simplify = T)
ids <- as.data.frame(ids)
names(ids) <- c('ID', 'Name')
ids$FBS <- 1
ids$ID <- as.character(ids$ID)
ids$Name <- as.character(ids$Name)

teams <- ncaaf %>% str_extract_all('(?<=" title=")(.*?)(?=" data-mptype=")') %>% unlist()
seasons <- c(2020:2000)

Schedule <- as.data.frame(matrix(NA, nrow = 0, ncol = 14))
names(Schedule) <- c('Date', 'Opponent', 'Result', 'Team', 'Team_ID', 'Season', 'Home', 'Points_For', 'Points_Against', 'Played', 'OT', 'Opp_ID', 'Opp_Name_ID', 'Game_ID')

ids_temp <- ids

while(nrow(ids_temp) > 0){
  # i = 58
  id <- ids_temp[1, ]
  # id <- ids[ids$ID == "333", ]
  print(id)
  
  for(j in 1:length(seasons)){
    # j = 2
    season <- seasons[j]
    print(season)
    
    # Add Try
    team_games <- try({read_html(paste0("https://www.espn.com/college-football/team/schedule/_/id/", id[1, 1], "/season/", season))})
    
    if(class(team_games) != "try-error"){
      schedule_ids <- team_games %>% str_extract_all('(?<=team/_/id/)(.*?)(?="><img alt=")') %>% unlist()
      
      schedule_ids <- as.data.frame(schedule_ids) %>% separate(schedule_ids, c("ID", "Name"), sep = "/")
      
      schedule_ids_ID <- schedule_ids$ID
      schedule_ids_Name <- schedule_ids$Name
      
      ## Handle Teams without an ID on ESPN
      
      #schedule_ids_blank_teams <- team_games %>% str_extract_all('(?<=height:20px)(.*?)(?="><img alt=")') %>% unlist()
      schedule_ids_blank_teams <- team_games %>% str_extract_all('(?<=height:20px)(.*?)(?=<span)') %>% unlist()
      schedule_ids_blank_teams <- which(!str_detect(schedule_ids_blank_teams, "/id/"))
      
      if(length(schedule_ids_blank_teams) > 0){
        for(i in 1:length(schedule_ids_blank_teams)){
          schedule_ids_ID <- append(schedule_ids_ID, NA, schedule_ids_blank_teams[i]-1)
          schedule_ids_Name <- append(schedule_ids_Name, NA, schedule_ids_blank_teams[i]-1)
        }
        
        nrow(team_games)
        
        schedule_ids <- data.frame(ID = schedule_ids_ID, Name = schedule_ids_Name)
        schedule_ids$FBS <- 0 
      }
      
      if(nrow(schedule_ids) == 0){
        schedule_ids <- as.data.frame(matrix(NA, ncol = 3, nrow = 0))
        names(schedule_ids) <- c("ID", "Name", "FBS")
      }else(
        schedule_ids$FBS <- 0
      )
  
  
    
      ##
      
      game_ids <- team_games %>% str_extract_all('(?<=gameId/)(.*?)(?=<!-- -->)') %>% unlist() %>% str_split('"', simplify = T)
      
      #### If no games in the current season for the current team go to next sesason ####
      if(length(game_ids) > 0 & !is.null(game_ids)){
        game_ids <- game_ids[ ,1]
        
        additional_ids <- as.data.frame(matrix(NA, nrow = 0, ncol = 3))
        names(additional_ids) <- c("ID", "Name", "FBS")
        
        for(i in 1:nrow(schedule_ids)){
          if(!any(schedule_ids[i, "ID"] %in% ids$ID)){
            ids <- rbind(ids, schedule_ids[i, ])
            ids_temp <- rbind(ids_temp, schedule_ids[i, ])
          }
        }
        
        team_games <- team_games %>% html_nodes('table') %>% html_table(fill = T)
        
        team_games <- team_games[[1]]
        
        team_games <- team_games[, c(1:3)]
        
        names(team_games) <- c("Date", "Opponent", "Result")
        
        team_games <- team_games[!(team_games$Date %in% c("Regular Season", "DATE")), ]
        
        team_games <- team_games[str_detect(team_games$Date, ", "), ]
        
        team_games <- team_games %>% 
          mutate(
            Team = id[1, 2],
            Team_ID = id[1, 1],
            Season = season,
            Home = ifelse(str_detect(Opponent, "vs"), TRUE, FALSE),
            Result_Delete = Result,
            Result = case_when(
              str_detect(Result, "W") & str_detect(Result, "-") ~ "W",
              str_detect(Result, "L") & str_detect(Result, "-") ~ "L",
              str_detect(Result, "D") & str_detect(Result, "-") ~ "D",
              TRUE ~ "TBD"
            ),
            Played = ifelse(Result %in% c("W", "L", "D"), "TRUE", "FALSE"),
            Neutral_Location = ifelse(str_detect(Opponent, "\\*"), TRUE, FALSE),
            Opponent = str_replace(Opponent, " \\*", ""),
            Opponent = str_replace(Opponent, "\\*", ""),
            Opponent = str_replace(Opponent, "vs", ""),
            Opponent = str_replace(Opponent, "@", ""),
            Opponent = str_replace(Opponent, "1 ", ""),
            Opponent = str_replace(Opponent, "2 ", ""),
            Opponent = str_replace(Opponent, "3 ", ""),
            Opponent = str_replace(Opponent, "4 ", ""),
            Opponent = str_replace(Opponent, "5 ", ""),
            Opponent = str_replace(Opponent, "6 ", ""),
            Opponent = str_replace(Opponent, "7 ", ""),
            Opponent = str_replace(Opponent, "8 ", ""),
            Opponent = str_replace(Opponent, "9 ", ""),
            Opponent = str_replace(Opponent, "10 ", ""),
            Opponent = str_replace(Opponent, "11 ", ""),
            Opponent = str_replace(Opponent, "12 ", ""),
            Opponent = str_replace(Opponent, "13 ", ""),
            Opponent = str_replace(Opponent, "14 ", ""),
            Opponent = str_replace(Opponent, "15 ", ""),
            Opponent = str_replace(Opponent, "16 ", ""),
            Opponent = str_replace(Opponent, "17 ", ""),
            Opponent = str_replace(Opponent, "18 ", ""),
            Opponent = str_replace(Opponent, "19 ", ""),
            Opponent = str_replace(Opponent, "20 ", ""),
            Opponent = str_replace(Opponent, "21 ", ""),
            Opponent = str_replace(Opponent, "22 ", ""),
            Opponent = str_replace(Opponent, "23 ", ""),
            Opponent = str_replace(Opponent, "24 ", ""),
            Opponent = str_replace(Opponent, "25 ", ""),
            Opponent = str_replace(Opponent, "1", ""),
            Opponent = str_replace(Opponent, "2", ""),
            Opponent = str_replace(Opponent, "3", ""),
            Opponent = str_replace(Opponent, "4", ""),
            Opponent = str_replace(Opponent, "5", ""),
            Opponent = str_replace(Opponent, "6", ""),
            Opponent = str_replace(Opponent, "7", ""),
            Opponent = str_replace(Opponent, "8", ""),
            Opponent = str_replace(Opponent, "9", ""),
            Opponent = str_replace(Opponent, "10", ""),
            Opponent = str_replace(Opponent, "11", ""),
            Opponent = str_replace(Opponent, "12", ""),
            Opponent = str_replace(Opponent, "13", ""),
            Opponent = str_replace(Opponent, "14", ""),
            Opponent = str_replace(Opponent, "15", ""),
            Opponent = str_replace(Opponent, "16", ""),
            Opponent = str_replace(Opponent, "17", ""),
            Opponent = str_replace(Opponent, "18", ""),
            Opponent = str_replace(Opponent, "19", ""),
            Opponent = str_replace(Opponent, "20", ""),
            Opponent = str_replace(Opponent, "21", ""),
            Opponent = str_replace(Opponent, "22", ""),
            Opponent = str_replace(Opponent, "23", ""),
            Opponent = str_replace(Opponent, "24", ""),
            Opponent = str_replace(Opponent, "25", ""),
            Result_Delete = ifelse(Played, str_replace(Result_Delete, Result, ""), Result_Delete),
            OT = ifelse(str_detect(Result_Delete, " OT"), TRUE, FALSE),
            Result_Delete = str_replace(Result_Delete, " OT", ""),
            Result_Delete = str_replace(Result_Delete, " 2OT", ""),
            Result_Delete = str_replace(Result_Delete, " 3OT", ""),
            Result_Delete = str_replace(Result_Delete, " 4OT", ""),
            Result_Delete = str_replace(Result_Delete, " 5OT", ""),
            Result_Delete = str_replace(Result_Delete, " 6OT", ""),
            Result_Delete = str_replace(Result_Delete, " 7OT", ""),
            Result_Delete = str_replace(Result_Delete, " 8OT", ""),
            Result_Delete = str_replace(Result_Delete, " 9OT", ""),
            Result_Delete = str_replace(Result_Delete, " 10OT", ""),
            Result_Delete = str_replace(Result_Delete, " 11OT", ""),
            Result_Delete = str_replace(Result_Delete, " 12OT", ""),
            Result_Delete = case_when(
              Result_Delete == "Postponed" ~ "Postponed",
              Result_Delete == "Canceled" ~ "Canceled",
              Result_Delete == "LIVE" ~ "LIVE",
              !str_detect(Result_Delete, "-") ~ "TBD",
              TRUE ~ Result_Delete
            ),
            Result_Delete = str_replace(Result_Delete, "Postponed", "Postponed-Postponed"),
            Result_Delete = str_replace(Result_Delete, "Canceled", "Canceled-Canceled"),
            Result_Delete = str_replace(Result_Delete, "LIVE", "LIVE-LIVE"),
            Result_Delete = str_replace(Result_Delete, "TBD", "TBD-TBD"),
            Date = str_replace(Date, "Sat, ", ""),
            Date = str_replace(Date, "Sun, ", ""),
            Date = str_replace(Date, "Mon, ", ""),
            Date = str_replace(Date, "Tue, ", ""),
            Date = str_replace(Date, "Wed, ", ""),
            Date = str_replace(Date, "Thu, ", ""),
            Date = str_replace(Date, "Fri, ", ""),
            Date = paste0(Date, ", ", season),
            Date = as.Date(Date, format = "%b %d, %Y"),
            Date = ifelse(month(Date) == 1 | month(Date) == 2 | month(Date) == 3, as.Date(Date + years(1)), as.Date(Date))
        
            # Opp_ID = ifelse(length(ids[str_detect(ids$Name, str_replace_all(str_to_lower(Opponent), " ", "-")), 1]) == 0, NA, ids[str_detect(ids$Name, str_replace_all(str_to_lower(Opponent), " ", "-")), 1])
          )
        
        team_games <- team_games %>% separate(Result_Delete, sep = "-", into = c("Points_For", "Points_Against"))
        
        team_games <- team_games %>% cbind(schedule_ids) %>% rename(Opp_ID = ID, Opp_Name_ID = Name)
        
        postponed_ids <- which(team_games$Points_For == "Postponed")
        canceled_ids <- which(team_games$Points_For == "Canceled")
        live_ids <- which(team_games$Points_For == "LIVE")
        
        insert_ids <- sort(c(postponed_ids, canceled_ids, live_ids))
        
        if(length(insert_ids)){
          for(i in 1:length(insert_ids)){
            game_ids <- append(game_ids, values = "Postponed/Canceled/Live", after = insert_ids[i]-1)
          }
        }
        
        if(length(postponed_ids)){
          for(i in 1:length(postponed_ids)){
            game_ids[postponed_ids[i]] <- "Postponed"
          }
        }
        
        if(length(canceled_ids)){
          for(i in 1:length(canceled_ids)){
            game_ids[canceled_ids[i]] <- "Canceled"
          }
        }
        
        if(length(live_ids)){
          for(i in 1:length(live_ids)){
            game_ids[live_ids[i]] <- "Live"
          }
        }
        
        team_games$Game_ID <- game_ids
        
        Schedule <- rbind(Schedule, team_games)
      }
    }
  }
  
  ids_temp <- ids_temp[-1, ]
  ids_temp <- ids_temp %>% filter(!is.na(ID))
  ids <- ids %>% filter(!is.na(ID))
  
}

ids <- ids %>% filter(!is.na(ID))

###

Schedule_raw <- Schedule

###

Schedule <- Schedule_raw

# Schedule$Date <- as.Date(Schedule$Date, origin = "1970-01-01")

Name_Mapping <- Schedule %>% select(Name = Opponent, Team_ID = Opp_ID, Name_ID = Opp_Name_ID) %>% arrange(Name)
Name_Mapping <- Name_Mapping %>% mutate(
  dup = ifelse(Name == lag(Name, 1), TRUE, FALSE)
)

Name_Mapping$dup[1] <- FALSE

Name_Mapping <- Name_Mapping %>% filter(dup == FALSE) %>% select(-dup)

Name_Mapping[Name_Mapping$Name == "ucf", "Name"] <- "UCF"

Name_Mapping[Name_Mapping$Name == "UTSA", "Name"] <- "UT San Antonio"

Name_Mapping[Name_Mapping$Name == "Smu", "Name"] <- "SMU"

Name_Mapping[Name_Mapping$Name == "Unlv", "Name"] <- "UNLV"

Name_Mapping[Name_Mapping$Name == "Louisiana-Monroe", "Name"] <- "UL Monroe"

Name_Mapping[Name_Mapping$Team_ID == "23" & !is.na(Name_Mapping$Team_ID), "Name"] <- "San Jose St"

Name_Mapping_NA <- Name_Mapping %>% filter(is.na(Team_ID))
Name_Mapping <- Name_Mapping %>% filter(!is.na(Team_ID))

Name_Mapping <- Name_Mapping %>% arrange(Team_ID) %>%  mutate(
  dup = ifelse(Team_ID == lag(Team_ID, 1), TRUE, FALSE)
)

Name_Mapping$dup[1] <- FALSE

Name_Mapping <- Name_Mapping %>% filter(dup == FALSE) %>% select(-dup)

Name_Mapping <- rbind(Name_Mapping, Name_Mapping_NA)

Name_Mapping <- Name_Mapping %>% left_join(ids %>% select(ID, FBS), by = c("Team_ID" = "ID"))

Name_Mapping <- Name_Mapping %>% mutate(
  FBS = ifelse(is.na(FBS), 0, FBS)
)

# Update Team Name
Schedule <- Schedule %>% left_join(Name_Mapping %>% filter(!is.na(Team_ID)) %>% select(Name, Team_FBS = FBS, Team_ID), by = c("Team_ID" = "Team_ID"))

Schedule$Team <- Schedule$Name

Schedule$Name <- NULL

Schedule <- Schedule %>% left_join(Name_Mapping %>% filter(!is.na(Team_ID)) %>% select(Name, Name_ID), by = c("Team" = "Name"))

# Update Opp Team Name
Schedule <- Schedule %>% left_join(Name_Mapping %>% filter(!is.na(Team_ID)) %>% select(Name, Opp_FBS = FBS, Team_ID), by = c("Opp_ID" = "Team_ID"))

Schedule$Opponent <- Schedule$Name

Schedule$Name <- NULL

# Correct FBS for teams with no ID
Schedule <- Schedule %>% mutate(
  Opp_FBS = ifelse(is.na(Opp_ID), 0, Opp_FBS),
  Team_FBS = ifelse(is.na(Team_ID), 0, Team_FBS)
) 

Schedule$FBS <- NULL

# Check for team names
length(unique(Schedule$Team))
length(unique(Name_Mapping$Team_ID))
length(unique(Schedule$Opponent))
length(unique(Name_Mapping$Name))

Schedule <- Schedule %>% select(Date, Season, Team, Opponent, Result, Points_For, Points_Against, Played, Home, Neutral_Location, OT, Game_ID, Team_FBS, Opp_FBS, Team_ID, Name_ID, Opp_ID, Opp_Name_ID)

#### Duplicate games with NA in Opp_ID ####

Schedule_Singles <- Schedule %>% group_by(Game_ID) %>% tally() %>% filter(n == 1) %>% select(Game_ID)
Schedule_Singles <- Schedule_Singles$Game_ID

Schedule_NA <- Schedule %>% filter(Game_ID %in% Schedule_Singles)
names(Schedule_NA) <- c('Date', 'Season', 'Opponent', 'Team', 'Result', 'Points_For', 'Points_Against', 'Played', 'Home', 'Neutral_Location', 'OT', 'Game_ID', 'Opp_FBS', 'Team_FBS', 'Opp_ID', 'Opp_Name_ID', 'Team_ID', 'Name_ID')

Schedule_NA <- Schedule_NA %>% select(Date, Season, Team, Opponent, Result, Points_For, Points_Against, Played, Home, Neutral_Location, OT, Game_ID, Team_FBS, Opp_FBS, Team_ID, Name_ID, Opp_ID, Opp_Name_ID)

Schedule_NA <- Schedule_NA %>% mutate(
  Result = case_when(
    Result == "L" ~ "W",
    Result == "W" ~ "L",
    TRUE ~ Result
  ),
  Home = ifelse(TRUE, FALSE, TRUE)
)

Schedule <- rbind(Schedule, Schedule_NA)

Schedule <- Schedule %>% mutate(
  Date = as.Date(Date, origin = "1970-01-01"),
  Points_For = ifelse(Played, as.numeric(Points_For), 0),
  Points_Against = ifelse(Played, as.numeric(Points_Against), 0),
  points_for_temp = ifelse(Result == "W", Points_For, Points_Against),
  points_against_temp = ifelse(Result == "L", Points_For, Points_Against),
  Points_For = points_for_temp,
  Points_Against = points_against_temp,
  Spread = Points_For - Points_Against,
  Home = ifelse(Home, 1, 0),
  Home = ifelse(Neutral_Location == TRUE, 0.5, Home)
)

Schedule <- Schedule %>% select(Date, Season, Team, Opponent, Result, Points_For, Points_Against, Spread, Played, Home, Neutral_Location, OT, Team_FBS, Opp_FBS, Game_ID, Team_ID, Name_ID, Opp_ID, Opp_Name_ID) %>% 
  arrange(desc(Played), desc(Date), Game_ID)

NCAAF_Level_One <- Schedule %>% select(Date, Season, Team, Opponent, Result, Points_For, Points_Against, Spread, Played, Home, Neutral_Location, OT, Team_FBS, Opp_FBS, Game_ID) 

write.csv(NCAAF_Level_One, "C:/Users/Matt C137/Documents/GitHub/Open_Data/Data/Sports/NCAAF_Level_One.csv")
write.csv(Name_Mapping %>% select(Team = Name, FBS), "C:/Users/Matt C137/Documents/GitHub/Open_Data/Data/Sports/Team_List.csv")

Played <- Schedule %>% filter(Played == TRUE)
length(unique(Played$Game_ID)) * 2 == nrow(Played)

