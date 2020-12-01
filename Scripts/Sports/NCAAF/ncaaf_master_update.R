library(tidyr)
library(dplyr)
library(stringr)
library(rvest)
library(lubridate)
library(readr)

seasons <- c(2020)

ncaaf <- read_html("https://www.espn.com/college-football/teams")

ids <- ncaaf %>% str_extract_all('(?<=href="/college-football/team/_/id/)(.*?)(?="><img class=")') %>% unlist()
ids <- ids %>% str_split("/", simplify = T)
ids <- as.data.frame(ids)
names(ids) <- c('ID', 'Name')
ids$FBS <- 1
ids$ID <- as.character(ids$ID)
ids$Name <- as.character(ids$Name)

teams <- ncaaf %>% str_extract_all('(?<=" title=")(.*?)(?=" data-mptype=")') %>% unlist()

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
              str_detect(Result, "T") & str_detect(Result, "-") ~ "T",
              TRUE ~ "TBD"
            ),
            Played = ifelse(Result %in% c("W", "L", "T"), "TRUE", "FALSE"),
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

Name_Mapping[Name_Mapping$Name == "Ucf", "Name"] <- "UCF"

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
  # Home = ifelse(Neutral_Location == TRUE, 0.5, Home)
)

Schedule <- Schedule %>% select(Date, Season, Team, Opponent, Result, Points_For, Points_Against, Spread, Played, Home, Neutral_Location, OT, Team_FBS, Opp_FBS, Game_ID, Team_ID, Name_ID, Opp_ID, Opp_Name_ID) %>% 
  arrange(desc(Played), desc(Date), Game_ID) %>% filter(complete.cases(.))

ncaaf_l1 <- Schedule %>% select(Date, Season, Team, Opponent, Result, Points_For, Points_Against, Spread, Played, Home, Neutral_Location, OT, Team_FBS, Opp_FBS, Game_ID) 

# Rename again....

ncaaf_l1[ncaaf_l1$Team == "Ucf", "Team"] <- "UCF"
ncaaf_l1[ncaaf_l1$Opponent == "Ucf", "Opponent"] <- "UCF"

ncaaf_l1[ncaaf_l1$Team == "UTSA", "Team"] <- "UT San Antonio"
ncaaf_l1[ncaaf_l1$Opponent == "UTSA", "Opponent"] <- "UT San Antonio"

ncaaf_l1[ncaaf_l1$Team == "Smu", "Team"] <- "SMU"
ncaaf_l1[ncaaf_l1$Opponent == "Smu", "Opponent"] <- "SMU"

ncaaf_l1[ncaaf_l1$Team == "Unlv", "Team"] <- "UNLV"
ncaaf_l1[ncaaf_l1$Opponent == "Unlv", "Opponent"] <- "UNLV"

ncaaf_l1[ncaaf_l1$Team == "Louisiana-Monroe", "Team"] <- "UL Monroe"
ncaaf_l1[ncaaf_l1$Opponent == "Louisiana-Monroe", "Opponent"] <- "UL Monroe"

ncaaf_l1 <- ncaaf_l1 %>% filter(Game_ID != "Postponed", Game_ID != "Canceled", )

Played <- Schedule %>% filter(Played == TRUE)
length(unique(Played$Game_ID)) * 2 == nrow(Played)

#### NCAA L2 Start ####

ncaaf_l1 <- ncaaf_l1 %>% mutate(
  Line_Favored = "NA",
  Line_Amount = 0,
  Line_Over_Under = 0,
  
  ##
  
  First_Down = 0,
  Third_Down_Conversions = 0,
  Third_Down_Att = 0,
  Fourth_Down_Conversions = 0,
  Fourth_Down_Att = 0,
  Total_Yards = 0,
  Passing_Yards = 0,
  Pass_Completions = 0,
  Pass_Att = 0,
  Yards_Per_Pass = 0,
  Interceptions_Thrown = 0,
  Rushing_Yards = 0,
  Rushing_Att = 0,
  Yards_Per_Rush = 0,
  Penalties = 0,
  Penalties_Yards = 0,
  Turnovers = 0,
  Fumbles_Lost = 0,
  Possession = 0,
  Possession_Percent = 0,
  
  ##
  
  First_Down_Against = 0,
  Third_Down_Conversions_Against = 0,
  Third_Down_Att_Against = 0,
  Fourth_Down_Conversions_Against = 0,
  Fourth_Down_Att_Against = 0,
  Total_Yards_Against = 0,
  Passing_Yards_Against = 0,
  Pass_Completions_Against = 0,
  Pass_Att_Against = 0,
  Yards_Per_Pass_Against = 0,
  Interceptions_Thrown_Against = 0,
  Rushing_Yards_Against = 0,
  Rushing_Att_Against = 0,
  Yards_Per_Rush_Against = 0,
  Penalties_Against = 0,
  Penalties_Yards_Against = 0,
  Turnovers_Against = 0,
  Fumbles_Lost_Against = 0,
  Possession_Against = 0,
  Possession_Percent_Against = 0
)

# ncaaf_l1 <- ncaaf_l1 %>% filter(Game_ID %in% c(401249405, 401249012, 401212545, 401249861, 401207218, 401207200, 401135295))
# ncaaf_l1 <- ncaaf_l1 %>% filter(Result == "TBD" & Played == "FALSE")

for(i in 1:length(unique(ncaaf_l1$Game_ID))){
  
  # game_id <- 401261245
  
  # This makes games that haven't been played first
  game_id <- sort(unique(ncaaf_l1$Game_ID), decreasing = T)[i]
  print(game_id)
  
  #### Update L2 Data ####
  
  end_while <- FALSE
  j <- 1
  while(!end_while){
    game <- try({read_html(paste0("https://www.espn.com/college-football/matchup?gameId=", game_id))})
    game_stats <- try(game %>% html_node(".mod-data") %>% html_table())
    
    if(is.data.frame(game_stats)){
      if(nrow(game_stats) > 0){
        update_data <- TRUE
      }else{
        ## I'll leave this as true for now. Change to false to avoid looping through games that haven't been played. Currently working this way but code isn't clean.
        update_data <- TRUE
      }
      
    }else{
      update_data <- FALSE
    }
    
    end_while <- ifelse(is.data.frame(game_stats) | j == 3, TRUE, FALSE)
    j <- j + 1
  }
  
  
  #### Correct for Neutral Location Home team ####
  if(nrow(ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Neutral_Location"]) == 2){
    teams <- ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Team"]
    home_team <- ifelse(game %>% str_detect(paste0("vs. ", as.character(teams[1, 1]))), teams[1, 1] %>% as.character(), teams[2, 1] %>% as.character())
    away_team <- ifelse(game %>% str_detect(paste0("vs. ", as.character(teams[1, 1]))), teams[2, 1] %>% as.character(), teams[1, 1] %>% as.character())
    
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Team == away_team, "Home"] <- 0
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Team == home_team, "Home"] <- 1
    
  }
  
  if(update_data){
    names(game_stats) <- c("Stat", "Home", "Away")
    class(game_stats)
    
    played <- ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Played"] %>% as.character()
    result <- ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Result"] %>% as.character()
    
    if(played == "TRUE"){
      Line <- game %>% 
        html_nodes(".odds-details") %>% 
        html_text("li") %>% 
        str_extract_all('(?<=Line: )(.*?)(?=\n)') %>% 
        unlist() %>% 
        str_split(" ") %>% 
        unlist()
      
      line_favored <- Line[1]
      line_amount <- ifelse(line_favored == "Even", 0, as.numeric(Line[2]))
      
      over_under <- game %>% 
        html_nodes(".odds-details") %>% 
        html_text("li") %>% 
        str_extract_all('(?<=Over/Under: )(.*?)(?=\n)') %>% 
        unlist() %>% 
        as.numeric()
      
    }else if(result == "TBD" & played == "FALSE"){
      # game <- try({read_html(paste0("https://www.espn.com/college-football/game/_/gameId/401212545"))})
      # game_id <- 401212545
      
      future_game_table <- try({read_html(paste0("https://www.espn.com/college-football/game/_/gameId/", game_id))})
      
      # First logic statement is to test if future game has a line yet 
      if(future_game_table %>% str_detect("mediumTable")){
        future_game_table <- try({future_game_table %>% html_node(".mediumTable") %>% html_table()})
        names(future_game_table) <- c("d1", "d2", "d3", "d4", "Spread", "ML", "Over_Under")
        future_game_table <- future_game_table %>% select(Spread, Over_Under)
        
        home_team <- ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Team"]
        away_team <- ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Opponent"]
        
        if(is.numeric(future_game_table$Spread[1]) & is.numeric(future_game_table$Over_Under[1])){
          line_favored <- ifelse(future_game_table$Spread[1] > 0, home_team, away_team)
          line_amount <- -abs(future_game_table$Spread[1])
          over_under <- future_game_table$Over_Under[1]
          
        }else{
          line_favored <- NA
          line_amount <- NA
          over_under <- NA
        }
        
      }else{
        line_favored <- NA
        line_amount <- NA
        over_under <- NA
        
      }
    }
    
    #### Get Team Stats ####
    
    First_Down_Home <- game_stats[game_stats$Stat == "1st Downs", 3] %>% as.numeric()
    First_Down_Away <- game_stats[game_stats$Stat == "1st Downs", 2] %>% as.numeric()
    
    Third_Down_Eff_Raw_Home <- game_stats[game_stats$Stat == "3rd down efficiency", 3] %>% str_split("-") %>% unlist() %>% as.numeric()
    Third_Down_Eff_Raw_Away <- game_stats[game_stats$Stat == "3rd down efficiency", 2] %>% str_split("-") %>% unlist() %>% as.numeric()
    
    Third_Down_Conversions_Home <- Third_Down_Eff_Raw_Home[1]
    Third_Down_Conversions_Away <- Third_Down_Eff_Raw_Away[1]
    
    Third_Down_Att_Home <- Third_Down_Eff_Raw_Home[2]
    Third_Down_Att_Away <- Third_Down_Eff_Raw_Away[2]
    
    Fourth_Down_Eff_Raw_Home <- game_stats[game_stats$Stat == "4th down efficiency", 3] %>% str_split("-") %>% unlist() %>% as.numeric()
    Fourth_Down_Eff_Raw_Away <- game_stats[game_stats$Stat == "4th down efficiency", 2] %>% str_split("-") %>% unlist() %>% as.numeric()
    
    Fourth_Down_Conversions_Home <- Fourth_Down_Eff_Raw_Home[1]
    Fourth_Down_Conversions_Away <- Fourth_Down_Eff_Raw_Away[1]
    
    Fourth_Down_Att_Home <- Fourth_Down_Eff_Raw_Home[2]
    Fourth_Down_Att_Away <- Fourth_Down_Eff_Raw_Away[2]
    
    Total_Yards_Home <- game_stats[game_stats$Stat == "Total Yards", 3] %>% as.numeric()
    Total_Yards_Away <- game_stats[game_stats$Stat == "Total Yards", 2] %>% as.numeric()
    
    Passing_Yards_Home <- game_stats[game_stats$Stat == "Passing", 3] %>% as.numeric()
    Passing_Yards_Away <- game_stats[game_stats$Stat == "Passing", 2] %>% as.numeric()
    
    Comp_Att_Raw_Home <- game_stats[game_stats$Stat == "Comp-Att", 3] %>% str_split("-") %>% unlist() %>% as.numeric()
    Comp_Att_Raw_Away <- game_stats[game_stats$Stat == "Comp-Att", 2] %>% str_split("-") %>% unlist() %>% as.numeric()
    
    Pass_Completions_Home <- Comp_Att_Raw_Home[1]
    Pass_Completions_Away <- Comp_Att_Raw_Away[1]
    
    Pass_Att_Home <- Comp_Att_Raw_Home[2]
    Pass_Att_Away <- Comp_Att_Raw_Away[2]
    
    Yards_Per_Pass_Home <- game_stats[game_stats$Stat == "Yards per pass", 3] %>% as.numeric()
    Yards_Per_Pass_Away <- game_stats[game_stats$Stat == "Yards per pass", 2] %>% as.numeric()
    
    Interceptions_Thrown_Home <- game_stats[game_stats$Stat == "Interceptions thrown", 3] %>% as.numeric()
    Interceptions_Thrown_Away <- game_stats[game_stats$Stat == "Interceptions thrown", 2] %>% as.numeric()
    
    Rushing_Yards_Home <- game_stats[game_stats$Stat == "Rushing", 3] %>% as.numeric()
    Rushing_Yards_Away <- game_stats[game_stats$Stat == "Rushing", 2] %>% as.numeric()
    
    Rushing_Att_Home <- game_stats[game_stats$Stat == "Rushing Attempts", 3] %>% as.numeric()
    Rushing_Att_Away <- game_stats[game_stats$Stat == "Rushing Attempts", 2] %>% as.numeric()
    
    Yards_Per_Rush_Home <- game_stats[game_stats$Stat == "Yards per rush", 3] %>% as.numeric()
    Yards_Per_Rush_Away <- game_stats[game_stats$Stat == "Yards per rush", 2] %>% as.numeric()
    
    Penalties_Raw_Home <- game_stats[game_stats$Stat == "Penalties", 3] %>% str_split("-") %>% unlist() %>% as.numeric()
    Penalties_Raw_Away <- game_stats[game_stats$Stat == "Penalties", 2] %>% str_split("-") %>% unlist() %>% as.numeric()
    
    Penalties_Home <- Penalties_Raw_Home[1]
    Penalties_Away <- Penalties_Raw_Away[1]
    
    Penalties_Yards_Home <- Penalties_Raw_Home[2]
    Penalties_Yards_Away <- Penalties_Raw_Away[2]
    
    Turnovers_Home <- game_stats[game_stats$Stat == "Turnovers", 3] %>% as.numeric()
    Turnovers_Away <- game_stats[game_stats$Stat == "Turnovers", 2] %>% as.numeric()
    
    Fumbles_Lost_Home <- game_stats[game_stats$Stat == "Fumbles lost", 3] %>% as.numeric()
    Fumbles_Lost_Away <- game_stats[game_stats$Stat == "Fumbles lost", 2] %>% as.numeric()
    
    Possession_Raw_Home <- game_stats[game_stats$Stat == "Possession", 3] %>% str_split(":") %>% unlist() %>% as.numeric()
    Possession_Raw_Away <- game_stats[game_stats$Stat == "Possession", 2] %>% str_split(":") %>% unlist() %>% as.numeric()
    
    Possession_Home <- Possession_Raw_Home[1] + Possession_Raw_Home[2]/60
    Possession_Away <- Possession_Raw_Away[1] + Possession_Raw_Away[2]/60
    
    Possession_Percent_Home <- Possession_Home / (Possession_Home + Possession_Away)
    Possession_Percent_Away <- Possession_Away / (Possession_Home + Possession_Away)
    
    #### Fill out dataset ####
    
    ncaaf_l1[ncaaf_l1$Game_ID == game_id, "Line_Favored"] <- ifelse(length(line_favored) == 0, "No Line", line_favored)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id, "Line_Amount"] <- ifelse(length(line_amount) == 0, NA, line_amount)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id, "Line_Over_Under"] <- ifelse(length(over_under) == 0, NA, over_under)
    
    ##
    
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "First_Down"] <- ifelse(length(First_Down_Home) == 0, 0, First_Down_Home)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Third_Down_Conversions"] <- ifelse(length(Third_Down_Conversions_Home) == 0, 0, Third_Down_Conversions_Home)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Third_Down_Att"] <- ifelse(length(Third_Down_Att_Home) == 0, 0, Third_Down_Att_Home)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Fourth_Down_Conversions"] <- ifelse(length(Fourth_Down_Conversions_Home) == 0, 0, Fourth_Down_Conversions_Home)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Fourth_Down_Att"] <- ifelse(length(Fourth_Down_Att_Home) == 0, 0, Fourth_Down_Att_Home)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Total_Yards"] <- ifelse(length(Total_Yards_Home) == 0, 0, Total_Yards_Home)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Passing_Yards"] <- ifelse(length(Passing_Yards_Home) == 0, 0, Passing_Yards_Home)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Pass_Completions"] <- ifelse(length(Pass_Completions_Home) == 0, 0, Pass_Completions_Home)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Pass_Att"] <- ifelse(length(Pass_Att_Home) == 0, 0, Pass_Att_Home)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Yards_Per_Pass"] <- ifelse(length(Yards_Per_Pass_Home) == 0, 0, Yards_Per_Pass_Home)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Interceptions_Thrown"] <- ifelse(length(Interceptions_Thrown_Home[1]) == 0, 0, Interceptions_Thrown_Home[1])
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Rushing_Yards"] <- ifelse(length(Rushing_Yards_Home) == 0, 0, Rushing_Yards_Home)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Rushing_Att"] <- ifelse(length(Rushing_Att_Home) == 0, 0, Rushing_Att_Home)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Yards_Per_Rush"] <- ifelse(length(Yards_Per_Rush_Home) == 0, 0, Yards_Per_Rush_Home)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Penalties"] <- ifelse(length(Penalties_Home) == 0, 0, Penalties_Home)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Penalties_Yards"] <- ifelse(length(Penalties_Yards_Home) == 0, 0, Penalties_Yards_Home)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Turnovers"] <- ifelse(length(Turnovers_Home) == 0, 0, Turnovers_Home)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Fumbles_Lost"] <- ifelse(length(Fumbles_Lost_Home) == 0, 0, Fumbles_Lost_Home)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Possession"] <- ifelse(length(Possession_Home) == 0, 0, Possession_Home)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Possession_Percent"] <- ifelse(length(Possession_Percent_Home) == 0, 0, Possession_Percent_Home)
    
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "First_Down_Against"] <- ifelse(length(First_Down_Away) == 0, 0, First_Down_Away)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Third_Down_Conversions_Against"] <- ifelse(length(Third_Down_Conversions_Away) == 0, 0, Third_Down_Conversions_Away)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Third_Down_Att_Against"] <- ifelse(length(Third_Down_Att_Away) == 0, 0, Third_Down_Att_Away)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Fourth_Down_Conversions_Against"] <- ifelse(length(Fourth_Down_Conversions_Away) == 0, 0, Fourth_Down_Conversions_Away)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Fourth_Down_Att_Against"] <- ifelse(length(Fourth_Down_Att_Away) == 0, 0, Fourth_Down_Att_Away)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Total_Yards_Against"] <- ifelse(length(Total_Yards_Away) == 0, 0, Total_Yards_Away)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Passing_Yards_Against"] <- ifelse(length(Passing_Yards_Away) == 0, 0, Passing_Yards_Away)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Pass_Completions_Against"] <- ifelse(length(Pass_Completions_Away) == 0, 0, Pass_Completions_Away)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Pass_Att_Against"] <- ifelse(length(Pass_Att_Away) == 0, 0, Pass_Att_Away)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Yards_Per_Pass_Against"] <- ifelse(length(Yards_Per_Pass_Away) == 0, 0, Yards_Per_Pass_Away)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Interceptions_Thrown_Against"] <- ifelse(length(Interceptions_Thrown_Away[1]) == 0, 0, Interceptions_Thrown_Away[1])
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Rushing_Yards_Against"] <- ifelse(length(Rushing_Yards_Away) == 0, 0, Rushing_Yards_Away)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Rushing_Att_Against"] <- ifelse(length(Rushing_Att_Away) == 0, 0, Rushing_Att_Away)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Yards_Per_Rush_Against"] <- ifelse(length(Yards_Per_Rush_Away) == 0, 0, Yards_Per_Rush_Away)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Penalties_Against"] <- ifelse(length(Penalties_Away) == 0, 0, Penalties_Away)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Penalties_Yards_Against"] <- ifelse(length(Penalties_Yards_Away) == 0, 0, Penalties_Yards_Away)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Turnovers_Against"] <- ifelse(length(Turnovers_Away) == 0, 0, Turnovers_Away)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Fumbles_Lost_Against"] <- ifelse(length(Fumbles_Lost_Away) == 0, 0, Fumbles_Lost_Away)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Possession_Against"] <- ifelse(length(Possession_Away) == 0, 0, Possession_Away)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == TRUE, "Possession_Percent_Against"] <- ifelse(length(Possession_Percent_Away) == 0, 0, Possession_Percent_Away)
    
    ##
    
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "First_Down"] <- ifelse(length(First_Down_Away) == 0, 0, First_Down_Away)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "Third_Down_Conversions"] <- ifelse(length(Third_Down_Conversions_Away) == 0, 0, Third_Down_Conversions_Away)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "Third_Down_Att"] <- ifelse(length(Third_Down_Att_Away) == 0, 0, Third_Down_Att_Away)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "Fourth_Down_Conversions"] <- ifelse(length(Fourth_Down_Conversions_Away) == 0, 0, Fourth_Down_Conversions_Away)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "Fourth_Down_Att"] <- ifelse(length(Fourth_Down_Att_Away) == 0, 0, Fourth_Down_Att_Away)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "Total_Yards"] <- ifelse(length(Total_Yards_Away) == 0, 0, Total_Yards_Away)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "Passing_Yards"] <- ifelse(length(Passing_Yards_Away) == 0, 0, Passing_Yards_Away)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "Pass_Completions"] <- ifelse(length(Pass_Completions_Away) == 0, 0, Pass_Completions_Away)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "Pass_Att"] <- ifelse(length(Pass_Att_Away) == 0, 0, Pass_Att_Away)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "Yards_Per_Pass"] <- ifelse(length(Yards_Per_Pass_Away) == 0, 0, Yards_Per_Pass_Away)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "Interceptions_Thrown"] <- ifelse(length(Interceptions_Thrown_Away[1]) == 0, 0, Interceptions_Thrown_Away[1])
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "Rushing_Yards"] <- ifelse(length(Rushing_Yards_Away) == 0, 0, Rushing_Yards_Away)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "Rushing_Att"] <- ifelse(length(Rushing_Att_Away) == 0, 0, Rushing_Att_Away)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "Yards_Per_Rush"] <- ifelse(length(Yards_Per_Rush_Away) == 0, 0, Yards_Per_Rush_Away)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "Penalties"] <- ifelse(length(Penalties_Away) == 0, 0, Penalties_Away)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "Penalties_Yards"] <- ifelse(length(Penalties_Yards_Away) == 0, 0, Penalties_Yards_Away)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "Turnovers"] <- ifelse(length(Turnovers_Away) == 0, 0, Turnovers_Away)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "Fumbles_Lost"] <- ifelse(length(Fumbles_Lost_Away) == 0, 0, Fumbles_Lost_Away)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "Possession"] <- ifelse(length(Possession_Away) == 0, 0, Possession_Away)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "Possession_Percent"] <- ifelse(length(Possession_Percent_Away) == 0, 0, Possession_Percent_Away)
    
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "First_Down_Against"] <- ifelse(length(First_Down_Home) == 0, 0, First_Down_Home)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "Third_Down_Conversions_Against"] <- ifelse(length(Third_Down_Conversions_Home) == 0, 0, Third_Down_Conversions_Home)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "Third_Down_Att_Against"] <- ifelse(length(Third_Down_Att_Home) == 0, 0, Third_Down_Att_Home)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "Fourth_Down_Conversions_Against"] <- ifelse(length(Fourth_Down_Conversions_Home) == 0, 0, Fourth_Down_Conversions_Home)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "Fourth_Down_Att_Against"] <- ifelse(length(Fourth_Down_Att_Home) == 0, 0, Fourth_Down_Att_Home)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "Total_Yards_Against"] <- ifelse(length(Total_Yards_Home) == 0, 0, Total_Yards_Home)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "Passing_Yards_Against"] <- ifelse(length(Passing_Yards_Home) == 0, 0, Passing_Yards_Home)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "Pass_Completions_Against"] <- ifelse(length(Pass_Completions_Home) == 0, 0, Pass_Completions_Home)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "Pass_Att_Against"] <- ifelse(length(Pass_Att_Home) == 0, 0, Pass_Att_Home)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "Yards_Per_Pass_Against"] <- ifelse(length(Yards_Per_Pass_Home) == 0, 0, Yards_Per_Pass_Home)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "Interceptions_Thrown_Against"] <- ifelse(length(Interceptions_Thrown_Home[1]) == 0, 0, Interceptions_Thrown_Home[1])
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "Rushing_Yards_Against"] <- ifelse(length(Rushing_Yards_Home) == 0, 0, Rushing_Yards_Home)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "Rushing_Att_Against"] <- ifelse(length(Rushing_Att_Home) == 0, 0, Rushing_Att_Home)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "Yards_Per_Rush_Against"] <- ifelse(length(Yards_Per_Rush_Home) == 0, 0, Yards_Per_Rush_Home)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "Penalties_Against"] <- ifelse(length(Penalties_Home) == 0, 0, Penalties_Home)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "Penalties_Yards_Against"] <- ifelse(length(Penalties_Yards_Home) == 0, 0, Penalties_Yards_Home)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "Turnovers_Against"] <- ifelse(length(Turnovers_Home) == 0, 0, Turnovers_Home)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "Fumbles_Lost_Against"] <- ifelse(length(Fumbles_Lost_Home) == 0, 0, Fumbles_Lost_Home)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "Possession_Against"] <- ifelse(length(Possession_Home) == 0, 0, Possession_Home)
    ncaaf_l1[ncaaf_l1$Game_ID == game_id & ncaaf_l1$Home == FALSE, "Possession_Percent_Against"] <- ifelse(length(Possession_Percent_Home) == 0, 0, Possession_Percent_Home) 
  }
  
}

ncaaf_l2_raw <- ncaaf_l1

ncaaf_l2 <- ncaaf_l2_raw

#### Get the ID for the Line_Favored column ####

# Line_Favored_Ids <- ncaaf_l2 %>% filter(Line_Favored != "NA", Team_FBS == 1, Opp_FBS == 1) %>% group_by(Team, Line_Favored) %>% tally() %>% arrange(Team, desc(n))
# Line_Favored_Max <- Line_Favored_Ids %>% group_by(Team) %>% summarize(max_n = max(n))
# Line_Favored_Ids <- Line_Favored_Ids %>% left_join(Line_Favored_Max, by = c("Team" = "Team"))
# Line_Favored_Ids <- Line_Favored_Ids %>% filter(n == max_n) %>% select(-max_n) %>% arrange(n)
# 
# write.csv(Line_Favored_Ids, file = "~/GitHub/Open_Data/Data/Sports/NCAAF/NCAAF_Short_Names.csv")

#### Bring in short names ####

Short_Names <- read_csv("~/GitHub/Open_Data/Data/Sports/NCAAF/NCAAF_Short_Names.csv")

ncaaf_l2 <- ncaaf_l2 %>% left_join(Short_Names, by = c("Line_Favored" = "Short_Name"))

ncaaf_l2 <- ncaaf_l2 %>% mutate(
  Line_Favored = Favored,
  Vegas_Result = case_when(
    Team == Line_Favored & Result == "W" ~ 1,
    Team != Line_Favored & Result == "L" ~ 1,
    TRUE ~ 0
  )
) %>% select(-Favored)

Last_Week <- ncaaf_l2 %>% filter(Date <= "2020-11-07" & Date >= "2020-10-31" & Home == T & Team_FBS == 1 & Opp_FBS == 1 & Played == TRUE)
sum(Last_Week$Vegas_Result)/nrow(Last_Week)

Season_2019 <- ncaaf_l2 %>% filter(Season == 2019 & Home == T & Team_FBS == 1 & Opp_FBS == 1 & Played == TRUE)
sum(Season_2019$Vegas_Result)/nrow(Season_2019)
View(Season_2019 %>% select())

#### Join historical and current season ####

ncaaf_l1_historical <- read_csv("C:/Users/Matt C137/Documents/GitHub/Open_Data/Data/Sports/NCAAF/NCAAF_Level_One.csv")
ncaaf_l2_historical <- read_csv("C:/Users/Matt C137/Documents/GitHub/Open_Data/Data/Sports/NCAAF/NCAAF_Level_Two.csv")

write.csv(ncaaf_l1_historical, paste0("C:\Users\Matt C137\Documents\Open_Data_Archive\NCAAF_Level_One", Sys.Date(), ".csv"))
write.csv(ncaaf_l2_historical, paste0("C:\Users\Matt C137\Documents\Open_Data_Archive\NCAAF_Level_Two", Sys.Date(), ".csv"))

ncaaf_l1_historical <- ncaaf_l1_historical %>% filter(Season != seasons)
ncaaf_l2_historical <- ncaaf_l2_historical %>% filter(Season != seasons)

ncaaf_l1 <- rbind(ncaaf_l1, ncaaf_l1_historical)
ncaaf_l2 <- rbind(ncaaf_l2, ncaaf_l2_historical)

ncaaf_l1 <- ncaaf_l1 %>% arrange(desc(Played), desc(Date), Game_ID) %>% filter(complete.cases(.))
ncaaf_l2 <- ncaaf_l2 %>% arrange(desc(Played), desc(Date), Game_ID) %>% filter(complete.cases(.))

write.csv(Name_Mapping %>% select(Team = Name, FBS), "C:/Users/Matt C137/Documents/GitHub/Open_Data/Data/Sports/NCAAF/NCAAF_Team_List.csv", row.names = F)

write.csv(ncaaf_l1, "C:/Users/Matt C137/Documents/GitHub/Open_Data/Data/Sports/NCAAF/NCAAF_Level_One.csv", row.names = F)
write.csv(ncaaf_l2, "C:/Users/Matt C137/Documents/GitHub/Open_Data/Data/Sports/NCAAF/NCAAF_Level_Two.csv", row.names = F)

termId <- rstudioapi::terminalCreate()
rstudioapi::terminalSend(termId, paste0('
                         cd GitHub/Open_Data\n
                         git add .\n
                         git commit -m "NCAAF Update ', Sys.Date(), '"\n
                         git push origin master\n
                         '))
