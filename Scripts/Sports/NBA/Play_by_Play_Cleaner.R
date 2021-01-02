library(dplyr)
library(rvest)
library(readr)
library(tidyr)
library(stringr)
library(lubridate)

Season <- 2021

#### Clean Player Id Str ####

# pid = "kentavious-caldwell-pope"
# pid = "troy-daniels"
# pid = "zach-norvell-jr"
# pid = "terence-davis-ii"

Clean_Player_Id_Str <- function(pid){
  
  isJr <- ifelse(substr(pid, nchar(pid) - 2, nchar(pid)) == "-jr", 1, 0)
  pid <- ifelse(isJr, substr(pid, 1, nchar(pid) - 3), pid)
  pid <- str_remove(pid, "-ii")
  
  hypen_locations <- str_locate_all(pid, "-")
  hypen_locations <- hypen_locations[[1]][, 1]
  
  if(length(hypen_locations) == 1){
    names <- str_split(pid, "-")
    names <- names[[1]]
    names <- str_to_title(names)
    
    rtn_name <- ifelse(isJr, paste0(names[1], " ", names[2], " ", "Jr."), paste0(names[1], " ", names[2]))
  }
  
  if(length(hypen_locations) == 2){
    names <- str_split(pid, "-")
    names <- names[[1]]
    names <- str_to_title(names)
    
    rtn_name <- ifelse(isJr, paste0(names[1], " ", names[2], "-", names[3], " ", "Jr."), paste0(names[1], " ", names[2], "-", names[3]))
  }
  
  return(as.vector(rtn_name))
}


play_by_play <- play_by_play %>% mutate(
  Quarter = 1,
  Total_Minutes = 0,
  Away_Player_1 = NA,
  Away_Player_2 = NA,
  Away_Player_3 = NA,
  Away_Player_4 = NA,
  Away_Player_5 = NA,
  Home_Player_1 = NA,
  Home_Player_2 = NA,
  Home_Player_3 = NA,
  Home_Player_4 = NA,
  Home_Player_5 = NA,
  DATA_CHECK = "No Issue"
) %>% separate(Score, into = c("Away_Score", "Home_Score"), sep = " - ")

Play_by_Play_Cleaned <- play_by_play[0,]

Additional_Players <- as.data.frame(matrix(nrow = 0, ncol = 6))
names(Additional_Players) <- c("Player_Id", "Players", "Player_Id_Str", "Team", "Position", "Player")

Dupped_Players <- as.data.frame(matrix(nrow = 0, ncol = 6))
names(Dupped_Players) <- c("Player_Id", "Players", "Player_Id_Str", "Team", "Position", "Player")

for(i in 1:length(unique(play_by_play$Game_Id))){
  # for(i in 1:30){
  # i = 3
  
  pbp_game_id <- unique(play_by_play$Game_Id)[i]
  
  # pbp_game_id <- 401160666
  pbp <- play_by_play %>% filter(Game_Id == pbp_game_id)
  
  print(paste(i, pbp_game_id))
  
  #### Classify Play-by-Play ####
  
  pbp <- pbp %>% mutate(
    Play_Type = case_when(
      str_detect(Play, " vs. ") ~ "Jump Ball",
      str_detect(Play, "makes") | str_detect(Play, "misses") | str_detect(Play, "blocks") ~ "Shot",
      str_detect(Play, "rebound") ~ "Rebound",
      str_detect(Play, "foul") ~ "Foul",
      str_detect(Play, "timeout") ~ "Timeout",
      str_detect(Play, "enters the game for") ~ "Substitution",
      str_detect(Play, "delay of game") ~ "Delay of Game",
      str_detect(Play, "Coach's Challenge") ~ "Challenge",
      str_detect(Play, "bad pass") ~ "Bad Pass",
      str_detect(Play, "traveling") ~ "Travel",
      str_detect(Play, "turnover") ~ "Turnover",
      str_detect(Play, "offensive charge") ~ "Offensive Charge",
      str_detect(Play, "kicked ball violation") ~ "Kicked Ball Violation",
      str_detect(Play, "End of the 1st Quarter") ~ "End of the 1st Quarter",
      str_detect(Play, "End of the 2nd Quarter") ~ "End of the 2nd Quarter",
      str_detect(Play, "End of the 3rd Quarter") ~ "End of the 3rd Quarter",
      str_detect(Play, "End of the 4th Quarter") ~ "End of the 4th Quarter",
      str_detect(Play, "End of Game") ~ "End of Game",
      TRUE ~ "NEED TO CLASSIFY"
    )
  )
  
  #### Player Substitutions ####
  
  players <- Box_Scores %>%
    rowwise() %>% 
    filter(Game_Id == pbp_game_id) %>% 
    select(Player_Id, Players, Player_Id_Str, Team, Position, Home) %>% 
    left_join(Salaries %>% select(Player_Id, Player), by = c("Player_Id" = "Player_Id")) %>% 
    mutate(
      Player = ifelse(is.na(Player), Clean_Player_Id_Str(Player_Id_Str), Player)
    ) %>% as.data.frame()
  
  additional_players <- Box_Scores %>%
    rowwise() %>% 
    filter(Game_Id == pbp_game_id) %>% 
    select(Player_Id, Players, Player_Id_Str, Team, Position, Home) %>% 
    left_join(Salaries %>% select(Player_Id, Player), by = c("Player_Id" = "Player_Id")) %>%
    filter(is.na(Player)) %>% 
    mutate(
      Player = ifelse(is.na(Player), Clean_Player_Id_Str(Player_Id_Str), Player)
    ) %>% as.data.frame()
  
  Additional_Players <- rbind(Additional_Players, additional_players)
  
  home_starters <- players %>% filter(Home == TRUE) %>% select(Player)
  home_starters <- home_starters[1:5, 1]
  
  away_starters <- players %>% filter(Home == FALSE) %>% select(Player)
  away_starters <- away_starters[1:5, 1]
  
  pbp[1, which(names(pbp) == "Away_Player_1"):which(names(pbp) == "Away_Player_5")] <- away_starters
  pbp[1, which(names(pbp) == "Home_Player_1"):which(names(pbp) == "Home_Player_5")] <- home_starters
  
  for(j in 1:nrow(pbp)){
    # for(j in 1:80){
    # j = 1
    # j = 2
    # j = 81
    #print(j)
    play <- pbp[j, "Play"]
    
    players_temp <- players
    
    players_temp <- players_temp %>% mutate(
      Involved = str_locate(play, Player)[, 1]
    ) %>% filter(!is.na(Involved)) %>% arrange(Involved)
    
    players_temp <- players_temp[ ,"Player"]
    
    if(j > 1){
      
      pbp[j, which(names(pbp) == "Away_Player_1"):(which(names(pbp) == "Away_Player_5"))] <- pbp[j-1, which(names(pbp) == "Away_Player_1"):(which(names(pbp) == "Away_Player_5"))]
      pbp[j, which(names(pbp) == "Home_Player_1"):(which(names(pbp) == "Home_Player_5"))] <- pbp[j-1, which(names(pbp) == "Home_Player_1"):(which(names(pbp) == "Home_Player_5"))]
      
      if(pbp[j, "Play_Type"] == "Substitution"){
        # First I need to check that the pbp is entered correctly
        #
        # Error 1: Incomplete sub
        #
        # Season 2020
        # i = 2
        # j = 112 (Lines 112 & 114)
        # j = 158
        # pbp_game_id <- 401160624
        #
        # https://www.espn.com/nba/playbyplay?gameId=401160624
        #
        #  @1:16 25-17 "Kawhi Leonard enters the game for"
        #  @1:16 25-17 "enters the game for Maurice Harkless"
        #
        # Should be "Kawhi Leonard enters Maurice Harkless"
        
        if(length(players_temp) < 2){
          
          play_split <- str_split(play, " enters the game for ")
          play_split <- str_trim(play_split[[1]])
          
          if(length(play_split) == 1){
            
            fragments = pbp[pbp$Time == pbp$Time[j] & pbp$Home_Score == pbp$Home_Score[j] & pbp$Away_Score == pbp$Away_Score[j] & pbp$Team == pbp$Team[j] & str_detect(pbp$Play, pattern = "enters the game for"), "Play"]
            
            if(any(str_detect(fragments[2:length(fragments)], fragments[1]))){
              fragment_main <- fragments[1]
              fragments <- fragments[2:length(fragments)]
              fragment_replacement <- fragments[str_detect(fragments, fragment_main)]
              pbp[pbp$Time == pbp$Time[j] & pbp$Home_Score == pbp$Home_Score[j] & pbp$Away_Score == pbp$Away_Score[j] & pbp$Team == pbp$Team[j] & pbp$Play == fragment_replacement, "Play_Type"] <- "Remove"
              
              play_split <- str_split(fragments[str_detect(fragments, fragment_main)], " enters the game for ")
              play_split <- str_trim(play_split[[1]])
              
            }else{
              if(length(fragments) > 2){
                fragments <- flatten(str_split(fragments, " enters the game for "))
                fragments <- unlist(fragments[str_detect(fragments, "enters the game for")])
              }
              
              sub_update = ifelse(str_locate(fragments[1], "enters")[1] == 1, 
                                  paste0(fragments[2], " ", str_remove(fragments[1], "enters the game for ")), 
                                  paste0(fragments[1], " ", str_remove(fragments[2], "enters the game for ")))
              
              pbp[j, "Play"] <- sub_update
              
              play <- pbp[j, "Play"]
              
              play_split <- str_split(play, " enters the game for ")
              play_split <- str_trim(play_split[[1]])
              
              # players_temp <- players
              # 
              # players_temp <- players_temp %>% mutate(
              #   Involved = str_locate(play, Player)[, 1]
              # ) %>% filter(!is.na(Involved)) %>% arrange(Involved)
              # 
              # players_temp <- players_temp[ ,"Player"]
              
              pbp[which(pbp$Play == fragments[2]), "Play_Type"] <- "Remove"
            }
          }
          
          if(length(play_split) == 2 | length(play_split) == 0){
            
            player1 <- play_split[1] %>% 
              str_replace_all(" ", "-")  %>% 
              str_replace_all("\\.", "")%>% 
              str_replace_all("'", "")%>% 
              tolower()
            
            player1 <- players[str_detect(players$Player_Id_Str, player1), "Player"]
            
            player2 <- play_split[2] %>% 
              str_replace_all(" ", "-")  %>% 
              str_replace_all("\\.", "")%>% 
              str_replace_all("'", "")%>% 
              tolower()
            
            player2 <- players[str_detect(players$Player_Id_Str, player2), "Player"]
            
            sub_update <- paste0(player1, " enters the game for ", player2)
            
            pbp[j, "Play"] <- sub_update
            
            play <- pbp[j, "Play"]
            
            players_temp <- players
            
            players_temp <- players_temp %>% mutate(
              Involved = str_locate(play, Player)[, 1]
            ) %>% filter(!is.na(Involved)) %>% arrange(Involved)
            
            players_temp <- players_temp[ ,"Player"]
            
          }
          
          # pbp[which(pbp$Play == fragments[2]), "Play"] <- "Remove"
          # print(j)
          # print(which(pbp$Play == fragments[2]))
          # print("Break")
        }
        
        if(length(players_temp) > 2){
          
          if(length(players_temp == 3)){
            if(sum(players_temp[1] == players_temp) == 2){
              Dupped_Players <- rbind(Dupped_Players, players[players$Player == players_temp[1], ])
            }
            
            if(sum(players_temp[2] == players_temp) == 2){
              Dupped_Players <- rbind(Dupped_Players, players[players$Player == players_temp[2], ])
            }
            
            players_temp <- unique(players_temp)
          }
        }
        
        if(length(players_temp) == 2 & str_detect(pbp[j, "Play"], " enters the game for ")){  
          pbp[j, which(pbp[j, ] == players_temp[2])] <- players_temp[1]
        }else{
          pbp[which(pbp$Play == fragments[2]), "Play_Type"] <- "Substitution Error"
        }
        
        sub_error <- any(pbp[j, which(names(pbp) == "Away_Player_1"):(which(names(pbp) == "Home_Player_5"))] != pbp[j-1, which(names(pbp) == "Away_Player_1"):(which(names(pbp) == "Home_Player_5"))])
        
        pbp[j, "DATA_CHECK"] <- ifelse(sub_error, pbp[j, "DATA_CHECK"], "Substitution Error")
        
      }
    }
    
    # View(pbp %>% select(Play, play_type) %>% filter(is.na(play_type)))
    
  }
  
  Play_by_Play_Cleaned <- rbind(Play_by_Play_Cleaned, pbp)  
  
  # View(pbp %>% filter(Play_Type == "Substitution" | Play_Type == "Remove") %>% select(Play, Play_Type, Away_Player_1, Away_Player_2, Away_Player_3, Away_Player_4, Away_Player_5, Home_Player_1, Home_Player_2, Home_Player_3, Home_Player_4, Home_Player_5)) 
  # View(pbp %>% select(Play, Play_Type, Away_Player_1, Away_Player_2, Away_Player_3, Away_Player_4, Away_Player_5, Home_Player_1, Home_Player_2, Home_Player_3, Home_Player_4, Home_Player_5)) 
  
}

#### Play-by-play Data Checks ####
table(Play_by_Play_Cleaned$DATA_CHECK)

View(Play_by_Play_Cleaned %>% filter(DATA_CHECK == "Substitution Error"))