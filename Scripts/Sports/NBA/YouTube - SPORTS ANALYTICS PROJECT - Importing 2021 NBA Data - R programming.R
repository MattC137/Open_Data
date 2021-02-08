library(readr)
library(dplyr)

games_2021 <- read_csv("https://raw.githubusercontent.com/MattC137/Open_Data/master/Data/Sports/NBA/NBA_2021_Games.csv")
glimpse(games_2021)

View(games_2021 %>% filter(Season_Type == "Regular-Season", Game_Id == "401267173"))

View(games_2021 %>% filter(Result == "TBD"))

games_2021[9, "Spread_Winner"] <- ifelse((games_2021[9, "Short_Name"] == line_favored & games_2021[9, "Result"] == "W") | (games_2021[9, "Short_Name"] != line_favored & games_2021[9, "Result"] == "L"), 1, 0)
games_2021[9+1, "Spread_Winner"] <- ifelse((Schedule[9+1, "Short_Name"] == tolower(line_favored) & Schedule[i, "Result"] == "W") | (Schedule[i, "Short_Name"] != tolower(line_favored) & Schedule[i, "Result"] == "L"), 1, 0)


#### Stat Leaders ####



































