library(readr)
library(dplyr)

games_2021 <- read_csv("https://raw.githubusercontent.com/MattC137/Open_Data/master/Data/Sports/NBA/NBA_2021_Games.csv")
games_2021 <- as.data.frame(games_2021)
View(games_2021 %>% filter(Result == "TBD"))

