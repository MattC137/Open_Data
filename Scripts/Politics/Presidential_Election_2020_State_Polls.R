library(dplyr)
library(tidyr)
library(stringr)
library(rvest)
library(lubridate)

setwd("~/GitHub/Open_Data")

#### Electoral Votes

votes_map_html <- read_html(
  "https://www.realclearpolitics.com/epolls/2020/president/2020_elections_electoral_college_map.html"
)

Toss_ups <- votes_map_html %>%
  str_extract_all('(?<=<span class="full">)(.*?)(?=</span>)') %>%
  unlist()

Solid_States <- votes_map_html %>%
  html_nodes("table") %>%
  html_table(fill = T)

Electoral_Votes <- c(Toss_ups,
                     Solid_States[[16]][[1]],
                     Solid_States[[16]][[2]],
                     Solid_States[[17]][[1]],
                     Solid_States[[17]][[2]])

Electoral_Votes <- Electoral_Votes[str_detect(Electoral_Votes, " ")]

Electoral_Votes <- Electoral_Votes %>% str_replace_all('\\)', '')

Electoral_Votes <- str_split_fixed(Electoral_Votes, " \\(", 2) %>%
  as.data.frame()

names(Electoral_Votes) <- c("State", "Votes")

Electoral_Votes$Votes <- as.numeric(
  as.character(Electoral_Votes$Votes)
)

rm(votes_map_html, Toss_ups, Solid_States)

# sum(Electoral_Votes$Votes, na.rm = T)

#### Getting the 2020 Links and Data

Summary_2020 <- read_html(
  "https://www.realclearpolitics.com/epolls/2020/president/2020_elections_electoral_college_map.html"
)

Summary_2020 <- Summary_2020 %>%
  str_extract_all('(?<=href=")(.*?)(?=">)') %>%
  unlist() %>%
  unique()

Summary_2020 <- Summary_2020[str_detect(Summary_2020, "trump_vs_biden")]

Summary_2020 <- Summary_2020 %>%
  str_replace_all("/epolls/2020/president/", "") %>%
  str_replace_all(".html", "") %>%
  str_replace_all("-", "/") %>%
  as.data.frame()

names(Summary_2020) <- "Link"

Summary_2020 <- str_split_fixed(Summary_2020$Link, "/", 3) %>%
  as.data.frame()

names(Summary_2020) <- c("Abbrev", "state_id", "id")

Summary_2020 <- Summary_2020 %>% mutate(
  State = state_id %>%
    str_replace_all("_trump", "") %>%
    str_replace_all("_vs", "") %>%
    str_replace_all("_biden", "") %>%
    str_replace_all("_jorgensen", "") %>%
    str_replace_all("_hawkins", "") %>%
    str_replace_all("_", " ") %>%
    str_to_title() %>%
    str_replace_all("Cd", "CD"),
  Link = paste0("https://www.realclearpolitics.com/",
                "epolls/2020/president/",
                Abbrev, "/", state_id, "-", id, ".html")
) %>% arrange(State)

State_Polls_2020_PA <- read_html(
  "https://www.realclearpolitics.com/epolls/2020/president/pa/pennsylvania_trump_vs_biden-6861.html") %>%
  html_nodes("table") %>%
  html_table()

State_Polls_2020_PA[[4]]

rcp_average <- State_Polls_2020_PA[[4]][1,1]

State_Polls_2020 <- as.data.frame(matrix(NA, nrow = 0, ncol = 7))
names(State_Polls_2020) <- c("Poll", "Date", "Sample",
                             "Biden (D)", "Trump (R)",
                             "State", "Rank")

for(i in 1:nrow(Summary_2020)){
  # i = 2
  link <- Summary_2020$Link[i]
  state <- Summary_2020$State[i]
  print(state)
  
  state_polls <- read_html(link) %>%
    html_nodes("table") %>%
    html_table()
  
  max <- length(state_polls)
  
  if(max > 0){
    state_polls <- state_polls[[max]] %>%
      select(Poll, Date, Sample, `Biden (D)`, `Trump (R)`)
    state_polls <- state_polls %>% separate(Date, " - ", into = c("Start_Date", "End_Date"))
    state_polls %>% mutate(Sample = ifelse(nchar(Sample) == 2, paste0("0 ", Sample), Sample))
    state_polls <- state_polls %>% separate(Sample, " ", into = c("Sample_Size", "Sample_Type"))
    state_polls <- state_polls %>% mutate(
      Start_Date = as.Date(paste0(Start_Date, "/2020"), format = "%m/%d/%Y", origin = "1970-01-01"),
      End_Date = as.Date(paste0(End_Date, "/2020"), format = "%m/%d/%Y", origin = "1970-01-01")
    )
    state_polls$State <- state
    state_polls <- state_polls %>% filter(Poll != rcp_average)
    state_polls$Rank <- 1:nrow(state_polls)
    
    if(nrow(State_Polls_2020) > 1){
      State_Polls_2020 <- State_Polls_2020 %>% mutate(
        Start_Date = ifelse(as.numeric(Start_Date - lag(Start_Date, 1)) > 31, as.Date(paste(year(Start_Date), month(Start_Date), day(Start_Date), sep = "-"), origin = "1970-01-01"), End_Date),
        End_Date = ifelse(as.numeric(End_Date - lag(End_Date, 1)) > 31, as.Date(paste(year(End_Date), month(End_Date), day(End_Date), sep = "-"), origin = "1970-01-01"), End_Date)
      )
    }
    
    State_Polls_2020 <- rbind(State_Polls_2020, state_polls)
    
  }
}

State_Polls_2020 <- State_Polls_2020 %>%
  rename("Biden" = `Biden (D)`, "Trump" = `Trump (R)`) %>%
  mutate(Spread = Trump - Biden)
