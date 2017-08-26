library(ggplot2)
library(tidyr)
library(dplyr)
library(stringr)
library(rvest)
library(PlayerRatings)

get_game_data <- function() {
    load("data/game-data.Rda")
    game_data %>% 
        as_data_frame() %>% 
        arrange(Year, Date, Field)
}

get_tidy_game_data <- function() {
    data <- get_game_data()
    
    one <- data %>% 
        mutate(Team = TeamOne, Opponent = TeamTwo, Score = TeamOneScore, 
            OpponentScore = TeamTwoScore, 
            Game = paste(Year, Season, Week, Date, Field, sep = "-")) %>% 
        select(-TeamOne, -TeamTwo, -TeamOneScore, -TeamTwoScore)
    
    two <- data %>% 
        mutate(Team = TeamTwo, Opponent = TeamOne, Score = TeamTwoScore, 
            OpponentScore = TeamOneScore, 
            Game = paste(Year, Season, Week, Date, Field, sep = "-")) %>% 
        select(-TeamOne, -TeamTwo, -TeamOneScore, -TeamTwoScore)
    
    combined <- bind_rows(one, two)
    
    combined$Outcome <- case_when(
        combined$Score == combined$OpponentScore ~ "Tie", 
        combined$Score > combined$OpponentScore ~ "Win", 
        combined$Score < combined$OpponentScore ~ "Loss")
    
    combined %>% 
        select(Year, Season, Week, Date, Field, Outcome, everything()) %>% 
        as_data_frame() %>% 
        arrange(Year, Date, Field)
}
