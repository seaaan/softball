library(PlayerRatings)
source("helpers.R")

# could I adjust weights to more heavily weight games from the current season?





game_data <- get_game_data()

determine_outcome <- function(x, y) {
    z <- x-y
    ifelse(z > 0, 1, ifelse(z == 0, 0.5, 0))
}

input <- game_data %>% 
    transmute(Date = as.numeric(lubridate::ymd_hms(Date)), 
        TeamOne = TeamOne, TeamTwo = TeamTwo, 
        Outcome = determine_outcome(TeamOneScore, TeamTwoScore))
        
s <- steph(input, history = TRUE)

history <- s$history %>% 
    .[, , 1] %>% 
    t() %>% 
    as.data.frame() %>% 
    gather(Team, Rating) %>% 
    mutate(Time = rep(unique(game_data$Date), nrow(s$history)), 
        Time = lubridate::ymd_hms(Time), 
        Game = rep(1:ncol(s$history),  nrow(s$history))) %>% 
    as_data_frame()

games <- game_data %>% 
    group_by(Season, Week, Date) %>% 
    summarise(Year = Year[1]) 

history <- merge(games, history, by.y = "Time", by.x = "Date") %>% 
    as_data_frame()

history %>% 
    filter(Team %in% c("Stranger Danger", "Sluggernauts",
        "Balls Deep", "Hitmen", "Flesh Eating Sharks")) %>%
    filter(Year == 2017) %>% 
    ggplot(aes(x = Game, y = Rating, color = Team)) + 
        geom_step() +
        scale_color_brewer(type = "qual", palette = 2)

expected_score <- function(team_ranking, opponent_ranking) {
    10^(team_ranking/400) / (10^(team_ranking/400) + 10^(opponent_ranking/400))
}

