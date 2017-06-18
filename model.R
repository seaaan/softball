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
        Time = lubridate::ymd_hms(Time))

seasons <- game_data %>% 
    group_by(Season, Year) %>% 
    summarise(Time = lubridate::ymd_hms(min(Date)))

history %>% 
    filter(Team %in% c("Stranger Danger", "Sluggernauts",
        "Balls Deep", "Hitmen", "Flesh Eating Sharks")) %>%
    ggplot(aes(x = Time, y = Rating, color = Team)) + 
        geom_step() +
#        geom_vline(xintercept = as.numeric(seasons$Time), color = "black") + 
        scale_color_brewer(type = "qual", palette = 2)

