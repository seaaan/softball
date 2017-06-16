library(dplyr)
library(PlayerRatings)
library(ggplot2)
library(tidyr)


# could I adjust weights to more heavily weight games from the current season?





game_data <- read.csv("data/game-data.csv", stringsAsFactors = FALSE)

determine_outcome <- function(x, y) {
    z <- x-y
    ifelse(z > 0, 1, ifelse(z == 0, 0.5, 0))
}

input <- game_data %>% 
    mutate(Season = recode(Season, "Spring" = 1, "Summer" = 2, Fall = 3),
        Time = factor(Time)) %>% 
    # create data represented by numeric
    transmute(Date = 100*Year + 10*Season + Week + 0.1*as.numeric(Time), 
        TeamOne = TeamOne, TeamTwo = TeamTwo, 
        Outcome = determine_outcome(TeamOneScore, TeamTwoScore))
        
s <- steph(input, history = TRUE)

history <- s$history %>% 
    .[, , 1] %>% 
    t() %>% 
    as.data.frame() %>% 
    gather(Team, Rating) %>% 
    mutate(Time = rep(1:303, 80))

history %>% 
    filter(Team %in% c("Stranger Danger", "Sluggernauts",
        "Balls Deep", "Hitmen", "Afternoon Delight")) %>%
    ggplot(aes(x = Time, y = Rating, color = Team)) + geom_line() +
     scale_color_brewer(type = "qual", palette = 2) 

# need to combine: 


