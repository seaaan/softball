source("helpers.R")
combined <- get_tidy_game_data()

all_time <- combined %>% 
    group_by(Team) %>% 
    summarise(Win = sum(Outcome == "Win"), Tie = sum(Outcome == "Tie"), 
        Loss = sum(Outcome == "Loss"), n = n(), pct = Win / n) %>% 
    arrange(desc(n))

all_time %>% 
    arrange(pct) %>% 
    mutate(index = 1:77) %>% 
    ggplot(aes(x = index, y = pct)) + 
        geom_point(aes(color = Team == "Stranger Danger")) + 
        facet_wrap(~ n > 20)

by_season <- combined %>% 
    mutate(Date = recode(Season, 
        "Fall" = "01-09", "Spring" = "01-06", "Summer" = "15-07")) %>% 
    mutate(time = paste(Date, Year, sep = "-")) %>% 
    mutate(Season = lubridate::dmy(time)) %>% 
    group_by(Team, Season) %>% 
    summarise(Win = sum(Outcome == "Win"), Tie = sum(Outcome == "Tie"), 
        Loss = sum(Outcome == "Loss"), n = n(), pct = Win / n, 
        margin = mean(Score - OpponentScore), 
        net = sum(Score) - sum(OpponentScore)) %>% 
    arrange(desc(n))

by_season %>% 
    ggplot(aes(x = Season, y=pct, color = Team == "Stranger Danger")) +
    geom_point() + geom_line(aes(group=Team)) 

by_season %>% 
    ggplot(aes(x = Season, y = margin, color = Team == "Stranger Danger")) +
    geom_point() + geom_line(aes(group=Team)) 

by_season %>% 
    ggplot(aes(x = Season, y = net, color = Team == "Stranger Danger")) +
    geom_point() + geom_line(aes(group=Team)) 

by_season %>% 
    ggplot(aes(x = pct, y = margin, color = Team == "Stranger Danger")) +
    geom_point() + facet_wrap(~ Season)

filter(combined, Team %in% c("Stranger Danger", "Balls Deep", "Hitmen", "#Blessed")) %>% 
    ggplot(aes(x = Year, y = Score - OpponentScore, color = Outcome)) + 
        geom_point() + scale_color_brewer(type = "qual", palette = 2) + 
        facet_wrap(~ Team)

combined %>% 
    filter(Team == "Stranger Danger") %>% group_by(Opponent) %>% 
    summarise(n = n(), Win = sum(Outcome == "Win"), Tie = sum(Outcome == "Tie"), 
        Loss = sum(Outcome == "Loss")) %>% 
        filter(n > 1) %>% 
        arrange((Win + Tie) / n)
