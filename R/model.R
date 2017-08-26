source("R/helpers.R")

# could I adjust weights to more heavily weight games from the current season?

win_loss_outcome <- function(x, y) {
    z <- x-y
    ifelse(z > 0, 1, ifelse(z == 0, 0.5, 0))
}

score_outcome <- function(x, y) {
    x / (x + y)
}

model <- function(model_fun, model_fun_args, outcome_fun, plot = TRUE) {
    game_data <- get_game_data()
    input <- game_data %>% 
        transmute(Date = as.numeric(lubridate::ymd_hms(Date)), 
            TeamOne = TeamOne, TeamTwo = TeamTwo, 
            Outcome = outcome_fun(TeamOneScore, TeamTwoScore))
    
    s <- do.call(model_fun, c(list(input, history = TRUE), model_fun_args))
    
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
    
    if(plot) {
        p <- history %>% 
            # filter(Team %in% c("Stranger Danger", "Sluggernauts",
            # "Balls Deep", "Hitmen", "Flesh Eating Sharks")) %>%
            #filter(Year == 2017) %>% 
            ggplot(aes(x = Game, y = Rating, color = Team == "Stranger Danger")) + 
            geom_step(aes(group = Team)) + scale_color_discrete(guide = FALSE)  
        print(p)
    }
    
    invisible(history)
}


save_model <- function(m, ma, o, file) {
    model(m, ma, o) %>% 
        write.csv(file = paste0("model-outputs/", file, ".csv"), row.names = FALSE)
}

save_model(elo, list(kfac = 32), win_loss_outcome, 
    "elo-k32-winloss")

save_model(elo, list(kfac = 32), score_outcome, 
    "elo-k32-score")

save_model(steph, NULL, win_loss_outcome, 
    "steph--winloss")

save_model(steph, NULL, score_outcome, 
    "steph--score")

save_model(glicko, NULL, win_loss_outcome, 
    "glicko--winloss")

save_model(glicko, NULL, score_outcome, 
    "glicko--score")

# remove previous versions of all-models
file.remove("model-outputs/all-models.csv")

all_models <- lapply(list.files("model-outputs/", full.names = TRUE), read.csv)
names(all_models) <- str_replace(list.files("model-outputs/"), "\\.csv", "")

all_models <- bind_rows(all_models, .id = "Model")  %>% 
    tidyr::separate(Model, c("Model", "Arguments", "Outcome"), sep = "-")

write.csv(all_models, "model-outputs/all-models.csv", row.names = FALSE)
