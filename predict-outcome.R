rate <- function(x, y) {
    a <- 10^(x/400)
    b <- 10^(y/400)
    a / (a + b)
}

read.csv("model-outputs/all-models.csv") %>% 
    filter(Game == max(Game), Team %in% c("Stranger Danger", "Pinjers")) %>% 
    arrange(Team) %>% 
    group_by(Model, Arguments, Outcome) %>% 
    summarise(prob = rate(Rating[1], Rating[2]), Team1 = Team[1]) %>% 
    arrange(desc(prob))

all_models %>% filter(Team %in% c("Stranger Danger", "Pinjers", "Sluggernauts", "Balls Deep")) %>%
    ggplot(aes(y = Rating, x = Game, color = Team)) + geom_path() + facet_grid(Model ~ Outcome)



# add method predicting outcome based on previous meetings of the teams



get_ranking <- function(team) {
    # from model.R
    history %>% 
        filter_(~Team == team) %>% 
        arrange(desc(Date)) %>% 
        slice(1) %>% 
        .$Rating
}

get_record <- function(team) {
    # from summary-statistics.R
    games %>% 
        filter_(~Team == team) %>% 
        arrange(desc(Date)) %>% 
        slice(1) %>% 
        .$Record
}

elo_score <- function(team, opponent) {
    team_ranking <- get_ranking(team)
    opponent_ranking <- get_ranking(opponent)
    10^(team_ranking/400) / (10^(team_ranking/400) + 10^(opponent_ranking/400))
}

record_score <- function(team, opponent) {
    team_record <- get_record(team)
    opponent_record <- get_record(opponent)
    diff <- (team_record - opponent_record) / 2
    0.5 + diff
}

score_df <- function(team1, team2, method) {
    result <- method(team1, team2)
    d <- data.frame(round(result, 2), round(1-result, 2))
    names(d) <- c(team1, team2)
    d$method <- as.character(substitute(method))
    d
}

faceoff <- function(team1, team2) {
    bind_rows(
        score_df(team1, team2, elo_score), 
        score_df(team1, team2, record_score)
    )
}