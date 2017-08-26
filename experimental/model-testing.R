library(PlayerRatings)
source("helpers.R")

game_data <- get_game_data()

determine_outcome <- function(x, y) {
    # z <- x-y
    # ifelse(z > 0, 1, ifelse(z == 0, 0.5, 0))
    x / (x + y)
}

input <- game_data %>% 
    transmute(Date = as.numeric(lubridate::ymd_hms(Date)), 
        TeamOne = TeamOne, TeamTwo = TeamTwo, 
        Outcome = determine_outcome(TeamOneScore, TeamTwoScore))

# doesn't work with tibbles
input <- as.data.frame(input)

dividing_line <- game_data %>% filter(Year == 2017, Season == "Spring") %>% 
    .$Date %>% min() %>% as.numeric()

test <- input[input$Date >= dividing_line, ]
input <- input[input$Date < dividing_line, ]

models <- list(
    elo(input, history = TRUE, kfac = 100), 
    elo(input, history = TRUE, kfac = 50), 
    elo(input, history = TRUE, kfac = 45), 
    elo(input, history = TRUE, kfac = 40), 
    elo(input, history = TRUE, kfac = 35), 
    elo(input, history = TRUE, kfac = 30),
    elo(input, history = TRUE, kfac = 25), 
    elo(input, history = TRUE, kfac = 20), 
    steph(input, history = TRUE, kfac = 32),     
    glicko(input, history = TRUE, kfac = 32))

names(models) <- LETTERS[1:length(models)]

preds <- lapply(models, function(m) predict(m, test, trat = c(2200, 300), tng = 15))

metrics(test$Outcome, sapply(preds, cbind))
cbind(test, sapply(preds, cbind)) %>% 
    mutate(Game = rownames(.)) %>% 
    select(Outcome:Game) %>% 
    gather(Model, Pred, -Game, -Outcome) %>% 
    mutate(Diff = (Pred - Outcome)^2) %>% 
    ggplot(aes(x = Model, y = Diff)) + geom_boxplot() + geom_point() + 
        geom_line(aes(group = Game))

for(i in unique(test$Week)) {
    testi <- test[test$Week == i,]
    predi <- predict(sobj, testi, trat = c(1900,300), gamma = 30, 
        thresh = 0.5)
    pred <- c(pred, predi)
    sobj <- steph(testi, sobj$ratings, init = c(2200,300), cval = 8, 
        hval = 8, lambda = 5)
}

table(Result=test$Score, Predictions=pred)
