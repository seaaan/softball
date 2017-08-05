source("helpers.R")
combined <- get_tidy_game_data()

# ----------------------------------------------------------------------------
# all time summaries ---------------------------------------------------------
# ----------------------------------------------------------------------------
all_time <- combined %>% 
    group_by(Team) %>% 
    summarise(Win = sum(Outcome == "Win"), Tie = sum(Outcome == "Tie"), 
        Loss = sum(Outcome == "Loss"), Margin = mean(Score - OpponentScore), 
        MeanScore = mean(Score), Score = sum(Score), n = n(), pct = Win / n) %>% 
    arrange(desc(n)) %>% 
    mutate(us = ifelse(Team == "Stranger Danger", "Stranger Danger", "Other"))

all_time %>% 
    arrange(pct) %>% 
    mutate(index = 1:nrow(.)) %>% 
    ggplot(aes(x = index, y = pct)) + 
        geom_point(aes(color = us)) + 
        facet_wrap(~ n > 20) + 
        ggtitle("All time winning pct by teams with 20 or more games")

all_time %>% 
    ggplot(aes(x = Margin, y = n)) + 
    geom_point(aes(color = us)) + 
    stat_smooth(method = "lm") + 
    ggtitle("Correlation between number of games and mean margin")
    
# ----------------------------------------------------------------------------
# score-level summaries ------------------------------------------------------
# ----------------------------------------------------------------------------
to_plot <- combined %>% 
    group_by(Team) %>% 
    filter(n() > 25, Outcome != "Tie") %>% 
    ungroup() %>% 
    mutate(Team = str_replace(Team, " ", "\n"))

ggplot(to_plot, aes(x = Score, fill = Outcome)) + 
    geom_histogram(bins = max(abs(to_plot$Score - to_plot$OpponentScore))) +
    ggtitle("Score distributions for wins vs losses for\nteams with more than 25 games") + 
    facet_wrap(~ Team, scales = "free_y")

# ---------------------------------------------------------------------------
# selected teams ------------------------------------------------------------
# ---------------------------------------------------------------------------
filter(combined, Team %in% c("Stranger Danger", "Balls Deep", "Hitmen", "#Blessed")) %>% 
    arrange(Outcome == "Tie") %>% 
    ggplot(aes(x = Year, y = Score - OpponentScore, color = Outcome)) + 
        geom_point() + scale_color_brewer(type = "qual", palette = 2) + 
        facet_wrap(~ Team) + 
        ggtitle("Margin in every game for selected teams")

# results against all teams we've played at least once
combined %>% 
    filter(Team == "Stranger Danger") %>% group_by(Opponent) %>% 
    summarise(n = n(), Win = sum(Outcome == "Win"), Tie = sum(Outcome == "Tie"), 
        Loss = sum(Outcome == "Loss")) %>% 
        filter(n > 1) %>% 
        arrange((Win + Tie / 2) / n)

selected_teams <- function(y) {
    by_season %>% 
        group_by(Team) %>% 
        filter(n() > 3) %>% 
        ungroup() %>% 
        mutate(Team = str_replace(Team, " ", "\n")) %>% 
        ggplot(aes_string(x = "Team", y = y)) +        
        geom_boxplot() + geom_point(aes(color = Year))
}

selected_teams("pct") + 
    ggtitle("Average winning percent by season for\nteams with more than 3 seasons")

selected_teams("margin") + 
    ggtitle("Average margin by season for\nteams with more than 3 seasons")

# ---------------------------------------------------------------------------
# game-level summaries ----------------------------------------------------
# ---------------------------------------------------------------------------
game_n <- combined %>% 
    select(Date, Field) %>%
    unique() %>% 
    arrange(Date, Field) %>% 
    mutate(GameN = 1:n())

highlight_teams <- combined %>% 
    filter(Team == "Stranger Danger") %>% 
    group_by(Opponent) %>% 
    summarise(n=n()) %>% 
    filter(n > 2) %>%  
    .$Opponent %>% 
    c(., "Stranger Danger")

games <- combined %>% 
    merge(game_n) %>% 
    group_by(Team) %>% 
    arrange(Date) %>% 
    mutate(Record = cumsum(Outcome == "Win") / seq_along(Outcome), 
        Margin = cumsum(Score - OpponentScore) / seq_along(Outcome)) %>% 
    # pick out certain teams to highlight
    mutate(Highlight = ifelse(Team %in% highlight_teams, Team, "Other")) %>% 
    mutate(Highlight = factor(Highlight, levels = c(highlight_teams, "Other"))) %>% 
    arrange(Highlight)

high <- filter(games, Highlight != "Other")
ggplot(games, mapping = aes(x = GameN, y = Record, color = Highlight)) + 
    geom_line(aes(group = Team)) + 
    geom_line(data = high, aes(group = Team)) + 
    # color by Set1 palette except for "other" teams, which should be grey
    scale_color_manual(values = c(scale_color_brewer(palette = "Set1")$palette(length(highlight_teams)), "grey80")) +
    ggtitle("Complete record")

ggplot(games, mapping = aes(x = GameN, y = Record, color = Highlight)) + 
    geom_line(aes(group = Team)) + 
    geom_line(data = high, aes(group = Team)) + 
    # color by Set1 palette except for "other" teams, which should be grey
    scale_color_manual(values = c(scale_color_brewer(palette = "Set1")$palette(length(highlight_teams)), "grey80")) +
    ggtitle("Complete margin")

# ---------------------------------------------------------------------------
# season-level summaries ----------------------------------------------------
# ---------------------------------------------------------------------------
by_season <- combined %>% 
    group_by(Team, Year, Season) %>% 
    summarise(Win = sum(Outcome == "Win"), Tie = sum(Outcome == "Tie"), 
        Loss = sum(Outcome == "Loss"), n = n(), pct = Win / n, 
        margin = mean(Score - OpponentScore), 
        net = sum(Score) - sum(OpponentScore)) %>% 
    arrange(desc(n)) %>% 
    ungroup() %>% 
    # put year and season in the correct order 
    arrange(Year, Season) %>% 
    mutate(YearSeason = paste(Year, as.character(Season)), 
        YearSeason = factor(YearSeason, levels = unique(YearSeason)))

season_plot <- function(...) {
    ggplot(by_season, 
        aes_string(color = 'Team == "Stranger Danger"', x = "YearSeason", ...)) + 
        labs(color = "Us?") + 
        geom_line(aes(group=Team)) +
        gghlab::tilt_x_labels()
}

season_plot(y = "pct") +
    ggtitle("Winning percent by season for all teams")

season_plot(y = "margin") + 
    ggtitle("Average score margin by season for all teams")

season_plot(y = "net") + 
    ggtitle("Net score for the whole season for all teams")

by_season %>% 
    ggplot(aes(x = pct, y = margin, color = Team == "Stranger Danger")) +
    geom_point() + facet_grid(Season ~ Year) + labs(color = "Us?") +
    ggtitle("Average score margin against winning percent")

seasonal_record <- combined %>% 
    merge(game_n) %>% 
    group_by(Team, Year, Season) %>% 
    arrange(Date) %>% 
    summarise(Record = mean(Outcome == "Win")) %>%
    ungroup() %>% 
    # put year and season in the correct order 
    arrange(Year, Season) %>% 
    mutate(YearSeason = paste(Year, as.character(Season)), 
        YearSeason = factor(YearSeason, levels = unique(YearSeason))) %>% 
    # pick out certain teams to highlight
    mutate(Highlight = ifelse(Team %in% highlight_teams, Team, "Other")) %>% 
    mutate(Highlight = factor(Highlight, levels = c(highlight_teams, "Other"))) %>% 
    arrange(Highlight)

ggplot(seasonal_record, aes(x = YearSeason, y = Record, color = Highlight)) + 
    geom_line(aes(group = Team)) + 
    # color by Set1 palette except for "other" teams, which should be grey
    scale_color_manual(values = c(scale_color_brewer(palette = "Set1")$palette(length(highlight_teams)), "grey80")) +
    ggtitle("Seasonal record") + gghlab::tilt_x_labels()