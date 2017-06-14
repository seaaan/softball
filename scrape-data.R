library(rvest)
library(stringr)
library(tidyr)
library(dplyr)
    
process_table <- function(table, week) {
    table %>% 
        # give name to first column
        select(Time = 1, everything()) %>% 
        tidyr::gather(Field, Game, -1) %>% 
        mutate_(Week = week) %>% 
        mutate(Game = str_replace_all(Game, "\t|\n", ""), 
            Field = str_replace(Field, "\t|\n*", " ")) %>% 
        mutate(TeamOne = str_extract(string = Game, pattern = "[:alpha:]*.*vs"), 
            TeamTwo = str_extract(Game, "vs.*")) %>% 
        mutate(TeamOneScore = str_extract(TeamOne, "(?<=\\[)\\d+"),
            TeamTwoScore = str_match(TeamTwo, "(?<=\\[)\\d+")[,1]) %>% 
        mutate(TeamOne = str_replace(TeamOne, "\\[.*$", ""), 
            TeamTwo = str_replace(TeamTwo, "\\[.*$", ""), 
            TeamTwo = str_replace(TeamTwo, "vs", "")) %>% 
        select(-Game)
}

process_tables <- function(tables) {
    lapply(1:(length(tables)-1), 
        function(i) process_table(html_table(tables[[i]]), i)) %>% 
    bind_rows()
}

process_years <- function(files) {
    lapply(files, function(file) {
        read_html(file) %>% 
            html_nodes("table") %>%
            process_tables() %>% 
            mutate(Year = str_extract(file, "\\d\\d\\d\\d"), 
                Season = str_extract(file, "-[:alpha:]*-")) %>% 
            mutate(Season = str_replace_all(Season, "-", ""), 
                Season = str_to_title(Season))
    }) %>% 
        bind_rows()
}

files <- data.frame(f = list.files("data/raw")) %>% 
    filter(str_detect(f, "schedule"), str_detect(f, ".html")) %>% 
    .$f %>% 
    paste("data/raw/", ., sep = "")

all <- files %>% 
    process_years()

# there are a variety of NAs, all either blank lines or games with
# no reported score (presumably forfeits)
all <- all %>% 
    filter(!is.na(TeamOneScore))

# rearrange columns
game_data <- all %>% 
    select(Year, Season, Week, Time, Field, everything())

write.csv(game_data, file = "data/game-data.csv", row.names = FALSE)
