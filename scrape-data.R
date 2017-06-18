library(rvest)
source("helpers.R")

# take an html_table containing a week's games and return it as a 
# useful data frame
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

# process a bunch of week's games and return a combined data frame
process_tables <- function(tables) {
    lapply(1:(length(tables)-1), 
        function(i) process_table(html_table(tables[[i]]), i)) %>% 
    bind_rows()
}

# process a whole season
process_years <- function(files) {
    lapply(files, function(file) {
        scores <- read_html(file) %>% 
            html_nodes("table") %>%
            process_tables() %>% 
            mutate(Year = str_extract(file, "\\d\\d\\d\\d"), 
                Season = str_extract(file, "-[:alpha:]*-")) %>% 
            mutate(Season = str_replace_all(Season, "-", ""), 
                Season = str_to_title(Season))
        weeks <- read_html(file) %>% 
            html_nodes("h2") %>% 
            str_extract("\".*\"") %>% 
            str_replace_all("\"", "") %>% 
            .[1:(length(.)-1)]
        scores$Date <- lubridate::mdy(weeks[scores$Week])
        scores$Date <- lubridate::ymd_hm(paste(scores$Date, scores$Time))
        scores
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

# clean up names
clean_names <- function(n) n %>% str_replace("- Indy.*", "") %>% str_trim() %>% str_to_title() %>% combine_duplicates()
combine_duplicates <- function(n) {
    # note: Son of Pitches and Sons of Pitches are *not* the same
    n %>% 
        # reddit team
        str_replace("\\/R\\/Seattle", "#Blessed") %>% 
        str_replace("Upvotes", "#Blessed") %>% 
        # oregon sucks
        str_replace("Oregon Sucks$", "Oregon Sucks!")
}

all <- all %>% 
    mutate(TeamOne = clean_names(TeamOne), TeamTwo = clean_names(TeamTwo)) %>% 
    mutate(Field = str_replace_all(Field, "\\t", ""), 
        Field = str_replace(Field, "\\n", " "), 
        Field = str_trim(Field)) 

# fix up dates
all <- all %>% 
    mutate(Season = factor(Season, levels = c("Spring", "Summer", "Fall"))) %>% 
    select(-Time)

# correctly type scores
all <- all %>% 
    mutate(TeamOneScore = as.numeric(TeamOneScore), 
        TeamTwoScore = as.numeric(TeamTwoScore))

# rearrange columns
all <- all %>% 
    select(Year, Season, Week, Date, Field, everything())

game_data <- all
write.csv(all, file = "data/game-data.csv", row.names = FALSE)
save(game_data, file = "data/game-data.Rda")