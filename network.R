source("helpers.R")
combined <- get_tidy_game_data()

number_of_games <- 30

teams <- combined %>% 
    group_by(Team) %>% 
    count() %>% 
    arrange(desc(n)) %>% 
    filter(n >= number_of_games) %>% 
    .$Team

library(ggraph)
library(igraph)
games <- combined %>% 
    filter(Team %in% teams & Opponent %in% teams, Outcome != "Loss") %>% 
    mutate(Winner = ifelse(Outcome == "Win", Team, "Tie")) %>% 
    # ties get doubled bc includes row for each direction
    # so group by Game and only take the first row for each game
    # which will remove this problem
    group_by(Game) %>% 
    summarise_all(function(x) x[1])

team_summaries <- combined %>% 
    filter(Team %in% teams & Opponent %in% teams) %>% 
    group_by(Team) %>% 
    summarise(Winningness = mean(Outcome == "Win"))

attempt <- igraph::graph_from_data_frame(select(games, Team, Opponent, Outcome, Winner))
V(attempt)$Team <- names(V(attempt))
V(attempt)$Winningness <- team_summaries$Winningness[match(names(V(attempt)), team_summaries$Team)] * 100

 
ggraph(attempt, layout = 'kk') + 
    geom_edge_fan(aes(color = Winner), show.legend = TRUE) + 
    geom_node_point(aes(color = Team, size = Winningness)) + 
    scale_edge_color_brewer(type = "qual", palette = "Paired") + 
    scale_color_brewer(type = "qual", palette = "Paired", guide = FALSE) + 
    scale_size(limits = c(0, 100))

# brewer colors work (bc sequential) but ggplot auto colors are fucked
# because diff colors assigned depending on # of levels bc of tie level 
# in Winner but not in teams
ggraph(attempt, layout = 'linear', circular = TRUE) + 
    geom_edge_fan(aes(colour = Winner)) + 
    geom_node_point(aes(color = Team)) +
    coord_fixed()

# ties (or something) don't connect correctly with start and end cap args
ggraph(attempt, layout = 'linear', circular = TRUE) + 
    geom_edge_fan(aes(colour = Winner, start_cap = label_rect(node1.name),
        end_cap = label_rect(node2.name))) + 
    geom_node_text(aes(label = Team, color = Team)) +
    coord_fixed() + 
    scale_edge_color_brewer(type = "qual", palette = "Paired") + 
    scale_color_brewer(type = "qual", palette = "Paired", guide = FALSE)



# little spheres for nodes
# arrows for direction
V(attempt)$shape = "sphere"
V(attempt)$size = 15

plot(attempt)