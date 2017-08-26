# Load helper functions and R packages. 
source("R/helpers.R")

# Rebuild the data from the source HTML files to ensure it's up to data. 
source("R/scrape-data.R")

# Update the models based on the latest game outcomes.
source("R/model.R")

# Run the scouting report analysis on the team for next week
rmarkdown::render("Scouting-report.Rmd")

# Run the scoring distribution analysis
rmarkdown::render("How-to-win-a-game.Rmd")
