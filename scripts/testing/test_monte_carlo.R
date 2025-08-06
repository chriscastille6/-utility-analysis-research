# Test script for Sturman (2000) Monte Carlo Module
# This script tests the module as a standalone app

# Source the module
source("scripts/sturman_2000_monte_carlo.R")

# Create UI and server
ui <- monteCarloUI("monte_carlo")

server <- function(input, output, session) {
  monteCarloServer("monte_carlo")
}

# Run the app
shinyApp(ui, server) 