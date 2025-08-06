# Integration script for adding Monte Carlo module to the main modular app
# This shows how to add the Sturman (2000) Monte Carlo module to your existing app

# 1. Source the Monte Carlo module
source("scripts/sturman_2000_monte_carlo.R")

# 2. Add to main UI (add this to your navbarPage)
# tabPanel("Monte Carlo Analysis", 
#   monteCarloUI("monte_carlo")
# )

# 3. Add to main server (add this inside your server function)
# monteCarloServer("monte_carlo")

# Example of updated main app structure:
if (FALSE) {  # Set to TRUE to see example
  
  ui <- navbarPage(
    "UA+ Complete Modular",
    
    tabPanel("Overview", 
      fluidPage(
        h2("Utility Analysis+ Complete App"),
        p("This complete modular version includes Compensation & Benefits, Training Utility, and Monte Carlo Analysis modules.")
      )
    ),
    
    tabPanel("Comp & Ben Utility", 
      sturmanUI("sturman")
    ),
    
    tabPanel("Training Utility", 
      trainingUI("training")
    ),
    
    # ADD THIS: Monte Carlo Analysis Tab
    tabPanel("Monte Carlo Analysis", 
      monteCarloUI("monte_carlo")
    )
  )
  
  server <- function(input, output, session) {
    # Existing module servers
    sturmanServer("sturman")
    trainingServer("training")
    
    # ADD THIS: Monte Carlo module server
    monteCarloServer("monte_carlo")
  }
  
  shinyApp(ui, server)
} 