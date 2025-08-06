library(shiny)
library(ggplot2)
library(plotly)

# Load Table 1 data (existing comprehensive database)
table1_correlations <- data.frame(
  entry = 1:60,
  variable1 = c(
    "Sugar consumption", "Aspirin consumption", "Antihypertensive medication", "Chemotherapy treatment", "Post-MI cardiac rehabilitation",
    "Alendronate medication", "Baseball batting average", "Aspirin and heparin", "Antibiotic treatment", "Calcium intake",
    "Coronary artery bypass surgery", "Ever smoking", "Gender (male vs female)", "Parental divorce", "Alcohol use during pregnancy",
    "Antihistamine use", "Combat exposure", "Low-level lead exposure", "Familial social support", "Media violence exposure",
    "Relapse prevention", "Nonsteroidal anti-inflammatory drugs", "Self-disclosure", "Post-high school grades", "Movie critic ratings",
    "Relating material to oneself", "Brain tissue destruction", "Nicotine patch use", "Criminal history", "Clozapine medication",
    "Employment interview scores", "Social support", "Parents' marital relationship", "Family/couples therapy", "Psychological treatments",
    "Alcohol consumption", "Positive parenting behavior", "Viagra side effects", "Gender (male vs female)", "Screening procedures",
    "Psychotherapy sessions", "ECT treatment", "Sleeping pill use", "Clinical depression", "Psychotherapy", "Gender (male vs female)",
    "Test reliability", "Elevation above sea level", "Viagra use", "Observer attractiveness ratings", "Past behavior",
    "Habitat loss", "Social conformity", "Gender (male vs female)", "Weight", "Parental attachment", "Age", "Gender (male vs female)",
    "Latitude (distance from equator)", "Gender (male vs female)"
  ),
  variable2 = c(
    "Children's behavior and cognitive processes", "Reduced risk of death by heart attack", "Reduced risk of stroke", "Surviving breast cancer", "Reduced death from cardiovascular complications",
    "Reduction in fractures", "Hit success in a particular at-bat", "Reduced MI or death", "Improvement in middle ear pain", "Bone mass",
    "Survival at 5 years", "Incidence of lung cancer", "Observed risk-taking behavior", "Child well-being and functioning", "Premature birth",
    "Reduced runny nose and sneezing", "Subsequent PTSD", "Reduced childhood IQ", "Lower blood pressure", "Subsequent interpersonal aggression",
    "Improvement in substance abusers", "Pain reduction", "Likability", "Job performance", "Box office success",
    "Improved memory", "Impaired learning behavior", "Smoking abstinence", "Subsequent recidivism", "Clinical improvement in schizophrenia",
    "Job success ratings", "Enhanced immune functioning", "Parent-child relationship", "Drug abuse treatment outcome", "Treatment effectiveness",
    "Aggressive behavior", "Child externalizing behavior problems", "Side effects of headache and flushing", "Body weight", "Job personnel selection validity",
    "Symptom improvement", "Subsequent improvement", "Insomnia improvement", "Suppressed immune functioning", "Subsequent well-being", "Self-reported assertiveness",
    "Construct validity coefficients", "Lower daily temperatures", "Improved male sexual functioning", "Attractiveness in romantic partnerships", "Future behavior",
    "Population decline", "Conformity behavior", "Self-reported empathy and nurturance", "Height", "Child's attachment quality", "Declining information processing speed", "Arm strength",
    "Daily temperature", "Height"
  ),
  variable1_desc = c(
    "Daily sugar intake in grams (measured through food diaries)",
    "Regular aspirin use (yes/no, measured through medication logs)",
    "Antihypertensive medication treatment (yes/no, medical records)",
    "Receiving chemotherapy treatment (yes/no, medical records)",
    "Post-myocardial infarction cardiac rehabilitation (yes/no, medical records)",
    "Alendronate medication for osteoporosis (yes/no, clinical trial)",
    "Season batting average as a professional baseball player",
    "Aspirin and heparin treatment for unstable angina (yes/no, clinical trial)",
    "Antibiotic treatment for acute middle ear pain (yes/no, medical records)",
    "Daily calcium intake in milligrams (measured through dietary assessment)",
    "Coronary artery bypass surgery (yes/no, medical records)",
    "History of smoking (yes/no, self-report)",
    "Biological sex (male/female, self-reported)",
    "Parental divorce during childhood (yes/no, family records)",
    "Alcohol consumption during pregnancy (yes/no, medical records)",
    "Taking antihistamine medication (yes/no, self-report)",
    "Combat exposure in Vietnam (yes/no, military records)",
    "Blood lead levels in micrograms per deciliter (medical lab)",
    "Extent of familial social support (measured through questionnaires)",
    "Exposure to media violence (measured through content analysis)",
    "Relapse prevention treatment (yes/no, clinical trial)",
    "Nonsteroidal anti-inflammatory drug use (yes/no, clinical trial)",
    "Level of self-disclosure in conversations (measured through observation)",
    "Post-high school grade point average (0.0-4.0 scale)",
    "Average rating from professional movie critics (1-10 scale)",
    "Relating material to oneself vs. others (experimental manipulation)",
    "Percentage of brain tissue destroyed in experimental animals",
    "Using nicotine patch treatment (yes/no, clinical trial)",
    "History of criminal convictions (yes/no, court records)",
    "Clozapine vs. conventional neuroleptics (yes/no, clinical trial)",
    "Structured interview scores rated by hiring managers",
    "Extent of social support network (measured through questionnaires)",
    "Quality of parents' marital relationship (measured through questionnaires)",
    "Family/couples therapy vs. alternative interventions (yes/no, clinical trial)",
    "General effectiveness of psychological treatments (meta-analysis)",
    "Alcohol consumption levels (measured through self-report)",
    "Positive parenting behaviors (rated by observers)",
    "Viagra side effects of headache and flushing (yes/no, clinical trial)",
    "Biological sex (male/female, self-reported)",
    "General validity of screening procedures for job selection (meta-analysis)",
    "Number of psychotherapy sessions attended",
    "ECT vs. simulated ECT treatment (yes/no, clinical trial)",
    "Taking prescription sleeping medication (yes/no)",
    "Clinical depression diagnosis (yes/no, clinical assessment)",
    "Psychotherapy treatment (yes/no, clinical trial)",
    "Biological sex (male/female, self-reported)",
    "Internal consistency reliability coefficient of psychological tests",
    "City elevation in feet above sea level",
    "Taking Viagra medication (yes/no, clinical trial)",
    "Observer ratings of attractiveness (1-10 scale)",
    "Previous behavior patterns (measured through observation)",
    "Loss in habitat size (measured in hectares)",
    "Social conformity under Asch line judgment task (experimental manipulation)",
    "Biological sex (male/female, self-reported)",
    "Body weight in pounds",
    "Parental reports of attachment to their parents (measured through questionnaires)",
    "Age in years (demographic variable)",
    "Biological sex (male/female, self-reported)",
    "Distance from equator in degrees latitude",
    "Biological sex (male/female, self-reported)"
  ),
  variable2_desc = c(
    "Children's behavior and cognitive processes (measured through standardized assessments)",
    "Reduced risk of death by heart attack (medical diagnosis)",
    "Reduced risk of stroke (medical diagnosis)",
    "5-year survival rate after breast cancer diagnosis",
    "Reduced death from cardiovascular complications (medical records)",
    "Reduction in fractures (medical diagnosis)",
    "Success in getting a hit in a specific at-bat",
    "Reduced myocardial infarction or death (medical diagnosis)",
    "Improvement in middle ear pain (self-reported)",
    "Bone mass density (measured through DEXA scan)",
    "Survival at 5 years (medical records)",
    "Incidence of lung cancer within 25 years (medical diagnosis)",
    "Observed risk-taking behavior (measured through experimental tasks)",
    "Child well-being and functioning (measured through standardized assessments)",
    "Premature birth (medical diagnosis)",
    "Reduction in allergy symptoms (self-reported improvement)",
    "Subsequent PTSD diagnosis (clinical assessment)",
    "Reduced childhood IQ scores (standardized intelligence test)",
    "Lower blood pressure readings (medical measurement)",
    "Subsequent naturally occurring interpersonal aggression (observed behavior)",
    "Improvement in substance abuse (clinical assessment)",
    "Pain reduction (self-reported improvement)",
    "Likability ratings (peer assessments)",
    "Job performance ratings after 1 year of employment",
    "Total box office revenue in millions of dollars",
    "Improved memory performance (experimental memory tests)",
    "Impaired learning behavior in monkeys (maze performance)",
    "Successfully quitting smoking for 6 months",
    "Subsequent recidivism (court records)",
    "Clinical improvement in schizophrenia (clinical assessment)",
    "Job success ratings after 1 year of employment",
    "Enhanced immune functioning (laboratory measures)",
    "Quality of parent-child relationship (measured through questionnaires)",
    "Drug abuse treatment outcome (clinical assessment)",
    "Treatment effectiveness (meta-analysis of outcomes)",
    "Aggressive behavior (observed in experimental settings)",
    "Child externalizing behavior problems (parent and teacher ratings)",
    "Side effects of headache and flushing (self-reported)",
    "Body weight in pounds",
    "Job personnel selection validity (meta-analysis)",
    "Improvement in psychological symptoms (clinical assessment)",
    "Subsequent improvement in depression (clinical assessment)",
    "Improvement in sleep quality (self-reported)",
    "Suppressed immune functioning (laboratory measures)",
    "Subsequent well-being (standardized well-being scales)",
    "Self-reported assertiveness (standardized personality scales)",
    "Construct validity coefficients (meta-analysis)",
    "Lower daily temperatures in degrees Fahrenheit",
    "Improved male sexual functioning (self-reported)",
    "Attractiveness ratings in romantic partnerships (peer assessments)",
    "Future behavior patterns (measured through observation)",
    "Population decline (ecological surveys)",
    "Conformity behavior (experimental manipulation)",
    "Self-reported empathy and nurturance (standardized personality scales)",
    "Height in inches",
    "Child's attachment quality (standardized attachment measures)",
    "Declining speed of information processing (cognitive tests)",
    "Arm strength measured in pounds of force",
    "Average daily temperature in degrees Fahrenheit",
    "Height in inches"
  ),
  correlation = c(0.00, 0.02, 0.03, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.08, 0.08, 0.08, 0.09, 0.09, 0.09, 0.11, 0.11, 0.12, 0.12, 0.13, 0.14, 0.14, 0.14, 0.16, 0.17, 0.17, 0.17, 0.18, 0.18, 0.20, 0.20, 0.21, 0.22, 0.23, 0.23, 0.23, 0.24, 0.25, 0.26, 0.27, 0.27, 0.29, 0.30, 0.32, 0.32, 0.32, 0.33, 0.34, 0.38, 0.39, 0.39, 0.40, 0.42, 0.42, 0.44, 0.47, 0.52, 0.55, 0.60, 0.67),
  sample_size = c(
    560, 22071, 59086, 9069, 4044, 1602, 0, 1353, 1843, 2493, 2649, 3956, 741, 94, 238, 1023, 2490, 3210, 12, 12, 26, 8488, 94, 13984, 15, 69, 283, 5098, 6475, 1850, 25244, 9, 253, 13, 9400, 47, 47, 861, 16950, 138, 56, 205, 680, 438, 375, 19546, 129, 19724, 779, 1299, 16, 2406, 4627, 19546, 16948, 854, 11044, 12392, 19724, 16962
  ),
  # K values for meta-analyses (number of studies contributing to the estimate)
  k_studies = c(
    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 94, NA, 15, 69, 283, NA, NA, NA, NA, 9, 253, 13, 9400, 47, 47, NA, NA, 138, 56, NA, NA, NA, 375, NA, NA, 129, NA, NA, NA, 16, NA, NA, NA, NA, NA, NA, NA, NA
  ),
  # Flag for meta-analyses (entries with K values)
  is_meta_analysis = c(
    FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE
  ),
  description = c(
    "Effect of sugar consumption on children's behavior and cognitive processes",
    "Aspirin and reduced risk of death by heart attack",
    "Antihypertensive medication and reduced risk of stroke",
    "Chemotherapy and surviving breast cancer",
    "Post-MI cardiac rehabilitation and reduced death from cardiovascular complications",
    "Alendronate and reduction in fractures in postmenopausal women with osteoporosis",
    "General batting skill as a Major League baseball player and hit success on a given instance at bat",
    "Aspirin and heparin (vs. aspirin alone) for unstable angina and reduced MI or death",
    "Antibiotic treatment of acute middle ear pain in children and improvement at 2-7 days",
    "Calcium intake and bone mass in premenopausal women",
    "Coronary artery bypass surgery for stable heart disease and survival at 5 years",
    "Ever smoking and subsequent incidence of lung cancer within 25 years",
    "Gender and observed risk-taking behavior (males are higher)",
    "Impact of parental divorce on problems with child well-being and functioning",
    "Alcohol use during pregnancy and subsequent premature birth",
    "Antihistamine use and reduced runny nose and sneezing",
    "Combat exposure in Vietnam and subsequent PTSD within 18 years",
    "Extent of low-level lead exposure and reduced childhood IQ",
    "Extent of familial social support and lower blood pressure",
    "Impact of media violence on subsequent naturally occurring interpersonal aggression",
    "Effect of relapse prevention on improvement in substance abusers",
    "Effect of nonsteroidal anti-inflammatory drugs (e.g., ibuprofen) on pain reduction",
    "Self-disclosure and likability",
    "Post-high school grades and job performance",
    "Prominent movie critics' ratings of 1998 films and U.S. box office success",
    "Relating material to oneself (vs. general 'others') and improved memory",
    "Extent of brain tissue destruction on impaired learning behavior in monkeys",
    "Nicotine patch (vs. placebo) and smoking abstinence at outcome",
    "Adult criminal history and subsequent recidivism among mentally disordered offenders",
    "Clozapine (vs. conventional neuroleptics) and clinical improvement in schizophrenia",
    "Validity of employment interviews for predicting job success",
    "Extent of social support and enhanced immune functioning",
    "Quality of parents' marital relationship and quality of parent-child relationship",
    "Family/couples therapy vs. alternative interventions and outcome of drug abuse treatment",
    "General effectiveness of psychological treatments (meta-analysis)",
    "Effect of alcohol on aggressive behavior",
    "Positive parenting behavior and lower rates of child externalizing behavior problems",
    "Viagra (oral sildenafil) and side effects of headache and flushing",
    "Gender and weight for U.S. adults (men are heavier)",
    "General validity of screening procedures for selecting job personnel: 1964-1992",
    "Effect of psychological therapy under clinically representative conditions",
    "ECT for depression (vs. simulated ECT) and subsequent improvement",
    "Sleeping pills (benzodiazepines or zolpidem) and short-term improvement in chronic insomnia",
    "Clinical depression and suppressed immune functioning",
    "Psychotherapy and subsequent well-being",
    "Gender and self-reported assertiveness (males are higher)",
    "Test reliability and the magnitude of construct validity coefficients",
    "Elevation above sea level and lower daily temperatures in the U.S.A.",
    "Viagra (oral sildenafil) and improved male sexual functioning",
    "Observer ratings of attractiveness for each member of a romantic partnership",
    "Past behavior as a predictor of future behavior",
    "Loss in habitat size and population decline for interior-dwelling species",
    "Social conformity under the Asch line judgment task",
    "Gender and self-reported empathy and nurturance (females are higher)",
    "Weight and height for U.S. adults",
    "Parental reports of attachment to their parents and quality of their child's attachment",
    "Increasing age and declining speed of information processing in adults",
    "Gender and arm strength for adults (men are stronger)",
    "Nearness to the equator and daily temperature in the U.S.A.",
    "Gender and height for U.S. adults (men are taller)"
  ),
  context = c(
    rep("This represents a very small effect size. Meyer et al. note that even small correlations can be meaningful in real-world research. Examples of similar effect sizes include: aspirin's effect on heart attack risk (r = 0.02), chemotherapy on breast cancer survival (r = 0.03), and sugar consumption on children's behavior (r = 0.00).", 15),
    rep("This represents a small to moderate effect size. Many important medical and psychological interventions produce correlations in this range. Examples of similar effect sizes include: antihistamine effectiveness (r = 0.11), college grades and job performance (r = 0.16), and movie critics' reviews and box office success (r = 0.17).", 25),
    rep("This represents a moderate effect size. Correlations of this magnitude are considered quite substantial in psychological research. Examples of similar effect sizes include: gender and arm strength (r = 0.55), gender and height (r = 0.67), and latitude and daily temperature (r = 0.60).", 15),
    rep("This represents a large effect size. Correlations of this magnitude are relatively rare in psychological research. Examples of similar effect sizes include: gender and arm strength (r = 0.55), gender and height (r = 0.67), and latitude and daily temperature (r = 0.60).", 5)
  )
)

# Load Table 2 data (new psychological and medical test validity coefficients)
table2_correlations <- read.csv("correlation_data_table2_20250703.csv", stringsAsFactors = FALSE)

# Rename columns in Table 2 to match Table 1 structure
names(table2_correlations)[names(table2_correlations) == "description"] <- "description"
names(table2_correlations)[names(table2_correlations) == "correlation"] <- "correlation"
names(table2_correlations)[names(table2_correlations) == "sample_size"] <- "sample_size"

# Add missing columns to Table 2 to match Table 1 structure
table2_correlations$entry <- 1:nrow(table2_correlations)
table2_correlations$variable1 <- table2_correlations$description
table2_correlations$variable2 <- table2_correlations$description
table2_correlations$variable1_desc <- table2_correlations$description
table2_correlations$variable2_desc <- table2_correlations$description
table2_correlations$k_studies <- NA
table2_correlations$is_meta_analysis <- FALSE
table2_correlations$context <- "This represents a test validity coefficient from Meyer et al. (2001) Table 2. These correlations show the strength of relationship between psychological and medical tests and their criterion measures."

# Select only the columns that exist in both datasets for combining
table1_cols <- c("entry", "variable1", "variable2", "variable1_desc", "variable2_desc", 
                 "correlation", "sample_size", "k_studies", "is_meta_analysis", "description", "context")
table2_cols <- c("entry", "variable1", "variable2", "variable1_desc", "variable2_desc", 
                 "correlation", "sample_size", "k_studies", "is_meta_analysis", "description", "context")

# Combine datasets with source identification
all_correlations <- rbind(
  cbind(table1_correlations[, table1_cols], source = "Table 1"),
  cbind(table2_correlations[, table2_cols], source = "Table 2")
)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .btn-primary { background-color: #007bff; border-color: #007bff; }
      .btn-success { background-color: #28a745; border-color: #28a745; }
      .btn-info { background-color: #17a2b8; border-color: #17a2b8; }
      .btn-secondary { background-color: #6c757d; border-color: #6c757d; }
    ")),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('showConsentModal', function(message) {
        document.getElementById('consent_modal').style.display = 'block';
      });
      
      Shiny.addCustomMessageHandler('hideConsentModal', function(message) {
        document.getElementById('consent_modal').style.display = 'none';
      });
    "))
  ),
  
  titlePanel("Interactive Correlation Learning Game"),
  subtitle = "Based on Meyer et al. (2001) Table 1 - Create plots and test your correlation intuition",
  
  # GDPR Consent Modal
  tags$div(
    id = "consent_modal",
    style = "display: none; position: fixed; z-index: 1000; left: 0; top: 0; width: 100%; height: 100%; background-color: rgba(0,0,0,0.5);",
    tags$div(
      style = "background-color: white; margin: 15% auto; padding: 20px; border-radius: 10px; width: 80%; max-width: 600px;",
      h3("Data Collection Consent"),
      p("This app may collect anonymous data about correlation guessing performance for educational research purposes."),
      p("Data collected includes:"),
      tags$ul(
        tags$li("Your correlation guesses"),
        tags$li("Correct correlation values"),
        tags$li("Error rates and accuracy"),
        tags$li("Session duration"),
        tags$li("No personal information (name, email, IP address)")
      ),
      p("All data is anonymous and will only be used for educational research to understand how people estimate correlations."),
      p("You can play the game without consenting to data collection."),
      tags$div(
        style = "text-align: center; margin-top: 20px;",
        actionButton("consent_yes", "I consent to data collection", class = "btn-primary", style = "margin-right: 10px;"),
        actionButton("consent_no", "Play without data collection", class = "btn-secondary")
      )
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 4,
      
      h3("What do you think is the correlation between:"),
      uiOutput("variable_question"),
      br(),
      
      sliderInput("user_correlation", "Your correlation guess (r):", value = 0, min = -1, max = 1, step = 0.01),
      
      checkboxInput("show_trendline", "Show Trendline", value = FALSE),
      
      actionButton("generate_plot", "Generate Plot", class = "btn-info"),
      br(), br(),
      
      conditionalPanel(
        condition = "input.generate_plot > 0",
        actionButton("submit_guess", "Submit for Feedback", class = "btn-primary"),
        br(), br()
      ),
      
      conditionalPanel(
        condition = "input.submit_guess > 0",
        h4("Feedback"),
        textOutput("feedback_text"),
        br(),
        actionButton("next_question", "Next Question", class = "btn-success")
      ),
      
      br()
    ),
    
    mainPanel(
      width = 8,
      
      tabsetPanel(
        tabPanel(
          "How to Play",
          h3("How to Play"),
          p("1. Read the variable names and think about what relationship you expect"),
          p("2. Enter your correlation coefficient (r) in the sidebar"),
          p("3. Click 'Generate Plot' to see your prediction visualized"),
          p("4. Adjust your guess if needed, then submit for feedback"),
          p("5. See how close you were to the real correlation from research!")
        ),
        tabPanel(
          "Understanding Correlation Coefficients",
          h3("Understanding Correlation Coefficients"),
          p("Correlation coefficients (r) range from -1 to +1:"),
          tags$ul(
            tags$li("r = 0: No linear relationship"),
            tags$li("r = ±0.1: Weak relationship"),
            tags$li("r = ±0.3: Moderate relationship"), 
            tags$li("r = ±0.5: Strong relationship"),
            tags$li("r = ±0.7: Very strong relationship"),
            tags$li("r = ±1.0: Perfect linear relationship")
          ),
          p("Note: The scatter plots show standardized variables (z-scores), so the scale is in standard deviation units.")
        )
      ),
      br(),
      plotlyOutput("scatter_plot", height = "500px"),
      
      # Move statistics and progress to main panel
      conditionalPanel(
        condition = "input.submit_guess > 0",
        br(),
        wellPanel(
          h4("Your Statistics"),
          textOutput("game_stats"),
          br(),
          downloadButton("download_data", "Download Session Data (CSV)", class = "btn-info")
        ),
        br(),
        h4("Your Progress"),
        plotlyOutput("progress_plot", height = "300px")
      ),
      
      # Footer with attributions
      tags$div(
        style = "margin-top: 30px; padding: 20px; background-color: #f8f9fa; border-radius: 5px; text-align: center;",
        p("This app is inspired by Daniel Läkens' 'Guess the Correlation' game", style = "font-style: italic;"),
        p("Built with R Shiny for educational purposes", style = "font-size: 12px; color: #666;")
      )
    )
  )
)

server <- function(input, output, session) {
  
  game_state <- reactiveValues(
    current_question = 1,
    current_question_index = 1,
    correct_guesses = 0,
    total_error = 0,
    questions_answered = 0,
    current_correlation = 0,
    current_sample_size = 100,
    current_description = "",
    current_variable1 = "",
    current_variable2 = "",
    current_variable1_desc = "",
    current_variable2_desc = "",
    current_context = "",
    current_k_studies = NA,
    current_is_meta_analysis = FALSE,
    guess_history = data.frame(question = integer(), guess = numeric(), correct = numeric(), error = numeric()),
    guess_submitted = FALSE,
    user_correlation_guess = 0,
    plot_generated = FALSE,
    session_id = paste0("session_", format(Sys.time(), "%Y%m%d_%H%M%S"), "_", sample(1000:9999, 1)),
    consent_given = FALSE
  )
  
  # Show consent modal on app start
  observe({
    if (!game_state$consent_given) {
      session$sendCustomMessage("showConsentModal", list())
    }
  })
  
  # Handle consent responses
  observeEvent(input$consent_yes, {
    game_state$consent_given <- TRUE
    session$sendCustomMessage("hideConsentModal", list())
    start_new_game()
  })
  
  observeEvent(input$consent_no, {
    game_state$consent_given <- FALSE
    session$sendCustomMessage("hideConsentModal", list())
    start_new_game()
  })
  

  
  # Data collection function
  save_guess_data <- function(user_guess, correct_correlation, error, question_data) {
    if (game_state$consent_given) {
      data_entry <- data.frame(
        timestamp = Sys.time(),
        session_id = game_state$session_id,
        question_number = game_state$current_question,
        variable1 = question_data$variable1,
        variable2 = question_data$variable2,
        user_guess = user_guess,
        correct_correlation = correct_correlation,
        error = error,
        sample_size = question_data$sample_size,
        k_studies = question_data$k_studies,
        is_meta_analysis = question_data$is_meta_analysis
      )
      
      # Save to file (GDPR compliant - no personal data)
      filename <- paste0("correlation_data_", format(Sys.Date(), "%Y%m%d"), ".csv")
      
      # Check if file exists to determine whether to write headers
      file_exists <- file.exists(filename)
      
      write.table(data_entry, file = filename, append = TRUE, sep = ",", 
                  col.names = !file_exists, row.names = FALSE)
    }
  }
  
  observe({
    if (game_state$current_question == 1 && game_state$consent_given != FALSE) {
      # Only start game after consent is handled
    }
  })
  
  # Use Table 1 as the default dataset
  current_dataset <- reactive({
    return(all_correlations[all_correlations$source == "Table 1", ])
  })
  
  start_new_game <- function() {
    dataset <- current_dataset()
    random_index <- sample(1:nrow(dataset), 1)
    game_state$current_question_index <- random_index
    setup_question(random_index, dataset)
  }
  
  setup_question <- function(question_index, dataset = NULL) {
    cat("Setting up question with index:", question_index, "\n")
    
    if (is.null(dataset)) {
      dataset <- current_dataset()
    }
    
    current_row <- dataset[question_index, ]
    game_state$current_question_index <- question_index
    game_state$current_correlation <- current_row$correlation
    game_state$current_sample_size <- current_row$sample_size
    game_state$current_description <- current_row$description
    game_state$current_variable1 <- current_row$variable1
    game_state$current_variable2 <- current_row$variable2
    game_state$current_variable1_desc <- current_row$variable1_desc
    game_state$current_variable2_desc <- current_row$variable2_desc
    game_state$current_context <- current_row$context
    game_state$current_k_studies <- current_row$k_studies
    game_state$current_is_meta_analysis <- current_row$is_meta_analysis
    game_state$guess_submitted <- FALSE
    game_state$plot_generated <- FALSE
    
    # For meta-analyses (entries with K values), use default sample size of 500
    if (!is.null(current_row$is_meta_analysis) && current_row$is_meta_analysis) {
      game_state$current_sample_size <- 500
    }
    
    cat("Question setup complete. Sample size:", game_state$current_sample_size, "Correlation:", game_state$current_correlation, "\n")
    
    updateSliderInput(session, "user_correlation", value = 0)
  }
  
  plot_data <- reactive({
    req(input$user_correlation)
    
    # Use current_question_index for consistent seeding
    set.seed(42 + game_state$current_question_index)
    n <- game_state$current_sample_size
    
    # Ensure n is valid
    if (is.na(n) || n <= 0) {
      n <- 100  # fallback sample size
    }
    
    z1 <- rnorm(n, 0, 1)
    z2 <- rnorm(n, 0, 1)
    
    rho <- input$user_correlation
    
    x <- z1
    y <- rho * z1 + sqrt(1 - rho^2) * z2
    
    data.frame(x = x, y = y)
  })
  
  output$variable_question <- renderUI({
    # Check if this is Table 2 data (which has different structure)
    if (!is.null(game_state$current_variable1) && game_state$current_variable1 != "") {
      # Table 1 format
      question_text <- paste0(
        "X: ", game_state$current_variable1, "<br>",
        "<small><em>", game_state$current_variable1_desc, "</em></small><br><br>",
        "Y: ", game_state$current_variable2, "<br>",
        "<small><em>", game_state$current_variable2_desc, "</em></small>"
      )
    } else {
      # Table 2 format - use description field
      question_text <- paste0(
        "<strong>", game_state$current_description, "</strong>"
      )
    }
    
    # Add footnote for meta-analyses
    if (!is.null(game_state$current_is_meta_analysis) && game_state$current_is_meta_analysis && !is.na(game_state$current_k_studies)) {
      question_text <- paste0(
        question_text,
        "<br><br><small><strong>Note:</strong> This is a meta-analysis based on ", 
        game_state$current_k_studies, " studies. The sample size of 500 is used as a placeholder because Meyer et al. did not provide the total sample size across all studies.</small>"
      )
    }
    
    HTML(question_text)
  })
  
  # Create initial plot
  output$scatter_plot <- renderPlotly({
    suppressWarnings({
      if (!game_state$plot_generated) {
        plot_ly() %>%
          add_annotations(
            text = "Enter your correlation guess and click 'Generate Plot' to see your prediction",
            xref = "paper", yref = "paper",
            x = 0.5, y = 0.5,
            showarrow = FALSE,
            font = list(size = 16)
          ) %>%
          layout(
            title = "Your Correlation Prediction",
            xaxis = list(title = "X (Standardized)", range = c(-3, 3)),
            yaxis = list(title = "Y (Standardized)", range = c(-3, 3)),
            showlegend = FALSE
          ) %>%
          config(displayModeBar = FALSE)
      } else {
        # Add reactive dependency on the slider and ensure it's available
        req(input$user_correlation)
        
        # Debug: print to console
        cat("Generating plot with correlation:", input$user_correlation, "and sample size:", game_state$current_sample_size, "\n")
        
        tryCatch({
          data <- plot_data()
          
          p <- plot_ly(
            data = data,
            x = ~x, 
            y = ~y,
            type = 'scatter',
            mode = 'markers',
            marker = list(size = 8, color = 'steelblue', opacity = 0.7),
            name = "Data Points",
            hovertemplate = paste(
              ifelse(!is.null(game_state$current_variable1) && game_state$current_variable1 != "", 
                     paste0(game_state$current_variable1, ": %{x:.2f}<br>"), 
                     "Predictor: %{x:.2f}<br>"),
              ifelse(!is.null(game_state$current_variable2) && game_state$current_variable2 != "", 
                     paste0(game_state$current_variable2, ": %{y:.2f}<br>"), 
                     "Criterion: %{y:.2f}<br>"),
              "Sample size: ", game_state$current_sample_size, "<br>",
              "<extra></extra>"
            )
          ) %>%
            layout(
              title = paste0("Your Prediction: r = ", input$user_correlation, " (n = ", game_state$current_sample_size, 
                            ifelse(game_state$current_is_meta_analysis, paste0(", K = ", game_state$current_k_studies, " studies"), ""), ")"),
              xaxis = list(title = ifelse(!is.null(game_state$current_variable1) && game_state$current_variable1 != "", 
                                         paste0(game_state$current_variable1, " (Standardized)"), 
                                         "Predictor (Standardized)"), range = c(-3, 3)),
              yaxis = list(title = ifelse(!is.null(game_state$current_variable2) && game_state$current_variable2 != "", 
                                         paste0(game_state$current_variable2, " (Standardized)"), 
                                         "Criterion (Standardized)"), range = c(-3, 3)),
              showlegend = TRUE
            ) %>%
            config(displayModeBar = FALSE)
          
          if (input$show_trendline) {
            # Use full plot range for consistency with true correlation line
            x_full_range <- seq(-3, 3, length.out = 100)
            y_guess <- input$user_correlation * x_full_range
            
            p <- p %>% add_trace(
              x = x_full_range,
              y = y_guess,
              type = 'scatter',
              mode = 'lines',
              line = list(color = 'black', width = 8, shape = 'linear'),
              name = paste0('Your prediction (r = ', input$user_correlation, ')'),
              showlegend = TRUE,
              hoverinfo = 'skip'
            )
          }
          
          if (game_state$guess_submitted) {
            # Use full plot range for true correlation line
            x_full_range <- seq(-3, 3, length.out = 100)
            y_true <- game_state$current_correlation * x_full_range
            
            # Add the true correlation line with much more visible styling
            p <- p %>% add_trace(
              x = x_full_range,
              y = y_true,
              type = 'scatter',
              mode = 'lines',
              line = list(color = 'limegreen', width = 8, dash = 'solid', shape = 'linear'),
              name = paste0('True correlation (r = ', round(game_state$current_correlation, 3), ')'),
              showlegend = TRUE,
              hoverinfo = 'skip'
            )
          }
          
          return(p)
        }, error = function(e) {
          cat("Error generating plot:", e$message, "\n")
          # Return a simple error plot
          plot_ly() %>%
            add_annotations(
              text = paste("Error generating plot:", e$message),
              xref = "paper", yref = "paper",
              x = 0.5, y = 0.5,
              showarrow = FALSE,
              font = list(size = 16, color = "red")
            ) %>%
            layout(
              title = "Error",
              xaxis = list(title = "X (Standardized)", range = c(-3, 3)),
              yaxis = list(title = "Y (Standardized)", range = c(-3, 3)),
              showlegend = FALSE
            ) %>%
            config(displayModeBar = FALSE)
        })
      }
    })
  })
  
  # Use plotlyProxy for smooth updates
  observeEvent(input$user_correlation, {
    if (game_state$plot_generated) {
      plotlyProxy("scatter_plot", session) %>%
        plotlyProxyInvoke("restyle", 
          list(
            x = list(plot_data()$x),
            y = list(plot_data()$y)
          ),
          list(0)  # Update the first trace (data points)
        ) %>%
        plotlyProxyInvoke("relayout", 
          list(
            title = paste0("Your Prediction: r = ", input$user_correlation, " (n = ", game_state$current_sample_size, 
                          ifelse(game_state$current_is_meta_analysis, paste0(", K = ", game_state$current_k_studies, " studies"), ""), ")")
          )
        )
      
      # Update trend line if shown
      if (input$show_trendline) {
        x_full_range <- seq(-3, 3, length.out = 100)
        y_guess <- input$user_correlation * x_full_range
        
        plotlyProxy("scatter_plot", session) %>%
          plotlyProxyInvoke("restyle", 
            list(
              x = list(x_full_range),
              y = list(y_guess)
            ),
            list(1)  # Update the second trace (trend line)
          )
      }
    }
  })
  
  observeEvent(input$generate_plot, {
    cat("Generate Plot button clicked!\n")
    game_state$plot_generated <- TRUE
    cat("plot_generated set to:", game_state$plot_generated, "\n")
  })
  
  observeEvent(input$submit_guess, {
    if (!is.null(input$user_correlation)) {
      user_correlation_guess <- as.numeric(input$user_correlation)
      correct_correlation <- game_state$current_correlation
      
      game_state$user_correlation_guess <- user_correlation_guess
      game_state$guess_submitted <- TRUE
      
      correlation_close <- abs(user_correlation_guess - correct_correlation) < 0.05
      is_correct <- correlation_close
      
      if (is_correct) {
        game_state$correct_guesses <- game_state$correct_guesses + 1
      }
      
      error <- abs(user_correlation_guess - correct_correlation)
      game_state$total_error <- game_state$total_error + error
      game_state$questions_answered <- game_state$questions_answered + 1
      
      new_row <- data.frame(
        question = game_state$current_question,
        guess = user_correlation_guess,
        correct = correct_correlation,
        error = error
      )
      game_state$guess_history <- rbind(game_state$guess_history, new_row)
      
      # Save data if consent given
      current_question_data <- all_correlations[game_state$current_question_index, ]
      save_guess_data(user_correlation_guess, correct_correlation, error, current_question_data)
      
      feedback <- paste0(
        "Your guess: r = ", user_correlation_guess, "\n",
        "Correct answer: r = ", correct_correlation, "\n",
        "Error: ", round(error, 3)
      )
      
      output$feedback_text <- renderText(feedback)
    }
  })
  
  observeEvent(input$next_question, {
    game_state$current_question <- game_state$current_question + 1
    dataset <- current_dataset()
    random_index <- sample(1:nrow(dataset), 1)
    game_state$current_question_index <- random_index
    setup_question(random_index, dataset)
    
    output$feedback_text <- renderText("")
    game_state$plot_generated <- FALSE
  })
  
  output$game_stats <- renderText({
    if (game_state$questions_answered > 0) {
      accuracy <- round(game_state$correct_guesses / game_state$questions_answered * 100, 1)
      avg_error <- round(game_state$total_error / game_state$questions_answered, 3)
      
      paste0(
        "Questions answered: ", game_state$questions_answered, "\n",
        "Accuracy: ", accuracy, "%\n",
        "Average Error: ", avg_error
      )
    }
  })
  
  output$progress_plot <- renderPlotly({
    suppressWarnings({
      if (nrow(game_state$guess_history) > 0) {
        p <- plot_ly(
          data = game_state$guess_history,
          x = ~question,
          y = ~guess,
          type = 'scatter',
          mode = 'lines+markers',
          name = 'Your Guesses',
          line = list(color = 'blue'),
          marker = list(size = 8)
        ) %>%
          add_trace(
            y = ~correct,
            name = 'Correct Values',
            line = list(color = 'red', dash = 'dash'),
            mode = 'lines+markers',
            marker = list(size = 8)
          ) %>%
          layout(
            title = "Your Correlation Guesses vs. Correct Values",
            xaxis = list(title = "Question Number", tickmode = 'linear', tick0 = 1, dtick = 1, showticklabels = TRUE),
            yaxis = list(title = "Correlation Coefficient (r)", range = c(-1, 1), tickmode = 'linear', tick0 = -1, dtick = 0.2),
            showlegend = TRUE
          ) %>%
          config(displayModeBar = FALSE)
        
        return(p)
      }
    })
  })
  
  # Download handler for session data
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("correlation_game_session_", game_state$session_id, ".csv")
    },
    content = function(file) {
      if (nrow(game_state$guess_history) > 0) {
        session_data <- game_state$guess_history
        session_data$session_id <- game_state$session_id
        session_data$timestamp <- Sys.time()
        write.csv(session_data, file, row.names = FALSE)
      } else {
        # Create empty file with headers
        empty_data <- data.frame(
          question = integer(),
          guess = numeric(),
          correct = numeric(),
          error = numeric(),
          session_id = character(),
          timestamp = as.POSIXct(character())
        )
        write.csv(empty_data, file, row.names = FALSE)
      }
    }
  )
}

shinyApp(ui = ui, server = server) 