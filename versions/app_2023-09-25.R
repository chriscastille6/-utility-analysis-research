#Load Libraries
library(shiny)
library(shinydashboard)
library(iopsych)
library(scales)
library(shinyjs)
library(mvtnorm)
library(ggplot2)
library(dplyr)
library(stringr)
library(grid)
library(gridExtra)
library(gridtext)

#Define Custom Functions
Expectancyfunc <- function(Validity, PredLowerCut, PredUpperCut, CritLowerCut, CritUpperCut){
  n <- 1000
  mean <- c(0,0)
  lower <- c(PredLowerCut, CritLowerCut)
  upper <- c(PredUpperCut, CritUpperCut)
  corr <- diag(2)
  corr[lower.tri(corr)] <- Validity
  corr[upper.tri(corr)] <- Validity
  jtprob <- pmvnorm(lower, upper, mean, corr, algorithm = Miwa(steps=128))
  xprob <- pnorm(PredUpperCut, mean = 0, sd = 1) - pnorm(PredLowerCut, mean = 0, sd = 1)
  expectancy <- jtprob/xprob
  return(expectancy[1])
}

#References
reference_content <- HTML(
  ' <div style="padding-left: 30px; text-indent: -30px;">
    <p>Alexander III, L., Mulfinger, E., & Oswald, F. L. (2019). Investing in People Online (Version 2.0) [Software], Rice University, Houston, Texas. Available from <a  href="https://orgtools.shinyapps.io/IIP3/">https://orgtools.shinyapps.io/IIP3/</a><p>
    <p>Avolio, B. J., Avey, J. B., & Quisenberry, D. (2010). Estimating return on leadership development investment. The Leadership Quarterly, 21(4), 633–644. <a href="https://doi.org/10.1016/j.leaqua.2010.06.006">https://doi.org/10.1016/j.leaqua.2010.06.006</a><p>
    <p>Carson, K. P., Becker, J. S., & Henderson, J. A. (1998). Is utility really futile? A failure to replicate and an extension. Journal of Applied Psychology, 83(1), 84–96. <a href="https://doi.org/10.1037/0021-9010.83.1.84">https://doi.org/10.1037/0021-9010.83.1.84</a><p>
    <p>Cucina, Jeffrey M.; Berger, Julia L.; and Busciglio, Henry H. (2017) "Communicating Criterion-Related Validity Using Expectancy Charts: A New Approach," Personnel Assessment and Decisions: Vol. 3 : Iss. 1 , Article 1. DOI: 10.25035/pad.2017.001 Available at: <a href="http://scholarworks.bgsu.edu/pad/vol3/iss1/1">http://scholarworks.bgsu.edu/pad/vol3/iss1/1</a><p>
    <p>Magnusson, K. (2023, June 9). A Causal Inference Perspective on Therapist Effects. <a href="https://doi.org/10.31234/osf.io/f7mvz">https://doi.org/10.31234/osf.io/f7mvz</a><p>
    <p>Schmidt, F. L. (2013). The economic value of goal setting to employers. In E. A. Locke & G. P. Latham (Eds.), New developments in goal setting and task performance (pp. 16–20). Routledge/Taylor & Francis Group.<p>
    <p>Sturman, M. C. (2000). Implications of utility analysis adjustments for estimates of human resource intervention value [Electronic version]. Retrieved September 9, 2023, from Cornell University, School of Hospitality Administration site: <a href="http://scholarship.sha.cornell.edu/articles/117">http://scholarship.sha.cornell.edu/articles/117</a></p>
    </div>'
)

# Opening UI
opening_ui <- fluidPage(
  useShinyjs(),
  titlePanel("Welcome to the Utility Analysis App"),
  mainPanel(
    h4("Introduction"),
    p("Thank you for using the Utility Analysis App. You can navigate within the app using the above tabs. The Staffing Utility
        tab has multiple tools available for calculating returns on staffing procedures as well as producing reports and visuals.
        The default values used for computing utility are obtained from Sturman's \"Implications of Utility Analysis Adjustments for
        Estimates of Human Resource Intervention Value.\" You can compare the output from the Utility app to the result found there to
        ensure the app is working correctly. To generate a plain text description, you must first enter the required information into both
        the Staffing Utility section and Utility Adjustments section."),
    p("The Glossary tab includes helpful definitions to better understand the purpose of this app. The References tab includes all references
        used in the creation of this app.")
  )
)

#Reference UI
reference_ui <- fluidPage(
  titlePanel("References"),
  mainPanel(
    reference_content
  )
)

#Glossary UI
glossary_ui <- fluidPage(
  tags$style(HTML(".size {font-size: 16px}")),
  titlePanel("Glossary"),
  mainPanel(
    p(HTML("<strong>Break-Even SDy</strong> - Minimum possible value of SDy to recoup losses from conducting a staffing/training program."), class = "size"),
    p(HTML("<strong>Discount Rate</strong> - Rate of accrual of monetary investments from production revenues."), class = "size"),
    p(HTML("<strong>Effect Size</strong> - Standard Deviation of the performance difference between a treated group and an untreated group."), class = "size"),
    p(HTML("<strong>Expectancy</strong> - A measure of the ability of a test to predict outcomes. For example, a test with high validity has a high expectancy of predicting high performance based on a high test score."), class = "size"),
    p(HTML("<strong>SDy</strong> - Monetary value of job performance. This value indicates the difference between an average worker (50th percentile) and a worker with high production (85th percentile). Methods for measuring SDy include the 40% rule, Global Estimation, and CREPID among others."), class = "size"),
    p(HTML("<strong>Selection ratio</strong> - Ratio of the number of employees to be hired from an applicant pool."), class = "size"),
    p(HTML("<strong>Tax Rate</strong> - Company tax rate."), class = "size"),
    p(HTML("<strong>Utility</strong> - A measure of the return on investment in workers. Utility compares the quality and quantity of a treated group of employees to the cost of the treatment. This returns an estimation of the economic value that these employees add to the company."), class = "size"),
    p(HTML("<strong>Validity</strong> - Correlation between selection test performance and actual job performance. A validity of 0 indicates no relationship, while a validity of 1.00 indicates a perfect relationship. Validity will almost always be below .50 (Carson et. al. (1998))."), class = "size"),
    p(HTML("<strong>Variable Costs</strong> - Costs that rise with greater production. For example, material costs, incentives, commission, variable overhead, etc."), class = "size")
  )
)

#Staffing UI
main_ui <- dashboardPage(
  dashboardHeader(title = "Staffing Utility"),
  
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar",
      menuItem("Staffing Utility", tabName = "staffing_utility", icon = icon("file-alt")),
      menuItem("Expectancy", tabName = "expectancy", icon = icon("dashboard")),
      menuItem("Utility Output", tabName = "utility_adjustments", icon = icon("sliders-h"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "staffing_utility",
              fluidPage(
                titlePanel("Staffing Utility"),
                mainPanel(
                  p("Staffing utility description")
                )
              )
      ),
      tabItem(tabName = "expectancy",
              fluidPage(
                useShinyjs(),
                titlePanel("Expectancy of High Job Performance"),
                sidebarLayout(
                  sidebarPanel(
                    numericInput("n", "Number of hires:", 470, min = 1, step = 1),
                    numericInput("sdy", "SD of performance in monetary units:", 16290, min = 0.01, step = 1),
                    numericInput("rxy1", "Validity of old procedure:", 0, min = -1, max = 1, step = 0.01),
                    numericInput("rxy2", "Validity of new procedure:", 0.4, min = -1, max = 1, step = 0.01),
                    numericInput("sr", "Selection ratio:", 0.33, min = 0, max = 1, step = 0.01),
                    numericInput("cost1", "Cost per applicant of old procedure:", 0, min = 0.01, step = 1),
                    numericInput("cost2", "Cost per applicant of new procedure:", 304.33, min = 0.01, step = 1),
                    numericInput("period", "Anticipated tenure of selected employees:", 18, min = 0.01, step = 1),
                    actionButton("go", "Compute Expectancy")
                  ),
                  
                  mainPanel(
                    #Expectancy Chart
                    plotOutput("utility_graph"),
                    textOutput("expectancy_text"),
                    downloadButton("download_plot", "Export PDF")
                  )
                )
              )
      ),
      tabItem(tabName = "utility_adjustments",
              fluidPage(
                useShinyjs(),
                titlePanel("Predicting Returns on Improved Staffing Procedures"),
                sidebarLayout(
                  sidebarPanel(
                    HTML('<h4>Economic Factors</h4>'),
                    numericInput("vcost", "Variable Costs (%):", 2, min = 0, max = 100, step = 1),
                    numericInput("tax", "Tax Rate (%):", 30, min = 0, max = 100, step = 1),
                    numericInput("drate", "Discount Rate (%):", 1, min = 0, max = 100, step = 1),
                    HTML('<h4>Workflow</h4>'),
                    numericInput("pyears" , "Program Length" , 15, min = 1, step = 1),
                    numericInput("nadd" , "Employees Added per Year:" , 618, min = 0, step = 1),
                    numericInput("nsub" , "Employees Lost per Year(After Tenure):", 618, min = 0, step = 1),
                    actionButton("go2", "Compute Adjustments")
                  ),
                  mainPanel(
                    textOutput("total_utility"),
                    textOutput("break_even_SDy"),
                    textOutput("adjusted_utility"),
                    textOutput("adjusted_utility_perHire"),
                    textOutput("adjusted_utility_perHire_perYear"),
                    br(),
                    textOutput("plainText1"),
                    br(),
                    textOutput("plainText2"),
                    br(),
                    textOutput("plainText3"),
                    downloadButton("download_pdf", "Export Report")
                  )
                )
              )
      )
    )
  )
)

#Training UI
training_ui <- dashboardPage(
  dashboardHeader(title = "Training Utility"),
  
  dashboardSidebar(
    sidebarMenu(
      id = "sidebarTrain",
      menuItem("Training Utility", tabName = "training_utility", icon = icon("file-lines")),
      menuItem("Effect Size", tabName = "effect_size", icon = icon("chart-simple")),
      menuItem("Utility Outputs", tabName = "utility_outputs", icon = icon("sliders-h"))
    )
  ),
  
  dashboardBody(
  
    tabItems(
      tabItem(tabName = "training_utility",
              fluidPage(
                titlePanel("Training Utility"),
                p("This is the training utility section of the UA+ app. Training utility can be described as a return on investment in the development of workers (Avolio et al., 2010). 
                  This return on develpoment investment (RODI) is calculated using the effect size of the training process. In the panel on the left, there is a tab for effect size that gives
                  a descriptive analysis of the effect size that you choose to input. This consists of a modified graph from Alexander et al's Investing in People Online app (Alexander et al., 2019),
                  as well as a modified version of Magnusson's description of effect size (Magnusson, 2023). This tool is useful for displaying how effect size relates to the productivity of trained
                  versus untrained workers."),
                br(),
                p("The Utility Outputs tab displays the results of utility analysis as well as a plain text description. This utility analysis takes into account the quality and quantity of
                the training program and workers and compares this value to the cost of the program. It additionally takes into account economic factors such as variable costs, taxes, and investments.
                The results from this analysis can be displayed in two forms, the RODI as discussed earlier, or as the dollar value of goal-setting. Displaying the results of utility analysis in the form
                of RODI provides managers with an easily understandable way of seeing the production gains from training programs (Avolio et al., 2010). On the other hand, the goal-setting approach has been 
                shown to be a useful tool for increasing job performance that managers react positively to (Schmidt, 2013). Sets of inputs from both studies have been provided.")
              )
              ),
      tabItem(tabName = "effect_size",
    
  fluidPage(
    useShinyjs(),
    titlePanel("Effect Size Description"),
    sidebarLayout(
    sidebarPanel(
      id = "sidebar",
                 HTML('<h3>Utility Inputs</h3>'),
                 numericInput("nTrain_1", "Number of Employees Trained", 0, min = 0, step = 1),
                 numericInput("tTrain_1", "Training Effect Duration", 0, min = 0),
                 numericInput("dTrain2_1", "Effect Size of Procedure", 0),
                 numericInput("sdyTrain_1", "SD of Performance in Monetary Units", 0),
                 numericInput("costTrain2_1", "Cost per Employee of Procedure", 0, min = 0),
                 actionButton("goAvolio_1", "RODI Inputs"),
                 actionButton("goSchmidt_1", "Goal Setting Inputs"),
                 br(),
                 actionButton("goEffect", "Display Effect Size")
        
      
    ),
    mainPanel(
     
      plotOutput("training_graph_1"),
      br(),
      textOutput("effectText"),
      downloadButton("effect_download", "Download PDF")
            )
          )
        )
      ),
  tabItem(tabName = "utility_outputs",
          fluidPage(
            useShinyjs(),
            titlePanel("Predicting Returns on Training and Developing Employees"),
            sidebarLayout(
              sidebarPanel(
                id = "sidebar",
                tabsetPanel(
                  id = "pageTabs",
                  tabPanel("Page 1",
                           HTML('<h3>Utility Inputs (Page 1)</h3>'),
                           numericInput("nTrain", "Number of Employees Trained", 0, min = 0, step = 1),
                           numericInput("tTrain", "Training Effect Duration", 0, min = 0),
                           numericInput("dTrain2", "Effect Size of Procedure", 0),
                           numericInput("sdyTrain", "SD of Performance in Monetary Units", 0),
                           numericInput("sdP", "SD of Work Output as % of Mean Output", 20, min = 0, max = 100),
                           numericInput("costTrain2", "Cost per Employee of Procedure", 0, min = 0),
                           actionButton("goAvolio", "RODI Inputs"),
                           actionButton("goSchmidt", "Goal Setting Inputs"),
                           br(),
                           actionButton("goUn", "Compute Utility(No Adjustments)")
                  ),
                  tabPanel("Page 2",
                           HTML('<h3>Utility Inputs (Page 2)</h3>'),
                           HTML('<h4>Economic Factors</h4>'),
                           numericInput("vrateTrain", "Variable Costs (%)", 2, min = 0, max = 100),
                           numericInput("taxTrain", "Tax Rate (%)", 30, min = 0, max = 100),
                           numericInput("discTrain", "Discount Rate (%)", 1, min = 0, max = 100),
                           HTML('<h4>Workflow</h4>'),
                           numericInput("lengthTrain", "Program Length", 15, min = 0),
                           numericInput("addTrain", "Employees Trained per Year", 618, min = 0),
                           numericInput("subTrain", "Loss of Trained Employees per Year (After Effect Duration)", 618, min = 0),
                           radioButtons("outputOpt", "Output Type:",
                                        choices = c("Training Program", "Goal-Setting"),
                                        selected = "Training Program"
                           ),
                           actionButton("go4", "Compute Utility")
                  )
                )
              ),
              mainPanel(
                textOutput("training_utilityUn"),
                textOutput("training_utility"),
                textOutput("training_utility_per_employee"),
                textOutput("training_utility_per_employee_per_year"),
                textOutput("break_even_train"),
                br(),
                textOutput("trainingText"),
                downloadButton("training_download", "Export Report")
              )
            )
        )
    )
)
)
)

#Server
main_server <- function(input, output, session) {
  utilityUn <- reactiveVal()
  utilityUnPer <- reactiveVal()
  utilityUnPerYear <- reactiveVal()
  utilityAdj <- reactiveVal()
  utilityAdjPer <- reactiveVal()
  utilityAdjPerYear <- reactiveVal()
  staffSDy <- reactiveVal()
  costAdj <- reactiveVal()
  elo <- reactiveVal()
  elmo <- reactiveVal()
  eumo <- reactiveVal()
  eto <- reactiveVal()
  eln <- reactiveVal()
  elmn <- reactiveVal()
  eumn <- reactiveVal()
  etn <- reactiveVal()
  expPro <- reactiveVal()
  expPro2 <- reactiveVal()
  expDiff <- reactiveVal()
  expDiff2 <- reactiveVal()
  selectedPage <- reactiveVal(1)
  
#Staffing Utility 
  observe({ 
    #Staffing Toggle 1
    toggleState(
      id = "go",
      condition = 
        input$n > 0 &
        input$sdy >= 0 &
        -1 <= input$rxy1  &
        1 >= input$rxy1 &
        -1 <= input$rxy2  &
        1 >= input$rxy2 &
        input$sr > 0 &
        input$cost1 >= 0 &
        input$cost2 >= 0 &
        input$period > 0
    )
    #Staffing Toggle 2
    toggleState(
      id = "go2",
      condition =
        input$vcost >= 0 &
        input$vcost <= 100 &
        input$tax >= 0 &
        input$tax <= 100 &
        input$drate >= 0 &
        input$drate <= 100 &
        input$pyears >0 &
        input$nadd >= 0 &
        input$nsub >= 0
    )
    #Training Toggle 1
    toggleState(
      id = "goEffect",
      condition =
        input$nTrain_1 > 0 &
        input$tTrain_1 > 0 &
        input$sdyTrain_1 > 0 &
        input$costTrain2_1 >= 0
    )
    if (!isTruthy(input$go)) {
      disable("download_plot")
    }
    if (!isTruthy(input$go2)) {
      disable("download_pdf")
    }
    if (!isTruthy(input$goEffect)) {
      disable("effect_download")
    }
    if (!isTruthy(input$go4)) {
      disable("training_download")
    }
    if (!isTruthy(input$goSchmidt)) {
      disable("sdP")
    }
  })
   
  observeEvent(input$go, {
    
    enable("download_plot")
    
    n <- input$n
    period <- input$period
    
    # Utility Calculation
    utility_value <- utilityBcg(
      n = (input$n),
      sdy = input$sdy,
      rxy = (input$rxy2 - input$rxy1),
      uxs = ux(input$sr),
      sr = input$sr,
      pux = NULL,
      cost = ((input$cost2 - input$cost1) * (input$n / input$sr)),
      period = input$period
    )
    
    # Compute Break Even SDy
    breakEvenSDy <- (
      ((input$cost2)*(input$n/input$sr))/(input$n*(input$rxy2)*input$period*ux(input$sr))
    )
    
    # Compute old top expectancy
    expectancyTopOld <- 100*round(Expectancyfunc(input$rxy1, ux(input$sr)+.67 , Inf, input$rxy1*(ux(input$sr))+.67, Inf),2)
    
    # Compute old upper middle expectancy
    expectancyUMOld <- 100*round(Expectancyfunc(input$rxy1, ux(input$sr)+0 , ux(input$sr)+.67, input$rxy1*(ux(input$sr))+.67, Inf),2)
    
    #Compute old lower middle expectancy
    expectancyLMOld <- 100*round(Expectancyfunc(input$rxy1, ux(input$sr)-.67 , ux(input$sr)+0, input$rxy1*(ux(input$sr))+.67, Inf),2)
    
    #compute old lower expectancy
    expectancyLowOld <- 100*round(Expectancyfunc(input$rxy1, -Inf , ux(input$sr)-.67, input$rxy1*(ux(input$sr))+.67, Inf),2)
    
    # Compute new top expectancy
    expectancyTopNew <- 100*round(Expectancyfunc(input$rxy2, ux(input$sr)+.67 , Inf, input$rxy2*(ux(input$sr))+.67, Inf),2)
    
    # Compute new upper middle expectancy
    expectancyUMNew <- 100*round(Expectancyfunc(input$rxy2, ux(input$sr)+0 , ux(input$sr)+.67, input$rxy2*(ux(input$sr))+.67, Inf),2)
    
    #Compute new lower middle expectancy
    expectancyLMNew <- 100*round(Expectancyfunc(input$rxy2, ux(input$sr)-.67 , ux(input$sr)+0, input$rxy2*(ux(input$sr))+.67, Inf),2)
    
    #compute new lower expectancy
    expectancyLowNew <- 100*round(Expectancyfunc(input$rxy2, -Inf , ux(input$sr)-.67, input$rxy2*(ux(input$sr))+.67, Inf),2)
    
    elo(expectancyLowOld)
    elmo(expectancyLMOld)
    eumo(expectancyUMOld)
    eto(expectancyTopOld)
    eln(expectancyLowNew)
    elmn(expectancyLMNew)
    eumn(expectancyUMNew)
    etn(expectancyTopNew)
    
    # Calculate the per-hire utility
    per_hire_utility <- utility_value / n
    
    # Calculate the per-hire per-year utility
    per_year_utility <- per_hire_utility / period
    
    # Format the total utility value as a dollar amount
    
    
    formatted_total_utility <- label_dollar(scale = .001, prefix= "~$", suffix = "K")(signif(utility_value, 2))
    utilityUn(formatted_total_utility)
    
    # Format the per-hire utility value as a dollar amount
    formatted_per_hire_utility <- label_dollar(scale = .001, prefix= "~$", suffix = "K")(signif(per_hire_utility, 2))
    utilityUnPer(formatted_per_hire_utility)
    
    # Format the per-hire per-year utility value as a dollar amount
    formatted_per_year_utility <- label_dollar(scale = .001, prefix= "~$", suffix = "K")(signif(per_year_utility, 2))
    utilityUnPerYear(formatted_per_year_utility)
    
    # Format the break even value as a dollar amount
    formatted_breakEvenSDy <- label_dollar(scale = .001, prefix= "~$", suffix = "K")(signif(breakEvenSDy, 2))
    staffSDy(formatted_breakEvenSDy)
    
    
    
    
    exp_diff <- if (expectancyTopNew > expectancyTopOld) {
      expectancyTopNew - expectancyTopOld
    }
    else {
      expectancyTopOld - expectancyTopNew
    }
    expDiff(exp_diff)
    exp_diff2 <- if (expectancyLowNew > expectancyLowOld) {
      expectancyTopNew - expectancyTopOld
    }
    else {
      expectancyLowOld - expectancyLowNew
    }
    expDiff2 <- exp_diff2
    exp_procedure <- if (expectancyTopNew > expectancyTopOld) {
      "new procedure"
    }
    else {
      "old procedure"
    }
    expPro(exp_procedure)
    exp_procedure2 <- if (expectancyLowNew > expectancyLowOld) {
      "increases"
    }
    else {
      "decreases"
    }
    expPro2 <- exp_procedure2
    #expectancy chart
    
    plot_data <- reactive({
      bar_data <- data.frame(
        Quartile = rep(c("Bottom 25%", "Lower Middle 25%", "Upper Middle 25%", "Top 25%"), each = 2),
        Procedure = factor(c("Old", "New"), levels = c("Old", "New")),  # Specify the order of levels
        Probability = c(
          expectancyLowOld, expectancyLowNew,
          expectancyLMOld, expectancyLMNew,
          expectancyUMOld, expectancyUMNew,
          expectancyTopOld, expectancyTopNew
        )
      )
      bar_data$Quartile <- str_wrap(bar_data$Quartile, width = 10)
      max_y_value <- 1.2 * max(max(bar_data$Probability))
      title_text <- paste0("Using the ", exp_procedure, " improves the probability of hiring a high performer by ", exp_diff, " percentage points.")
      wrapped_title <- str_wrap(title_text, width = 90)
      p <- ggplot(bar_data, aes(x = reorder(Quartile, Probability), y = Probability, fill = Procedure)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(
          x = "", 
          y = "Probability of High Job Performance",
          fill = "Procedure"
        ) +
        ylim(0, max_y_value) +  # Set the y-axis limits
        scale_fill_manual(values = c("Old" = "#FF9999", "New" = "#9999FF")) +
        theme_minimal() +  # Customize the plot theme (optional)
        theme(
          plot.title = element_text(hjust = 0, margin = margin(b = 10), size = 16, lineheight = 1.2),
          axis.text = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.text.x = element_text(angle = 0, hjust = .5)
        ) +
        ggtitle(paste0("Figure 1\n", wrapped_title))
      
      return(p + geom_text(aes(label = paste0(Probability, "%")), vjust = -0.5, size = 4, position = position_dodge(width = 0.9)))
    })
    output$utility_graph <- renderPlot({
      plot_data()
    })
    
    output$expectancy_text <- renderText({
      paste0("The above chart compares the expectancy of the new staffing procedure to the old staffing procedure. Expectancy is a 
             measure of the ability of a test to predict outcomes. The ", exp_procedure, " improves the probability of hiring a high performer by ", exp_diff, " percentage
             points. It is also important to know how many bad hires a staffing procedure is avoiding. The above chart tells us that the new procedure 
             ", exp_procedure2, " the risk of a bad hire by ", exp_diff2, " percentage points.")
    })
    
    output$download_plot <- downloadHandler(
      filename = function() {
        "expectancy.pdf"  # Set the filename for the downloaded file
      },
      
      content = function(file) {
        
        capWrap <- strwrap(paste0("The above chart compares the expectancy of the new staffing procedure to the old staffing procedure. Expectancy is a 
             measure of the ability of a test to predict outcomes. The ", exp_procedure, " improves the probability of hiring a high performer by ", exp_diff, " percentage
             points. It is also important to know how many bad hires a staffing procedure is avoiding. The above chart tells us that the new procedure 
             ", exp_procedure2, " the risk of a bad hire by ", exp_diff2, " percentage points."), width = 100, simplify = TRUE)
        capWrap <- paste(capWrap, collapse = "\n")
        # Create the ggplot2 plot with a caption
        plot <- plot_data() +
          labs(
            caption = capWrap
          ) +
          theme(
            plot.caption = element_text(
              hjust = 0,
              margin = margin(t = 10, unit = "pt"),
              size = 14
            ),
            plot.margin = margin(1,0.5,0.5,0.5, "in")
          )
        
        
        # Save the ggplot2 plot as a PDF file with custom width and height
        ggsave(
          file,
          plot,
          device = "pdf",
          width = 10.625,  # Adjust the width as needed
          height = 13.75,  # Adjust the height as needed
          units = "in",  # Specify units (inches in this case)
          dpi = 300  # Adjust DPI if necessary
        )
      }
    )
    
  })
  
  observeEvent(input$go2,{
    enable("download_pdf")
    varCosts <- -input$vcost/100
    tax <- input$tax
    disc <- input$drate
    costOrd <- input$cost1
    costAc <- input$cost2
    validOrd <- input$rxy1
    validAc <- input$rxy2
    SDjp <- input$sdy
    sr1  <- input$sr
    ordsr1 <- dnorm(qnorm(1-sr1),0,1)
    tenure1 <- input$period
    last <- input$pyears
    add <- input$nadd
    subt <- input$nsub
    
    nk <- 0
    paytot <- 0
    paysel <- 0
    payselfir <- 0
    
    discProp <- disc / 100
    
    valid <- validAc - validOrd
    
    ord <- ordsr1 / sr1
    ck <- add * ((costAc - costOrd) / sr1)
    taxProp <- tax / 100
    discRat <- 1 / (1 + discProp)
    
    numyr <- tenure1 + last
    
    totDelta <- 0
    totDelta1 <- 0
    
    for (i in 1:numyr) {
      if (i > tenure1) {nk <- nk - subt}
      if (i <= last) {nk <- nk + add}
      if (i > last) {ck <- 0}
      if (nk >= 0) {
        delta1 <- nk * ((discRat^i) * valid * ord * SDjp * (1 + varCosts) * (1 - taxProp))
      }
      delta2 <- ck * (1 - taxProp) * (discRat^(i - 1))
      totDelta1 <- totDelta1 + delta2
      delta <- delta1 - delta2
      totDelta <- totDelta + delta
    }
    
    adjusted_utility <- totDelta
    adjusted_utility_perHire <- totDelta / (last*add)
    adjusted_utility_perHire_perYear <- (totDelta / (last * add * tenure1))
    
    # Format the total utility value as a dollar amount
    formatted_adjusted_utility <- label_dollar(scale = .001, prefix= "~$", suffix = "K")(signif(adjusted_utility, 2))
    utilityAdj(formatted_adjusted_utility)
    
    # Format the per-hire utility value as a dollar amount
    formatted_adjusted_per_hire_utility <- label_dollar(scale = .001, prefix= "~$", suffix = "K")(signif(adjusted_utility_perHire, 2))
    utilityAdjPer(formatted_adjusted_per_hire_utility)
    
    # Format the per-hire per-year utility value as a dollar amount
    formatted_adjusted_per_year_utility <- label_dollar(scale = .001, prefix= "~$", suffix = "K")(signif(adjusted_utility_perHire_perYear, 2))
    utilityAdjPerYear(formatted_adjusted_per_year_utility)
    
    costAdj(label_dollar(scale = .001, prefix= "~$", suffix = "K")(signif(totDelta1, 2)))
    
    # Display the formatted unadjusted utility
    
    output$total_utility <- renderText({
      paste("The total predicted returns (unadjusted) on improved staffing are:", 
            utilityUn())
    })
    
    # Display the formatted break even value
    output$break_even_SDy <- renderText({
      paste("The break-even value for Standard Deviation of the new procedure is:", 
            staffSDy())
    })
    
    # Display the formatted total utility value
    output$adjusted_utility <- renderText({
      paste("The total predicted returns on improved staffing are:", 
            formatted_adjusted_utility)
    })
    
    # Display the formatted per-hire utility value
    output$adjusted_utility_perHire <- renderText({
      paste("The predicted returns on improved staffing per hire are:", 
            formatted_adjusted_per_hire_utility)
    })
    
    # Display the formatted per-hire per-year utility value
    output$adjusted_utility_perHire_perYear <- renderText({
      paste("The predicted returns on improved staffing per hire per year are:", 
            formatted_adjusted_per_year_utility)
    })
    
    lowHigh <- if(input$rxy1 > input$rxy2) {
      "old"
    }
    else{
      "new"
    }
    
    staff1 <- paste0("In order to assess the effectiveness of new staffing procedures, an analysis has been conducted 
                     comparing the quality and quantity of new hires to the cost of the selection processes. As part of this analysis, validity was found for each of the staffing procedures. The new procedure to be used has a validity of ", input$rxy2, ". This is in comparison to the old procedure which has a validity of ", input$rxy1, ". This means that the ", lowHigh, " procedure more accurately predicts the successfulness of hired candidates.")
    staff2 <- paste0("The costs associated with administering the new procedure were $", input$cost2, " per applicant. This is in 
                     comparison to the cost of the old procedure which is $", input$cost1, " per applicant. Additionally, there are variable costs at a rate of ", input$vcost,"% taxes at a rate of ", input$tax, "%, and a discount rate of ", input$drate,"%. Since ", input$n, " new employees are to be hired from a large selection pool per year, and the new procedure is expected to last ", input$pyears, " years, the total additional cost of the new procedure compared to the old procedure is projected to be ", costAdj(), ".")
    staff3 <- paste0("The value of more productive employees at this position within the company was found to be $", input$sdy, ".
                     Since these employees are expected to stay with the company for an average of ", input$period, " years, the average employee selected under the new system is expected to be worth ", utilityAdjPer(), " more than employees selected under the old system. When we consider that the program is expected to last ", input$pyears, " years, ", input$nadd, " employees will be added each year of the program, and ", input$nsub, " employees will leave the company each year after their expected tenure, then we can expect the total value added to the company by utilizing the new procedure is ", utilityAdj(), ".")
    s1wrap <- strwrap(staff1, width = 100, simplify = TRUE)
    s1wrap <- paste(s1wrap, collapse = "\n")
    s2wrap <- strwrap(staff2, width = 100, simplify = TRUE)
    s2wrap <- paste(s1wrap, collapse = "\n")
    s3wrap <- strwrap(staff3, width = 100, simplify = TRUE)
    s3wrap <- paste(s1wrap, collapse = "\n")
    output$plainText1 <- renderText({
      staff1
    })
    output$plainText2 <- renderText({
      staff2
    })
    output$plainText3 <- renderText({
      staff3
    })
    
    elo1 <- elo()
    eln1 <- eln()
    elmo1 <- elmo()
    elmn1 <- elmn()
    eumo1 <- eumo()
    eumn1 <- eumn()
    eto1 <- eto()
    etn1 <- etn()
    
    plot_data <- reactive({
      bar_data <- data.frame(
        Quartile = rep(c("Bottom 25%", "Lower Middle 25%", "Upper Middle 25%", "Top 25%"), each = 2),
        Procedure = factor(c("Old", "New"), levels = c("Old", "New")),  # Specify the order of levels
        Probability = c(
          elo1, eln1,
          elmo1, elmn1,
          eumo1, eumn1,
          eto1, etn1
        )
      )
      bar_data$Quartile <- str_wrap(bar_data$Quartile, width = 10)
      max_y_value <- 1.2 * max(max(bar_data$Probability))
      title_text <- paste0("Using the ", expPro(), " improves the probability of hiring a high performer by ", expDiff(), " percentage points.")
      wrapped_title <- str_wrap(title_text, width = 90)
      p <- ggplot(bar_data, aes(x = reorder(Quartile, Probability), y = Probability, fill = Procedure)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(
          x = "", 
          y = "Probability of High Job Performance",
          fill = "Procedure"
        ) +
        ylim(0, max_y_value) +  # Set the y-axis limits
        scale_fill_manual(values = c("Old" = "#FF9999", "New" = "#9999FF")) +
        theme_minimal() +  # Customize the plot theme (optional)
        theme(
          plot.title = element_text(hjust = 0, margin = margin(b = 10), size = 16, lineheight = 1.2),
          axis.text = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.text.x = element_text(angle = 0, hjust = .5)
        ) +
        ggtitle(paste0("Figure 1\n", wrapped_title))
      
      return(p + geom_text(aes(label = paste0(Probability, "%")), vjust = -0.5, size = 4, position = position_dodge(width = 0.9)))
    })
    
    output$download_pdf <- downloadHandler(
      filename = function() {
        "staffing_report.pdf"  # Set the filename for the downloaded file
      },
      
      content = function(file) {
        
        title <- textGrob(
          "Staffing Report",
          x = 0.5, y = .75,
          gp = gpar(fontsize = 20),
        )
        
        par1 <- textGrob(
          s1wrap,
          x = 0.05, y = 0.75,
          gp = gpar(fontsize = 16),
          hjust = 0
        )
        
        par2 <- textGrob(
          s2wrap,
          x = 0.05, y = 0.5,
          gp = gpar(fontsize = 16),
          hjust = 0
        )
        
        par3 <- textGrob(
          s3wrap,
          x = 0.05, y = 0.25,
          gp = gpar(fontsize = 16),
          hjust = 0
        )
        
        capWrap <- strwrap(paste0("The above chart compares the expectancy of the new staffing procedure to the old staffing procedure. Expectancy is a 
             measure of the ability of a test to predict outcomes. The ", expPro(), " improves the probability of hiring a high performer by ", expDiff(), " percentage
             points. It is also important to know how many bad hires a staffing procedure is avoiding. The above chart tells us that the new procedure 
             ", expPro2(), " the risk of a bad hire by ", expDiff2(), " percentage points."), width = 100, simplify = TRUE)
        capWrap <- paste(capWrap, collapse = "\n")
        # Create the ggplot2 plot with a caption
        plot <- plot_data() +
          labs(
            caption = capWrap
          ) +
          theme(
            plot.caption = element_text(
              hjust = 0,
              margin = margin(t = 10, unit = "pt"),
              size = 14
            ),
            plot.margin = margin(1,0.5,0.5,0.5, "in")
          )
        
        plot_text <- arrangeGrob(title, par1, par2, par3, plot, nrow= 5, heights = unit(c(1, 1, 1, 1, 5), "null"))
        
        # Save the ggplot2 plot as a PDF file with custom width and height
        ggsave(
          file,
          plot_text,
          device = "pdf",
          width = 10.625,  # Adjust the width as needed
          height = 13.75,  # Adjust the height as needed
          units = "in",  # Specify units (inches in this case)
          dpi = 300  # Adjust DPI if necessary
        )
      }
    )
    
  })
  
#Training Utility
  
  observeEvent(input$selectPage1, {
    updateTabsetPanel(session, "pageTabs", selected = "Page 1")
  })
  
  observeEvent(input$selectPage2, {
    updateTabsetPanel(session, "pageTabs", selected = "Page 2")
  })
  
  observeEvent(input$goAvolio_1, {
    updateNumericInput(session, "nTrain_1", value = 30, min = 0, step = 1)
    updateNumericInput(session, "tTrain_1", value = .167, min = 0)
    updateNumericInput(session, "dTrain2_1", value = .52)
    updateNumericInput(session, "sdyTrain_1", value = 40000, min = 0)
    updateNumericInput(session, "costTrain2_1", value = 2155.77, min = 0)
  })
  
  observeEvent(input$goSchmidt_1, {
    updateNumericInput(session, "nTrain_1", value = 35, min = 0, step = 1)
    updateNumericInput(session, "tTrain_1", value = 5, min = 0)
    updateNumericInput(session, "dTrain2_1", value = .46)
    updateNumericInput(session, "sdyTrain_1", value = 20000, min = 0)
    updateNumericInput(session, "costTrain2_1", value = 200, min = 0)
  })
  
  observeEvent(input$goAvolio, {
    disable("sdP")
    updateNumericInput(session, "nTrain", value = 30, min = 0, step = 1)
    updateNumericInput(session, "tTrain", value = .167, min = 0)
    updateNumericInput(session, "dTrain2", value = .52)
    updateNumericInput(session, "sdyTrain", value = 40000, min = 0)
    updateNumericInput(session, "costTrain2", value = 2155.77, min = 0)
  })
  
  observeEvent(input$goSchmidt, {
    enable("sdP")
    updateNumericInput(session, "nTrain", value = 35, min = 0, step = 1)
    updateNumericInput(session, "tTrain", value = 5, min = 0)
    updateNumericInput(session, "dTrain2", value = .46)
    updateNumericInput(session, "sdyTrain", value = 20000, min = 0)
    updateNumericInput(session, "costTrain2", value = 200, min = 0)
  })
#Effect Size Tab  
  observeEvent(input$goEffect,{
    
    enable("effect_download")
    
    u3 <- round(pnorm(input$dTrain2_1), 2)*100
    sup <- round(pnorm(input$dTrain2_1/sqrt(2)), 2)*100
    
    dt <- input$dTrain2_1
    z1 <- 0
    z2 <- dt
    
    x <- seq(-4, 4, length = 500)
    y1 <- dnorm(x, mean = z1, sd = 1)
    y2 <- dnorm(x, mean = z2, sd = 1)
    
    # Create a data frame for ggplot2
    df <- data.frame(x = x, y1 = y1, y2 = y2)
    
    # Create the ggplot2 plot
    gg <- ggplot(df, aes(x = x)) +
      geom_ribbon(aes(ymin = 0, ymax = y1), fill = "red", alpha = 0.3) +
      geom_ribbon(aes(ymin = 0, ymax = y2), fill = "blue", alpha = 0.3) +
      geom_line(aes(y = y1), color = "red") +
      geom_line(aes(y = y2), color = "blue") +
      geom_vline(xintercept = z1, color = "red") +
      geom_vline(xintercept = z2, color = "blue") +
      geom_segment(
        aes(xend = z2, y = 0.2, x = z1, yend = 0.2),
        arrow = arrow(type = "closed", length = unit(0.05, "inches")),
        lineend = "round",
        color = "black"
      ) +
      annotate("text", x = z2/2, y = 0.17, label = paste(dt), color = "black", size = 4) +
      annotate("text", x = z1 - 0.3, y = 0.45, label = "Untrained", color = "red", size = 4) +
      annotate("text", x = z2 + 0.3, y = 0.45, label = "Trained", color = "blue", size = 4) +
      labs(
        y = "",
        x = "Standardized Job Performance"
      ) +
      scale_x_continuous(limits = c(-4, 4), breaks = seq(-4, 4, by = 1)) +
      ylim(0, 0.5) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0, margin = margin(b = 10), size = 16, lineheight = 1.2),
        axis.text = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        legend.position = "topright",
        legend.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
      ) +
      ggtitle(paste0("Figure 1\nAn effect size of ", dt, " is a ", u3, "% increase in mean production."))
    
    
    
    output$training_graph_1 <- renderPlot(gg)
    
    output$effectText <- renderText({
      paste0("The above graph shows the comparison of the effect size of a trained group to an untrained group. 
             With an effect size of ", input$dTrain2_1, ", ", u3, "% of the trained group will be above the
             mean production value of the untrained group, and there is a ", sup, "% chance that a person picked at
             random from the trained group will have a higher production value than a person picked at random
             from the untrained group.")
    })
    
    output$effect_download <- downloadHandler(
      filename = function() {
        "expectancy.pdf"  # Set the filename for the downloaded file
      },
      
      content = function(file) {
        
        capWrap <- strwrap(paste0("The above graph shows the comparison of the effect size of a trained group to an untrained group. 
             With an effect size of ", input$dTrain2_1, ", ", u3, "% of the trained group will be above the
             mean production value of the untrained group, and there is a ", sup, "% chance that a person picked at
             random from the trained group will have a higher production value than a person picked at random
             from the untrained group."), width = 100, simplify = TRUE)
        capWrap <- paste(capWrap, collapse = "\n")
        # Create the ggplot2 plot with a caption
        plot <- gg +
          labs(
            caption = capWrap
          ) +
          theme(
            plot.caption = element_text(
              hjust = 0,
              margin = margin(t = 10, unit = "pt"),
              size = 14
            ),
            plot.margin = margin(1,0.5,0.5,0.5, "in")
          )
        
        
        # Save the ggplot2 plot as a PDF file with custom width and height
        ggsave(
          file,
          plot,
          device = "pdf",
          width = 10.625,  # Adjust the width as needed
          height = 6.875,  # Adjust the height as needed
          units = "in",  # Specify units (inches in this case)
          dpi = 300  # Adjust DPI if necessary
        )
      }
    )
    
  })
  
#Training Utility Output Tab
  
  observeEvent(input$goUn,{
    utilityTrainUn <- input$nTrain*input$dTrain2*input$sdyTrain*input$tTrain-input$nTrain*input$costTrain2
    
    formatted_utilityTrainUn <-label_dollar(scale = .001, prefix= "~$", suffix = "K")(signif(utilityTrainUn, 2))
    
    output$training_utilityUn <- renderText({
      paste("The total unadjusted returns on training utility are:", formatted_utilityTrainUn)
    })
  })
  
  observeEvent(input$go4,{
    enable("training_download")
    breakEven <- (input$nTrain*input$costTrain2)/(input$nTrain*input$tTrain*input$dTrain2)
    
    u3 <- round(pnorm(input$dTrain2), 2)*100
    sup <- round(pnorm(input$dTrain2/sqrt(2)), 2)*100
    
    dt <- input$dTrain2
    z1 <- 0
    z2 <- dt
    
    x <- seq(-4, 4, length = 500)
    y1 <- dnorm(x, mean = z1, sd = 1)
    y2 <- dnorm(x, mean = z2, sd = 1)
    
    # Create a data frame for ggplot2
    df <- data.frame(x = x, y1 = y1, y2 = y2)
    
    # Create the ggplot2 plot
    gg <- ggplot(df, aes(x = x)) +
      geom_ribbon(aes(ymin = 0, ymax = y1), fill = "red", alpha = 0.3) +
      geom_ribbon(aes(ymin = 0, ymax = y2), fill = "blue", alpha = 0.3) +
      geom_line(aes(y = y1), color = "red") +
      geom_line(aes(y = y2), color = "blue") +
      geom_vline(xintercept = z1, color = "red") +
      geom_vline(xintercept = z2, color = "blue") +
      geom_segment(
        aes(xend = z2, y = 0.2, x = z1, yend = 0.2),
        arrow = arrow(type = "closed", length = unit(0.05, "inches")),
        lineend = "round",
        color = "black"
      ) +
      annotate("text", x = z2/2, y = 0.17, label = paste(dt), color = "black", size = 4) +
      annotate("text", x = z1 - 0.3, y = 0.45, label = "Untrained", color = "red", size = 4) +
      annotate("text", x = z2 + 0.3, y = 0.45, label = "Trained", color = "blue", size = 4) +
      labs(
        y = "",
        x = "Standardized Job Performance"
      ) +
      scale_x_continuous(limits = c(-4, 4), breaks = seq(-4, 4, by = 1)) +
      ylim(0, 0.5) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0, margin = margin(b = 10), size = 16, lineheight = 1.2),
        axis.text = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        legend.position = "topright",
        legend.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
      ) +
      ggtitle(paste0("Figure 1\nAn effect size of ", dt, " is a ", u3, "% increase in mean production."))
    
    formatted_breakEvenTrain <- label_dollar(scale = .001, prefix= "~$", suffix = "K")(signif(breakEven, 2))
    
    option <- input$outputOpt
    varCosts <- -input$vrateTrain/100
    tax <- input$taxTrain
    disc <- input$discTrain
    costAc <- input$costTrain2
    validAc <- input$dTrain2
    SDjp <- input$sdyTrain
    tenure1 <- input$tTrain
    last <- input$lengthTrain
    add <- input$addTrain
    subt <- input$subTrain
    
    intermediate_totDelta <- numeric(0)
    
    nk <- 0
    paytot <- 0
    paysel <- 0
    payselfir <- 0
    
    discProp <- disc / 100
    
    valid <- validAc
    
    ck <- add * (costAc)
    taxProp <- tax / 100
    discRat <- 1 / (1 + discProp)
    
    numyr <- tenure1 + last
    
    totDelta <- 0
    totDelta1 <- 0
    
    plot_data <- data.frame(step = c(-2, 1), intermediate_totDelta = c(0, 0))
    
    for (i in 1:numyr) {
      if (i > tenure1) {nk <- nk - subt}
      if (i <= last) {nk <- nk + add}
      if (i > last) {ck <- 0}
      if (i == 1) {
        delta1 <- nk * ((discRat^i) * valid *  SDjp * (1 + varCosts) * (1 - taxProp))
      }
      if (i > 1 & i < numyr - 1) {
        delta1 <- nk * ((discRat^i) * valid/(nk/add) *  SDjp * (1 + varCosts) * (1 - taxProp))
      }
      if (i == numyr - 1) {
        delta1 <- 0
      }
      delta2 <- ck * (1 - taxProp) * (discRat^(i - 1))
      totDelta1 <- totDelta1 + delta2
      delta <- delta1 - delta2
      totDelta <- totDelta + delta
      
      # Store intermediate totDelta value
      intermediate_totDelta <- c(intermediate_totDelta, totDelta)
      
      # Append to plot_data
      plot_data <- rbind(plot_data, data.frame(step = i, intermediate_totDelta = totDelta))
    }
    
    adjusted_utility <- totDelta
    adjusted_utility_perHire <- totDelta / (last*add)
    adjusted_utility_perHire_perYear <- (totDelta / (last * add * tenure1))
    
    # Format the total utility value as a dollar amount
    formatted_adjusted_utility <- label_dollar(scale = .001, prefix= "~$", suffix = "K")(signif(adjusted_utility, 2))
    
    # Format the per-hire utility value as a dollar amount
    formatted_adjusted_per_hire_utility <- label_dollar(scale = .001, prefix= "~$", suffix = "K")(signif(adjusted_utility_perHire, 2))
    
    # Format the per-hire per-year utility value as a dollar amount
    formatted_adjusted_per_year_utility <- label_dollar(scale = .001, prefix= "~$", suffix = "K")(signif(adjusted_utility_perHire_perYear, 2))
    
    if (option == "Training Program"){
      # Display the formatted total utility value
      output$training_utility <- renderText({
        paste("The total predicted returns on an improved training program are:", 
              formatted_adjusted_utility)
      })
      
      # Display the formatted per-hire utility value
      output$training_utility_per_employee <- renderText({
        paste("The predicted returns on an improved training program per hire are:", 
              formatted_adjusted_per_hire_utility)
      })
      
      # Display the formatted per-hire per-year utility value
      output$training_utility_per_employee_per_year <- renderText({
        paste("The predicted returns on an improved training program per hire per year are:", 
              formatted_adjusted_per_year_utility)
      })
      
      output$break_even_train <- renderText({
        paste("The break even value of SDy is:", 
              formatted_breakEvenTrain)
      })
      
      training_text <- paste0("An analysis was conducted in order to assess the return on development investment(RODI) for an employee training program.
               The program consisted of ", input$nTrain, " employees attending leadership sessions over a short number of days. It was found that
               the effects of the program are expected to last ", input$tTrain, " years. The cost of the program was assessed to be around $",
                              input$costTrain2, " per employee. In order to accurately assess RODI, additional costs were considered. These costs include variable
               costs, at ", input$vrateTrain, "% and taxes, at ", input$taxTrain, "%. To help offset these costs, the company is expected to invest
               in funds at a discount rate of ", input$discTrain, "% using production revenues. The performance of employees before training was also
               assessed. The standard deviation of untrained employees was found to be $", input$sdyTrain, ". ",
                              u3, "% of employees who participated in the program were assessed to be more productive than those who did not participate. If the program
               is repeated every year for ", input$lengthTrain, " years, then the total RODI from the program is expected to be ", formatted_adjusted_utility,
                              ". The RODI per employee who participates in the program is expected to be ", formatted_adjusted_per_hire_utility, ", and the RODI per
               employee per year is expected to be ", formatted_adjusted_per_year_utility, "."
      )
      ttwrap <- strwrap(training_text, width = 100, simplify = TRUE)
      ttwrap <- paste(ttwrap, collapse = "\n")
      
      output$trainingText <- renderText({
        training_text
      })
      
      output$training_download <- downloadHandler(
        filename = function() {
          "training_report.pdf"  # Set the filename for the downloaded file
        },
        
        content = function(file) {
          
          capWrap <- strwrap(paste0("The above graph shows the comparison of the effect size of a trained group to an untrained group. 
             With an effect size of ", input$dTrain2, ", ", u3, "% of the trained group will be above the
             mean production value of the untrained group, and there is a ", sup, "% chance that a person picked at
             random from the trained group will have a higher production value than a person picked at random
             from the untrained group."), width = 100, simplify = TRUE)
          capWrap <- paste(capWrap, collapse = "\n")
          # Create the ggplot2 plot with a caption
          plot <- gg +
            labs(
              caption = capWrap
            ) +
            theme(
              plot.caption = element_text(
                hjust = 0,
                margin = margin(t = 10, unit = "pt"),
                size = 14
              ),
              plot.margin = margin(1,0.5,0.5,0.5, "in")
            )
          title <- textGrob(
            "Training Report",
            x = 0.5, y = .75,
            gp = gpar(fontsize = 20),
          )
          
          par1 <- textGrob(
            ttwrap,
            x = 0.05, y = 0.6,
            gp = gpar(fontsize = 16),
            hjust = 0
          )
          
          plot_text <- arrangeGrob(title, par1, plot, nrow= 3, heights = unit(c(1, 1, 4), "null"))
          
          ggsave(
            file,
            plot_text,
            device = "pdf",
            width = 10.625,  # Adjust the width as needed
            height = 13.75,  # Adjust the height as needed
            units = "in",  # Specify units (inches in this case)
            dpi = 300  # Adjust DPI if necessary
          )
        }
      )
      
    }
    else{
      
      workOut <- (input$sdP/100)*input$dTrain2
      workOutPer <- round(100*workOut, 2)
      empred <- round((1 - (1/workOut))*100,2)
      
      # Display the formatted total utility value
      output$training_utility <- renderText({
        paste("The total predicted returns on an improved goal-setting program are:", 
              formatted_adjusted_utility)
      })
      
      # Display the formatted per-hire utility value
      output$training_utility_per_employee <- renderText({
        paste("The predicted returns on an improved goal-setting program per hire are:", 
              formatted_adjusted_per_hire_utility)
      })
      
      # Display the formatted per-hire per-year utility value
      output$training_utility_per_employee_per_year <- renderText({
        paste("The predicted returns on an improved goal-setting program per hire per year are:", 
              formatted_adjusted_per_year_utility)
      })
      
      output$break_even_train <- renderText({
        paste("The break even value of SDy is:", 
              formatted_breakEvenTrain)
      })
      
      goal_text <- paste0("An analysis was conducted in order to assess the dollar value of an employee goal-setting program.
               The program consisted of ", input$nTrain, " employees who participated in setting goals for their production. The
               program is set to last ", input$tTrain, "years. The cost of implementing the program was assessed to be around $",
                          input$costTrain2, " per employee. In order to accurately assess the total value, additional costs were considered. These costs include variable
               costs, at ", input$vrateTrain, "% and taxes, at ", input$taxTrain, "%. To help offset these costs, the company is expected to invest
               in funds at a discount rate of ", input$discTrain, "% using production revenues. The performance of employees before implementing 
               goal-setting was also assessed. The standard deviation of employees before goal-setting was found to be $", input$sdyTrain, ". ",
                          u3, "% of employees who participated in the program were assessed to be more productive than those who did not participate. If the program
               is repeated for additional employees every year for ", input$lengthTrain, " years, then the total value of goal-setting is expected to be ", formatted_adjusted_utility,
                          ". The value added per employee who participates in the program is expected to be ", formatted_adjusted_per_hire_utility, ", and the value added per
               employee per year is expected to be ", formatted_adjusted_per_year_utility, ". An increase in employee output of ", workOutPer, "% is expected 
               based on the effects of goal setting on mean employee output. This means that the company can reduce the workforce by ", empred, "% and
               maintain current levels of production."
      )
      
      gtwrap <- strwrap(goal_text, width = 100, simplify = TRUE)
      gtwrap <- paste(gtwrap, collapse = "\n")
      
      output$trainingText <- renderText({
        goal_text
      })
      
      output$training_download <- downloadHandler(
        filename = function() {
          "training_report.pdf"  # Set the filename for the downloaded file
        },
        
        content = function(file) {
          
          capWrap <- strwrap(paste0("The above graph shows the comparison of the effect size of a trained group to an untrained group. 
             With an effect size of ", input$dTrain2, ", ", u3, "% of the trained group will be above the
             mean production value of the untrained group, and there is a ", sup, "% chance that a person picked at
             random from the trained group will have a higher production value than a person picked at random
             from the untrained group."), width = 100, simplify = TRUE)
          capWrap <- paste(capWrap, collapse = "\n")
          # Create the ggplot2 plot with a caption
          plot <- gg +
            labs(
              caption = capWrap
            ) +
            theme(
              plot.caption = element_text(
                hjust = 0,
                margin = margin(t = 10, unit = "pt"),
                size = 14
              ),
              plot.margin = margin(1,0.5,0.5,0.5, "in")
            )
          title <- textGrob(
            "Goal Setting Report",
            x = 0.5, y = .75,
            gp = gpar(fontsize = 20),
          )
          
          par1 <- textGrob(
            gtwrap,
            x = 0.05, y = 0.6,
            gp = gpar(fontsize = 16),
            hjust = 0
          )
          
          plot_text <- arrangeGrob(title, par1, plot, nrow= 3, heights = unit(c(1, 1, 4), "null"))
          
          ggsave(
            file,
            plot_text,
            device = "pdf",
            width = 10.625,  # Adjust the width as needed
            height = 13.75,  # Adjust the height as needed
            units = "in",  # Specify units (inches in this case)
            dpi = 300  # Adjust DPI if necessary
          )
        }
      )
      
    }
    
  })
  
}

# Create the opening page and main app within a navbarPage
ui <- navbarPage(
  "UA+",
  tabPanel("Opening Page", opening_ui),
  tabPanel("Staffing Utility", main_ui),
  tabPanel("Training Utility", training_ui),
  tabPanel("Glossary", glossary_ui),
  tabPanel("References", reference_ui)
)

# Create a Shiny app
shinyApp(ui, main_server)