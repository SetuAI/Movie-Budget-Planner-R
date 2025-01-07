# Movie Marketing Budget Simulator helps movie marketing managers optimize their budgets by using predictive analytics 
# The app uses real-world box office patterns to forecast revenue, incorporating standard industry
# metrics like weekly revenue decay rates (30% drop per week) and screen count adjustments, giving hands-on experience with actual movie distribution patterns.

library(shiny)
library(shinydashboard)
library(DT) # Creates the KPI table
library(ggplot2)
library(dplyr)
library(plotly)

# creating dashboard header
ui <- dashboardPage(
  dashboardHeader(title = "Movie Marketing Budget Planner"),
  
  # side bar menu
  dashboardSidebar(
    sidebarMenu(
      menuItem("Input Parameters", tabName = "inputs", icon = icon("sliders")),
      menuItem("Budget Allocation", tabName = "budget", icon = icon("dollar-sign")),
      menuItem("Projections", tabName = "projections", icon = icon("chart-line"))
    )
  ),
  
  # input parameters tab
  dashboardBody(
    tabItems(
      # Input Parameters Tab
      tabItem(tabName = "inputs",
              fluidRow(
                box(
                  title = "Movie Details",
                  width = 6,
                  textInput("movie_name", "Movie Name", ""),
                  selectInput("genre", "Genre", 
                              choices = c("Action", "Drama", "Comedy", "Horror", "Sci-Fi")),
                  dateInput("release_date", "Release Date"),
                  numericInput("production_budget", "Production Budget ($)", 
                               value = 1000000, min = 0)
                ),
                # creating distribution strategy box
                box(
                  title = "Distribution Strategy",
                  width = 6,
                  numericInput("screens_week1", "Number of Screens (Week 1)", 
                               value = 1000, min = 0),
                  numericInput("screens_week2", "Number of Screens (Week 2)", 
                               value = 800, min = 0),
                  numericInput("ticket_price", "Average Ticket Price ($)", 
                               value = 12, min = 0),
                  selectInput("release_strategy", "Release Strategy",
                              choices = c("Wide Release", "Limited Release", "Platform Release"))
                )
              ),
              # target audience box
              fluidRow(
                box( 
                  title = "Target Audience",
                  width = 6,
                  sliderInput("age_range", "Target Age Range", 
                              min = 0, max = 80, value = c(15, 45)),
                  checkboxGroupInput("target_demographics", "Target Demographics",
                                     choices = c("Students", "Young Professionals", 
                                                 "Families", "Senior Citizens")),
                  selectInput("competition_level", "Competition Level",
                              choices = c("High", "Medium", "Low"))
                ),
                # Performance targets box
                box(
                  title = "Performance Targets",
                  width = 6,
                  numericInput("target_weekend1", "Target First Weekend Revenue ($)", 
                               value = 500000, min = 0),
                  numericInput("target_weekend2", "Target Second Weekend Revenue ($)", 
                               value = 300000, min = 0),
                  numericInput("target_total", "Target Total Box Office ($)", 
                               value = 2000000, min = 0)
                )
              )
      ),
      
      # Budget Allocation Tab
      tabItem(tabName = "budget",
              fluidRow(
                box(
                  title = "Marketing Budget Allocation",
                  width = 12,
                  column(4,
                         numericInput("total_marketing_budget", "Total Marketing Budget ($)", 
                                      value = 500000, min = 0)
                  ),
                  column(8,
                         sliderInput("digital_marketing", "Digital Marketing (%)", 
                                     value = 30, min = 0, max = 100),
                         sliderInput("traditional_advertising", "Traditional Advertising (%)", 
                                     value = 25, min = 0, max = 100),
                         sliderInput("pr_events", "PR & Events (%)", 
                                     value = 20, min = 0, max = 100),
                         sliderInput("influencer_marketing", "Influencer Marketing (%)", 
                                     value = 15, min = 0, max = 100),
                         sliderInput("misc_expenses", "Miscellaneous (%)", 
                                     value = 10, min = 0, max = 100)
                  )
                )
              ),
              fluidRow(
                box(
                  title = "Budget Breakdown",
                  width = 12,
                  plotlyOutput("budget_pie_chart")
                )
              )
      ),
      
      # Projections Tab
      tabItem(tabName = "projections",
              fluidRow(
                box(
                  title = "Revenue Projections",
                  width = 12,
                  plotlyOutput("revenue_projection_chart")
                )
              ),
              fluidRow(
                box(
                  title = "KPI Dashboard",
                  width = 12,
                  DTOutput("kpi_table")
                )
              )
      )
    )
  )
)

# Server Logic : Budget calculations
server <- function(input, output) {
  
  # Reactive Budget Calculations
  budget_data <- reactive({
    data.frame(
      Category = c("Digital Marketing", "Traditional Advertising", 
                   "PR & Events", "Influencer Marketing", "Miscellaneous"),
      Percentage = c(input$digital_marketing, input$traditional_advertising,
                     input$pr_events, input$influencer_marketing, 
                     input$misc_expenses),
      Amount = c(input$digital_marketing * input$total_marketing_budget / 100,
                 
                 input$traditional_advertising * input$total_marketing_budget / 100,
                 
                 input$pr_events * input$total_marketing_budget / 100,
                 
                 input$influencer_marketing * input$total_marketing_budget / 100,
                 
                 input$misc_expenses * input$total_marketing_budget / 100)
    )
  })
  
  # Budget Pie Chart
  output$budget_pie_chart <- renderPlotly({
    plot_ly(budget_data(), labels = ~Category, values = ~Amount, type = 'pie') %>%
      layout(title = "Marketing Budget Allocation")
  })
  
  # Revenue Projection Calculations
  
  revenue_projection <- reactive({
    weeks <- 1:8
    # Basic decay model for weekly revenue
    base_revenue <- input$target_weekend1
    decay_rate <- 0.7  # 30% weekly decay , 

    
    projected_revenue <- base_revenue * decay_rate^(weeks-1)
    
    # projected revenue :
    # If base_revenue = $100,000
    #Week 1: $100,000 * 0.7^(1-1) = $100,000    (no decay)
    #Week 2: $100,000 * 0.7^(2-1) = $70,000     (30% drop)
    #Week 3: $100,000 * 0.7^(3-1) = $49,000     (another 30% drop)
    # And so on...
    
    data.frame( # 3cols : week,revenue and screens
      Week = weeks,
      Revenue = projected_revenue, # # The calculated declining revenue 
      Screens = c(input$screens_week1,  # week 1 screen
                  input$screens_week2,  # week 2 screen 
                  rep(input$screens_week2 * 0.8, 6)) # rep(value,times)
    )
  })
  
  # logic for the above data.frame() :
  # Week 1 (Opening Week): Maximum screens (say 1000)
  #Movies usually get their widest release in first week
  #Example: 1000 screens
  #Week 2: Slight drop in screens (say 800)
  #Some theaters start reducing showings
  #Example: 800 screens (20% drop)
  # Weeks 3-8: Stabilized but lower number of screens (640)
  
  # Revenue Projection Chart
  output$revenue_projection_chart <- renderPlotly({
    plot_ly(revenue_projection(), x = ~Week) %>%
      add_lines(y = ~Revenue, name = "Projected Revenue") %>%
      add_bars(y = ~Screens, name = "Screen Count", yaxis = "y2") %>%
      layout(
        title = "Weekly Revenue and Screen Count Projection",
        yaxis = list(title = "Revenue ($)"),
        yaxis2 = list(title = "Screen Count", overlaying = "y", side = "right")
      )
  })
  
  # KPI Calculations
  output$kpi_table <- renderDT({
    proj_data <- revenue_projection()
    
    # Calculate KPIs within the reactive context
    kpi_data <- data.frame(
      Metric = c(
        "Marketing Cost per Screen",
        "Expected First Weekend ROI",
        "Marketing Budget % of Production Budget",
        "Expected Total Revenue",
        "Marketing Cost per Expected Viewer"
      ),
      Value = c(
        sprintf("$%.2f", input$total_marketing_budget / input$screens_week1),
        sprintf("%.1f%%", (input$target_weekend1 / input$total_marketing_budget - 1) * 100),
        sprintf("%.1f%%", (input$total_marketing_budget / input$production_budget) * 100),
        sprintf("$%.2f", sum(proj_data$Revenue)),
        sprintf("$%.2f", input$total_marketing_budget / (sum(proj_data$Revenue) / input$ticket_price))
      )
    )
    
    datatable(kpi_data, options = list(dom = 't'))
  })
}

# Run the application
shinyApp(ui = ui, server = server)