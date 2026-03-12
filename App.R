library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Degree Quantifier"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Pay in Cash", tabName = "CashPayment", icon = icon("th")),
      menuItem("Pay with Student Loan", tabName = "LoanOption", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tabItems(
      # First page content
      tabItem(tabName = "CashPayment",
              titlePanel("Quantify Your College Degree"),
              helpText("Lots of factors should be considered when making a decision to go certain college or not. These factors could be:
           how much passion we have for certain study field?
           what is our career plan?
           whether it is financially making sense?
           school reputation, scholarship, family opinions...
           This is a demo project to show how we make this decision solely based on cost and return, which I believe it should not be the sole factor that we consider..."),
              
              sidebarLayout(
                sidebarPanel(
                  numericInput("Tuition", "Total Tuition Cost ($):", value = 50000, min = 0),
                  numericInput("Current_Salary", "Current Annual Salary (Do Your Best Estimation) ($):", value = 40000, min = 0),
                  numericInput("Income_During_Study", "Any Annual Income Estimation During Study? ($):", value = 10000, min = 0),
                  numericInput("Program_Duration", "How Many Years To Finish the Degree:", value = 4, min = 0),
                  numericInput("Salary_After_School", "Annual Salary First Year After Study (Do Your Best Estimation) ($):", value = 80000, min = 0),
                  numericInput("Salary_Increase_Percentage", "Annual Salary Increase Factor (Do Your Best Estimation):", value = 0.05, min = 0),
                  sliderInput("Years_Post_Study",
                              "How Soon Would You Expect to Recover the College Cost (Years):",
                              min = 1,
                              max = 10,
                              value = 3),
                  hr(),
                  helpText("Tuition: please only fill up out-of-pocket expenses only. Current Salary: please do your best to estimate about the salary if you go out to get a job right now. I do recognize that not every college student works full time before they go to college, but this is the opportunity cost to attend a college. Income during study: do your best estimation about some income such as part time job or scholarship. ")
                ),
                
                mainPanel(
                  h3("Expected Return"),
                  wellPanel(
                    textOutput("net_profit"),
                    tags$h2(textOutput("roi_pct"), style = "color: #2c3e50;")
                  ),
                  plotOutput("roi_plot", height = "300px"),
                  wellPanel(
                    tags$h2(textOutput("profitability"), style = "color: #2c3e50;")
                  )
                )
              )
      ),
      
      # Second page content
      tabItem(tabName = "LoanOption",
              h2("Quantify Your College Dgree"),
              helpText("Well, most of students in USA would use student loan. This introduced an interesting issue: interests. We'll get you covered!"),
              fluidRow(
                box(
                  title = "Placeholder Title",
                  width = 12,
                  height = 1200,
                  status = "primary",
                  solidHeader = FALSE,
                  background = NULL, # Default is white
                  "Under developing..."
                )
              )
      )
    )
  )
  
)

server <- function(input, output, session) {
  
  ##calculate cost and return
  cost = reactive({
    opp_cost = input$Current_Salary -input$Income_During_Study
    for (j in 1: input$Program_Duration-1){
      opp_cost = opp_cost + opp_cost * (1 + input$Salary_Increase_Percentage)^j
    }
    return(input$Tuition + opp_cost)
  })
  
  
  revenue = reactive({
    rev_t = input$Salary_After_School - input$Current_Salary *(1+input$Salary_Increase_Percentage)^input$Program_Duration
    for ( i in 1: input$Years_Post_Study - 1){
      rev_t = rev_t + rev_t * (1 + input$Salary_Increase_Percentage) ^ i
    }
    return(rev_t)
  })
  # Calculate Net Profit
  profit <- reactive({revenue() - cost()})
  
  # Calculate ROI percentage
  roi_val <- reactive({
    req(input$Tuition > 0) # Ensure cost is not zero to avoid error
    (profit() / cost()) * 100
  })
  
  output$net_profit <- renderText({
    paste("Total Net Profit: $", format(profit(), big.mark = ","))
  })
  
  output$roi_pct <- renderText({
    paste0("ROI: ", round(roi_val(), 2), "%")
  })
  
  # Optional: Simple bar chart comparing Cost vs Revenue
  output$roi_plot <- renderPlot({
    barplot(c(cost(), revenue()), 
            names.arg = c("Investment Cost", "Total Revenue"),
            col = c("#DB4437", "#4285F4"),
            main = "Cost vs. Revenue Comparison",
            ylab = "Amount ($)")
  })
  
  output$profitability <- renderText({
    # Conditional logic
    if (profit() > 0) {
      "The degree is worth going! "
    } else {
      "The estimated return from this degree is not meeting expectation."
    }
  })
  
}

shinyApp(ui, server)
