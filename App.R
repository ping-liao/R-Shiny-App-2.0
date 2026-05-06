# ---------------------------------------------------------------
# Quantify Your Degree
#
# A Shiny app that estimates the financial ROI of a U.S. college
# degree under both cash-pay and student-loan financing scenarios.
#
# Author: Ping Liao
# Repo:   https://github.com/ping-liao/R-Shiny-App-2.0
# Live:   https://pingliao.shinyapps.io/Quantify_Your_Degree/
# ---------------------------------------------------------------

library(shiny)
library(shinydashboard)

# ---- Finance helpers ------------------------------------------

# Sum of the geometric series 1 + (1+r) + (1+r)^2 + ... + (1+r)^(n-1).
# Closed-form is faster, more accurate, and avoids the off-by-one
# loop bugs from the previous version of this app.
geom_sum <- function(r, n) {
  if (n <= 0) return(0)
  if (abs(r) < 1e-9) return(n)
  ((1 + r)^n - 1) / r
}

# Total opportunity cost across `years` of study, assuming the
# student forgoes (current_salary - income_during_study) in year 0
# and that this gap grows at `growth` each subsequent study year.
opportunity_cost <- function(current_salary, income_during_study, years, growth) {
  base <- max(current_salary - income_during_study, 0)
  base * geom_sum(growth, years)
}

# Total post-graduation earnings uplift over a `horizon`-year
# recovery window. In year t (1..horizon):
#   post-school salary  = salary_after  * (1 + growth)^(t - 1)
#   counterfactual wage = current_salary * (1 + growth)^(years + t - 1)
# Net uplift summed across the horizon collapses to:
earnings_uplift <- function(salary_after, current_salary, years, horizon, growth) {
  per_year_delta <- salary_after - current_salary * (1 + growth)^years
  per_year_delta * geom_sum(growth, horizon)
}

# Standard amortizing-loan monthly payment.
loan_monthly_payment <- function(principal, apr, term_years) {
  if (principal <= 0 || term_years <= 0) return(0)
  m <- apr / 12
  n <- term_years * 12
  if (abs(m) < 1e-9) return(principal / n)
  principal * m / (1 - (1 + m)^(-n))
}

# Total interest paid over the life of an amortizing loan.
loan_total_interest <- function(principal, apr, term_years) {
  pay <- loan_monthly_payment(principal, apr, term_years)
  if (pay == 0) return(0)
  pay * term_years * 12 - principal
}

# Render a USD figure as a thousand-separated dollar string.
fmt_usd <- function(x) {
  paste0("$", format(round(x), big.mark = ",", scientific = FALSE))
}

# Shared bar chart for cost vs. revenue, used by both tabs.
plot_cost_vs_revenue <- function(cost, revenue, title) {
  barplot(
    c(cost, revenue),
    names.arg = c("Investment Cost", "Total Revenue"),
    col       = c("#DB4437", "#4285F4"),
    main      = title,
    ylab      = "Amount ($)",
    border    = NA
  )
}

# ---- UI ------------------------------------------------------

ui <- dashboardPage(
  dashboardHeader(title = "Degree Quantifier"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Pay in Cash",           tabName = "CashPayment", icon = icon("wallet")),
      menuItem("Pay with Student Loan", tabName = "LoanOption",  icon = icon("hand-holding-usd"))
    )
  ),
  dashboardBody(
    tabItems(

      # ----- Cash tab -----
      tabItem(
        tabName = "CashPayment",
        titlePanel("Quantify Your College Degree — Cash Pay"),
        helpText(
          "This is a back-of-the-envelope ROI calculator. Many factors matter ",
          "in choosing a college (passion, fit, scholarships, family) — this ",
          "demo isolates the financial dimension only."
        ),
        sidebarLayout(
          sidebarPanel(
            numericInput("Tuition",              "Total Tuition Cost ($):",                   value = 50000, min = 0),
            numericInput("Current_Salary",       "Current Annual Salary ($):",                value = 40000, min = 0),
            numericInput("Income_During_Study",  "Income During Study ($/yr):",               value = 10000, min = 0),
            numericInput("Program_Duration",     "Years to Finish the Degree:",               value = 4,     min = 1),
            numericInput("Salary_After_School",  "Expected Salary First Year After Study ($):", value = 80000, min = 0),
            numericInput("Salary_Increase_Percentage",
                         "Annual Salary Growth Rate (e.g. 0.05 = 5%):",
                         value = 0.05, min = 0, step = 0.01),
            sliderInput("Years_Post_Study",
                        "Years to Recover the Cost:",
                        min = 1, max = 30, value = 5),
            hr(),
            helpText(
              "Tuition: out-of-pocket costs only. Current Salary: best estimate of what ",
              "you would earn if you skipped college (the opportunity cost). Income During Study: ",
              "part-time wages, scholarships, or stipends that offset that cost."
            )
          ),
          mainPanel(
            h3("Expected Return"),
            wellPanel(
              textOutput("net_profit"),
              tags$h2(textOutput("roi_pct"), style = "color: #2c3e50;")
            ),
            plotOutput("roi_plot", height = "300px"),
            wellPanel(
              tags$h3(textOutput("profitability"), style = "color: #2c3e50;")
            )
          )
        )
      ),

      # ----- Loan tab -----
      tabItem(
        tabName = "LoanOption",
        titlePanel("Quantify Your College Degree — Student Loan"),
        helpText(
          "Most U.S. students borrow. This tab folds amortized loan interest ",
          "into the cost side and recomputes ROI."
        ),
        sidebarLayout(
          sidebarPanel(
            h4("Degree assumptions"),
            numericInput("L_Tuition",              "Total Tuition Cost ($):",                   value = 50000, min = 0),
            numericInput("L_Current_Salary",       "Current Annual Salary ($):",                value = 40000, min = 0),
            numericInput("L_Income_During_Study",  "Income During Study ($/yr):",               value = 10000, min = 0),
            numericInput("L_Program_Duration",     "Years to Finish the Degree:",               value = 4,     min = 1),
            numericInput("L_Salary_After_School",  "Expected Salary First Year After Study ($):", value = 80000, min = 0),
            numericInput("L_Salary_Increase_Percentage",
                         "Annual Salary Growth Rate:",
                         value = 0.05, min = 0, step = 0.01),
            sliderInput("L_Years_Post_Study",
                        "Years to Recover the Cost:",
                        min = 1, max = 30, value = 10),
            hr(),
            h4("Loan assumptions"),
            numericInput("L_Principal",
                         "Loan Principal ($) — portion of tuition financed:",
                         value = 40000, min = 0),
            numericInput("L_APR",
                         "Annual Interest Rate (e.g. 0.065 = 6.5%):",
                         value = 0.065, min = 0, step = 0.005),
            numericInput("L_Term",
                         "Loan Term (years):",
                         value = 10, min = 1, max = 30)
          ),
          mainPanel(
            h3("Loan Summary"),
            wellPanel(
              textOutput("loan_payment"),
              textOutput("loan_total_interest"),
              textOutput("loan_total_paid")
            ),
            h3("Expected Return (incl. interest)"),
            wellPanel(
              textOutput("L_net_profit"),
              tags$h2(textOutput("L_roi_pct"), style = "color: #2c3e50;")
            ),
            plotOutput("L_roi_plot", height = "300px"),
            wellPanel(
              tags$h3(textOutput("L_profitability"), style = "color: #2c3e50;")
            )
          )
        )
      )
    )
  )
)

# ---- Server --------------------------------------------------

server <- function(input, output, session) {

  # ----- Cash scenario -----
  cash_cost <- reactive({
    req(input$Program_Duration > 0)
    opp <- opportunity_cost(
      current_salary      = input$Current_Salary,
      income_during_study = input$Income_During_Study,
      years               = input$Program_Duration,
      growth              = input$Salary_Increase_Percentage
    )
    input$Tuition + opp
  })

  cash_revenue <- reactive({
    req(input$Years_Post_Study > 0)
    earnings_uplift(
      salary_after   = input$Salary_After_School,
      current_salary = input$Current_Salary,
      years          = input$Program_Duration,
      horizon        = input$Years_Post_Study,
      growth         = input$Salary_Increase_Percentage
    )
  })

  cash_profit <- reactive({ cash_revenue() - cash_cost() })
  cash_roi    <- reactive({
    req(cash_cost() > 0)
    100 * cash_profit() / cash_cost()
  })

  output$net_profit <- renderText({
    paste0("Total Net Profit: ", fmt_usd(cash_profit()))
  })
  output$roi_pct <- renderText({
    paste0("ROI: ", round(cash_roi(), 2), "%")
  })
  output$roi_plot <- renderPlot({
    plot_cost_vs_revenue(cash_cost(), cash_revenue(), "Cost vs. Revenue (Cash Pay)")
  })
  output$profitability <- renderText({
    if (cash_profit() > 0)
      "Verdict: financially worthwhile."
    else
      "Verdict: estimated return falls short."
  })

  # ----- Loan scenario -----
  loan_pay  <- reactive({ loan_monthly_payment(input$L_Principal, input$L_APR, input$L_Term) })
  loan_int  <- reactive({ loan_total_interest(input$L_Principal, input$L_APR, input$L_Term) })
  loan_paid <- reactive({ input$L_Principal + loan_int() })

  loan_cost <- reactive({
    req(input$L_Program_Duration > 0)
    opp <- opportunity_cost(
      current_salary      = input$L_Current_Salary,
      income_during_study = input$L_Income_During_Study,
      years               = input$L_Program_Duration,
      growth              = input$L_Salary_Increase_Percentage
    )
    out_of_pocket <- max(input$L_Tuition - input$L_Principal, 0)
    out_of_pocket + loan_paid() + opp
  })

  loan_revenue <- reactive({
    req(input$L_Years_Post_Study > 0)
    earnings_uplift(
      salary_after   = input$L_Salary_After_School,
      current_salary = input$L_Current_Salary,
      years          = input$L_Program_Duration,
      horizon        = input$L_Years_Post_Study,
      growth         = input$L_Salary_Increase_Percentage
    )
  })

  loan_profit <- reactive({ loan_revenue() - loan_cost() })
  loan_roi    <- reactive({
    req(loan_cost() > 0)
    100 * loan_profit() / loan_cost()
  })

  output$loan_payment <- renderText({
    paste0("Estimated Monthly Payment: ", fmt_usd(loan_pay()))
  })
  output$loan_total_interest <- renderText({
    paste0("Total Interest Paid: ", fmt_usd(loan_int()))
  })
  output$loan_total_paid <- renderText({
    paste0("Total Loan Outflow (principal + interest): ", fmt_usd(loan_paid()))
  })
  output$L_net_profit <- renderText({
    paste0("Total Net Profit: ", fmt_usd(loan_profit()))
  })
  output$L_roi_pct <- renderText({
    paste0("ROI: ", round(loan_roi(), 2), "%")
  })
  output$L_roi_plot <- renderPlot({
    plot_cost_vs_revenue(loan_cost(), loan_revenue(), "Cost vs. Revenue (Student Loan)")
  })
  output$L_profitability <- renderText({
    if (loan_profit() > 0)
      "Verdict: financially worthwhile, even after interest."
    else
      "Verdict: estimated return falls short of total loan cost."
  })
}

shinyApp(ui, server)