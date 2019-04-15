# Workout02, Stat 133, Fall 2019, Eunice Sou
# Shiny Workout 2: Saings/Investments Scenario
# Inputs:
#   amount: initial amount 
#   contrib: annual contributions
#   rate: return rate
#   growth: growth rate
#   years: years
#   facet: facet
#
# Outputs:
#   timelines of savings/investments scenario

library(shiny)
library(ggplot2)
library(reshape2)

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Savings/Investment Scenario"),
  
  # Sidebar  
  fluidRow(
    column(4,
           sliderInput("amount",
                       label = "Initial Amount",
                       min = 1,
                       max = 100000,
                       value = 1000,
                       step = 500, pre = "$", sep = ","),
           sliderInput("contrib",
                       label = "Annual Contribution",
                       min = 0,
                       max = 50000,
                       value = 2000,
                       step = 500, pre = "$", sep = ",")
           ),
    column(4, offset = 0.4,
           sliderInput("rate",
                       label = "Return Rate (in %)",
                       min = 0,
                       max = 20,
                       value = 5,
                       step = 0.1),
           sliderInput("growth",
                       label = "Growth Rate (in %)",
                       min = 0,
                       max = 20,
                       value = 2,
                       step = 0.1)
           ),
    column(4,
           sliderInput("years",
                       label = "Years",
                       min = 0,
                       max = 50,
                       value = 10,
                       step = 1),
           selectInput("facet",
                       "Facet?",
                       c("No", "Yes"))
           
    )
  ), 
  
   # Plot
   hr(),
   h4("Timelines"),
   plotOutput("time_plot"),
   hr(),
   h4("Balances"),
   verbatimTextOutput("summary_table")
    
  
)

# Define server logic required to draw the plot
server <- function(input, output) {
  
  ##1) Future Value Function
  #' @title Future Value Function
  #' @description 
  #' @param amount initial invested amount 
  #' @param rate annual rate of return
  #' @param years number of years
  #' @return future value of an investment
  
  future_value <- function(amount, rate, years) {
    return(amount*(1+rate)^years)
  }
  
  
  ##2) Future Value of Annuity 
  
  #' @title Future Value of Annuity 
  #' @description 
  #' @param contrib contributed amount 
  #' @param rate annual rate of return
  #' @param years number of years
  #' @return Future value of an annuity
  
  annuity <- function(contrib,rate,years) {
    return(contrib*((((1+rate)^years)-1)/rate))
  }
  
  
  ##3) Future Value of Growing Annuity 
  
  #' @title Future Value of Growing Annuity
  #' @description 
  #' @param contrib contrib: contributed amount 
  #' @param rate rate: annual rate of return
  #' @param growth growth: annual growth rate
  #' @param years years: number of years
  #' @return Future value of growing annuity 
  
  growing_annuity <- function(contrib, rate, growth, years) {
    return(contrib*((((1+rate)^years)-(1+growth)^years)/(rate-growth)))
  }
  
  ##Investing Modalities
  ## For-loop 
  
  #no_contrib
modalities <- reactive({ 
  return_rate <- input$rate /100
  growth_rate <- input$growth /100
  
  modalities <- data.frame("year" = 0:input$years, 
                           "no_contrib" =rep(input$amount, input$years + 1),
                           "fixed_contrib" =rep(input$amount, input$years + 1),
                           "growing_contrib" = rep(input$amount, input$years + 1)
  )
  
  for(i in 0:input$years){
    fv <- future_value(input$amount, return_rate, i)
    fva <- annuity(input$contrib, return_rate, i)
    fvga <- growing_annuity(input$contrib, return_rate, growth_rate, i)
    modalities$no_contrib[i+1] <- fv
    modalities$fixed_contrib[i+1] <- fv + fva
    modalities$growing_contrib[i+1] <- fv + fvga
  }
  
  modalities
  })  
  
  # Fill in the spot we created for a plot
  output$time_plot <- renderPlot({
    
    
    melt_modalities <- melt(modalities(), id.vars = "year")
    
    if(input$facet == "Yes") {
      ggplot(data = melt_modalities, aes(x = year, y = value)) +
        geom_point(aes(color = variable)) +
        geom_line(aes(color = variable), size = 1) +
        ggtitle("Three Modes of Investing") +
        geom_area(aes(fill = variable), alpha = 0.5) +
        facet_grid(~ variable) +
        theme_bw()
      
     } else{
    
    ggplot(data = melt_modalities, aes(x = year, y = value)) +
      geom_point(aes(color = variable)) +
         geom_line(aes(color = variable), size = 1) +
         ggtitle("Three Modes of Investing")
     }
       })
  
  output$summary_table <- renderPrint({
    print(modalities(), print.gap = 2)
  })
  
}

  # Run the application 
  shinyApp(ui = ui, server = server)


