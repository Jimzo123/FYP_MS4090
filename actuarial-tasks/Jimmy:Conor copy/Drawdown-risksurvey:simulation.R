library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(DT)
library(tidyverse)
library(readxl)
library(readr)
library(ggthemes)
library(gganimate)
library(hrbrthemes)
library(plotly)
library(lifecontingencies)
options(scipen=999)

#setwd("~/College/FYP/FYP_MS4090/actuarial-tasks")
Qlist <- read.csv("Qlist.csv")
myLists = vector("list", nrow(Qlist))
for(i in(1:nrow(Qlist))){
  myListX = list()
  for(j in (1:(ncol(Qlist)-2))){
    myListX[Qlist[i,j+2]] = ncol(Qlist) - 1 - j
  }
  myLists[[i]] = myListX
}

freq_list = c("Annually", "Semi-Annually", "Quarterly", "Bi-Monthly", "Monthly", "Fortnightly", "Weekly", "Daily")
p_list = c(1, 2, 4, 6, 12, 26, 52, 365)

ui <- dashboardPage(
  dashboardHeader(title = "Actuarial Tasks in R"),
  
  dashboardSidebar(
    
    sidebarMenu(
      menuItem("Drawdown Simulator", tabName = "drawdown")
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    fluidRow(
      tabItems(
        # Drawdown UI -------------------------------------------------------------
        tabItem(tabName = 'drawdown',
                
                box(h1("Drawdown Simulator"), width = 12, background = "light-blue"),
                
                sidebarLayout(
                  
                  sidebarPanel(
                    
                    tabsetPanel( type = "tabs",
                                 
                                 tabPanel("Parameters",
                                          style = "margin-top:1em",
                                          numericInputIcon(inputId = "start_capital", label = "Starting Capital:", value = 750000, min = 0, icon = icon("euro")),
                                          numericInputIcon(inputId = "annual_withdrawals", label = "Total Withdrawals per Annum:", value = 28000, min = 0, icon = icon("euro")),
                                          
                                          numericInputIcon(inputId = "annual_mean_return", label = "Mean Annual Return:", value = 5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                                          numericInputIcon(inputId = "annual_ret_std_dev", label = "Standard Deviation of Annual Return:", value = 7, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                                          numericInputIcon(inputId = "annual_inflation", label = "Mean Annual Inflation:", value = 2.5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                                          numericInputIcon(inputId = "annual_inf_std_dev", label = "Standard Inflation of Annual Inflation:", value = 1.5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                                          numericInputIcon(inputId = "n_years", label = "Time to Run (in Years):", value = 25, min = 0, max = 39, icon = list(NULL, "Years"))
                                          
                                 ),
                                 
                                 tabPanel("Risk Profiler",
                                          h6(textOutput("save.results")),
                                          h2("Portfolio suggestion tool"),
                                          uiOutput("main"),
                                          actionButton("Submit", "Next")
                                 )
                                 
                    )
                    
                  ),
                  
                  mainPanel(
                    box(title = "Drawdown Simulations", status = "primary", width = 12, solidHeader = T, plotlyOutput("drawdown_sim_plot"))
                  )
                )
                
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  Drawdown_Sim <- reactive({
    
    #-------------------------------------
    #Assignment
    #-------------------------------------
    
    p = 1
    start.capital = input$start_capital
    
    # Investment
    annual.mean.return = input$annual_mean_return / 100
    annual.ret.std.dev = input$annual_ret_std_dev / 100
    
    # Inflation
    annual.inflation = input$annual_inflation / 100
    annual.inf.std.dev = input$annual_inf_std_dev / 100
    
    # Withdrawals
    periodic.withdrawls = input$annual_withdrawals / p
    
    # Number of observations (in Years)
    n.years = input$n_years
    
    # Number of simulations
    n.sim = 10000
    
    # number of periods to simulate
    n.obs = p * n.years
    
    # periodic Investment and Inflation assumptions
    periodic.mean.return = effective2Convertible(i=annual.mean.return,k=p)/p
    periodic.ret.std.dev = annual.ret.std.dev / sqrt(p)
    
    periodic.inflation = effective2Convertible(i=annual.inflation,k=p)/p
    periodic.inf.std.dev = annual.inf.std.dev / sqrt(p)
    
    #-------------------------------------
    # Simulation
    #-------------------------------------
    
    Spaths = matrix(0, n.sim, n.obs+1)
    Spaths[,1] = start.capital
    
    periodic.invest.returns = numeric(n.obs)
    periodic.inflation.returns = numeric(n.obs)
    
    for(i in 1:n.sim){
      
      periodic.invest.returns = rnorm(n.obs, mean = periodic.mean.return, sd = periodic.ret.std.dev)
      periodic.inflation.returns = rnorm(n.obs, mean = periodic.inflation, sd = periodic.inf.std.dev)
      if (Spaths[i,1]*(1+periodic.invest.returns[1]-periodic.inflation.returns[1])-periodic.withdrawls <= 0) {break}
      Spaths[i,2] = Spaths[i,1]*(1+periodic.invest.returns[1]-periodic.inflation.returns[1])-periodic.withdrawls
      
      for(j in 2:n.obs){
        if (Spaths[i,j] <= 0) {
          break
        } else if (Spaths[i,j]*(1+periodic.invest.returns[j]-periodic.inflation.returns[j])-periodic.withdrawls <= 0) {
          break
        } else {
          Spaths[i,j+1] = Spaths[i,j]*(1+periodic.invest.returns[j]-periodic.inflation.returns[j])-periodic.withdrawls
        }
      }
    }
    
    return(Spaths)
  })
  
  
  output$drawdown_sim_plot <- renderPlotly({
    
    Spaths <- Drawdown_Sim()
    
    SpathsHigh = apply(Spaths,2,quantile,probs=0.95)
    SpathsLow = apply(Spaths,2,quantile,probs=0.05)
    SpathsMean = apply(Spaths,2,quantile,probs=0.5)
    
    df = data.frame(Time = rep(0:(input$n_years),3),
                    Capital = c(SpathsHigh,SpathsLow,SpathsMean),
                    Market = c(rep("Over-perform",input$n_years + 1), rep("Under-perform", input$n_years + 1), rep("Expected", input$n_years + 1)))
    
    graph = ggplot(data = df, aes(x = Time, y = Capital, color = Market )) +
      geom_line() +
      geom_point() +
      ggtitle("Drawdown") +
      theme_ipsum() +
      ylab("Capital") +
      theme(
        legend.position = c(.95, .80),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)
      )
    
    #dat = vector("list", 4)
    #graph = ggplot()
    
    #dat[[1]] = data.frame(time = (0:((input$n_years))), capital = SpathsHigh)
    #graph = graph  + geom_point(data = dat[[1]], mapping = aes(x = time, y = capital), col = 5) +
    #  geom_line(data = dat[[1]], mapping = aes(x = time, y = capital), col = 5) +
    #  theme(legend.position="right")
    
    #dat[[2]] = data.frame(time = (0:((input$n_years))), capital = SpathsLow)
    #graph = graph + geom_point(data = dat[[2]], mapping = aes(x = time, y = capital), col = 6) +
    #  geom_line(data = dat[[2]], mapping = aes(x = time, y = capital), col = 6) +
    #  theme(legend.position="right")
    
    #dat[[3]] = data.frame(time = (0:((input$n_years))), capital = SpathsMean)
    #graph = graph + geom_point(data = dat[[3]], mapping = aes(x = time, y = capital), col = 7) +
    #  geom_line(data = dat[[3]], mapping = aes(x = time, y = capital), col = 7) +
    #  theme(legend.position="right")
    
    #deterministic
    #dat[[4]] = data.frame(time = (0:((n.years))), capital = SpathsAvg)
    #p = p + geom_point(data = dat[[4]], mapping = aes(x = time, y = capital), col = 4)
    
    return((p = ggplotly(graph)))
    
  })
  
  portfolioA = function(){
    updateNumericInput(session,"annual_mean_return", value = 3)
    updateNumericInput(session,"annual_ret_std_dev", value = 1)
  }
  
  portfolioB = function(){
    updateNumericInput(session,"annual_mean_return", value = 5)
    updateNumericInput(session,"annual_ret_std_dev", value = 7)
  }
  
  portfolioC = function(){
    updateNumericInput(session,"annual_mean_return", value = 7)
    updateNumericInput(session,"annual_ret_std_dev", value = 15)
  }
  
  #-----------Risk Profiler---------#
  num.quest = nrow(Qlist)
  results <<- rep("", nrow(Qlist))
  names(results)  <<- Qlist[,2]
  
  output$main <- renderUI( {
    dynamicUi()
  })
  
  dynamicUi <- reactive({
    # Initially it shows a welcome message.
    if (input$Submit %% (num.quest+2) ==0)
      return(
        list(
          h5("Click next to begin survey")
        )
      )
    
    if (input$Submit %% (num.quest+2) >0 & input$Submit %% (num.quest+2) <= num.quest){
      return(
        list(
          h5(textOutput("question")),
          radioButtons("survey", "Please Select:",
                       myLists[[input$Submit %% (num.quest+2)]])
        )
      )
    }
    
    if (input$Submit %% (num.quest+2) >num.quest)
      return(
        list(
          h4("Results:"),
          #tableOutput("surveyresults"),
          h2(textOutput("portfolio"))
        )
      )  
    
  })
  
  output$save.results <- renderText({
    if ((input$Submit %% (num.quest+2)>0)&(input$Submit %% (num.quest+2)<=nrow(Qlist)))
      try(results[input$Submit] <<- input$survey)
    
    ""
  })
  
  output$surveyresults <- renderTable({
    t(results)
  })
  
  
  output$question <- renderText({
    paste0(
      "Q", input$Submit %% (num.quest+2),":",
      Qlist[input$Submit %% (num.quest+2),2]
    )
  })
  
  output$portfolio <- renderText({
    num.results = parse_vector(results, col_integer())
    x = sum(num.results[(input$Submit-num.quest):(input$Submit-1)])
    if(x < 12){
      portfolioA()
      paste0("Low risk appetite: Final score ", x )
    } else if (x < 19){
      portfolioB()
      paste0("Moderate risk appetite: Final score ", x)
    } else{
      portfolioC()
      paste0("High risk appetite: Final score ", x)
    }
  })
  
}

# Run the application
shinyApp(ui, server)
        