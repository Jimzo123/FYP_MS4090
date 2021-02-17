list(
  n.sim <- 10000,
  
  drawdown_react <- reactive({
    return(Drawdown_Sim(input$retire_age, input$start_capital, input$withdraw_freq, input$annual_mean_return, input$annual_ret_std_dev, input$annual_inflation, input$annual_inf_std_dev, n.sim, annual_withdrawals = input$annual_withdrawals))
  }),
  
  output$drawdown_ruin_prob <- renderText({
    Spaths <- drawdown_react()
    p = p_list[match(input$withdraw_freq, freq_list)]
    n.obs =  p * exn(ILT15_female_reduced, input$retire_age)
    ruin = (length(which(Spaths[, n.obs] == 0)) * 100) / n.sim
    return(c(format(round(as.numeric(ruin), 2), nsmall = 2, big.mark = ",", scientific=FALSE), "%"))
  }),
  
  output$drawdown_average_fund <- renderText({
    Spaths <- drawdown_react()
    p = p_list[match(input$withdraw_freq, freq_list)]
    n.obs =  p * exn(ILT15_female_reduced, input$retire_age)
    average = mean(Spaths[, n.obs])
    return(c("€", format(round(as.numeric(average), 2), nsmall = 2, big.mark = ",", scientific=FALSE)))
  }),
  
  output$drawdown_sim_plot <- renderPlot({
    Spaths <- drawdown_react()
    dat <- vector("list")
    p <- ggplot()
    for (i in seq(100)) {
      dat[[i]] <- data.frame(time = (0:((p_list[match(input$withdraw_freq, freq_list)] * exn(ILT15_female_reduced, input$retire_age)))), capital = Spaths[i,])
      p <- p + geom_line(data = dat[[i]], mapping = aes(x = time, y = capital), col = i)
    } 
    return(p)
  }),
  
  output$life_ex <- renderText({
    ex = exn(ILT15_female_reduced, input$retire_age)
    return(c(format(round(as.numeric(ex), 2), nsmall = 2, big.mark = ",", scientific=FALSE), " Years"))
  }),
  
  observeEvent(input$resim, {
    updateNumericInputIcon(session, "start_capital", value = input$start_capital + 1)
    updateNumericInputIcon(session, "start_capital", value = input$start_capital)
  }),
  
  output$drawdown_sim_plot_three <- renderPlotly({
    
    Spaths <- drawdown_react()
    SpathsHigh = apply(Spaths,2,quantile,probs=0.95)
    SpathsLow = apply(Spaths,2,quantile,probs=0.05)
    SpathsMean = apply(Spaths,2,quantile,probs=0.5)
    df = data.frame(Time = rep(0:((p_list[match(input$withdraw_freq, freq_list)] * exn(ILT15_female_reduced, input$retire_age))),3),
                    Capital = c(SpathsHigh,SpathsLow,SpathsMean),
                    Market = c(rep("Over-perform", (p_list[match(input$withdraw_freq, freq_list)] * exn(ILT15_female_reduced, input$retire_age)) + 1), 
                               rep("Under-perform", (p_list[match(input$withdraw_freq, freq_list)] * exn(ILT15_female_reduced, input$retire_age)) + 1), 
                               rep("Expected", (p_list[match(input$withdraw_freq, freq_list)] * exn(ILT15_female_reduced, input$retire_age)) + 1)))
    
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

    return(ggplotly(graph))
    
  }),
  
  output$main <- renderUI( {
    dynamicUi()
  }),
  
  dynamicUi <- reactive({
    # Initially it shows a welcome message.
    if (input$Submit %% (num.quest+2) == 0)
    {results <<- rep("", nrow(Qlist))
    names(results)  <<- Qlist[,2]
    return(
      list(
        h5("Click next to begin survey")
      )
    )}
    
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
          h2(textOutput("portfolio"))
        )
      )  
    
  }),
  
  output$save.results <- renderText({
    if ((input$Submit %% (num.quest+2)>0)&(input$Submit %% (num.quest+2)<=nrow(Qlist)))
      try(results[input$Submit] <<- input$survey)
    
    ""
  }),
  
  output$question <- renderText({
    paste0(
      "Q", input$Submit %% (num.quest+2),":",
      Qlist[input$Submit %% (num.quest+2),2]
    )
  }),
  
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
  }),
  
  
  portfolioA <- function(){
    updateNumericInputIcon(session,"annual_mean_return", value = 3)
    updateNumericInputIcon(session,"annual_ret_std_dev", value = 1)
  },
  
  portfolioB <- function(){
    updateNumericInputIcon(session,"annual_mean_return", value = 5)
    updateNumericInputIcon(session,"annual_ret_std_dev", value = 7)
  },
  
  portfolioC <- function(){
    updateNumericInputIcon(session,"annual_mean_return", value = 7)
    updateNumericInputIcon(session,"annual_ret_std_dev", value = 15)
  }
)
