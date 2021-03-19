list(
  box(h1("Sequencing Risk Demonstration"), width = 12, background = "light-blue"),
  
  column(width = 4, 
             tabBox(
               title = "Inputs",
               id = "tabset3", width = 12, height = "520px",
               tabPanel("Initial inputs",
                        fluidRow(
                          column(10,offset = 1,
                                 actionButton(inputId = "seq_resim1", label = "Re-Run Simulation", style = "background-color: white; float:right", icon("random")),
                                 br(),br(),
                                 numericInputIcon(inputId = "seq_start_capital", label = "Starting Capital:", value = 750000, min = 0, icon = icon("euro")),
                                 awesomeRadio("seq_withdraw_type", "Withdrawal Type:", choices = list("Fixed" = F, "Percentage" = T), inline = TRUE),
                                 div(id = "seq_annual_withdrawals_input", numericInputIcon(inputId = "seq_annual_withdrawals", label = "Total Withdrawals per Annum:", value = 30000, min = 0, icon = icon("euro"))),
                                 div(id = "seq_percent_withdrawal_input", numericInputIcon(inputId = "seq_percent_withdrawal", label = "Percentage Withdrawn per Annum:", value = 4, min = 0, max = 100, icon = list(NULL, icon("percent")))),
                                 br(),  
                                 #numericInputIcon(inputId = "seq_percent_withdrawal", label = "Percentage Withdrawn per Annum:", value = 4, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                                 numericInputIcon(inputId = "seq_n_years", label = "Number of Years:", value = 25, min = 0, icon = list(NULL, "Years")),
                                 selectInput("seq_withdraw_freq", "Withdrawal Frequency:", freq_list)
                                 ),
                                 
                        )),
               
               tabPanel("Assumptions", 
                        fluidRow(
                          column(10, offset = 1,
                                 actionButton(inputId = "seq_resim2", label = "Re-Run Simulation", style = "background-color: white; float:right", icon("random")),
                                 br(),br(),
                                 numericInputIcon(inputId = "seq_annual_mean_return", label = "Mean Annual Return:", value = 5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                                 numericInputIcon(inputId = "seq_best_annual_return", label = "Highest Annual Return Achieved:", value = 15, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                                 br(),
                                 numericInputIcon(inputId = "seq_worst_annual_return", label = "Lowest Annual Return Achieved:", value = -5, min = -100, max = 100, icon = list(NULL, icon("percent"))),
                                 numericInputIcon(inputId = "seq_annual_inflation", label = "Annual Inflation:", value = 2.5, min = 0, max = 100, icon = list(NULL, icon("percent")))
                                 
                                 
                          )
                        ))
               
             )
             ),
  tabBox(
    id = "tabset4", width = 8, height = "520px",
    tabPanel("Summary",
             plotlyOutput("sequencing_sim_plot_three"),
    ),
    tabPanel("Table", DT::dataTableOutput("seq_drawdown_table"), rownames= FALSE, style = "height:430px; overflow-y: scroll;overflow-x: scroll;"
             
    )
    
  ),
  
  box(status = "primary", width = 12, solidHeader = T, title = "What is sequencing risk?",
      h5("Sequencing risk is the danger that the timing of withdrawals from a retirement account will have a negative impact on the overall rate of return available to the investor. 
                     This can have a significant impact on a retiree who depends on the income from a lifetime of investing and is no longer contributing new capital that could offset losses.")
  )
)
