list(
# Input Function ----------------------------------------------------------
  sd_inputs <- eventReactive({input$sd_resim1; input$sd_resim2; input$sd_resim3; input$sd_resim4; input$sd_submit %% (num.quest + 2) > num.quest}, {
    return(reactiveValuesToList(input))
  }, ignoreNULL = FALSE),

# Reactive Functions - SORP ------------------------------------------------------
  sd_annuity_reactive <- eventReactive({input$sd_resim1; input$sd_resim2; input$sd_resim3; input$sd_resim4; input$sd_submit %% (num.quest + 2) > num.quest}, {
    sd_inputs = sd_inputs()
    return(SORP_Annuity(age_1 = sd_inputs$sd_age[1], 
                      age_2 = sd_inputs$sd_age[2], 
                      relationship = sd_inputs$sd_relationship, 
                      freq = sd_inputs$sd_post_freq, 
                      annuity_interest = sd_inputs$sd_annuity_interest, 
                      annuity_esc = sd_inputs$sd_annuity_esc, 
                      guaranteed = sd_inputs$sd_guaranteed))
  }, ignoreNULL = FALSE),

  sd_annuity_spouse_reactive <- eventReactive({input$sd_resim1; input$sd_resim2; input$sd_resim3; input$sd_resim4; input$sd_submit %% (num.quest + 2) > num.quest}, {
    sd_inputs = sd_inputs()
    if(sd_inputs$sd_relationship == 2){
      return(SORP_Annuity(age_1 = sd_inputs$sd_age[1], 
                          age_2 = sd_inputs$sd_age[2], 
                          relationship = sd_inputs$sd_relationship, 
                          freq = sd_inputs$sd_post_freq_spouse, 
                          annuity_interest = sd_inputs$sd_annuity_interest, 
                          annuity_esc = sd_inputs$sd_annuity_esc, 
                          guaranteed = sd_inputs$sd_guaranteed))
    } else {
      return(NULL)
    }
  }, ignoreNULL = FALSE),

  sd_contributions_reactive <- eventReactive({input$sd_resim1; input$sd_resim2; input$sd_resim3; input$sd_resim4; input$sd_submit %% (num.quest + 2) > num.quest}, {
    sd_inputs = sd_inputs()
    return(SORP_Contributions(age_1 = sd_inputs$sd_age[1], 
                            age_2 = sd_inputs$sd_age[2], 
                            salary = sd_inputs$sd_salary, 
                            current_fundvalue = sd_inputs$sd_current_fundvalue, 
                            freq = sd_inputs$sd_pre_freq, 
                            emp_contri = sd_inputs$sd_emp_contri, 
                            empr_contri = sd_inputs$sd_empr_contri, 
                            salary_esc = sd_inputs$sd_salary_esc,
                            investment_charges = sd_inputs$sd_investment_charges,
                            equity_prop = sd_inputs$sd_equity_prop,
                            equity_rate = sd_inputs$sd_equity_rate,
                            fixed_prop = sd_inputs$sd_fixed_prop, 
                            fixed_rate = sd_inputs$sd_fixed_rate, 
                            cash_prop = sd_inputs$sd_cash_prop, 
                            cash_rate = sd_inputs$sd_cash_rate))
  }, ignoreNULL = FALSE),

  sd_contributions_spouse_reactive <- eventReactive({input$sd_resim1; input$sd_resim2; input$sd_resim3; input$sd_resim4; input$sd_submit %% (num.quest + 2) > num.quest}, {
    sd_inputs = sd_inputs()
    if(sd_inputs$sd_relationship == 2){
      return(SORP_Contributions(age_1 = sd_inputs$sd_age[1], 
                                age_2 = sd_inputs$sd_age[2], 
                                salary = sd_inputs$sd_salary_spouse, 
                                current_fundvalue = sd_inputs$sd_current_fundvalue_spouse, 
                                freq = sd_inputs$sd_pre_freq_spouse, 
                                emp_contri = sd_inputs$sd_emp_contri_spouse, 
                                empr_contri = sd_inputs$sd_empr_contri_spouse, 
                                salary_esc = sd_inputs$sd_salary_esc,
                                investment_charges = sd_inputs$sd_investment_charges,
                                equity_prop = sd_inputs$sd_equity_prop,
                                equity_rate = sd_inputs$sd_equity_rate,
                                fixed_prop = sd_inputs$sd_fixed_prop, 
                                fixed_rate = sd_inputs$sd_fixed_rate, 
                                cash_prop = sd_inputs$sd_cash_prop, 
                                cash_rate = sd_inputs$sd_cash_rate))
    } else {
      return(NULL)
    }
  }, ignoreNULL = FALSE),

  sd_fund_reactive <- eventReactive({input$sd_resim1; input$sd_resim2; input$sd_resim3; input$sd_resim4; input$sd_submit %% (num.quest + 2) > num.quest}, {
    sd_inputs = sd_inputs()
    return(SORP_Fund(SORP_Contributions = sd_contributions_reactive()))
    
  }, ignoreNULL = FALSE),

  sd_fund_spouse_reactive <- eventReactive({input$sd_resim1; input$sd_resim2; input$sd_resim3; input$sd_resim4; input$sd_submit %% (num.quest + 2) > num.quest}, {
    sd_inputs = sd_inputs()
    if(sd_inputs$sd_relationship == 2){
      return(SORP_Fund(SORP_Contributions = sd_contributions_spouse_reactive()))
      } else {
          return(NULL)
      }
  }, ignoreNULL = FALSE),

  sd_pension_payment_reactive <- eventReactive({input$sd_resim1; input$sd_resim2; input$sd_resim3; input$sd_resim4; input$sd_submit %% (num.quest + 2) > num.quest}, {
    sd_inputs = sd_inputs()
    return(SORP_Pension_Payment(SORP_Fund = sd_fund_reactive(), 
                                SORP_Annuity = sd_annuity_reactive(), 
                                freq = sd_inputs$sd_post_freq))
  }, ignoreNULL = FALSE),

  sd_pension_payment_spouse_reactive <- eventReactive({input$sd_resim1; input$sd_resim2; input$sd_resim3; input$sd_resim4; input$sd_submit %% (num.quest + 2) > num.quest}, {
    sd_inputs = sd_inputs()
    if(sd_inputs$sd_relationship == 2){
      return(SORP_Pension_Payment(SORP_Fund = sd_fund_spouse_reactive(), 
                                  SORP_Annuity = sd_annuity_spouse_reactive(), 
                                  freq = sd_inputs$sd_post_freq_spouse))
    } else {
      return(NULL)
    }
  }, ignoreNULL = FALSE),

# Reactive Functions - Drawdown ---------------------------------------
  sd_starting_capital_reactive <- eventReactive({input$sd_resim1; input$sd_resim2; input$sd_resim3; input$sd_resim4; input$sd_submit %% (num.quest + 2) > num.quest}, {
    sd_inputs = sd_inputs()
    if(sd_inputs$sd_relationship != 1){
      sd_start_capital = sd_fund_reactive() + sd_fund_spouse_reactive()
    } else {
      sd_start_capital = sd_fund_reactive()
    }
  }, ignoreNULL = FALSE),

  sd_simulations_reactive <- eventReactive({input$sd_resim1; input$sd_resim2; input$sd_resim3; input$sd_resim4; input$sd_submit %% (num.quest + 2) > num.quest}, {
    sd_inputs = sd_inputs()
    return(Drawdown_Simulations(retire_age = sd_inputs$sd_age[2], 
                                start_capital = sd_starting_capital_reactive(), 
                                withdraw_freq = sd_inputs$sd_withdraw_freq, 
                                annual_mean_return = sd_inputs$sd_annual_mean_return, 
                                annual_ret_std_dev = sd_inputs$sd_annual_ret_std_dev, 
                                annual_inflation = sd_inputs$sd_annual_inflation, 
                                annual_inf_std_dev = sd_inputs$sd_annual_inf_std_dev, 
                                n_sim = 10000, 
                                withdraw_type = sd_inputs$sd_withdraw_type, 
                                annual_withdrawals = sd_inputs$sd_annual_withdrawals, 
                                percent_withdrawal = sd_inputs$sd_percent_withdrawal))
  }, ignoreNULL = FALSE),

  sd_paths_reactive <- eventReactive({input$sd_resim1; input$sd_resim2; input$sd_resim3; input$sd_resim4; input$sd_submit %% (num.quest + 2) > num.quest}, {
    return(Drawdown_Paths(sd_simulations_reactive()))
  }, ignoreNULL = FALSE),

  sd_withdrawals_reactive <- eventReactive({input$sd_resim1; input$sd_resim2; input$sd_resim3; input$sd_resim4; input$sd_submit %% (num.quest + 2) > num.quest}, {
    return(Drawdown_Withdrawals(sd_simulations_reactive()))
  }, ignoreNULL = FALSE),

  sd_life_ex_reactive <- eventReactive({input$sd_resim1; input$sd_resim2; input$sd_resim3; input$sd_resim4; input$sd_submit %% (num.quest + 2) > num.quest}, {
    sd_inputs = sd_inputs()
    return(round_to_fraction(exn(ILT15_female_reduced, sd_inputs$sd_age[2]), p_list[match(sd_inputs$sd_withdraw_freq, freq_list_drawdown)]))
  }, ignoreNULL = FALSE),

# Output Functions - SORP -------------------------------------------------
  output$sd_text_fundvalue <- renderText({
    return(c("€", round_2d(sd_fund_reactive(), T)))
  }),

  output$sd_text_fundvalue_spouse <- renderText({
    return(c("€", round_2d(sd_fund_spouse_reactive(), T)))
  }),

  output$sd_text_pension_payment <- renderText({
    return(c("€", round_2d(sd_pension_payment_reactive(), T)))
  }),

  output$sd_text_pension_payment_spouse <- renderText({
    return(c("€", round_2d(sd_pension_payment_spouse_reactive(), T)))
  }),

  output$sd_text_fundvalue_discounted <- renderText({
    sd_inputs = sd_inputs()
    discounted_fund = SORP_Discount(x = sd_fund_reactive(),
                                    age_1 = sd_inputs$sd_age[1], 
                                    age_2 = sd_inputs$sd_age[2],
                                    discount_rate = sd_inputs$sd_discount_rate)
    return(c("€", round_2d(discounted_fund, T)))
  }),

  output$sd_text_fundvalue_discounted_spouse <- renderText({
    sd_inputs = sd_inputs()
    discounted_fund_spouse = SORP_Discount(x = sd_fund_spouse_reactive(),
                                    age_1 = sd_inputs$sd_age[1], 
                                    age_2 = sd_inputs$sd_age[2],
                                    discount_rate = sd_inputs$sd_discount_rate)
    return(c("€", round_2d(discounted_fund_spouse, T)))
  }),

  output$sd_text_pension_payment_discounted <- renderText({
    sd_inputs = sd_inputs()
    discounted_pension_payment = SORP_Discount(x = sd_pension_payment_reactive(),
                                               age_1 = sd_inputs$sd_age[1], 
                                               age_2 = sd_inputs$sd_age[2],
                                               discount_rate = sd_inputs$sd_discount_rate)
    return(c("€", round_2d(discounted_pension_payment, T)))
  }),

  output$sd_text_pension_payment_discounted_spouse <- renderText({
    sd_inputs = sd_inputs()
    discounted_pension_payment_spouse = SORP_Discount(x = sd_pension_payment_spouse_reactive(),
                                                      age_1 = sd_inputs$sd_age[1], 
                                                      age_2 = sd_inputs$sd_age[2],
                                                      discount_rate = sd_inputs$sd_discount_rate)
    return(c("€", round_2d(discounted_pension_payment_spouse, T)))
  }),

  output$sd_plot_fundvalue <- renderPlot({
    sd_inputs = sd_inputs()
    SORP_Plot_FundValue(SORP_Contributions = sd_contributions_reactive(), freq = sd_inputs$sd_pre_freq)
  }),

  output$sd_plot_fundvalue_spouse <- renderPlot({
    sd_inputs = sd_inputs()
    SORP_Plot_FundValue(SORP_Contributions = sd_contributions_spouse_reactive(), freq = sd_inputs$sd_pre_freq_spouse)
  }),

  output$sd_table_contributions <- renderDataTable({
    sd_inputs = sd_inputs()
    SORP_Table_Contributions(SORP_Contributions = sd_contributions_reactive(), 
                             freq = sd_inputs$sd_pre_freq)
  }),

  output$sd_table_contributions_spouse <- renderDataTable({
    sd_inputs = sd_inputs()
    SORP_Table_Contributions(SORP_Contributions = sd_contributions_spouse_reactive(), 
                             freq = sd_inputs$sd_pre_freq_spouse)
  }),

# Output Functions - Drawdown ---------------------------------------------
  output$sd_text_life_ex <- renderText({
    sd_inputs = sd_inputs()
    return(c(round_2d(sd_life_ex_reactive()), " Years"))
  }),

  output$sd_text_median_fund_life_ex <- renderText({
    sd_inputs = sd_inputs()
    median = Drawdown_Percentile_Life_Ex(Drawdown_Paths = sd_paths_reactive(),
                                         freq = sd_inputs$sd_withdraw_freq,
                                         ex = sd_life_ex_reactive())
    return(c("€", round_2d(median, T)))
  }),
  
  output$sd_text_ruin_prob_life_ex <- renderText({
    sd_inputs = sd_inputs()
    ruin = 100 * Drawdown_Ruin_Life_Ex(Drawdown_Paths = sd_paths_reactive(),
                                       freq = sd_inputs$sd_withdraw_freq,
                                       ex = sd_life_ex_reactive())
    return(c(round_2d(ruin), "%"))
  }),

  output$sd_table <- renderDataTable({
    sd_inputs = sd_inputs()
    freq = p_list[match(sd_inputs$sd_withdraw_freq, freq_list_drawdown)]
    series = list(1, (freq * seq(5, (length(sd_paths_reactive()[1, ]) / freq), 5)) + 1)
    points = list(sd_life_ex_reactive())
    colour = list('yellow')
    return(Drawdown_Table(Drawdown_Paths = sd_paths_reactive(),
                          Drawdown_Withdrawals = sd_withdrawals_reactive(),
                          freq = sd_inputs$sd_withdraw_freq, 
                          series = series,
                          points = points,
                          colour = colour))
  }),

  output$sd_plot_sims <- renderPlot({
    sd_inputs = sd_inputs()
    return(Drawdown_Plot_Sims(Drawdown_Paths = sd_paths_reactive(),
                              freq = sd_inputs$sd_withdraw_freq,
                              n_sims = 25,
                              points = list(sd_life_ex_reactive()),
                              colour = list('black')))
  }),

  output$sd_plot_percentiles <- renderPlotly({
    sd_inputs = sd_inputs()
    return(Drawdown_Plot_Percentile(Drawdown_Paths = sd_paths_reactive(), 
                                    Drawdown_Withdrawals = sd_withdrawals_reactive(), 
                                    freq = sd_inputs$sd_withdraw_freq, 
                                    lower = 0.05, upper = 0.95,
                                    points = list(sd_life_ex_reactive()),
                                    colour = list('black')))
  }),

# Observe Event Functions -------------------------------------------------
  observeEvent(input$sd_withdraw_type, {
    if(input$sd_withdraw_type == T) {
      shinyjs::hide("sd_annual_withdrawals_input")
      shinyjs::show("sd_percent_withdrawal_input")
      updateNumericInputIcon(session, "sd_percent_withdrawal", value = 4)
      enable("sd_percent_withdrawal")
      disable("sd_annual_withdrawals")
      updateNumericInputIcon(session, "sd_annual_withdrawals", value = NA)
    } else {
      shinyjs::show("sd_annual_withdrawals_input")
      shinyjs::hide("sd_percent_withdrawal_input")
      updateNumericInputIcon(session, "sd_annual_withdrawals", value = 12000)
      enable("sd_annual_withdrawals")
      disable("sd_percent_withdrawal")
      updateNumericInputIcon(session, "sd_percent_withdrawal", value = NA)
    }
  }),

  observeEvent(input$sd_default, {
    updateNumericInputIcon(session, "sd_salary_esc", value = 1.5)
    updateNumericInputIcon(session, "sd_discount_rate", value = 2.5)
    updateNumericInputIcon(session, "sd_annuity_interest", value = 0.5)
    updateNumericInputIcon(session, "sd_annuity_esc", value = 1)
    updateNumericInputIcon(session, "sd_guaranteed", value = 5)
    updateNumericInputIcon(session, "sd_investment_charges", value = 0.5)
    updateSliderInput(session, "sd_equity_prop", value = 40)
    updateSliderInput(session, "sd_equity_rate", value = 4.5)
    updateSliderInput(session, "sd_fixed_prop", value = 30)
    updateSliderInput(session, "sd_fixed_rate", value = 1)
    updateSliderInput(session, "sd_cash_prop", value = 30)
    updateSliderInput(session, "sd_cash_rate", value = 0)
  }),

  observeEvent(input$sd_equity_prop, {
    updateSliderInput(session, "sd_fixed_prop", max = 100 - input$sd_equity_prop)
    disable("sd_cash_prop") # putting this here keeps the slider disabled all the time (but still shows updating)
  }),

  observe({
    updateSliderInput(session, "sd_cash_prop", value = 100 - input$sd_equity_prop - input$sd_fixed_prop)
  }),

  observeEvent(input$sd_age[2], {
    updateNumericInputIcon(session, "sd_guaranteed", max = getOmega(ILT15_female_reduced) - input$sd_age[2])
  }),

  observeEvent(input$sd_relationship, {
    if(input$sd_relationship != 1) {
      showTab(inputId = "sd_sidebar", target = "Spouse SORP Parameters")
    }
    if(input$sd_relationship == 1){
      hideTab(inputId = "sd_sidebar", target = "Spouse SORP Parameters")
    }
  }),

  observe({
    shinyjs::hide("sd_submit")
  
    if((input$sd_surveydisplay %% 2 == 1 & input$sd_surveydisplay != 0)){
      shinyjs::show("sd_submit")
    }
  }),


# UI Functions ------------------------------------------------------------
  sd_ui_reactive <- reactive({
    sd_inputs = sd_inputs()
    mainui = list(
      tabsetPanel(type = "tabs",
                  tabPanel("SORP Summary",
                           style = "margin-top:1em",
                           box(title = "Future Values", status = "primary", solidHeader = T,
                               h4("Fund Value At Retirement:"),
                               h3(textOutput("sd_text_fundvalue")),
                               hr(),
                               h4("Periodic Pension Payment:"),
                               h3(textOutput("sd_text_pension_payment"))
                           ),
                           box(title = "Current Values", status = "primary", solidHeader = T,
                               h4("Fund Value At Retirement:"),
                               h3(textOutput("sd_text_fundvalue_discounted")),
                               hr(),
                               h4("Periodic Pension Payment:"),
                               h3(textOutput("sd_text_pension_payment_discounted"))
                           ),
                           tabBox(type = "tabs", width = 12,
                                       tabPanel("Accumulated Wealth", plotOutput("sd_plot_fundvalue")),                                       
                                       tabPanel("Contributions and Fund Value over Time", DT::dataTableOutput("sd_table_contributions"), rownames= FALSE)
                                  )
                           ),
                  if(sd_inputs$sd_relationship != 1){
                  tabPanel("Spouse SORP Summary",
                           style = "margin-top:1em",
                           box(title = "Future Values", status = "primary", solidHeader = T,
                               h4("Fund Value At Retirement:"),
                               h3(textOutput("sd_text_fundvalue_spouse")),
                               hr(),
                               h4("Periodic Pension Payment:"),
                               h3(textOutput("sd_text_pension_payment_spouse"))
                           ),
                           box(title = "Current Values", status = "primary", solidHeader = T,
                               h4("Fund Value At Retirement:"),
                               h3(textOutput("sd_text_fundvalue_discounted_spouse")),
                               hr(),
                               h4("Periodic Pension Payment:"),
                               h3(textOutput("sd_text_pension_payment_discounted_spouse"))
                           ),
                           tabBox(type = "tabs", width = 12,
                                       tabPanel("Accumulated Wealth", plotOutput("sd_plot_fundvalue_spouse")),
                                       tabPanel("Contributions and Fund Value over Time", DT::dataTableOutput("sd_table_contributions_spouse"), rownames= FALSE)
                                       )
                  )} else {
                  tabPanel("Drawdown Simulations",
                           style = "margin-top:1em",
                           box(
                             title = "Life Expectancy", status = 'primary', solidHeader = T, width = 4,
                             h3(textOutput('sd_text_life_ex'))
                           ),
                           box(
                             title = "Median Fund Value", status = "primary", solidHeader = T, width = 4,
                             h3(textOutput("sd_text_median_fund_life_ex"))
                           ),
                           box(
                             title = "Probability of Ruin", status = "primary", solidHeader = T, width = 4,
                             h3(textOutput("sd_text_ruin_prob_life_ex"))
                           ),
                           tabBox(type = "tabs", width = 12,
                                  tabPanel("Summary", plotlyOutput("sd_plot_percentiles")),
                                  tabPanel("Simulations", plotOutput("sd_plot_sims")),
                                  tabPanel("Table", DT::dataTableOutput("sd_table"), rownames = FALSE)
                           )
                  )},
                  
                  if(sd_inputs$sd_relationship != 1){
                    tabPanel("Drawdown Simulations",
                             style = "margin-top:1em",
                             box(
                               title = "Life Expectancy", status = 'primary', solidHeader = T, width = 4,
                               h3(textOutput('sd_text_life_ex'))
                             ),
                             box(
                               title = "Median Fund Value", status = "primary", solidHeader = T, width = 4,
                               h3(textOutput("sd_text_median_fund_life_ex"))
                             ),
                             box(
                               title = "Probability of Ruin", status = "primary", solidHeader = T, width = 4,
                               h3(textOutput("sd_text_ruin_prob_life_ex"))
                             ),
                             tabBox(type = "tabs", width = 12,
                                    tabPanel("Summary", plotlyOutput("sd_plot_percentiles")),
                                    tabPanel("Simulations", plotOutput("sd_plot_sims")),
                                    tabPanel("Table", DT::dataTableOutput("sd_table"), rownames = FALSE)
                             )
                    )} else {tabPanel("")}
      )
    )
    return(riskprofilerui(session = session,
                          surveydisplay = input$sd_surveydisplay,
                          submit = input$sd_submit, 
                          page = "sd",
                          input_mean_return = "sd_annual_mean_return", 
                          input_ret_std_dev = "sd_annual_ret_std_dev",
                          mainui = mainui))
  }),

  output$sd_save_results_text <- renderText({
    if(input$sd_submit %% (num.quest + 2) > 0 && (input$sd_submit %% (num.quest + 2) <= num.quest)){
      save_results(session, input$sd_submit, input$sd_survey)
    }
  }),

  output$sd_ui <- renderUI({
    sd_ui_reactive()
  })
)