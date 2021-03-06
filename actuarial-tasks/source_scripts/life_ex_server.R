list(
  life_ex <- reactive({
  current_age = input$current_age_ex
  if(input$gender_ex == 1){
    life_ex = current_age + exn(ILT15_female_reduced, current_age)
  } else {
    life_ex = current_age + exn(ILT15_male_reduced, current_age)
  }
  return(life_ex)
  }),
  
  output$percent_increase_ex <- renderPlotly({
    if(input$gender_ex == 1){
      ages_x <- unlist(life_table_female[,1])
      age_death = ex= numeric(length(ages_x))
      for(i in 1:length(ages_x)){
        ex[i] = round(exn(ILT15_female_reduced, ages_x[i]),2)
        age_death[i] = ages_x[i] + exn(ILT15_female_reduced, ages_x[i])
        
      }
    } else {
      ages_x <- unlist(life_table_male[,1])
      age_death = ex= numeric(length(ages_x))
      for(i in 1:length(ages_x)){
        ex[i] = round(exn(ILT15_male_reduced, ages_x[i]),2)
        age_death[i] = ages_x[i] + exn(ILT15_male_reduced, ages_x[i])
      }
    }

    percent_increase_ex = percent_increase_lifespan = numeric(length(ages_x))
    for(i in 1:length(ages_x)){
      percent_increase_ex[i] = round(((ex[i+1] - (ex[i] - 1))/(ex[i]-1))*100,2)
      percent_increase_lifespan[i] = round(((age_death[i+1] - age_death[i])/age_death[i])*100,2)
      
    }
   
    fig <- plot_ly(type = "scatter", x = ages_x, y = percent_increase_ex, color = if(input$gender_ex == 1){I('#f112be')}else{I('#4A8DBF')}, mode = "markers",
                   hovertemplate = paste("%{xaxis.title.text}: %{x}<br>",
                                         "%{yaxis.title.text}: %{y}<br>",
                                         '<extra></extra>'))
                   
    fig <- fig %>%
      layout(xaxis = list(title = "Age", range = c(59.5,105)), yaxis = list(title = "% Increase", range = c(0,40)))
  }),
  

  output$interactive_ex <- renderPlot({
    life_ex <- life_ex()
    life_ex_text = round_2d(life_ex)
    df = data.frame(x = c(0, 5, getOmega(ILT15_female_reduced)), y = c(0, 5, 10))
    p = ggplot(data = df, aes(x = x, y = y)) +
      xlim(-10, getOmega(ILT15_female_reduced) + 10) + ylim(0, 4) + xlab("Age") + ylab(NULL) +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            legend.position='none')

    xrange <- range(df$x)
    yrange <- range(df$y)
    ratioxy <- diff(xrange) / diff(yrange)

    mapping <- aes(x=x,
                   y=y,
                   scale=scale,
                   ratioxy=ratioxy,
                   angleofspine = angleofspine,
                   anglerighthumerus = anglerighthumerus,
                   anglelefthumerus = anglelefthumerus,
                   anglerightradius = anglerightradius,
                   angleleftradius = angleleftradius,
                   anglerightleg =  anglerightleg,
                   angleleftleg = angleleftleg,
                   angleofneck = angleofneck,
                   color = color)

    dataman <- data.frame(x = c(input$current_age_ex, life_ex), y = c(2.75, 2.75),
                           scale = c(1, 1),
                           ratioxy = ratioxy,
                           angleofspine = -pi/2,
                           anglerighthumerus = c(3*pi/2  + pi / 12, pi / 12),
                           anglelefthumerus = c(11*pi / 12, 3*pi/2  - pi / 12),
                           anglerightradius = c(3*pi/2  + pi / 12, 5*pi / 12),
                           angleleftradius = c(7*pi / 12, 3*pi/2  - pi / 12),
                           angleleftleg = 3*pi/2  + pi / 12,
                           anglerightleg = 3*pi/2  - pi / 12,
                           angleofneck = -pi/2,
                           color=c("A", "B"))
    p + xkcdman(mapping, dataman) + 
      geom_label(x = c(input$current_age_ex - 9, life_ex + 10, mean(c(input$current_age_ex, life_ex))), y = c(3, 3, 3.75), label = c(input$current_age_ex, life_ex_text, paste0("Life Expectancy = ", round_2d(life_ex - input$current_age_ex))), label.padding = unit(0.4, "lines"), size = c(rep(4, 2), 6.5)) + 
      scale_color_manual(values = c('chartreuse4', "firebrick1"))
  }),

  output$comparison_ex <- renderPlot({
    ages = input$ages_ex
    deaths = numeric(length(ages))
    if(input$gender_ex == 1){table = ILT15_female_reduced} else {table = ILT15_male_reduced}
    for(i in 1:length(ages)){
      deaths[i] = ages[i] + exn(table, ages[i])
    }
    df = data.frame(x = c(1, 2, 3, 4, 5), y = c(1, 2, 3, 4, 5))
    p = ggplot(data = df, aes(x = x, y = y)) + 
      xlim(50, getOmega(ILT15_female_reduced) + 5) + ylim(0, 4) + xlab("Age") + ylab(NULL) + 
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            legend.position='none')
    
    xrange <- range(df$x)
    yrange <- range(df$y)
    ratioxy <- diff(xrange) / diff(yrange)
    
    mapping <- aes(x=x,
                   y=y,
                   scale=scale,
                   ratioxy=ratioxy,
                   angleofspine = angleofspine,
                   anglerighthumerus = anglerighthumerus,
                   anglelefthumerus = anglelefthumerus,
                   anglerightradius = anglerightradius,
                   angleleftradius = angleleftradius,
                   anglerightleg =  anglerightleg,
                   angleleftleg = angleleftleg,
                   angleofneck = angleofneck,
                   color = color)

    dataman <- data.frame(x = c(ages, deaths), y = rep(2.75, 4),
                          scale = c(1, 1),
                          ratioxy = ratioxy,
                          angleofspine = -pi/2,
                          anglerighthumerus = c(rep(3*pi/2  + pi / 12, 2), rep(pi / 12, 2)),
                          anglelefthumerus = c(rep(11*pi / 12, 2), rep(3*pi/2  - pi / 12, 2)),
                          anglerightradius = c(rep(3*pi/2  + pi / 12, 2), rep(5*pi / 12, 2)),
                          angleleftradius = c(rep(7*pi / 12, 2), rep(3*pi/2  - pi / 12, 2)),
                          angleleftleg = 3*pi/2  + pi / 12,
                          anglerightleg = 3*pi/2  - pi / 12,
                          angleofneck = -pi/2,
                          color=c("A", "B", "A", "B"))
    
    p + xkcdman(mapping, dataman) + 
      geom_label(x = c(ages, deaths, mean(c(getOmega(table) + 5, 50))), y = c(rep(3.4, 4), 4), label = c(ages, lapply(deaths, round_2d), paste0("Difference in Lifespan = ", round_2d(deaths[2] - deaths[1]), " Years")), label.padding = unit(0.4, "lines"), size = c(rep(4, 4), 6.5)) +
      scale_color_manual(values = c("darkorange1", "darkorchid3")) 
  
  })
)