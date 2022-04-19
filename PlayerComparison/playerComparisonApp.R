

library(nflreadr)
library(data.table)
library(dplyr)
library(tidyr)
#library(tidyverse)
library(ggplot2)
library(shiny)
library(shinythemes)
library(rockchalk)
library(markdown)
library(fmsb)


dat = data.table(load_player_stats(2021)) # Load all the player stats for the season so far
colnames(dat)[1] = "gsis_id" # change the name of the column to make merging later easy
week = dat[player_name != "H.Ruggs"]
weekly_players = unique(week$gsis_id) # Get the unique IDs of the players for the week
names = data.table(load_rosters(2021)) # Load in the complete roster

players = names[names$gsis_id %in% weekly_players, ] # Filter players so only the ones who played count
all_dat = merge(week, players) # Merge player stats with roster data so we have player positions
all_dat = all_dat[position %in% c("QB", "WR", "TE", "RB"),]

# Get list of row names for regressions
v.names = colnames(all_dat)
v.names = v.names[c(7:47)]

# Make A.Rodgers the Aa.Rodgers
all_dat$player_name = gsub("A.Rodgers", "Aa.Rodgers", all_dat$player_name)
# Remove anyone who has played less than 5 games
t = data.frame(table(all_dat$gsis_id)) 
t = t[t$Freq >= 6,]
all_dat = all_dat[gsis_id %in% t$Var1,]

table_dat = all_dat # New variable just for the table and plot to keep things more simple
plot_dat = all_dat

keycol = "stats" # Not sure that keycol and valuecol really matter other than just providing a name for the column
valuecol = "measurement" 

cols = c("#00AFBB", "#E7B800", "#FC4E07")

# Define UI 
ui = fluidPage(theme = shinytheme("yeti"),
               titlePanel(paste0("NFL Stats Analysis -- Version 3.0: ", Sys.Date())), 
               navbarPage("JSommerfeld36",
                          tabPanel("Player Stats",
                                   sidebarLayout(
                                     
                                     sidebarPanel(
                                       selectInput('group1', 'pick a position', unique(table_dat$position)),
                                       selectInput('gameWeek1', 'Game Week', sort(unique(table_dat$week)), selected = "1"),
                                       selectInput('player1', 'Player Name', choices = NULL),
                                       selectInput('player2', 'Player Name', choices = NULL),
                                       selectInput('player3', 'Player Name', choices = NULL)
                                       
                                     ), # sidebarPanel
                                     
                                     mainPanel(
                                       plotOutput('radar'),
                                       br(),
                                       br(),
                                       tableOutput('t1')
                                     ) # mainPanel
                                     
                                   ) # sidebarLayout
                          ), #tabPanel
                          tabPanel("Regression",
                                   # Add in what output types you want here
                                   sidebarLayout(
                                     
                                     sidebarPanel(
                                       selectInput('group2', 'pick a position', unique(table_dat$position)),
                                       selectInput('gameWeek2', 'Game Week', choices = sort(unique(table_dat$week)), 
                                                   selected = 1),
                                       selectInput('x2', 'Predicted Variable', choices = v.names, 
                                                   selected = "fantasy_points"),
                                       selectInput('y2', 'Predictor Variable', choices = v.names, 
                                                   selected = NULL, multiple = TRUE)
                                       
                                     ), # sidebarPanel
                                     
                                     mainPanel(
                                       tags$style(type="text/css",
                                                  ".shiny-output-error { visibility: hidden; }",
                                                  ".shiny-output-error:before { visibility: hidden; }"
                                       ), # Hide the damn errors
                                       plotOutput('regress'),
                                       verbatimTextOutput("regress.results")
                                       
                                     ) # mainPanel
                                     
                                   ) # sidebarLayout
                          ), #tabPanel
                          
                          tabPanel("Summary", "This is intentionally blank"
                          #includeHTML("ClassSummary.html")
                          )

               ) # navbarPage

)


# Define server
server <- function(input, output) {
  
  # Tab 1 -------------------------------------------------------------------
  
  # Filter the player drop down list based on the position group selected
  this <- reactive({
    filter(table_dat, position == input$group1 & week == input$gameWeek1)
  })
  
  # Player 1
  observeEvent(this(), {
    newChoices <- unique(this()$player_name)
    updateSelectInput(inputId = "player1", choices = newChoices)
  })
  # Player 2
  observeEvent(this(), {
    newChoices <- unique(this()$player_name)
    updateSelectInput(inputId = "player2", choices = newChoices)
  })
  # Player 3
  observeEvent(this(), {
    newChoices <- unique(this()$player_name)
    updateSelectInput(inputId = "player3", choices = newChoices)
  })
  
  
  # calculate the max and min to set the range for the radar chart
  # week_max = reactive({
  #   
  #   plot_dat %>% 
  #     filter(plot_dat$position %in% input$group1 & plot_dat$week %in% input$gameWeek1) %>%
  #     #select(c(7:14)) %>%
  #     summarise_if(is.numeric, max)
  #     #lapply(max)
  #     #data.frame(lapply(plot_dat[,7:14], max))
  # })
  # 
  # week_min = reactive({
  #   plot_dat %>% 
  #     filter(plot_dat$position %in% input$group1 & plot_dat$week %in% input$gameWeek1) %>%
  #     #select(c(7:14)) %>%
  #     summarise_if(is.numeric, min)
  #     #lapply(max)
  #     #data.frame(lapply(plot_dat[,7:14], min))
  # })
  
  
  # Plot is created based on the player selected. It is completions over the season but perhaps other buttons 
  # can be added to allow for the user to select what they want to look at
  output$radar <- renderPlot({
    if (input$group1 == "QB") {
      
      week_max = reactive({
        plot_dat %>% 
          filter(plot_dat$position %in% input$group1 & plot_dat$week %in% input$gameWeek1) %>%
          select(c(7:14)) %>%
          summarise_if(is.numeric, max)
      })
      
      week_min = reactive({
        plot_dat %>% 
          filter(plot_dat$position %in% input$group1 & plot_dat$week %in% input$gameWeek1) %>%
          select(c(7:14)) %>%
          summarise_if(is.numeric, min)
      })
      
      p1 = reactive({
        plot_dat %>%
          filter(plot_dat$player_name %in% c(input$player1, input$player2, input$player3)  & plot_dat$week %in% input$gameWeek1) %>%
          select(c(7:14))
      })
      
      # p2 = reactive({
      #   plot_dat %>%
      #     filter(plot_dat$player_name %in% input$player2 & plot_dat$week %in% input$gameWeek1) %>%
      #     select(c(7:14))
      # })
      # p3 = reactive({
      #   plot_dat %>%
      #     filter(plot_dat$player_name %in% input$player3 & plot_dat$week %in% input$gameWeek1) %>%
      #     select(c(7:14))
      # })
      
      all = reactive({
        bind_rows(week_max(), week_min(), p1())#, p2(), p3())
      })
      
      radarchart(all(), axistype = 1, pcol = cols, pfcol = scales::alpha(cols, 0.3), plwd = 2, plty = 1,
                 cglcol = "grey", cglty = 1, cglwd = 0.8, 
                 axislabcol = "grey", 
                 vlcex = 1,
                 title = paste0("QB Comparison for week: ", input$gameWeek1))
      
      legend(-2.2, 1.2, legend = c(input$player1, input$player2, input$player3), seg.len = 1, title = "QB", pch = 1, 
             bty = "n", lwd = 2, y.intersp = 1.5, horiz = FALSE, col = cols, cex =1.2)
      
    } else if (input$group1 == "WR"){
      
      # plot_dat %>%
      #   filter(plot_dat$player_name %in% input$player1) %>%
      #   ggplot(plot_dat, mapping = aes(week)) + 
      #   geom_line(aes(y = receptions, color = "receptions")) +       
      #   geom_point(aes(y = receptions, color = "receptions")) +       
      #   geom_line(aes(y = targets, color = "targets")) + 
      #   geom_point(aes(y = targets, color = "targets")) + 
      #   scale_color_manual("", breaks = c("receptions","targets"), 
      #                      values = c("red", "blue")) +
      #   labs(title = paste(input$player1, "receptions/targets over the season")) + 
      #   ylab("Count") + xlab('Game Week') + 
      #   theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())
      week_max = reactive({
        plot_dat %>% 
          filter(plot_dat$position %in% input$group1 & plot_dat$week %in% input$gameWeek1) %>%
          select(c(31:39)) %>%
          summarise_if(is.numeric, max)
      })
      
      week_min = reactive({
        plot_dat %>% 
          filter(plot_dat$position %in% input$group1 & plot_dat$week %in% input$gameWeek1) %>%
          select(c(31:39)) %>%
          summarise_if(is.numeric, min)
      })
      
      p1 = reactive({
        plot_dat %>%
          filter(plot_dat$player_name %in% input$player1 & plot_dat$week %in% input$gameWeek1) %>%
          select(c(31:39))
      })
      
      p2 = reactive({
        plot_dat %>%
          filter(plot_dat$player_name %in% input$player2 & plot_dat$week %in% input$gameWeek1) %>%
          select(c(31:39))
      })
      p3 = reactive({
        plot_dat %>%
          filter(plot_dat$player_name %in% input$player3 & plot_dat$week %in% input$gameWeek1) %>%
          select(c(31:39))
      })
      
      all = reactive({
        bind_rows(week_max(), week_min(), p1(), p2(), p3())
      })
      
      radarchart(all(), axistype = 1, pcol = cols, pfcol = scales::alpha(cols, 0.3), plwd = 2, plty = 1,
                 cglcol = "grey", cglty = 1, cglwd = 0.8, 
                 axislabcol = "grey", 
                 vlcex = 1,
                 title = paste0("WR Comparison for week: ", input$gameWeek1))
      
      legend(-2.2, 1.2, legend = c(input$player1, input$player2, input$player3), seg.len = 1, title = "WR", pch = 1, 
             bty = "n", lwd = 2, y.intersp = 1.5, horiz = FALSE, col = cols, cex =1.2)
      
    } else if (input$group1 == "RB"){
      
      # plot_dat %>%
      #   filter(plot_dat$player_name %in% input$player1) %>%
      #   ggplot(plot_dat, mapping = aes(week, rushing_yards)) + 
      #   geom_line(aes(y = rushing_yards, color = "rushing_yards")) +
      #   geom_point(aes(y = rushing_yards, color = "rushing_yards")) +
      #   geom_line(aes(y = carries, color = "carries")) + 
      #   geom_point(aes(y = carries, color = "carries")) + 
      #   scale_color_manual("", breaks = c("rushing_yards","carries"), 
      #                      values = c("red", "blue")) +
      #   labs(title = paste(input$player1, "Rushing yards/carries over the season")) + 
      #   ylab("Count") + xlab('Game Week') + 
      #   theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())
      # 
      week_max = reactive({
        plot_dat %>% 
          filter(plot_dat$position %in% input$group1 & plot_dat$week %in% input$gameWeek1) %>%
          select(c(23:26,28,30)) %>%
          summarise_if(is.numeric, max)
      })
      
      week_min = reactive({
        plot_dat %>% 
          filter(plot_dat$position %in% input$group1 & plot_dat$week %in% input$gameWeek1) %>%
          select(c(23:26,28,30)) %>%
          summarise_if(is.numeric, min)
      })
      
      p1 = reactive({
        plot_dat %>%
          filter(plot_dat$player_name %in% c(input$player1, input$player2, input$player3)  & plot_dat$week %in% input$gameWeek1) %>%
          select(c(23:26,28,30))
      })
      
      # p2 = reactive({
      #   plot_dat %>%
      #     filter(plot_dat$player_name %in% input$player2 & plot_dat$week %in% input$gameWeek1) %>%
      #     select(c(7:14))
      # })
      # p3 = reactive({
      #   plot_dat %>%
      #     filter(plot_dat$player_name %in% input$player3 & plot_dat$week %in% input$gameWeek1) %>%
      #     select(c(7:14))
      # })
      
      all = reactive({
        bind_rows(week_max(), week_min(), p1())#, p2(), p3())
      })
      
      radarchart(all(), axistype = 1, pcol = cols, pfcol = scales::alpha(cols, 0.3), plwd = 2, plty = 1,
                 cglcol = "grey", cglty = 1, cglwd = 0.8, 
                 axislabcol = "grey", 
                 vlcex = 1,
                 title = paste0("RB Comparison for week: ", input$gameWeek1))
      
      legend(-2.2, 1.2, legend = c(input$player1, input$player2, input$player3), seg.len = 1, title = "RB", pch = 1, 
             bty = "n", lwd = 2, y.intersp = 1.5, horiz = FALSE, col = cols, cex =1.2)
      
    } else if (input$group1 == "TE"){
      
      # plot_dat %>%
      #   filter(plot_dat$player_name %in% input$player1) %>%
      #   ggplot(plot_dat, mapping = aes(week)) + 
      #   geom_line(aes(y = receptions, color = "receptions")) +       
      #   geom_point(aes(y = receptions, color = "receptions")) +       
      #   geom_line(aes(y = targets, color = "targets")) + 
      #   geom_point(aes(y = targets, color = "targets")) + 
      #   scale_color_manual("", breaks = c("receptions","targets"), 
      #                      values = c("red", "blue")) +
      #   labs(title = paste(input$player1, "receptions/targets over the season")) + 
      #   ylab("Count") + xlab('Game Week') + 
      #   theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())
      
      week_max = reactive({
        plot_dat %>% 
          filter(plot_dat$position %in% input$group1 & plot_dat$week %in% input$gameWeek1) %>%
          select(c(31:39)) %>%
          summarise_if(is.numeric, max)
      })
      
      week_min = reactive({
        plot_dat %>% 
          filter(plot_dat$position %in% input$group1 & plot_dat$week %in% input$gameWeek1) %>%
          select(c(31:39)) %>%
          summarise_if(is.numeric, min)
      })
      
      p1 = reactive({
        plot_dat %>%
          filter(plot_dat$player_name %in% c(input$player1, input$player2, input$player3)  & plot_dat$week %in% input$gameWeek1) %>%
          select(c(31:39))
      })
      
      # p2 = reactive({
      #   plot_dat %>%
      #     filter(plot_dat$player_name %in% input$player2 & plot_dat$week %in% input$gameWeek1) %>%
      #     select(c(7:14))
      # })
      # p3 = reactive({
      #   plot_dat %>%
      #     filter(plot_dat$player_name %in% input$player3 & plot_dat$week %in% input$gameWeek1) %>%
      #     select(c(7:14))
      # })
      
      all = reactive({
        bind_rows(week_max(), week_min(), p1())#, p2(), p3())
      })
      
      radarchart(all(), axistype = 1, pcol = cols, pfcol = scales::alpha(cols, 0.3), plwd = 2, plty = 1,
                 cglcol = "grey", cglty = 1, cglwd = 0.8, 
                 axislabcol = "grey", 
                 vlcex = 1,
                 title = paste0("TE Comparison for week: ", input$gameWeek1))
      
      legend(-2.2, 1.2, legend = c(input$player1, input$player2, input$player3), seg.len = 1, title = "TE", pch = 1, 
             bty = "n", lwd = 2, y.intersp = 1.5, horiz = FALSE, col = cols, cex =1.2)
      
      
    } else {}
  }, height = 500, width = 800)
  
  
  # Data table that is below the graph. This is filtered based on the player and week inputs selected
  output$t1 <- renderTable({
    
    
    
    if (input$group1 == "QB") {
      
      # table_dat %>%
      #   filter(table_dat$player_name %in% input$player1 & table_dat$week %in% input$gameWeek1) %>%
      #   select(c(2,3,7:12))
      
      table_dat %>%
        filter(table_dat$position %in% input$group1 & table_dat$week %in% input$gameWeek1) %>%
        arrange(desc(fantasy_points)) %>%
        select(c(3,7:14,47)) %>%
        head(10)
      
    } else if (input$group1 == "WR"){
      
      # table_dat %>%
      #   filter(table_dat$player_name %in% input$player1 & table_dat$week %in% input$gameWeek1) %>%
      #   select(c(2,3,31:39))
      table_dat %>%
        filter(table_dat$position %in% input$group1 & table_dat$week %in% input$gameWeek1) %>%
        arrange(desc(fantasy_points)) %>%
        select(c(3,31:39,47)) %>%
        head(10)
      
    } else if (input$group1 == "RB"){
      
      # table_dat %>%
      #   filter(table_dat$player_name %in% input$player1 & table_dat$week %in% input$gameWeek1) %>%
      #   select(c(2,3,22:27))
      table_dat %>%
        filter(table_dat$position %in% input$group1 & table_dat$week %in% input$gameWeek1) %>%
        arrange(desc(fantasy_points)) %>%
        select(c(3,23:26,28,30,47)) %>%
        head(10)
      
    } else if (input$group1 == "TE"){
      
      # table_dat %>%
      #   filter(table_dat$player_name %in% input$player1 & table_dat$week %in% input$gameWeek1) %>%
      #   select(c(2,3,30:38))
      table_dat %>%
        filter(table_dat$position %in% input$group1 & table_dat$week %in% input$gameWeek1) %>%
        arrange(desc(fantasy_points)) %>%
        select(c(3,31:39,47)) %>%
        head(10)
      
    } else {}
  })
  

  
  
  
  # Tab 2 -------------------------------------------------------------------
  
  # Add predictor variables to an object
  target.variable = reactive({
    paste(input$x2)
  })
  
  pred.variables = reactive({
    paste(input$y2)
  })  
  
  
  that = reactive({
    gather(x(), keycol, valuecol, pred.variables())
    
  })
  
  # Plot the values for the target variable vs the predictors
  output$regress = renderPlot({
    
    ggplot(that(), aes(x = get(target.variable()), y = valuecol, group = keycol, colour = keycol)) + 
      geom_point() + 
      geom_smooth(method = 'lm' , se = F, aes(group = 1),color = 'black') + 
      labs(title = paste0(input$group2)) + 
      ylab("Count") + xlab(paste(target.variable())) + 
      labs(colour = 'Predictor Variables') +
      theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())
  })
  
  
  
  # Filter the data and put it in 'x' so it can be accessed in the regression equation
  x = reactive({
    if (input$group2 == "QB") {
      
      filter(table_dat, position %in% input$group2 & week %in% input$gameWeek2)
    } else if (input$group2 == "WR") {
      
      filter(table_dat, position %in% input$group2 & week %in% input$gameWeek2)
      
    } else if (input$group2 == "RB"){
      
      filter(table_dat, position %in% input$group2 & week %in% input$gameWeek2)
      
    } else if (input$group2 == "TE"){
      
      filter(table_dat, position %in% input$group2 & week %in% input$gameWeek2)
  } else {}
})
  
  
  # Join predictor variables with a + and add them to the regression equation
  form = reactive({
    lm(as.formula(paste(target.variable()," ~ ", paste(pred.variables(), collapse = "+"))), x())
  })
  
  # Output the summary
  output$regress.results = renderPrint({
    
    if (!isTruthy(pred.variables())) {
      # This is to stop the error message that appears if a value is not yet selected
      cat("Select a game week and some values to build the regression model")
      
    } else {
      # Regression model output
      cat(paste0("Equation: ", "lm(", target.variable(), " ~ ", paste(pred.variables(), collapse = " + "), ")"), "\n")
      # cat("R^2: ", summary(form())$r.squared, "\n")
      # cat("Adjusted R^2: ", summary(form())$adj.r.squared, "\n")
      summary(form())
      #print(head(x()$fantasy_points))
      
    }
    
  }) # renderPrint()
  

# Tab 3 -------------------------------------------------------------------

output$data = renderPlot({
  plot(qb_model)
})  
  }

# Run the application 
shinyApp(ui = ui, server = server)

