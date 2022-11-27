

library(nflreadr)
library(data.table)
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(shiny)
library(shinythemes)
library(rockchalk)
library(markdown)
library(fmsb)
library(scales)
library(ggrepel)


dat = data.table(load_player_stats(2022)) # Load all the player stats for the season so far
colnames(dat)[1] = "gsis_id" # change the name of the column to make merging later easy
week = dat[player_name != "H.Ruggs"]
weekly_players = unique(week$gsis_id) # Get the unique IDs of the players for the week
names = data.table(load_rosters(2022)) # Load in the complete roster

players = names[names$gsis_id %in% weekly_players, ] # Filter players so only the ones who played count
all_dat = merge(week, players, by = "gsis_id") # Merge player stats with roster data so we have player positions
all_dat = all_dat[position.x %in% c("QB", "WR", "TE", "RB"),]
colnames(all_dat)[5] = "week"

# Get list of row names for regressions
v.names = colnames(all_dat)
v.names = v.names[c(11:45)]

# Make A.Rodgers the Aa.Rodgers
all_dat$player_name = gsub("A.Rodgers", "Aa.Rodgers", all_dat$player_name)
# Remove anyone who has played less than 5 games
t = data.frame(table(all_dat$gsis_id)) 
#t = t[t$Freq >= 6,]
all_dat = all_dat[gsis_id %in% t$Var1,]

table_dat = all_dat # New variable just for the table and plot to keep things more simple
plot_dat = all_dat

keycol = "stats" # Not sure that keycol and valuecol really matter other than just providing a name for the column
valuecol = "measurement" 

cols = c("#00AFBB", "#E7B800", "#FC4E07")

# pie chart of the play type tendencies
pbp = load_pbp()

# Define UI 
ui = fluidPage(theme = shinytheme("yeti"),
               titlePanel(paste0("NFL Stats Analysis -- Version 3.3.0: ", Sys.Date())), 
               navbarPage("JSommerfeld36",
                          tabPanel("Player Stats",
                                   sidebarLayout(
                                     
                                     sidebarPanel(
                                       selectInput('group1', 'pick a position', unique(table_dat$position.x)),
                                       selectInput('gameWeek1', 'Game Week', sort(unique(table_dat$week.x)), selected = "1"),
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
                                       selectInput('group2', 'pick a position', unique(table_dat$position.x)),
                                       selectInput('gameWeek2', 'Game Week', choices = sort(unique(table_dat$week.x)), 
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
                          
                          tabPanel("Team Stats",
                                   sidebarLayout(
                                     sidebarPanel(
                                       selectInput('footballTeam', 'Pick a Team', unique(plot_dat$recent_team)),
                                       selectInput('gameWeek3', 'Pick a Game Week', choices = sort(unique(plot_dat$week.x)), 
                                                   selected = 1)
                                     ),
                                     mainPanel(
                                       tags$style(type="text/css",
                                                  ".shiny-output-error { visibility: hidden; }",
                                                  ".shiny-output-error:before { visibility: hidden; }"
                                       ), # Hide the damn errors
                                       plotOutput('receivers'),
                                       br(),
                                       br(),
                                       plotOutput('tendencies'), 
                                     )
                                     
                                   ), 
                                   tabPanel("Instructions")
                          )
                          
               ) # navbarPage
               
)


# Define server
server <- function(input, output) {
  
  # Tab 1 -------------------------------------------------------------------
  
  # Filter the player drop down list based on the position group selected
  this <- reactive({
    filter(table_dat, position.x == input$group1 & week.x == input$gameWeek1)
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
  
  # Plot is created based on the player selected. It is completions over the season but perhaps other buttons 
  # can be added to allow for the user to select what they want to look at
  output$radar <- renderPlot({
    if (input$group1 == "QB") {
      
      week_max = reactive({
        plot_dat %>% 
          filter(plot_dat$position.x %in% input$group1 & plot_dat$week.x %in% input$gameWeek1) %>%
          select(c(11:16, 20,22)) %>%
          summarise_if(is.numeric, max)
      })
      
      week_min = reactive({
        plot_dat %>% 
          filter(plot_dat$position.x %in% input$group1 & plot_dat$week.x %in% input$gameWeek1) %>%
          select(c(11:16, 20,22)) %>%
          summarise_if(is.numeric, min)
      })
      
      p1 = reactive({
        plot_dat %>%
          filter(plot_dat$player_name %in% c(input$player1, input$player2, input$player3)  & plot_dat$week.x %in% input$gameWeek1) %>%
          select(c(11:16, 20,22))
      })
      
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
      
      week_max = reactive({
        plot_dat %>% 
          filter(plot_dat$position.x %in% input$group1 & plot_dat$week.x %in% input$gameWeek1) %>%
          select(c(35:38,41:42)) %>%
          summarise_if(is.numeric, max)
      })
      
      week_min = reactive({
        plot_dat %>% 
          filter(plot_dat$position.x %in% input$group1 & plot_dat$week.x %in% input$gameWeek1) %>%
          select(c(35:38,41:42)) %>%
          summarise_if(is.numeric, min)
      })
      
      p1 = reactive({
        plot_dat %>%
          filter(plot_dat$player_name %in% input$player1 & plot_dat$week.x %in% input$gameWeek1) %>%
          select(c(35:38,41:42))
      })
      
      p2 = reactive({
        plot_dat %>%
          filter(plot_dat$player_name %in% input$player2 & plot_dat$week.x %in% input$gameWeek1) %>%
          select(c(35:38,41:42))
      })
      p3 = reactive({
        plot_dat %>%
          filter(plot_dat$player_name %in% input$player3 & plot_dat$week.x %in% input$gameWeek1) %>%
          select(c(35:38,41:42))
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
      
      week_max = reactive({
        plot_dat %>% 
          filter(plot_dat$position.x %in% input$group1 & plot_dat$week.x %in% input$gameWeek1) %>%
          select(c(27:32)) %>%
          summarise_if(is.numeric, max)
      })
      
      week_min = reactive({
        plot_dat %>% 
          filter(plot_dat$position.x %in% input$group1 & plot_dat$week.x %in% input$gameWeek1) %>%
          select(c(27:32)) %>%
          summarise_if(is.numeric, min)
      })
      
      p1 = reactive({
        plot_dat %>%
          filter(plot_dat$player_name %in% c(input$player1, input$player2, input$player3)  & plot_dat$week.x %in% input$gameWeek1) %>%
          select(c(27:32))
      })
      
      
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
      
      week_max = reactive({
        plot_dat %>% 
          filter(plot_dat$position.x %in% input$group1 & plot_dat$week.x %in% input$gameWeek1) %>%
          select(c(35:38,41:42)) %>%
          summarise_if(is.numeric, max)
      })
      
      week_min = reactive({
        plot_dat %>% 
          filter(plot_dat$position.x %in% input$group1 & plot_dat$week.x %in% input$gameWeek1) %>%
          select(c(35:38,41:42)) %>%
          summarise_if(is.numeric, min)
      })
      
      p1 = reactive({
        plot_dat %>%
          filter(plot_dat$player_name %in% c(input$player1, input$player2, input$player3)  & plot_dat$week.x %in% input$gameWeek1) %>%
          select(c(35:38,41:42))
      })
      
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
      
      table_dat %>%
        filter(table_dat$position.x %in% input$group1 & table_dat$week.x %in% input$gameWeek1) %>%
        arrange(desc(completions)) %>%
        select(c(2,11:16,20,22)) %>%
        head(10)
      
    } else if (input$group1 == "WR"){
      
      table_dat %>%
        filter(table_dat$position.x %in% input$group1 & table_dat$week.x %in% input$gameWeek1) %>%
        arrange(desc(receptions)) %>%
        select(c(2,35:38,41:42)) %>%
        head(10)
      
    } else if (input$group1 == "RB"){
      
      table_dat %>%
        filter(table_dat$position.x %in% input$group1 & table_dat$week.x %in% input$gameWeek1) %>%
        arrange(desc(carries)) %>%
        select(c(2,27:32)) %>%
        head(10)
      
    } else if (input$group1 == "TE"){
      
      table_dat %>%
        filter(table_dat$position.x %in% input$group1 & table_dat$week.x %in% input$gameWeek1) %>%
        arrange(desc(receptions)) %>%
        select(c(2,35:38,41:42)) %>%
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
      
      filter(table_dat, position.x %in% input$group2 & week.x %in% input$gameWeek2)
    } else if (input$group2 == "WR") {
      
      filter(table_dat, position.x %in% input$group2 & week.x %in% input$gameWeek2)
      
    } else if (input$group2 == "RB"){
      
      filter(table_dat, position.x %in% input$group2 & week.x %in% input$gameWeek2)
      
    } else if (input$group2 == "TE"){
      
      filter(table_dat, position.x %in% input$group2 & week.x %in% input$gameWeek2)
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
  
  team = reactive({
    filter(plot_dat, recent_team %in% input$footballTeam & week.x %in% input$gameWeek3 & position.x != "QB") %>%
      drop_na(target_share)
  })
  
  # A nice color palette
  nice = reactive({
    colorspace::diverge_hcl(n = nrow(team()))
  }) 
  
  # Calculate catch percentage
  catch_rate = reactive({ round(team()$receptions/team()$targets*100, 2)  })
  # Add player positions to the legend
  concat = reactive({ paste0(team()$player_name, " - ", team()$position.x)  })
  
  output$receivers = renderPlot({
    
    ggplot(team(), aes(x = "", y = target_share, fill = player_name)) +
      geom_bar(stat = "identity", width = 1, color = "white") +
      coord_polar("y", start = 0) +
      theme_void() + 
      scale_fill_manual(values = nice(), name = "Player Name", labels = concat(), 
                        guide = guide_legend(reverse = TRUE)) +
      geom_text(aes(x = 1.3, label = percent(target_share)), position = position_stack(vjust = 0.5), size = 4) +
      geom_text(aes(x = 1.6, label = paste0(catch_rate(), "%")), position = position_stack(vjust = 0.5), size = 4) +
      labs(title = paste0("Target Share for ", input$footballTeam, " Week: ", input$gameWeek3))
    
  })  
  
  # Filter for when the selected team has possession
  plays = reactive({
    filter(pbp, posteam %in% input$footballTeam & week %in% input$gameWeek3) 
  })
  
  # new = reactive({
  #   b = data.table(table(a$play_type)) 
  #   b$pcent = b$Freq/sum(b$Freq)*100
  #   
  #   #c = mutate(pcent = b$Freq/sum(b$Freq)*100)#mutate(pcent = Freq/sum(Freq)*100)
  #     
  # })
  
  new = reactive({
    data.table(table(plays()$play_type))
  })
  
  # Calculate play type percentage
  play_types = reactive({  
    temp = new()
    temp$pcent = temp$N/sum(temp$N)*100
    # new()$N/sum(new()$N)*100  
  })
  
  total = reactive({
    cbind.data.frame(new(), play_types()) %>% rename(pcent = `play_types()`)
  })
  # Set colour palette
  pal = reactive({
    colorspace::diverge_hcl(n = nrow(total()))
  }) 
  
  output$tendencies = renderPlot({
    
    ggplot(total(), aes(x = "", y = pcent, fill = V1)) +
      geom_bar(stat = "identity", width = 1, color = "white") +
      coord_polar("y", start = 0) +
      theme_void() +
      scale_fill_manual(values = pal(), name = "Play Type", guide = guide_legend(reverse = TRUE)) +
      geom_text(aes(x = 1.3, label = percent(pcent/100)), position = position_stack(vjust = 0.5), size = 4) +
      labs(title = paste0("Play Type Percentages for ", input$footballTeam, " Week: ", input$gameWeek3))
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

