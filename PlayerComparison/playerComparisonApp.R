

library(nflreadr)
library(data.table)
library(dplyr)
library(ggplot2)
library(shiny)
library(shinythemes)


# Do any weird handling of names before the data gets loaded into the 'app'
# Filter players that only occur once
# Add 3 inputs so I can compare between players and then rendering a graph will be a little more straight forward

dat = data.table(load_player_stats()) # Load all the player stats for the season so far
colnames(dat)[1] = "gsis_id" # change the name of the column to make merging later easy
week = dat[player_name != "H.Ruggs"]
weekly_players = unique(week$gsis_id) # Get the unique IDs of the players for the week
names = data.table(load_rosters(2021)) # Load in the complete roster

players = names[names$gsis_id %in% weekly_players, ] # Filter players so only the ones who played count
all_dat = merge(week, players) # Merge player stats with roster data so we have player positions
all_dat = all_dat[position %in% c("QB", "WR", "TE", "RB"),]

# Make A.Rodgers the Aa.Rodgers
all_dat$player_name = gsub("A.Rodgers", "Aa.Rodgers", all_dat$player_name)
# Remove anyone who has played less than 5 games
t = data.frame(table(all_dat$gsis_id)) 
t = t[t$Freq >=5,]
all_dat = all_dat[gsis_id %in% t$Var1,]

table_dat = all_dat # New variable just for the table and plot to keep things more simple
plot_dat = all_dat


# Define UI 
ui = fluidPage(theme = shinytheme("yeti"),
               titlePanel(paste0("Fantasy Football Stuff -- Version 1.02: ", Sys.Date())), 
               sidebarLayout(
                 
                 sidebarPanel(
                   #radioButtons('positionGroup', label = h5("Select a position group"), 
                                #choices = list("QB" = 1, "WR" = 2, "RB" = 3)),
                   selectInput('group', 'pick a position', unique(table_dat$position)),
                   selectInput('player', 'Player Name', choices = NULL),
                   selectInput('gameWeek', 'Game Week', table_dat$week)
                   
                 ), # sidebarPanel
                 
                 mainPanel(
                   plotOutput('plot1'),
                   tableOutput('t1')
                 ) # mainPanel
                 
               ) # sidebarLayout

)


# Define server
server <- function(input, output) {
  
  # Filter the player drop down list based on the position group selected
  this <- reactive({
    filter(table_dat, position == input$group)
  })
  observeEvent(this(), {
  choices <- unique(this()$player_name)
  updateSelectInput(inputId = "player", choices = choices)
  })
  
  
  # Data table that is below the graph. This is filtered based on the player and week inputs selected
  output$t1 <- renderTable({
    
    if (input$group == "QB") {
      
    table_dat %>%
      filter(table_dat$player_name %in% input$player & table_dat$week %in% input$gameWeek) %>%
      select(c(2,3,7:12))
      
    } else if (input$group == "WR"){
      
      table_dat %>%
        filter(table_dat$player_name %in% input$player & table_dat$week %in% input$gameWeek) %>%
        select(c(2,3,30:38))
      
    } else if (input$group == "RB"){
      
      table_dat %>%
        filter(table_dat$player_name %in% input$player & table_dat$week %in% input$gameWeek) %>%
        select(c(2,3,22:27))
      
    } else if (input$group == "TE"){
      
      table_dat %>%
        filter(table_dat$player_name %in% input$player & table_dat$week %in% input$gameWeek) %>%
        select(c(2,3,30:38))
      
    } else {}
  })
  
  # Plot is created based on the player selected. It is completions over the season but perhaps other buttons 
  # can be added to allow for the user to select what they want to look at
  output$plot1 <- renderPlot({
    if (input$group == "QB") {
      
    plot_dat %>%
      filter(plot_dat$player_name %in% input$player) %>%
      ggplot(plot_dat, mapping = aes(week)) +
        geom_line(aes(y = completions, color = "completions")) +       
        geom_point(aes(y = completions, color = "completions")) +       
        geom_line(aes(y = attempts, color = "attempts")) + 
        geom_point(aes(y = attempts, color = "attempts")) + 
        scale_color_manual("", breaks = c("completions","attempts"), 
                           values = c("red", "blue")) +
      labs(title = paste(input$player, "completions/attempts over the season")) + 
      ylab("Count") + xlab('Game Week') + 
      theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())
      
    } else if (input$group == "WR"){
      
      plot_dat %>%
        filter(plot_dat$player_name %in% input$player) %>%
        ggplot(plot_dat, mapping = aes(week)) + 
        geom_line(aes(y = receptions, color = "receptions")) +       
        geom_point(aes(y = receptions, color = "receptions")) +       
        geom_line(aes(y = targets, color = "targets")) + 
        geom_point(aes(y = targets, color = "targets")) + 
        scale_color_manual("", breaks = c("receptions","targets"), 
                           values = c("red", "blue")) +
        labs(title = paste(input$player, "receptions/targets over the season")) + 
        ylab("Count") + xlab('Game Week') + 
        theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())
      
    } else if (input$group == "RB"){
      
      plot_dat %>%
        filter(plot_dat$player_name %in% input$player) %>%
        ggplot(plot_dat, mapping = aes(week, rushing_yards)) + 
        geom_line(aes(y = rushing_yards, color = "rushing_yards")) +
        geom_point(aes(y = rushing_yards, color = "rushing_yards")) +
        geom_line(aes(y = carries, color = "carries")) + 
        geom_point(aes(y = carries, color = "carries")) + 
        scale_color_manual("", breaks = c("rushing_yards","carries"), 
                           values = c("red", "blue")) +
        labs(title = paste(input$player, "Rushing yards/carries over the season")) + 
        ylab("Count") + xlab('Game Week') + 
        theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())
      
    } else if (input$group == "TE"){
      
      plot_dat %>%
        filter(plot_dat$player_name %in% input$player) %>%
        ggplot(plot_dat, mapping = aes(week)) + 
        geom_line(aes(y = receptions, color = "receptions")) +       
        geom_point(aes(y = receptions, color = "receptions")) +       
        geom_line(aes(y = targets, color = "targets")) + 
        geom_point(aes(y = targets, color = "targets")) + 
        scale_color_manual("", breaks = c("receptions","targets"), 
                           values = c("red", "blue")) +
        labs(title = paste(input$player, "receptions/targets over the season")) + 
        ylab("Count") + xlab('Game Week') + 
        theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())
      
    } else {}
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
