

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

table_dat = all_dat
plot_dat = all_dat


# Define UI 
ui = fluidPage(theme = shinytheme("yeti"),
               titlePanel("Fantasy Football Stuff"), 
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
      ggplot(plot_dat, mapping = aes(week, completions)) + 
      geom_line() + 
      labs(title = paste(input$player, "completions over the season")) + 
      ylab("# Completions") + xlab('Game Week') + 
      theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())
    } else if (input$group == "WR"){
      plot_dat %>%
        filter(plot_dat$player_name %in% input$player) %>%
        ggplot(plot_dat, mapping = aes(week, receptions)) + 
        geom_line() + 
        labs(title = paste(input$player, "receptions over the season")) + 
        ylab("# Receptions") + xlab('Game Week') + 
        theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())
    } else if (input$group == "RB"){
      plot_dat %>%
        filter(plot_dat$player_name %in% input$player) %>%
        ggplot(plot_dat, mapping = aes(week, rushing_yards)) + 
        geom_line() + 
        labs(title = paste(input$player, "Rushing yards over the season")) + 
        ylab("# Rushing Yards") + xlab('Game Week') + 
        theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())
    } else if (input$group == "TE"){
      plot_dat %>%
        filter(plot_dat$player_name %in% input$player) %>%
        ggplot(plot_dat, mapping = aes(week, receptions)) + 
        geom_line() + 
        labs(title = paste(input$player, "receptions over the season")) + 
        ylab("# Receptions") + xlab('Game Week') + 
        theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())
    } else {}
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
