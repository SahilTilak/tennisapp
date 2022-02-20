library(shiny)
library(DT)

unique_names <- unique(elo_data$loser_name)


ui <- 
  tabPanel('Display length',     
           fluidPage(
             title = "Search Bar",
             fluidRow(
               selectizeInput("playername", 
                              label = "Search Bar",
                              multiple = FALSE,
                              choices =  c("Search Bar" = "", unique_names),
                              options = list(
                                create = FALSE,
                                placeholder = "Search Me",
                                maxItems = '1',
                                onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
                                onType = I("function (str) {if (str === \"\") {this.close();}}")
                              )
               ))),
           plotOutput(outputId = "distPlot"))




server <- function(input, output, session) {
  # below is code to speed up selectize input - not working ##
  #updateSelectizeInput(session, 'playername', choices =  c("Search Bar" = "", unique_names), server = TRUE) #
  elo_data_filter <- reactive({
    data1 <- elo_data %>%
      filter(winner_name == input$playername) %>%
      select(winner_name, full_date, w_elo_after_game, clay_w_elo_after_game, grass_w_elo_after_game, hard_w_elo_after_game, carpet_w_elo_after_game) %>%
      rename(name = winner_name, elo_after_game = w_elo_after_game, clay_elo_after_game = clay_w_elo_after_game, grass_elo_after_game = grass_w_elo_after_game, hard_elo_after_game = hard_w_elo_after_game, carpet_elo_after_game = carpet_w_elo_after_game )
    data2 <- elo_data %>%
      filter(loser_name == input$playername) %>%
      select(loser_name, full_date, l_elo_after_game, clay_l_elo_after_game, grass_l_elo_after_game, hard_l_elo_after_game, carpet_l_elo_after_game) %>%
      rename(name = loser_name, elo_after_game = l_elo_after_game, clay_elo_after_game = clay_l_elo_after_game, grass_elo_after_game = grass_l_elo_after_game, hard_elo_after_game = hard_l_elo_after_game, carpet_elo_after_game = carpet_l_elo_after_game)
    data <- rbind(data1, data2)
    data
  })
  output$table <- renderDataTable({
    datatable(
      elo_data_filter()
    )
  })
  output$distPlot <- renderPlot({
    validate(
      need(input$playername, "Enter name above")
    )
    ggplot(data = elo_data_filter(), mapping = aes(x = full_date, y = elo_after_game)) +
      geom_line()
  })
  # Show Selected Value in Console
  observe({
    print(input$searchme)
  })
}


shinyApp(ui, server)