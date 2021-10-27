# Libraries----
library(shiny)
library(understatr)
library(tidyverse)
library(DT)
library(ggrepel)
library(ggimage)
library(shinycssloaders)
library(bslib)
library(thematic)

# Data----
ggplot2::theme_set(
    ggplot2::theme_minimal() +
        theme(
            axis.text = element_text(size = 15), axis.title = element_text(size = 18),
            legend.text = element_text(size = 10),
            legend.position = 'top', legend.justification = 'center'
        )
)
thematic_shiny(font = "auto", qualitative = c("#3f043f", "#04b46c"))

# epl_2021 <- read_csv('epl_2021.csv')
epl_2021 <- get_league_teams_stats(league_name = "EPL", year = 2021)
team_logo <- read_csv('team_logo.csv')

# Standings
epl_standings <- epl_2021 %>% 
    group_by(team_name) %>% 
    summarise(
        Loses = sum(loses),
        Draws = sum(draws),
        Wins = sum(wins),
        Pts = sum(pts),
        GD = sum(scored)-sum(missed),
        Comment = '',
        xG = sum(xG),
        xGA = sum(xGA),
        .groups = 'drop'
    ) %>% 
    arrange(desc(Pts)) %>% 
    left_join(team_logo) %>% 
    mutate(Team = sprintf('<img src="%s" height="28"></img>', logo)) %>% 
    relocate(Team)

# Cumulative xG
roll_xG <- epl_2021 %>% 
    group_by(team_name) %>%
    arrange(team_name, date) %>% 
    transmute(
        match_day = 1:n(),
        team_name,
        xG = cumsum(xG),
        xGA = cumsum(xGA),
        G = cumsum(scored),
        GA = cumsum(missed)
    ) %>% 
    ungroup() %>% 
    pivot_longer(contains('G')) %>% 
    mutate(type = if_else(name %in% c('G', 'GA'), 'real', 'expected'))

# UI-----

ui <- navbarPage(
    theme = bs_theme(primary = "#3f043f", success = "#04b46c", 
                     base_font = font_google("Rubik"), heading_font = font_google("Space Mono"), 
                     font_scale = NULL, bootswatch = "flatly"),
    title = div(
        img(src = "epl-logo.png", height = 70, width = 70, 
            style = "padding-right:15px"),
        "EPL Standings & xG"
    ),
    windowTitle = "EPL standings",
    tabPanel(
        'table-contest-2021',
        fluidRow(
            column(
                6,
                h3('Standings'),
                'Selectable DT - display cumulative xG per team selected. Editable DT - add comment to Total xG plot',
                DT::dataTableOutput('standings') %>% withSpinner()
            ),
            column(
                6,
                h3('Total xG vs xGA by Team'),
                br(),
                plotOutput('total_xG', height = '550px') %>% withSpinner()
            )
        )
    )
)

# Server-----
server <- function(input, output) {
    
    # Guide posts:
    # https://community.rstudio.com/t/editing-a-reactive-dt-table-that-remembers-the-filtering-context-without-page-flickering/28826/3
    # https://github.com/rstudio/DT/pull/793
    
    # Create Empty Reactive Value
    table_reactive_value <- reactiveValues(df = NULL)
    
    # observe in case of epl_standings would be reactive
    observe({
        table_reactive_value$df <- epl_standings
    })
    
    # Creating table proxy to store previous values
    standings_proxy <- dataTableProxy('standings')
    
    # Update table
    observeEvent(input$standings_cell_edit, {
        info = input$standings_cell_edit
        str(info)
        
        # Location and value of the edit
        i = info$row
        j = info$col 
        v = info$value
        
        # Coerce value of the same data type and replace proxy
        table_reactive_value$df[i, j] <- DT::coerceValue(v, pull(table_reactive_value$df[i, j]))
        aux_table <- table_reactive_value$df
        replaceData(standings_proxy, aux_table, resetPaging = FALSE, rownames = T)
    })
    
    team_selected <- reactive({
        selected_team <- epl_standings %>% 
            slice(input$standings_cells_selected[1,1]) %>% 
            pull(team_name)
    })
    
    # Modal to display cumulative xG
    observeEvent(
        input$standings_cells_selected,
        {
            if(nrow(input$standings_cells_selected)>0){
                showModal(
                    modalDialog(
                        title = paste0(team_selected(), " Cumulative xG"),
                        easyClose = T,
                        size = 'l',
                        plotOutput('rolling_xG')
                    )
                ) 
            }
        }
    )
    
    output$rolling_xG <- renderPlot({
        roll_xG %>%
            filter(team_name == team_selected()) %>% 
            ggplot(aes(x = match_day, y = value, color = name)) +
            geom_line(aes(linetype = type, alpha = type)) +
            scale_color_manual(breaks = c("xG", 'xGA'),
                               values = c("#3f043f", "#04b46c", "#3f043f", "#04b46c")) +
            scale_linetype_manual(breaks = c("real", "expected"), values = c("dotted", "solid")) +
            scale_alpha_manual(breaks = c("real", "expected"), values = c(.8, 1)) +
            scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1)) +
            theme(legend.title = element_blank(), axis.text.x = element_text(color = 'gray')) +
            labs(x = 'Match Day', y = 'Goals')
        
        
    })
    
    output$standings <- DT::renderDataTable({
        
        ncolumns <- ncol(epl_standings)
        nrows <- nrow(epl_standings)
        unselectable_matrix <- matrix(c(-(1:nrows), rep(-8, nrows)), ncol = 2)
        
        epl_standings %>%
            DT::datatable(
                rownames = T,
                style = 'bootstrap',
                selection = list(mode = 'single', target = "cell", selectable = unselectable_matrix),
                editable = list(target = "cell", disable = list(columns = 1:7)),
                escape = F,
                options = list(
                    searching = F,
                    paging = F,
                    info = F,
                    columnDefs = list(
                        list(visible=FALSE, targets = c(9,10,11)),
                        list(className = 'dt-center', targets = '_all')
                    )
                )
            )
        
    })

    output$total_xG <- renderPlot({
        
        table_reactive_value$df %>%
            ggplot(aes(x = xG, y = xGA, label = Comment)) +
            geom_image(aes(image = logo), asp = 10/5, size = .04) +
            geom_text_repel(box.padding = 1, max.overlaps = Inf, size = 4,
                            min.segment.length = 0, segment.curvature = -.5) +
            labs(x = 'Expected Goal (xG)', y = 'Expected Goal Against (xGA)')
        
    })
        
}

shinyApp(ui = ui, server = server)
