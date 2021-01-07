team_ui <- function(id){
  ns <- NS(id)
  
  
  tagList(
    ## Layout of the option sidebar
    # Option for selection of type of player/goalie
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h3("S57"),
        ## Selection of league
        radioButtons(
          inputId = ns("league"),
          label = "Select league to show",
          choices = 
            c(
              "SHL" = "shl",
              "SMJHL" = "smjhl"
            ),
          selected = "shl"
        ),
        uiOutput(
          outputId = ns("draftClass")
        ),
        actionButton(
          inputId = ns("reset"),
          label = "Reset team selection"
        ),
        br(),
        em(paste("This data is scraped from the S57 schedule and
        and the S57 index pages of the SHL. Data from", date_scraped))
      ),
      mainPanel(
        width = 9,
        em("Selecting a team in the standings will filter players from that team 
             in the table below."),
        tabsetPanel(
          tabPanel(
            "Western Conference",
            DTOutput(
              outputId = ns("dataTableWest")
            )  
          ),
          tabPanel(
            "Eastern Conference",
            DTOutput(
              outputId = ns("dataTableEast")
            )  
          )
        )
      )
    ),
    tabsetPanel(
      tabPanel(
        "Statistics Leaders",
        fluidRow(
          column(
            width = 4,
            selectInput(
              inputId = ns("statisticOne"),
              label = "Choose statistic",
              choices = 
                ## Uses the player statistic vector defined in data_loader.R
                playerStatistics,
              selected = "G"
            ),
            DTOutput(
              outputId = ns("dataTableOne")
            )
          ),
          column(
            width = 4, 
            selectInput(
              inputId = ns("statisticTwo"),
              label = "Choose statistic",
              choices = 
                playerStatistics,
              selected = "A"
            ),
            DTOutput(
              outputId = ns("dataTableTwo")
            )
          ),
          column(
            width = 4,
            selectInput(
              inputId = ns("statisticThree"),
              label = "Choose statistic",
              choices = 
                playerStatistics,
              selected = "P"
            ),
            DTOutput(
              outputId = ns("dataTableThree")
            )
          )
        ),
        fluidRow(
          column(
            width = 4,
            selectInput(
              inputId = ns("statisticFour"),
              label = "Choose statistic",
              choices = 
                playerStatistics,
              selected = "+/-"
            ),
            DTOutput(
              outputId = ns("dataTableFour")
            )
          ),
          column(
            width = 4, 
            selectInput(
              inputId = ns("statisticFive"),
              label = "Choose statistic",
              choices = 
                playerStatistics,
              selected = "PIM"
            ),
            DTOutput(
              outputId = ns("dataTableFive")
            )
          ),
          column(
            width = 4,
            selectInput(
              inputId = ns("statisticSix"),
              label = "Choose statistic",
              choices = 
                playerStatistics,
              selected = "SB"
            ),
            DTOutput(
              outputId = ns("dataTableSix")
            )
          )
        ),
        br()
      ),
      tabPanel(
        "Matchup results",
        br(),
        p("The following diagram gives an overview on the matchup results 
            during the season for each Away-Home matchup. A darker color indicates 
            a more onesided matchup. Hover over each cell to see more information."),
        plotlyOutput(
          outputId = ns("matchupHeatmap")
        ) 
      )
    )
  )
}


## Backend for vizualizations
team_server <- function(id){
  ns <- NS(id)
  ## Calling moduleServer function
  moduleServer(
    id,
    function(input, output, session){
      ## Initializes reactiveValues
      chosenRow <- reactiveVal(NULL)
      
      ## Observes options of player and goalie
      selectedData <- reactive({
        if(input$league == "shl"){
          data <- shl_data 
        } else {
          data <- smjhl_data 
        }
        
        return(data)
      })
      
      ## Outputs a UI with all available draft classes
      output$draftClass <- renderUI({
        data <- selectedData()$players
        
        selectInput(
          inputId = ns("draftClass"),
          label = "Select draft class",
          choices = 
            c(
              "ALL",
              sort(
                unique(data$season)
              )
            ),
          selected = "ALL"
        )
      })
      
      
      ## Outputs a datatable of all the teams in the Western conference
      output$dataTableWest <- DT::renderDT({
        selectedData()$standings %>% 
          left_join(
            division_key,
            by = c("Team" = "team")
          ) %>% 
          filter(Conference == "West") %>% 
          select(
            -Conference,
            -conference
            ) %>% 
          arrange(
            desc(Division),
            desc(Points)
          )
      },
      rownames = FALSE,
      class = 'compact cell-border stripe',
      selection = 'single',
      extensions = 'RowGroup',
      options = 
        list(
          dom = "t",
          paging = FALSE,
          ordering = FALSE,
          rowGroup = list(dataSrc = 11)
        )
      )
      
      ## Outputs a datatable of all the teams in the Eastern conference
      output$dataTableEast <- DT::renderDT({
        selectedData()$standings %>% 
          left_join(
            division_key,
            by = c("Team" = "team")
          ) %>%
          filter(Conference == "East") %>% 
          select(
            -Conference,
            -conference
          ) %>% 
          arrange(
            desc(Division),
            desc(Points)
          )
        
      },
      rownames = FALSE,
      class = 'compact cell-border stripe',
      selection = 'single',
      extensions = 'RowGroup',
      options = 
        list(
          dom = "t",
          paging = FALSE,
          ordering = FALSE,
          rowGroup = list(dataSrc = 11)
        )
      )
      
      ## Observes event of last row that is clicked
      observeEvent(
        input$dataTableEast_rows_selected,
        {
          chosenRow(input$dataTableEast_rows_selected)
        }
      )
      observeEvent(
        input$dataTableWest_rows_selected,
        {
          chosenRow(input$dataTableWest_rows_selected+(nrow(selectedData()$standings)/2))
        }
      )
      observeEvent(
        input$reset, {
        # reset the list
        chosenRow(NULL)
      })
      
      observeEvent(input$league, {
        # reset the list
        chosenRow(NULL)
      })
      
      ## Selects League or Team-specific data
      playerData <- reactive({
        
        if(input$draftClass == "ALL"){
          data <- selectedData()$players
        } else {
          data <- selectedData()$players %>% 
            filter(
              season %in% input$draftClass
            )
        }
        
        if(is.null(chosenRow())){
          data <- data %>%
            rename(
              GP = GP.x
            ) %>% 
            select(
              Team,
              season,
              First.Name,
              Last.Name,
              Position,
              GP:`FF% rel`
            ) %>% 
            mutate(
              Name = paste(First.Name, Last.Name)
            ) %>% 
            relocate(
              Name,
              .before = Position
            ) %>% 
            select(
              -First.Name,
              -Last.Name
            ) %>%  
            rename_with(
              str_to_upper
            ) 
        } else{
          chosenTeam <- selectedData()$standings %>% 
            left_join(
              division_key,
              by = c("Team" = "team")
            ) %>% 
            arrange(
              Conference,
              desc(Division),
              desc(Points)
            ) %>% 
            slice(
              chosenRow()
            ) %>% 
            select(
              Team
            )
          
          data <- data %>% 
            filter(
              team == chosenTeam$Team
            ) %>%
            rename(
              GP = GP.x
            ) %>% 
            select(
              Team,
              season,
              First.Name,
              Last.Name,
              Position,
              GP:`FF% rel`
            ) %>% 
            mutate(
              Name = paste(First.Name, Last.Name)
            ) %>% 
            relocate(
              Name,
              .before = Position
            ) %>% 
            select(
              -First.Name,
              -Last.Name
            ) %>% 
            rename_with(
              str_to_upper
            )
        }
        
        return(data)
      })
      
      ## Creates a function that uses the chosen statistic and creates the table
      statSelector <- function(statistic){
        data <- playerData() %>% 
          select(
            TEAM:GP,
            one_of(statistic)
          ) %>% 
          arrange(
            desc(.data[[statistic]])
          ) %>% 
          slice(
            1:10
          )
        
        if(str_detect(statistic, pattern = "TOI")){
          data <- 
            data %>% 
            mutate(
              "{statistic}" := 
                format(
                  as.POSIXct(
                    .data[[statistic]], 
                    origin = "1970-01-01"
                  ), 
                  "%M:%S"
                )
            )
        }
        
        return(data)
      }
      
      ## Outputs player statistics from selected team
      output$dataTableOne <- renderDT({
        statSelector(input$statisticOne)
      },
      rownames = FALSE,
      class = 'compact cell-border stripe',
      selection = 'single',
      options = 
        list(
          dom = "t",
          ordering = FALSE
        )
      )
      ## Outputs player statistics from selected team
      output$dataTableTwo <- renderDT({
        statSelector(input$statisticTwo)
      },
      rownames = FALSE,
      class = 'compact cell-border stripe',
      selection = 'single',
      options = 
        list(
          dom = "t",
          ordering = FALSE
        )
      )
      
      output$dataTableThree <- renderDT({
        statSelector(input$statisticThree)
      },
      rownames = FALSE,
      class = 'compact cell-border stripe',
      selection = 'single',
      options = 
        list(
          dom = "t",
          ordering = FALSE
        )
      )
      ## Outputs player statistics from selected team
      output$dataTableFour <- renderDT({
        statSelector(input$statisticFour)
      },
      rownames = FALSE,
      class = 'compact cell-border stripe',
      selection = 'single',
      options = 
        list(
          dom = "t",
          ordering = FALSE
        )
      )
      
      ## Outputs player statistics from selected team
      output$dataTableFive <- renderDT({
        statSelector(input$statisticFive)
      },
      rownames = FALSE,
      class = 'compact cell-border stripe',
      selection = 'single',
      options = 
        list(
          dom = "t",
          ordering = FALSE
        )
      )
      ## Outputs player statistics from selected team
      output$dataTableSix <- renderDT({
        statSelector(input$statisticSix)
      },
      rownames = FALSE,
      class = 'compact cell-border stripe',
      selection = 'single',
      options = 
        list(
          dom = "t",
          ordering = FALSE
        )
      )
      
      ## Creates the heatmap
      output$matchupHeatmap <- renderPlotly({
        ## Gets the order of the teams in each conference by alphabetical order
        teamOrder <- selectedData()$standings %>% 
          group_by(Conference) %>% 
          select(Team) %>% 
          ## Sorted by alphabetical order
          arrange(Team, .by_group = TRUE)
        
        ## Imports the schedule from the selected league
        data <- selectedData()$schedule %>% 
          ## Filters the regular season games that have been played
          filter(
            Played == TRUE,
            `Game Type` == "Regular Season"
          ) %>% 
          ## Groups the calculations of the specific away-home matchups
          group_by(`Away Team`, `Home Team`) %>% 
          ## Calculates new variables for each matchup
          mutate(
            n = n(),
            AwayPtsGained = sum(AwayPts),
            HomePtsGained = sum(HomePts)
          ) %>% 
          mutate(
            `AwayPts%` = AwayPtsGained / (n*2),
            `HomePts%` = HomePtsGained / (n*2)
          ) %>% 
          ## Calculates the equality of the matchup where values close to 0 
          ## indicate a an even matchup 
          mutate(
            equality = `AwayPts%`-`HomePts%`
          ) %>% 
          select(
            `Away Team`,
            `Home Team`,
            `AwayPts%`,
            `HomePts%`,
            n,
            equality
          ) 
        
        ## Creates the matrix of data for the base of the heatmap, showing equality
        plotData <- data %>% 
          select(
            `Away Team`,
            `Home Team`,
            equality
          ) %>% 
          mutate(
            `Away Team` = factor(`Away Team`, levels = teamOrder$Team)
          ) %>% 
          pivot_wider(
            names_from = `Home Team`,
            values_from = equality,
            values_fn = unique
          ) %>% 
          arrange(
            `Away Team`
          ) %>% 
          tibble::column_to_rownames(
            "Away Team"
          ) %>% 
          select(
            teamOrder$Team
          )
        
        ## Creates a matrix of away data for the hoverinfo
        awayData <- data %>% 
          select(
            `Away Team`,
            `Home Team`,
            `AwayPts%`
          ) %>% 
          mutate(
            `Away Team` = factor(`Away Team`, levels = teamOrder$Team)
          ) %>% 
          pivot_wider(
            names_from = `Home Team`,
            values_from = `AwayPts%`,
            values_fn = unique
          ) %>% 
          arrange(
            `Away Team`
          ) %>% 
          tibble::column_to_rownames(
            "Away Team"
          ) %>% 
          select(
            teamOrder$Team
          )
        
        ## Creates a matrix of home data for the hoverinfo
        homeData <- data %>% 
          select(
            `Away Team`,
            `Home Team`,
            `HomePts%`
          )  %>%  
          mutate(
            `Away Team` = factor(`Away Team`, levels = teamOrder$Team)
          ) %>% 
          pivot_wider(
            names_from = `Home Team`,
            values_from = `HomePts%`,
            values_fn = unique
          ) %>% 
          arrange(
            `Away Team`
          ) %>% 
          tibble::column_to_rownames(
            "Away Team"
          ) %>% 
          select(
            teamOrder$Team
          )
        
        ## Creates a matrix of number of games played for hoverinfo
        nGamesData <- data %>% 
          select(
            `Away Team`,
            `Home Team`,
            n
          ) %>%  
          mutate(
            `Away Team` = factor(`Away Team`, levels = teamOrder$Team)
          ) %>% 
          pivot_wider(
            names_from = `Home Team`,
            values_from = `n`,
            values_fn = unique
          ) %>% 
          arrange(
            `Away Team`
          ) %>% 
          tibble::column_to_rownames(
            "Away Team"
          ) %>% 
          select(
            teamOrder$Team
          )
        
        ## Creates a matrix of text annotations
        hovertext <- awayData
        for(i in 1:nrow(awayData)){
          for(j in 1:ncol(awayData)){
            hovertext[i,j] <- paste("Away:", rownames(awayData)[i], 
                                    "\nHome:", colnames(awayData)[j], 
                                    "\nAwayPts%:", round(awayData[i,j],3),
                                    "\nHomePts%:", round(homeData[i,j],3),
                                    "\nGames Played:", nGamesData[i,j],
                                    sep = " ")
          }
        }
        
        plotData %>% 
          plot_ly(
            type = 'heatmap',
            x = rownames(.),
            y = rownames(.),
            z = as.matrix(.),
            colors = colorRamp(c("#cf5b00", "white", "black")),
            colorbar = 
              list(
                title = "<b>Matchup favor</b>\n",
                tickvals = c(-1, 0, 1),
                ticktext = c("Home", "Even", "Away")
              ),
            width = 600,
            height = 500,
            text = as.matrix(hovertext),
            hoverinfo = "text"
          ) %>%
          config(
            modeBarButtonsToRemove =
              c("pan2d", "zoomIn2d", "zoomOut2d", "autoScale2d",
                "resetScale2d", "hoverClosestCartesian",
                "hoverCompareCartesian", "toggleSpikelines"
              )
          ) %>%
          layout(
            autosize = FALSE,
            xaxis =
              list(
                title = "Home Team",
                tickangle = -45
              ),
            yaxis =
              list(
                title = "Away Team"
              )
          )
        })
    }
  )
}

