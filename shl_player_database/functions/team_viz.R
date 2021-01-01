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
            h4("Goal Leader"),
            DTOutput(
              outputId = ns("dataTablePlayerGoal")
            )
          ),
          column(
            width = 4, 
            h4("Assist Leader"),
            DTOutput(
              outputId = ns("dataTablePlayerAssist")
            )
          ),
          column(
            width = 4,
            h4("Point Leader"),
            DTOutput(
              outputId = ns("dataTablePlayerPoint")
            )
          )
        ),
        fluidRow(
          column(
            width = 4,
            h4("Plus Minus Leader"),
            DTOutput(
              outputId = ns("dataTablePlayerPlus")
            )
          ),
          column(
            width = 4, 
            h4("Penalty Leader"),
            DTOutput(
              outputId = ns("dataTablePlayerPIM")
            )
          ),
          column(
            width = 4,
            h4("Power Play Leader"),
            DTOutput(
              outputId = ns("dataTablePlayerPPP")
            )
          )
        ),
        fluidRow(
          column(
            width = 4,
            h4("Hits Leader"),
            DTOutput(
              outputId = ns("dataTablePlayerHits")
            )
          ),
          column(
            width = 4, 
            h4("Time On Ice Leader"),
            DTOutput(
              outputId = ns("dataTablePlayerTOI")
            )
          ),
          column(
            width = 4,
            h4("Shot Blocking Leader"),
            DTOutput(
              outputId = ns("dataTablePlayerShotsBlocked")
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
          filter(Conference == "West") %>% 
          select(-Conference)
        
      },
      rownames = FALSE,
      class = 'compact cell-border stripe',
      selection = 'single',
      options = 
        list(
          dom = "t",
          paging = FALSE,
          orderClasses = TRUE
        )
      )
      
      ## Outputs a datatable of all the teams in the Eastern conference
      output$dataTableEast <- DT::renderDT({
        selectedData()$standings %>% 
          filter(Conference == "East") %>% 
          select(-Conference)
        
      },
      rownames = FALSE,
      class = 'compact cell-border stripe',
      selection = 'single',
      options = 
        list(
          dom = "t",
          paging = FALSE,
          orderClasses = TRUE
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
              GP:SHTOI
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
              GP:SHTOI
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
      
      ## Outputs player statistics from selected team
      output$dataTablePlayerGoal <- renderDT({
        playerData() %>% 
          select(
            TEAM:GP,
            G
          ) %>% 
          arrange(
            desc(G)
          ) %>% 
          slice(
            1:5
          )
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
      output$dataTablePlayerAssist <- renderDT({
        playerData() %>% 
          select(
            TEAM:GP,
            A
          ) %>% 
          arrange(
            desc(A)
          ) %>% 
          slice(
            1:5
          )
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
      
      output$dataTablePlayerPoint <- renderDT({
        playerData() %>% 
          select(
            TEAM:GP,
            P
          ) %>% 
          arrange(
            desc(P)
          ) %>% 
          slice(
            1:5
          )
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
      output$dataTablePlayerPlus <- renderDT({
        playerData() %>% 
          select(
            TEAM:GP,
            `+/-`
          ) %>% 
          arrange(
            desc(`+/-`)
          ) %>% 
          slice(
            1:5
          )
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
      output$dataTablePlayerPIM <- renderDT({
        playerData() %>% 
          select(
            TEAM:GP,
            PIM
          ) %>% 
          arrange(
            desc(PIM)
          ) %>% 
          slice(
            1:5
          )
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
      output$dataTablePlayerPPP <- renderDT({
        playerData() %>% 
          select(
            TEAM:GP,
            PPP
          ) %>% 
          arrange(
            desc(PPP)
          ) %>% 
          slice(
            1:5
          )
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
      
      #####################SOMETHING IS HAPPENING HERE WITH THE CONVERSION THAT IS NOT CORRECT!!!
      ## Outputs player statistics from selected team
      output$dataTablePlayerTOI <- renderDT({
        playerData() %>% 
          select(
            TEAM:GP,
            TOI
          ) %>% 
          arrange(
            desc(TOI)
          ) %>% 
          slice(
            1:5
          ) %>% 
          ## Converts TOI from nr of seconds to minutes:seconds format
          mutate(
            TOI = format(as.POSIXct(TOI, origin = "1970-01-01"), "%M:%S")
          )
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
      output$dataTablePlayerHits <- renderDT({
        playerData() %>% 
          select(
            TEAM:GP,
            HITS
          ) %>% 
          arrange(
            desc(HITS)
          ) %>% 
          slice(
            1:5
          )
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
      output$dataTablePlayerShotsBlocked <- renderDT({
        playerData() %>% 
          select(
            TEAM:GP,
            SB
          ) %>% 
          arrange(
            desc(SB)
          ) %>% 
          slice(
            1:5
          )
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

