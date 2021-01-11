radar_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    ## Layout of the option sidebar
    # Option for selection of type of player/goalie
    sidebarLayout(
      sidebarPanel(
        width = 2,
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
        
        ## Selection of player type to show
        radioButtons(
          inputId = ns("player_type"),
          label = "Select type to show",
          choices = 
            c(
              "Goalies" = "goalies",
              "Players" = "players"
            ),
          selected = "players"
        ),
        em(paste("This data is scraped from the S57 team rosters
        and the S57 index pages of the SHL. Data from", date_scraped))

      ),
      mainPanel(
        width = 8,
        plotlyOutput(
          outputId = ns("radarPlotly")
        ),
        DTOutput(
          outputId = ns("dataTable")
        ),
        p("The data contains more information that what is shown. Select your 
          own columns from the Player Stats, Advanced Stats or Ratings index
          pages with the", em("Columns Visibility"), "button.")
      )
    )
  )
}


## Backend for vizualizations
radar_server <- function(id){
  ## Calling moduleServer function
  moduleServer(
    id,
    function(input, output, session){
      ## Initializes reactiveValues
      chosenRow <- reactiveVal(NULL)
      
      ## Observes options of player and goalie
      selectedData <- reactive({
        if(input$league == "shl"){
          data <- shl_data[[input$player_type]]
        } else {
          data <- smjhl_data[[input$player_type]]
        }
        
        data <- data %>% 
          select(
            team,
            First.Name,
            Last.Name,
            user,
            season,
            Position,
            Handedness,
            tpe,
            Birthplace,
            where(is.numeric)
          ) %>% 
          mutate(
            Name = paste(First.Name, Last.Name)
          ) %>% 
          select(
            -PId,
            -HT,
            -WT
          ) 
        
        return(data)
      })
      
      ## Outputs a datatable of all the players
      output$dataTable <- DT::renderDT({
        selectedData() %>% 
          rename_with(
            str_to_upper
          ) %>% 
          select(-NAME)
        },
        filter = 'bottom',
        rownames = FALSE,
        class = 'compact cell-border stripe',
        selection = 'single',
        options = 
          list(
            orderClasses = TRUE, 
            ## Sets a scroller for the rows
            scrollY = '400px',
            ## Sets size of rows shown
            scrollCollapse = TRUE,
            ## Removes pages in the table
            paging = FALSE,
            ## Adds scrollable horizontal
            scrollX = '600px',
            # pageLength = 10,
            # lengthMenu = c(10, 25, 50, 100),
            dom = 'Bfrtip',
            bInfo = FALSE,
            buttons = c('copy', 'csv', 'excel', I('colvis')),
            ## Defines which columns the table shows by default
            columnDefs = list(
              list(
                visible = TRUE,
                targets = 0:6
              ),
              list(
                visible = FALSE,
                targets = '_all'
              )
            )
          ),
        extensions = c('Buttons')
      )
      
      ## Observes event of last row that is clicked
      observeEvent(
        input$dataTable_rows_selected,
        {
          chosenRow(input$dataTable_rows_selected)
        }
      )
      
      output$radarPlotly <- renderPlotly({
        if(is.null(chosenRow())){
          plotly_empty(
            type = "scatter", 
            mode = "markers",
            width = 500,
            height = 400
          ) %>%
            config(
              displayModeBar = FALSE
            ) %>%
            layout(
              title = list(
                text = "Select a player in the table below to show visualization",
                yref = "paper",
                y = 0.5
              )
            )
        } else {
          selectedData() %>%
            slice(
              chosenRow()
            ) %>%
            mutate(
              cap_tpe =
                case_when(
                  season %in% c("S58", "S57") & tpe > 350 ~ 350,
                  tpe > 425 ~ 425,
                  TRUE ~ tpe
                )
            ) %>%
            select(
              Name,
              Position,
              AGR:last_col()
            ) %>%
            pivot_longer(
              AGR:last_col(),
              names_to = "Attribute",
              values_to = "Rating"
            ) %>%
            left_join(
              att_key,
              by = c("Attribute" = "abb")
            ) %>%
            mutate(
              Attribute =
                factor(
                  Attribute,
                  levels = c("SCR", "GTO", 'PAS', 'PHA', 'SAC', 'SRA', 'OFR',
                             'CHE', 'HIT', 'POS', 'SCH', 'SBL', 'FOF', 'DFR',
                             'ACC', 'AGI', 'SPD', 'STA', 'STR', 'BAL', 'FIG',
                             'AGR', 'BRA', 'DET', 'TPL', 'LEA', 'TEM', 'PRO',
                             'MTO', 'GST', 'BLO', 'GLO', 'GPA', 'POK', 'GPO', 
                             'REB', 'REC', 'GPH', 'LOW', 'REF', 'GSK')
                ),
              text = paste(att, Rating, sep = ": ")
            ) %>%
            arrange(
              Attribute
            ) %>%
            plot_ly(
              type = 'scatterpolar',
              mode = "markers",
              r = ~Rating,
              theta = ~Attribute,
              text = ~text,
              fill = 'toself',
              hoverinfo = "text",
              color = I("#cf5b00"),
              name = ~Name,
              width = 500,
              height = 400
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
              polar =
                list(
                  radialaxis =
                    list(
                      visible = TRUE,
                      range = c(0,20)
                    )
                ),
              ## Legend is put to false so the plot is the same size
              showlegend = FALSE
          )
        }
      })
    }
  )
}

