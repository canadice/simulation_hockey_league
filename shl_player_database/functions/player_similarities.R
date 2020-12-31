### UI module for player similarities using MDS
similarity_ui <- function(id){
  
  ## Creating the namespacing function for all IDs
  ns <- NS(id)
  
  tagList(
    ## Layout of the option sidebar
    # Option for selection of season of players
    # Option for coloring in visualization
    sidebarLayout(
      sidebarPanel(
        width = 3,
        ## Season selection
        numericInput(
          inputId = ns("season"),
          label = "Which season do you want to visualize?",
          min = 53,
          max = 56,
          value = 56
        ),
        ## Coloring selection
        radioButtons(
          inputId = ns("color_group"),
          label = "What should define the colors?",
          choices = 
            c(
              "Position" = "Pos",
              "Team" = "Team"
            )
        ),
        em("Disclaimer: The used data is taken from the SHL Index page with 
        the given position from that table. I am aware that some differences 
        between the played and created position might be present. There also 
           exists 'scouting errors', where some attributes are shown +/- 1 from 
           the real values.")
      ),
      ## Shows the results of the visualization and explanation
      mainPanel(
        tabsetPanel(
          type = "tabs",
          ## Visualization
          tabPanel(
            "Visualization",
            br(),
            ## Some explanatory text of the visualization
            h4("This is an interactive plot"
            ),
            p("Clicking on individual groups in the legend,
              removes their players from the graph."
            ),
            p("If you double click on a specific value in the legend,
              only the players from that group are shown."
            ),
            p("Clicking on removed groups in the legend, adds them to the selection."
            ),
            p("Drag the mouse to create a box to select and view ratings 
              for at most three players. Hold in shift while dragging to 
              select players from different areas of the plot."
            ),
           ## The MDS plot
           plotlyOutput(
            outputId = ns("distPlot")
           ),
           
           ## The following is useful for testing purposes what data is shown
           # verbatimTextOutput("click"), 
           br(),
           
           ## The radar plot
           uiOutput(
             outputId = ns("ratingHeader")
           ),
           plotlyOutput(
             outputId = ns("ratingPlot")
           )
         ),
         ## The explanation of distances
         tabPanel(
           "Explanation", 
           withMathJax(
             includeMarkdown(
               "explanation_MDS.md"
             )
           )
         )
       )
     )
   )
 )
}

## Backend module for player similarities
similarity_server <- function(id){
  ## Calling moduleServer function
  moduleServer(
    id,
    
    ## Definining the mechanisms
    function(input, output, session){
      
      ## Selects the current data from the selected season
      current_data <- reactive({
        current_data <- shl_seasonal_player_data[[paste("S", 
                                                        input$season, 
                                                        sep ="")
                                                  ]]
        
        current_players <- 
          current_data %>% 
          tibble::column_to_rownames(
            var = "Name"
          ) %>% 
          select(
            PId, 
            AGR:DFR
          ) %>% 
          mutate(
            across(.fns = as.numeric)
          )
        
        ## Calculates Manhattan distances between the players
        d <- dist(
          current_players[,-1], 
          method = "manhattan"
        )
        
        ## Fits the MDS onto two dimensions based on the distances
        fit <- cmdscale(
          d, 
          eig = TRUE, 
          k = 2
        )
        
        ## Creates a data.frame with the two dimensional coordinates
        ## and the player ID
        
        distances <- 
          data.frame(
            x = fit$points[,1],
            y = fit$points[,2], 
            id = current_players$PId
          ) %>% 
          
          ## Joins the other data to the players by their ID
          left_join(
            current_data, 
            by = c("id" = "PId")
          ) %>% 
          
          ## Creates a column called group based on the option selected
          mutate(
            group = current_data[,input$color_group]
          )
        
        ## Returns the created data.frame of the players from the current season, 
        ## their distances, and the coloring column
        return(distances)
      })
      
      ## Produces the plot of the MDS results
      output$distPlot <- renderPlotly({
        ## Loads the currently selected data 
        distances <- current_data()
        
        ## Creates a color palette for the groupings
        ## "Team" has too many groups for any palette so some colors are added
        if(length(levels(distances$group)) < 13){
          fill <- 
            brewer.pal(
              n = length(levels(distances$group)),
              name = "Paired"
            )
        } else if(input$color_group == "Team") {
          fill <- c("ATL" = "#BE1E2D",
                    "BAP" = "#000080",
                    "BUF" = "#164833",
                    "CGY" = "#000000", #Alt. 93999F for silver
                    "CHI" = "#D9AD00",
                    "EDM" = "#00C2F2",
                    "HAM" = "#253b50",
                    "LAP" = "#227383",
                    "MAN" = "#DF0916",
                    "MIN" = "#123A1E", #Alt. D3B35B for gold
                    "NEW" = "#000000",
                    "NOL" = "#850EFF",
                    "SEA" = "#0B2042",
                    "SFP" = "#360854",
                    "TBB" = "#00839A",
                    "TEX" = "#000000",
                    "TOR" = "#195AA5",
                    "WPG" = "#081C41")
          
          color <- c("ATL" = "#F26522",
                     "BAP" = "#7B1113",
                     "BUF" = "#F9BB14", #Alt. F5B918
                     "CGY" = "#B91C25",
                     "CHI" = "#000000",
                     "EDM" = "#081C3E", 
                     "HAM" = "#FFFFFF", #Alt. D1B236 for yellow
                     "LAP" = "#FF882C",
                     "MAN" = "#000000",
                     "MIN" = "#EAE1E1",
                     "NEW" = "#006A36",
                     "NOL" = "#000000",
                     "SEA" = "#C5942C",
                     "SFP" = "#D4AF37",
                     "TBB" = "#000000",
                     "TEX" = "#61615F",
                     "TOR" = "#FFFFFF",
                     "WPG" = "#D10923")
          
        } else {
          fill <- c(brewer.pal(n = 12, 
                               name = "Paired")[-12], 
                    "ECB615",
                    c("9D9E9D",
                      "4D4D4D",
                      "b3fff4",
                      "10E5C5",
                      "ffc7fb",
                      "CF18C2")[1:(length(levels(distances$group))-12)])
        }
        
        if(input$color_group == "Team"){
          ggplot(distances) + aes(x, y, 
                                  fill = group,
                                  color = group,
                                  text = Name) + 
            geom_point(pch = 21, size = 3) +
            theme_bw() +
            theme(plot.title = element_text(hjust = 0.5)) + 
            scale_fill_manual("Team", values = fill) + 
            scale_color_manual("Team", values = color) +
            labs(x = "Dimension 1", y = "Dimension 2") +
            scale_x_continuous(labels = NULL) + 
            scale_y_continuous(labels = NULL)
        } else {
          ggplot(distances) + aes(x, y, 
                                  fill = group, 
                                  text = Name) + 
            geom_point(pch = 21, color = "black", size = 3) +
            theme_bw() +
            theme(plot.title = element_text(hjust = 0.5)) + 
            scale_fill_manual(input$color_group, values = fill) + 
            labs(x = "Dimension 1", y = "Dimension 2") +
            scale_x_continuous(labels = NULL) + 
            scale_y_continuous(labels = NULL) 
        }
        
        
        
        ggplotly(tooltip = c("fill", "text"), source = "distances") %>% 
          config(modeBarButtonsToRemove = c("pan2d", 
                                            "zoomIn2d", "zoomOut2d",
                                            "autoScale2d", "resetScale2d",
                                            "hoverClosestCartesian",
                                            "hoverCompareCartesian",
                                            "toggleSpikelines")) %>% 
          layout(title = paste("Season", input$season, "of the SHL"),
                 margin = list(t = 30),
                 dragmode = "select") %>% 
          event_register(event = "plotly_selected")
        
      })
      
      ##### For testing purposes #####
      # output$click <- renderPrint({
      #     d <- event_data("plotly_click", source = "distances")
      # 
      #     current_data <- current_data() %>%
      #         mutate(x = round(x, digits = 5),
      #                y = round(y, digits = 5))
      # 
      #     if(is.null(d)){
      #         "Click events appear here (double-click to clear)"
      #     } else{
      #         d <- d %>%
      #             mutate(x = round(x, digits = 5),
      #                    y = round(y, digits = 5))
      # 
      #         d <- d %>% left_join(current_data) %>%
      #             select(Name, AGR:DFR) %>%
      #             mutate(across(2:last_col(), .fns = as.numeric)) %>%
      #             pivot_longer(AGR:DFR, names_to = "Attribute", values_to = "Rating")
      # 
      #         d
      # 
      # 
      #     }
      # })
      #####
      
      output$ratingHeader <- renderUI({
        d <- event_data("plotly_selected", source = "distances")
        if(is.null(d)){
          NULL
        } else{
          h4("The attributes of the chosen player(s)")
        }

      })

      output$ratingPlot <- renderPlotly({
        d <- event_data("plotly_selected", source = "distances")

        current_data <- current_data() %>%
          mutate(x = round(x, digits = 5),
                 y = round(y, digits = 5))

        if(is.null(d) | is.null(nrow(d))){
          NULL
        } else if (nrow(d)<4){
          nrPlayers <- nrow(d)

          d <- d %>%
            mutate(x = round(x, digits = 5),
                   y = round(y, digits = 5))

          d <- d %>% left_join(current_data, by = c("x", "y")) %>%
            select(Name, AGR:DFR) %>%
            mutate(across(2:last_col(), .fns = as.numeric)) %>%
            pivot_longer(AGR:DFR, names_to = "Attribute", values_to = "Rating")

          d$Attribute <- factor(d$Attribute, levels = c("SCR", "GTO", 'PAS', 'PHA', 'SAC', 'SRA', 'OFR',
                                                        'CHE', 'HIT', 'POS', 'SCH', 'SBL', 'FOF', 'DFR',
                                                        'ACC', 'AGI', 'SPD', 'STA', 'STR', 'BAL', 'FIG',
                                                        'AGR', 'BRA', 'DET', 'TPL', 'LEA', 'TEM', 'PRO'))

          players <- d %>%
            group_by(Name) %>%
            group_split()


          fig <- plot_ly(type = 'scatterpolar',
                         mode = "markers",
                         r = players[[1]]$Rating,
                         theta = players[[1]]$Attribute,
                         text = paste(players[[1]]$Attribute, players[[1]]$Rating, sep = ":"),
                         fill = 'toself',
                         hoverinfo = "text",
                         color = I("darkorange"),
                         name = paste(unique(players[[1]]$Name), "Sum of Ratings:", sum(players[[1]]$Rating)))
          if(nrPlayers > 1){
            fig <- fig %>% add_trace(r = players[[2]]$Rating,
                                     theta = players[[2]]$Attribute,
                                     text = paste(players[[2]]$Attribute, players[[2]]$Rating, sep = ":"),
                                     color = I("black"),
                                     name = paste(unique(players[[2]]$Name), "Sum of Ratings:", sum(players[[2]]$Rating)))
          }
          if(nrPlayers==3){
            fig <- fig %>%
              add_trace(r = players[[3]]$Rating,
                        theta = players[[3]]$Attribute,
                        text = paste(players[[3]]$Attribute, players[[3]]$Rating, sep = ":"),
                        color = I("lightblue"),
                        name = paste(unique(players[[3]]$Name), "Sum of Ratings:", sum(players[[3]]$Rating)))
          }

          fig %>%
            config(modeBarButtonsToRemove = c("pan2d",
                                              "zoomIn2d", "zoomOut2d",
                                              "autoScale2d", "resetScale2d",
                                              "hoverClosestCartesian",
                                              "hoverCompareCartesian",
                                              "toggleSpikelines")) %>%
            layout(polar = list(radialaxis = list(visible = TRUE,
                                                  range = c(0,20))),
                   showlegend = TRUE)
        } else {
          NULL
        }
      })
    }
  )
}

