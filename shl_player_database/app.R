#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

require(XML)
require(ggplot2)
require(stringr)
require(stringi)
require(dplyr)
require(tidyr)
require(RColorBrewer)
require(lubridate)
require(plotly)
require(fuzzyjoin)
require(shiny)
require(DT)
require(knitr)
require(ggmap)
require(kableExtra)
require(janitor)
require(usmap)
require(shinythemes)


# Functions that are used
filesSources = list.files(path = "functions")
sapply(paste("functions",filesSources, sep = "/"), source)


# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("yeti"),
    
    navbarPage("Tools for SHL",
               ## Welcoming screen
               tabPanel(
                   "Welcome",
                   welcome_ui(id = "welcome")
                ),
               
               tabPanel(
                   "Players",
                   tabsetPanel(
                       ## Player visualization using radar charts
                       tabPanel(
                           "Players",
                           titlePanel(
                               "Visualization of player attributes"
                           ),
                           radar_ui(id = "radar_ui")
                       ),
                       ## Player similarity using multidimensional scaling
                       tabPanel(
                           "Player Similarity",
                           ## Application title
                           titlePanel(
                               "Player similarity using multidimensional scaling"),
                           similarity_ui(id = "sim_ui")
                       )
                   )
               ),
               
               ## Team visualization
               tabPanel(
                   "Teams",
                   titlePanel(
                       "Teams"
                   ),
                   team_ui(id = "team_ui")
               )
               
               
               #      ),
               # navbarMenu(
               #     "More",
               #     tabPanel("Summary"),
               #     "----",
               #     "Section header",
               #     tabPanel("Table")
               
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    ## Loads the backend for the player similarities using MDS
    similarity_server(id = "sim_ui")
    
    radar_server(id = "radar_ui")
 
    team_server(id = "team_ui")   
}

# Run the application 
shinyApp(ui = ui, server = server)


# Added a Teams tab that includes the current standings as 
# well as a heatmap showing the percentages of points the 
# Away team has received in different matchups. For example 
# in the attached map each row is the Away performance against 
# every team. Just realized that the home performance is not 
# easily read from the graph so I'll have to spend some time 
# to tweak that. https://canadice.shinyapps.io/shl_player_database/
