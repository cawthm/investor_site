#
# This is a Shiny web application. You can run the application by clicking


library(shiny)
library(tidyverse)
library(lubridate)
library(gt)

### get some data

source("global.R")


##

# Define UI for application that draws a histogram
ui <- fluidPage(
  br(),

    # Application title
  div(img(src = "indra_logo.png", height = 150, width = 300), 
      style="text-align: center;"),
  br(),
  br(),
  
  gt_output("ytdTable"),
  br(),
  br(),
  
  gt_output("mtdTable")
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  live_data <- reactiveFileReader(20000, session, "data/indra_acct_bal.csv", read_csv)

    output$ytdTable <- render_gt({
        
      live_data() %>% indra_df_format() %>% indra_ytd_gt()
      
    })
    
    output$mtdTable <- render_gt({
      
      live_data() %>% indra_df_format() %>% indra_mtd_gt()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
