#
# This is a Shiny web application. You can run the application by clicking


library(shiny)
library(tidyverse)
library(lubridate)
library(indratools2)
library(gt)

### get some data

source("global.R")


##

# Define UI for application that draws a histogram
ui <- fluidPage(
  #tags$head(includeHTML(("google-analytics.html"))),
  br(),

    # Application title
  div(img(src = "indra_logo.png", height = 160, width = 320), 
      style="text-align: center;"),
  br(),
  br(),
  
  gt_output("ytdTable"),
  br(),
  br(),
  
  gt_output("mtdTable"),
  br(),
  br(),
  br(),
  p(strong("This page does not constitute an offer to sell, or a solicitation of an offer to buy, an 
  Interest in any Fund described herein. Any such offer or solicitation may be made only by the 
  delivery of a private offering memorandum and other materials relating to such Interest. Before 
  making any investment decisions with respect to the Interest, potential investors are advised to 
  read carefully the private offering memorandum, the limited partnership agreement and related 
  subscription documents, and to consult with their tax, legal and financial advisors."), 
    style = "font-size:9px;",
    style = "color:grey;",
    style = "margin-left: auto;",
    style = "margin-right: auto;",
    style = "width: 50%")
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
