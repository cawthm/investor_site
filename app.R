#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(lubridate)
library(gt)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("INDRA Site Alpha"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           gt_output("distPlot")
        )
    )
)

holidays <- lubridate::ymd(paste0("2020", c("0101", "0120", "0217", "0410", "0525", "0703", "0907", "1126", "1225")))

  indra_history <- read_csv("data/indra_acct_bal.csv") %>% 
     mutate(pretty_date = lubridate::as_date(indratools2::ms_to_datetime(timestamp))) %>%
     group_by(pretty_date) %>%
     summarise(bal = last(liquidationValue)) %>%
     mutate(indra_rtn_bps = (bal/ lag(bal) - 1) * 10000,
            indra_rtn_log = log(bal/lag(bal)) * 10000) %>%
     mutate(holiday = (pretty_date %in% holidays))
 


### we'll also need historical data for SPY
# 
# spy_history <- indratools2::td_market_value_traded("SPY", n_years = 1) %>%
#     as_tibble() %>%
#     mutate(pretty_date = lubridate::as_date(pretty_date)) %>%
#     select(pretty_date, close) %>%
#     mutate(spy_rtn_bps = (close/ lag(close) - 1) * 10000,
#            spy_rtn_log = log(close/ lag(close))*10000)
# # 
# # 
# HISTORY <- left_join(indra_history, spy_history) %>%
#     filter(pretty_date <= lubridate::ymd(20200731)) %>%
#     mutate(day = lubridate::wday(pretty_date),
#            day_long = lubridate::wday(pretty_date, label = TRUE),
#            day_short = str_sub(day_long, 1, 1),
#            week = lubridate::epiweek(pretty_date),
#            mday = lubridate::mday(pretty_date),
#            month = lubridate::month(pretty_date, label = TRUE))

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- render_gt({
        indra_history
        # HISTORY %>%
        #     filter(month < lubridate::month(Sys.Date(), label = T)) %>%
        #     group_by(month) %>%
        #     summarise(indra_rtn_log = sum(indra_rtn_log, na.rm = T),
        #               spy_rtn_log = sum(spy_rtn_log, na.rm = T))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
