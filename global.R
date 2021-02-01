#### 
library(shiny)
library(tidyverse)
library(lubridate)
library(indratools2)
library(gt)
# We have some helper functions for our app
#
#
## First, two formatting functions which prep dfs for the gt helpers below

market_holidays <- lubridate::ymd("2021-01-01", "2021-01-18", "2021-02-15", "2021-04-02", "2021-05-31", "2021-07-05", "2021-09-06", "2021-11-25", "2021-12-24",
                                               "2022-01-01", "2022-01-17", "2021-02-21", "2021-04-15", "2021-05-30", "2021-07-04", "2021-09-05", "2021-11-24", "2021-12-26",
                                               "2023-01-01", "2023-01-16", "2021-02-20", "2021-04-07", "2021-05-29", "2021-07-04", "2021-09-04", "2021-11-23", "2021-12-24")


indra_df_format <- function(INDRA_raw_df) {
    INDRA_raw_df %>%
        mutate(datetime = indratools2::ms_to_datetime(timestamp),
               date = as.Date(datetime)) %>%
        select(datetime, date, nav = liquidationValue, spy_last) %>%
        group_by(date) %>%
        summarise(nav = last(nav),
                  spy = last(spy_last)) %>% 
        mutate(year = year(date),
               month = month(date),
               month_label = month(date, label = T),
               day = day(date),
               wday = wday(date, abbr = T, label = T),
               wday_short = str_sub(wday,1,1)) %>%
        fill(c(nav, spy)) %>%
        filter(date >= as.Date("2021-01-22")) %>%
        group_by(year) %>%
        mutate(indra_rtn = 10000 * (nav/ lag(nav) - 1),
               indra_rtn_cum = 100*(nav/first(nav)-1),
               spy_rtn = 10000 * (spy/lag(spy) - 1),
               spy_rtn_cum = 100*(spy/first(spy)-1),
               over_under = indra_rtn - spy_rtn,
               d_d = paste0(wday, ".", month_label, ".",day)) %>%
        ungroup() %>% 
        mutate(indra_rtn = ifelse(date %in% market_holidays, NA, indra_rtn),
               spy_rtn = ifelse(date %in% market_holidays, NA, spy_rtn))
    
}
    
#### YTD gt code
indra_ytd_gt <- function(input_df) {

    input_df %>%
        group_by(year) %>%
        summarise(days_so_far = as.integer(sum(!is.na(indra_rtn))), 
                  YTD_return_indra = (last(nav)/first(nav) - 1),
                  YTD_return_spy = (last(spy)/first(spy) - 1),
                  mean_indra_rtn = mean(indra_rtn, na.rm = T),
                  mean_spy_rtn = mean(spy_rtn, na.rm = T)) %>%
        ungroup() %>%
        select(-year) %>%
        gt() %>%
        fmt_percent(columns = 2:3, decimals = 2) %>%
        fmt_number(columns = 4:5, decimals = 1, pattern = "{x}bps") %>%
        tab_header(title = "Year to date performance",
                   subtitle = "Inception (Jan 22) to present") %>%
        tab_spanner(label = "Cumulative return",
                    columns = 2:3) %>%
        tab_spanner(label = "Avg daily return",
                    columns = 4:5) %>%
        cols_label(days_so_far = "Trading days",
                   YTD_return_indra = "Fund",
                   YTD_return_spy = "SPY",
                   mean_indra_rtn = "Fund",
                   mean_spy_rtn = "SPY") %>%
        tab_footnote(footnote = "All numbers are unaudited.",
                     locations = cells_column_labels(columns = c("YTD_return_indra", "YTD_return_spy"))) %>%
        tab_footnote(footnote = "We compare with the S&P ETF, which faces transaction & rebalancing costs.",
                     locations = cells_column_labels(columns = c("YTD_return_spy"))) %>%
        tab_options(footnotes.font.size = 11)
}


##### DAILY gt code
indra_mtd_gt <- function(input_df) {

    input_df %>%
        filter(month(date) == month( Sys.Date() ) & 
               year(date)  ==  year( Sys.Date() )) %>%
        select(d_d, indra_rtn, spy_rtn, over_under) %>%
        gt(rowname_col = "d_d") %>%
        fmt_number(columns = 2:4, decimals = 1) %>%
        cols_label(indra_rtn = md("**Fund**"),
                   spy_rtn = md("**SPY**"),
                   over_under = md("*diff*")) %>%
        tab_spanner(label = "Daily returns in basis points",
                    columns = 2:3) %>%
        tab_footnote(footnote = "Daily returns are not based on official closes, but reflect marks circa 5pm CST",
                     locations = cells_column_labels(columns = c("indra_rtn", "spy_rtn"))) %>%
        tab_options(footnotes.font.size = 11,
                    footnotes.marks = letters) %>%
        data_color(columns = 4,
                   colors = scales::col_numeric(palette = c("#fc5353", "#ff8585","#ffffff","#bdf7a6", "#59ff17"),
                                                domain = c(-400, 400),
                                                na.color = "#ECECEC"))
}
