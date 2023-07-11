#--- Script details ------------------------------------------------------------
# Creation date: 10 July 2023
# Client:        client
# Project:       scrape-tgps
# Description:   script description
# Author:        Nick Twort

library(tidyverse)
library(lubridate)
library(magrittr)
library(here)
library(janitor)
library(glue)
library(rvest)

#--- Import data ---------------------------------------------------------------

purl <- "https://www.pumaenergy.com.au/for-business/terminal-gate-price/?date={tgp_date}"

tgp_date <- Sys.Date()

dates <- seq.Date(as_date("2013-12-16"), Sys.Date(), 1)

new_dates <- split(dates, ceiling(seq_along(dates)/100))

p_data <- vector("list", length = length(new_dates))

for (i in seq.int(1, length(new_dates), 1)) {
  
  p_data[[i]] <- map_dfr(new_dates[[i]], function(tgp_date) {
    
    read_html(glue(purl)) |>
      html_nodes("table") |>
      html_table(fill = TRUE) |>
      map(\(x) mutate(x, across(where(is.character) & !c(Terminal), parse_number))) |> 
      bind_rows() |> 
      as_tibble() |>
      janitor::clean_names() |> 
      mutate(effective_date = tgp_date)
    
  })
  
  print(i)
  
}

p_data |> 
  bind_rows() |> 
  write_rds("data/processed/puma.rds")

