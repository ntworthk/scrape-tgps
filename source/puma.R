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

today_data <- read_html(glue(purl)) |>
  html_nodes("table") |>
  html_table(fill = TRUE) |>
  map(\(x) mutate(x, across(where(is.character) & !c(Terminal), parse_number))) |> 
  bind_rows() |> 
  as_tibble() |>
  janitor::clean_names() |> 
  mutate(effective_date = tgp_date)

old_data <- read_rds("data/processed/puma.rds")

old_data |> 
  bind_rows(today_data) |> 
  write_rds("data/processed/puma.rds")

