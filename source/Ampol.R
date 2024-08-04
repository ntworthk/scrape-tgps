#--- Script details ------------------------------------------------------------
# Creation date: 14 July 2023
# Client:        client
# Project:       scrape-tgps
# Description:   script description
# Author:        Nick Twort

library(tidyverse)
library(lubridate)
library(magrittr)
library(here)
library(janitor)
library(pdftools)
library(httr)

#--- Import data ---------------------------------------------------------------

tf <- tempfile(fileext = ".pdf")

ppp <- httr::GET(
  "https://www.ampol.com.au/-/media/files/ampol-au/business/terminal-gate-pricing/ampol-terminal-gate-prices.ashx",
  httr::write_disk(tf, overwrite = TRUE)
)

raw_text <- pdf_text(tf)

rr <- unlist(str_split(raw_text, "\\n"))
ss <- rr[11:(min(which(str_detect(rr, "\\* Terminal Gate prices")))-2)]

base_data <- map_dfr(ss, function(line) {
  
  ll <- unlist(str_split(line, " +"))
  names(ll) <- 1:length(ll)
  
  as_tibble_row(ll)
  
}) |> 
  (\(x) set_colnames(x, x[1, ]))() |> 
   clean_names() |> 
  rename(previous_1 = previous, current_1 = current) |> 
  filter(state != "State") |> 
  pivot_longer(
    cols = c(starts_with("previous"), starts_with("current")),
    names_to = c("day", "fuel"),
    names_sep = "_",
    values_to = "price"
    ) |> 
  mutate(price = parse_number(price)) |> 
  select(state, terminal = location, day, fuel, tgp = price)

current_day <- rr |>
  str_subset("Current E") |> 
  str_extract("[0-9].*") |> 
  parse_date(format = "%d %B %Y") |> 
  first()

previous_day <- rr |>
  str_subset("Previous E") |> 
  str_extract("[0-9].*") |> 
  parse_date(format = "%d %B %Y") |> 
  first()


ampol_data <- base_data |> 
  mutate(
    effective_date = if_else(day == "previous", previous_day, current_day),
    fuel = c("E10", "ULP", "PULP95", "PULP98", "DIESEL")[as.integer(fuel)]
  ) |> 
  mutate(
    date_downloaded = Sys.time()
  ) |> 
  select(-day)

ampol_data_previous <- read_rds("data/processed/ampol.rds")

ampol_data_previous |>
  bind_rows(ampol_data) |>
  write_rds("data/processed/ampol.rds")

print("Done Ampol")