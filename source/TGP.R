#--- Script details ------------------------------------------------------------
# Creation date: 26 June 2023
# Client:        client
# Project:       scrape-tgps
# Description:   script description
# Author:        Nick Twort

library(tidyverse)
library(lubridate)
library(magrittr)
library(here)
library(janitor)
library(readxl)
library(rvest)

download_excel <- function(url,
                           sheet = NULL,
                           range = NULL,
                           col_names = TRUE,
                           col_types = NULL,
                           na = "",
                           trim_ws = TRUE,
                           skip = 0,
                           n_max = Inf,
                           guess_max = min(1000, n_max),
                           progress = readxl_progress(),
                           .name_repair = "unique"
) {
  
  tmp_file <- tempfile() 
  utils::download.file(url, tmp_file, mode = "wb") 
  
  downloaded_file <- read_excel(tmp_file,
                                sheet = sheet,
                                range = range,
                                col_names = col_names,
                                col_types = col_types,
                                na = na,
                                trim_ws = trim_ws,
                                skip = skip,
                                n_max = n_max,
                                guess_max = guess_max,
                                progress = progress,
                                .name_repair = .name_repair
  )
  
  unlink(tmp_file)
  
  downloaded_file
  
}

#--- Import data ---------------------------------------------------------------

#--- * BP ----------------------------------------------------------------------

bp <- "https://www.bp.com/content/dam/bp/country-sites/en_au/australia/home/products-services/pricing/tgp-excel.xlsx"

bp <- download_excel(bp)

bp <- bp |> 
  clean_names() |> 
  mutate(splitter = lag(is.na(bp_terminal_gate_pricing_by_state), default = FALSE)) |> 
  mutate(splitter = cumsum(splitter)) |> 
  group_by(splitter) |> 
  group_split()

bp_data <- map_dfr(bp, function(state_data) {
  
  if (nrow(filter(state_data, !is.na(x3))) == 0) {
    return(tibble())
  }
  
  state <- state_data[1, 1][[1]]
  
  fuels <- state_data |> 
    filter(!is.na(x3)) |> 
    first() |> 
    as.character() |> 
    str_subset("^Effect|^Terminal|^NA|^[0-9]", negate = TRUE) |> 
    (\(x) set_names(x, x))() |> 
    clean_names() |> 
    names()
  
  state_data |> 
    select(where(function(x) !all(is.na(x))), -splitter) |> 
    filter(!is.na(x3)) |> 
    (\(x) {
      
      colnames(x) <- x[1, ]
      x
      
    })() |> 
    filter(Terminal != "Terminal") |> 
    clean_names() |> 
    mutate(effective_date = excel_numeric_to_date(as.numeric(effective_date))) |> 
    mutate(across(all_of(fuels), as.numeric))
  
}) |> 
  mutate(date_downloaded = Sys.time())

bp_data_previous <- read_rds("data/processed/bp.rds")

bp_data_previous |> 
  bind_rows(bp_data) |> 
  write_rds("data/processed/bp.rds")

print("Done BP")

#--- * Viva --------------------------------------------------------------------

url <- "https://www.vivaenergy.com.au/quick-links/terminal-gate-pricing"

ua <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/89.0.4389.90 Safari/537.36"

viva_page <- read_html(url, user_agent = user_agent(ua))

viva_date <- viva_page |>
  html_nodes("h4") |>
  as.character() |>
  str_subset("as at") |>
  str_extract("[0-9]{1,2} [A-z]{1,4} [0-9]{4}") |>
  parse_date(format = "%d %b %Y")

viva_data <- viva_page |>
  html_nodes("table") |>
  html_table(fill = TRUE) |>
  magrittr::extract2(1) |>
  janitor::clean_names() |>
  mutate(state = ifelse(state == "", NA_character_, state)) |>
  fill(state, .direction = "down") |>
  mutate(across(
    -c(state, city),
    \(x) parse_number(as.character(x))
  )) |>
  mutate(
    effective_date = viva_date,
    date_downloaded = Sys.time()
  )

viva_data_previous <- read_rds("data/processed/viva.rds")

viva_data_previous |>
  bind_rows(viva_data) |>
  write_rds("data/processed/viva.rds")

print("Done Viva")

#--- * United ------------------------------------------------------------------

#united <- "https://www.unitedpetroleum.com.au/terminal-gate-pricing-tgp/"
#
#united <- read_html(united) |>
  #html_nodes("table") |>
  #html_table(fill = TRUE) |>
  #magrittr::extract2(1) |>
  #janitor::clean_names()
#
#colnames(united) <- united[1, ]
#
#united_date <- parse_date(colnames(united)[2], "%d/%m/%Y")
#
#united_data <- united |>
  #filter(Terminal != "Terminal") |>
  #clean_names() |>
  #select(terminal, fuel = 2, everything()) |>
  #mutate(across(
    #-c(terminal, fuel),
    #parse_number
  #)) |>
  #mutate(
    #effective_date = united_date,
    #date_downloaded = Sys.time()
  #)
#
#united_data_previous <- read_rds("data/processed/united.rds")
#
#united_data_previous |>
  #bind_rows(united_data) |>
  #write_rds("data/processed/united.rds")

#print("Done United")

#--- * Mobil -------------------------------------------------------------------

mobil <- "https://www.mobil.com.au/en-au/commercial-fuels/terminal-gate"

mobil_page <- read_html(mobil)

mobil <- mobil_page |>
  html_nodes("table") |>
  html_table(fill = TRUE) |>
  magrittr::extract2(1) |> 
  janitor::clean_names()

colnames(mobil) <- c("state", "terminal", colnames(mobil)[3:ncol(mobil)])

mobil_date <- mobil_page |> 
  html_nodes("div") |> 
  as.character() |> 
  str_subset("As at") |> 
  str_extract("[0-9]{1,2} [A-z]+ [0-9]{4}") |> 
  first() |> 
  parse_date("%d %B %Y")

mobil_data <- mobil |> 
  filter(state != "") |> 
  clean_names() |> 
  mutate(across(
    -c(state, terminal),
    \(x) parse_number(as.character(x))
  )) |> 
  mutate(
    effective_date = mobil_date,
    date_downloaded = Sys.time()
  )

mobil_data_previous <- read_rds("data/processed/mobil.rds")

mobil_data_previous |> 
  bind_rows(mobil_data) |> 
  write_rds("data/processed/mobil.rds")

print("Done Mobil")


#--- * Puma --------------------------------------------------------------------


# puma <- "https://www.pumaenergy.com.au/for-business/terminal-gate-price/"
# 
# puma_page <- read_html(puma)
# 
# effective_date <- puma_page |> 
#   as.character() |> 
#   str_extract("Pricing effective from [0-9]+ [A-z]+ [0-9]{4}") |> 
#   str_extract("[0-9]{1,2} [A-z]+ [0-9]{4}") |> 
#   parse_date("%d %B %Y")
# 
# puma_data <- puma_page |>
#   html_nodes("table") |>
#   html_table(fill = TRUE) |>
#   map(\(x) mutate(x, across(where(is.character) & !c(Terminal), parse_number))) |> 
#   bind_rows() |> 
#   as_tibble() |>
#   janitor::clean_names() |> 
#   mutate(effective_date = effective_date,
#          date_downloaded = Sys.time())
# 
# puma_data_previous <- read_rds("data/processed/puma.rds")
# 
# puma_data_previous |> 
#   bind_rows(puma_data) |> 
#   write_rds("data/processed/puma.rds")

print("Done Puma")
