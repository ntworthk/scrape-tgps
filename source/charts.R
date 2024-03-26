#--- Script details ------------------------------------------------------------
# Creation date: 27 June 2023
# Client:        client
# Project:       scrape-tgps
# Description:   script description
# Author:        Nick Twort

library(tidyverse)
library(lubridate)
library(magrittr)
library(here)
library(janitor)

#--- Import data ---------------------------------------------------------------

bp_data <- read_rds("data/processed/bp.rds")
mobil_data <- read_rds("data/processed/mobil.rds")
viva_data <- read_rds("data/processed/viva.rds")
puma_data <- read_rds("data/processed/puma.rds")
ampol_data <- read_rds("data/processed/ampol.rds")
ampol_data <- ampol_data |> pivot_wider(names_from = fuel, values_from = "tgp")

fuels <- tibble(
  fuel = c("diesel", "ulp", "pulp", "e10", "x95_premium", "x98_premium", "premium_unleaded_petrol", "unleaded_petrol", "unleaded_petrol_98", "unleaded_petrol_e10", "b5", "biodiesel_b5", "e10_unleaded_petrol", "low_aromatic_fuel", "premium_unleaded_petrol_95", "premium_unleaded_petrol_98", "uls_automotive_diesel", "unleaded_petrol_91", "DIESEL", "E10", "PULP95", "PULP98", "ULP"),
  tidy_fuel = c("Diesel", "ULP", "PULP 95", "e10", "PULP 95", "PULP 98", "PULP 95", "ULP", "PULP 98", "e10", "b5", "b5", "e10", "Low aromatic", "PULP 95", "PULP 98", "ULS Diesel", "ULP", "Diesel", "e10", "PULP 95", "PULP 98", "ULP")
)

key_fuels <- c("Diesel", "ULP")

#--- Combine data --------------------------------------------------------------

g <- bind_rows(
  bp_data |> mutate(brand = "BP"),
  mobil_data |> mutate(brand = "Mobil"),
  viva_data |> mutate(brand = "Viva") |> rename(terminal = city),
  puma_data |> mutate(brand = "Puma"),
  ampol_data |> mutate(brand = "Ampol")
) |> 
  filter(str_detect(terminal, "Botany|SYDNEY|Sydney"), effective_date >= "2023-06-24") |> 
  select(-state) |> 
  pivot_longer(-c(brand, effective_date, terminal, date_downloaded), names_to = "fuel", values_to = "tgp") |> 
  left_join(fuels, by = "fuel") |> 
  filter(!is.na(tgp), tidy_fuel %in% key_fuels) |> 
  ggplot(aes(x = effective_date, y = tgp, colour = brand)) +
  geom_line(aes(linetype = tidy_fuel), linewidth = 0.7) +
  #geom_point() +
  geom_text(
    data = \(x) filter(x, effective_date == max(effective_date)),
    aes(label = brand),
    hjust = 0,
    nudge_x = 0.05,
    show.legend = FALSE
  ) +
  labs(x = "Effective date", y = "Terminal gate price (c/L)", colour = "Supplier", linetype = "Fuel") +
  scale_x_date(expand = expansion(mult = c(0.05, 0.1))) +
  scale_colour_manual(
    values = c("#008698", "#232C31", "#ECAA2B", "#E14D18", "#AA2E60"),
    guide = guide_none()
  ) +
  theme_light(base_family = "Arial", base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.position = "bottom",
    strip.background = element_rect(fill = "#E6E7E8"),
    legend.margin = margin(t = -8)
  )



#--- Analysis ------------------------------------------------------------------

# g <- bp_data |> 
#   filter(terminal == "Sydney-Botany") |> 
#   pivot_longer(-c(effective_date, terminal, date_downloaded), names_to = "fuel", values_to = "tgp") |> 
#   left_join(fuels, by = "fuel") |> 
#   ggplot(aes(x = effective_date, y = tgp, colour = tidy_fuel)) +
#   geom_line(linewidth = 0.7) +
#   geom_point() +
#   geom_text(
#     data = \(x) filter(x, effective_date == max(effective_date)),
#     aes(label = tidy_fuel),
#     hjust = 0,
#     nudge_x = 0.05,
#     show.legend = FALSE
#   ) +
#   labs(x = "Effective date", y = "Terminal gate price (c/L)", colour = "Fuel") +
#   scale_x_date(expand = expansion(mult = c(0.05, 0.1))) +
#   scale_colour_manual(
#     values = c("#008698", "#232C31", "#ECAA2B", "#8AC1AF"),
#     guide = guide_none()
#   ) +
#   theme_light(base_family = "Arial", base_size = 12) +
#   theme(
#     panel.grid.major.x = element_blank(),
#     panel.background = element_blank(),
#     plot.background = element_blank(),
#     legend.background = element_blank(),
#     legend.position = "bottom",
#     strip.background = element_rect(fill = "#E6E7E8")
#   )

ggsave(filename = "figures/png/bp.png",
       plot = g,
       width = 17.00,
       height = 11.46,
       units = "cm",
       bg = "transparent"
)
