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


fuels <- tibble(
  fuel = c("diesel", "ulp", "pulp", "e10"),
  tidy_fuel = c("Diesel", "ULP", "PULP", "e10")
)

#--- Analysis ------------------------------------------------------------------

g <- bp_data |> 
  filter(terminal == "Sydney-Botany") |> 
  pivot_longer(-c(effective_date, terminal, date_downloaded), names_to = "fuel", values_to = "tgp") |> 
  left_join(fuels, by = "fuel") |> 
  ggplot(aes(x = effective_date, y = tgp, colour = tidy_fuel)) +
  geom_line(linewidth = 0.7) +
  geom_point() +
  geom_text(
    data = \(x) filter(x, effective_date == max(effective_date)),
    aes(label = tidy_fuel),
    hjust = 0,
    nudge_x = 0.05,
    show.legend = FALSE
  ) +
  labs(x = "Effective date", y = "Terminal gate price (c/L)", colour = "Fuel") +
  scale_x_date(expand = expansion(mult = c(0.05, 0.1))) +
  scale_colour_manual(
    values = c("#008698", "#232C31", "#ECAA2B", "#8AC1AF"),
    guide = guide_none()
    ) +
  theme_light(base_family = "Arial", base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.position = "bottom",
    strip.background = element_rect(fill = "#E6E7E8")
  )

ggsave(filename = "figures/png/bp.png",
       plot = g,
       width = 17.00,
       height = 11.46,
       units = "cm",
       bg = "transparent"
)
