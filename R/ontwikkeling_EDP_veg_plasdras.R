# Libraries
library(tidyverse)
library(HHSKwkl)
library(twn)
library(glue)

theme_set(hhskthema())

## ---- load-data ----

# bio <- data_online("biologie.rds")
# meetpunten <- data_online("meetpunten.rds")
# download_data(c("biologie.rds", "meetpunten.rds"))
bio <- readRDS("data/biologie.rds")
meetpunten <- readRDS("data/meetpunten.rds")

naam_water <- "Eendragtspolder Plasdras"
aantal_rijen <- 3
puntgrootte <- 2.7

contour <- sf::st_read("data/EDP_plasdras.gpkg") 

planten_aq <- readxl::read_excel("data/planten_info.xlsx") %>% 
  filter(aquatisch == "aquatisch",
         bedekkingslaag != "kroos",
         bedekkingslaag != "alg") %>% 
  pull(naam)

mp_sel <- 
  meetpunten %>% 
  filter(str_detect(mp, "^VEDP")) %>% 
  sf::st_as_sf(coords = c(x = "x", y = "y"), crs = 28992, remove = FALSE) %>% 
  sf::st_intersection(sf::st_buffer(contour, dist = 10)) %>% 
  pull(mp)

veg <- 
  bio %>% 
  filter(str_detect(mp, "^VEDP"),
         mp %in% mp_sel,
         naam %in% planten_aq) %>% 
  select(mp, datum, naam, waarde = waarde_totaal) %>% 
  mutate(naam = increase_taxonlevel(naam, "Species")) %>% 
  add_jaar() %>% 
  filter(!str_detect(naam, "Butomus"),
         !str_detect(naam, "Sagittaria"))

veg_totaal <- # voor het toevoegen van de totale bedekking aan het plot
  veg %>% 
  group_by(mp, datum, jaar) %>% 
  summarise(waarde = sum(waarde)) %>% 
  mutate(naam = "Totaal") %>% 
  mutate(waarde = pmin(100, waarde))


veg_totaal_excl_chara <- # voor het toevoegen van de totale bedekking aan het plot
  veg %>% 
  filter(twn::increase_taxonlevel(naam, "Familia") != "Characeae") %>% 
  group_by(mp, datum, jaar) %>% 
  summarise(waarde = sum(waarde)) %>% 
  ungroup() %>% 
  mutate(naam = "Totaal zonder kranswieren") %>% 
  mutate(waarde = pmin(100, waarde))

veg_kranswieren <- # voor het toevoegen van de totale bedekking aan het plot
  veg %>% 
  filter(twn::increase_taxonlevel(naam, "Familia") == "Characeae") %>% 
  group_by(mp, datum, jaar) %>% 
  summarise(waarde = sum(waarde)) %>% 
  ungroup() %>% 
  mutate(naam = "Kranswieren") %>% 
  mutate(waarde = pmin(100, waarde))

# Plots per soort ---------------------------------------------------------

pdf(glue("output/vegetatie ontwikkeling {naam_water} 2013-2026 - per soort.pdf"), width = 18, height = 10)

# Totaal

veg_totaal %>% 
  left_join(meetpunten) %>% 
  sf::st_as_sf(coords = c("x", "y"), crs = 28992) %>% 
  ggplot() + 
  geom_sf(fill = NA, colour = "grey60", data = contour) + 
  geom_sf(aes(fill = waarde, color = waarde), shape = 21, size = puntgrootte, stroke = 0.3) +
  scale_color_continuous(palette = c("grey30", "grey30"), na.value = NA, guide = guide_none()) +
  scale_fill_viridis_c(option = "B", direction = -1, guide = guide_legend(title = "Bedekkingspercentage")) +
  hhskthema_kaart_oud() +
  facet_wrap(vars(jaar), labeller = label_wrap_gen(15), nrow = aantal_rijen) +
  labs(title = glue("Bedekking {naam_water} - Totaal")
  ) +
  theme(legend.position = "top")

# Zonder kranswieren

veg_totaal_excl_chara %>% 
  left_join(meetpunten) %>% 
  sf::st_as_sf(coords = c("x", "y"), crs = 28992) %>% 
  ggplot() + 
  geom_sf(fill = NA, colour = "grey60", data = contour) + 
  geom_sf(aes(fill = waarde, color = waarde), shape = 21, size = puntgrootte, stroke = 0.3) +
  scale_color_continuous(palette = c("grey30", "grey30"), na.value = NA, guide = guide_none()) +
  scale_fill_viridis_c(option = "B", direction = -1, guide = guide_legend(title = "Bedekkingspercentage")) +
  hhskthema_kaart_oud() +
  facet_wrap(vars(jaar), labeller = label_wrap_gen(15), nrow = aantal_rijen) +
  labs(title = glue("Bedekking {naam_water} - Totaal zonder kranswieren")
  ) +
  theme(legend.position = "top")

# Kranswieren

veg_kranswieren %>% 
  left_join(meetpunten) %>% 
  sf::st_as_sf(coords = c("x", "y"), crs = 28992) %>% 
  ggplot() + 
  geom_sf(fill = NA, colour = "grey60", data = contour) + 
  geom_sf(aes(fill = waarde, color = waarde), shape = 21, size = puntgrootte, stroke = 0.3) +
  scale_color_continuous(palette = c("grey30", "grey30"), na.value = NA, guide = guide_none()) +
  scale_fill_viridis_c(option = "B", direction = -1, guide = guide_legend(title = "Bedekkingspercentage")) +
  hhskthema_kaart_oud() +
  facet_wrap(vars(jaar), labeller = label_wrap_gen(15), nrow = aantal_rijen) +
  labs(title = glue("Bedekking {naam_water} - Kranswieren")
  ) +
  theme(legend.position = "top")

# Soorten

veg_plot <- 
  veg %>% 
  mutate(naam = case_when(
    str_detect(naam, "Callitriche") ~ "Callitriche",
    str_detect(naam, "Characeae") ~ "Chara",
    str_detect(naam, "Nymphaea") ~ "Nymphaea",
    str_detect(naam, "Nitella") ~ "Nitella",
    .default = naam
  )) %>%
  summarise(waarde = sum(waarde), .by = c(mp, jaar, naam)) %>% 
  complete(mp, jaar, naam)

namen <- veg_plot %>% pull(naam) %>% unique() %>%  sort()

for (naam_sel in namen){
  
  p <- 
    veg_plot %>% 
    complete(mp, jaar, naam) %>% 
    filter(naam == naam_sel) %>% 
    left_join(meetpunten) %>% 
    sf::st_as_sf(coords = c("x", "y"), crs = 28992) %>% 
    ggplot() + 
    geom_sf(fill = NA, colour = "grey60", data = contour) + 
    geom_sf(aes(fill = waarde, color = waarde), shape = 21, size = puntgrootte, stroke = 0.3) +
    scale_color_continuous(palette = c("grey30", "grey30"), na.value = NA, guide = guide_none()) +
    scale_fill_viridis_c(option = "B", direction = -1, guide = guide_legend(title = "Bedekkingspercentage"), na.value = NA, limits = c(0,100)) +
    hhskthema_kaart_oud() +
    facet_wrap(vars(jaar), labeller = label_wrap_gen(15), nrow = aantal_rijen) +
    labs(title = glue("Bedekking {naam_water} - {naam_sel} - {twn_localname(naam_sel)}", .na = "")
    ) +
    theme(legend.position = "top")
  
  print(p)
  
}

dev.off()

# Plot totaal -------------------------------------------------------------

# pdf(glue("output/vegetatie ontwikkeling {naam_water} 2008-2026.pdf"), width = 30, height = 30)
# 
# veg %>% 
#   filter(naam %in% planten_aq) %>% 
#   mutate(naam = case_when(
#     naam == "Zannichellia" ~ "Zannichellia palustris [1]",
#     TRUE ~ naam
#   )) %>% 
#   group_by(mp, datum, naam, jaar) %>% 
#   summarise(waarde = sum(waarde)) %>% 
#   ungroup() %>% 
#   bind_rows(veg_totaal) %>% 
#   bind_rows(veg_totaal_excl_chara) %>% 
#   bind_rows(veg_kranswieren) %>% 
#   group_by(naam) %>% 
#   filter(n() > 10) %>% 
#   ungroup() %>% 
#   mutate(naam = fct_reorder(naam, naam, .fun = length, .desc = TRUE)) %>% 
#   # mutate(jaar = case_when(
#   #   jaar == 2022 ~ paste0(jaar, " - ", lubridate::month(datum, label = FALSE)),
#   #   TRUE ~ as.character(jaar)
#   # )) %>% 
#   left_join(meetpunten) %>% 
#   sf::st_as_sf(coords = c("x", "y"), crs = 28992) %>% 
#   ggplot() + 
#   geom_sf(fill = NA, colour = "grey60", data = contour) + 
#   geom_sf(aes(fill = waarde), shape = 21) +
#   scale_fill_viridis_c(option = "B", direction = -1, guide = guide_legend(title = "Bedekkingspercentage")) +
#   hhskthema_kaart_oud() +
#   facet_grid(cols = vars(jaar), rows = vars(naam), labeller = label_wrap_gen(15)) +
#   labs(title = paste("Bedekking", naam_water),
#        caption = "Alleen soorten die meer dan 10 keer zijn aangetroffen") +
#   theme(legend.position = "top")
# 
# dev.off()
