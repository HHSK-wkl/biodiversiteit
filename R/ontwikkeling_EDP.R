# Libraries
library(tidyverse)
library(lubridate)
library(HHSKwkl)
library(vegan)
library(twn)

options(OutDec = ",")
theme_set(hhskthema())

## ---- load-data ----

# download_data(c("biologie.rds", "meetpunten.rds"))
bio <- readRDS("data/biologie.rds")
meetpunten <- readRDS("data/meetpunten.rds")

planten_aq <- readxl::read_excel("data/planten_info.xlsx") %>% filter(aquatisch == "aquatisch") %>% pull(naam)

edp <- sf::st_read("data/waterlichamen.gpkg") %>% filter(OWMNAAM == "Eendragtspolder_roeibaan")

edp_mp <- meetpunten %>% 
  filter(str_detect(mp, "^VEDP")) %>% 
  sf::st_as_sf(coords = c("x", "y"), crs = 28992,  remove = FALSE) %>% 
  sf::st_intersection(edp)

edp_veg <- 
  bio %>% 
  filter(str_detect(mp, "^VEDP"), !str_detect(mp, "^VEDP_9"), naam %in% planten_aq) %>% 
  select(mp, datum, naam, waarde = waarde_totaal) %>% 
  mutate(naam = increase_taxonlevel(naam, "Species")) %>% 
  group_by(mp, datum, naam) %>%
  summarise(waarde = sum(waarde)) %>%
  ungroup() %>% 
  add_jaar() 

edp_veg_totaal <- # voor het toevoegen van de totale bedekking aan het plot
  edp_veg %>% 
  filter(!str_detect(naam, "Vaucheria"),
         !str_detect(naam, "Sagittaria"),
         !str_detect(naam, "Enteromorpha"),
         !str_detect(naam, "Lemna"),
         !str_detect(naam, "Spirodela")) %>% 
 
  group_by(mp, datum, jaar) %>% 
  summarise(waarde = sum(waarde)) %>% 
  mutate(naam = "Totaal") %>% 
  mutate(waarde = pmin(100, waarde))

edp_veg_totaal_excl_chara <- # voor het toevoegen van de totale bedekking zonder kranswieren aan het plot
  edp_veg %>% 
  filter(!str_detect(naam, "Vaucheria"),
         !str_detect(naam, "Sagittaria"),
         !str_detect(naam, "Enteromorpha"),
         !str_detect(naam, "Lemna"),
         !str_detect(naam, "Spirodela")) %>% 
  filter(!twn::increase_taxonlevel(naam, "Familia") == "Characeae") %>% 
  mutate(naam = case_when(
    naam == "Zannichellia" ~ "Zannichellia palustris [1]",
    naam == "Zannichellia palustris [2]"  ~ "Zannichellia palustris [1]",
    .default = naam
  )) %>% 
  group_by(mp, datum, jaar) %>% 
  summarise(waarde = sum(waarde)) %>% 
  mutate(naam = "Totaal zonder kranswieren") %>% 
  mutate(waarde = pmin(100, waarde))

edp_veg_kranswieren <- # voor het toevoegen van de totale bedekking kranswieren aan het plot
  edp_veg %>% 
  filter(twn::increase_taxonlevel(naam, "Familia") == "Characeae") %>% 
  group_by(mp, datum, jaar) %>% 
  summarise(waarde = sum(waarde)) %>% 
  mutate(naam = "Kranswieren") %>% 
  mutate(waarde = pmin(100, waarde))

pdf("veg_mult_edp.pdf", width = 25, height = 30)

edp_veg %>% 
  filter(!str_detect(naam, "Vaucheria"),
         !str_detect(naam, "Sagittaria"),
         !str_detect(naam, "Enteromorpha")) %>% 
  mutate(naam = case_when(
    naam == "Zannichellia" ~ "Zannichellia palustris [1]",
    naam == "Zannichellia palustris [2]"  ~ "Zannichellia palustris [1]",
    .default = naam
  )) %>% 
  bind_rows(edp_veg_totaal) %>% 
  bind_rows(edp_veg_totaal_excl_chara) %>% 
  bind_rows(edp_veg_kranswieren) %>% 
  inner_join(edp_mp) %>% 
  group_by(naam) %>% 
  filter(n() > 20) %>% 
  ungroup() %>% 
  # filter(jaar == 2014, naam == "Chara vulgaris") %>% 
  sf::st_as_sf(coords = c("x", "y"), crs = 28992) %>% 
  mutate(naam = fct_reorder(naam, naam, .fun = length, .desc = TRUE)) %>% 
  ggplot() + 
  geom_sf(fill = NA, colour = "grey60", data = edp) + 
  geom_sf(aes(fill = waarde), shape = 21) +
  scale_fill_viridis_c(option = "B", direction = -1, guide = guide_legend(title = "Bedekkingspercentage")) +
  hhskthema_kaart_oud() +
  facet_grid(cols = vars(jaar), rows = vars(naam), switch = "y", labeller = label_wrap_gen(15)) +
  labs(title = "Bedekking Eendragtspolder",
       caption = "Alleen soorten die meer dan 20 keer zijn aangetroffen")

dev.off()
