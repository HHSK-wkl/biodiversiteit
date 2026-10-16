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

bz <- sf::st_read("data/waterlichamen.gpkg") %>% filter(OWMNAAM == "Bleiswijkse Zoom")

bz_mp <- meetpunten %>% 
  filter(str_detect(mp, "^VBLZ")) %>% 
  sf::st_as_sf(coords = c("x", "y"), crs = 28992,  remove = FALSE) %>% 
  sf::st_intersection(bz)

bz_veg <- 
  bio %>% 
  filter(str_detect(mp, "^VBLZ"), !str_detect(mp, "^VBLZ_9"), naam %in% planten_aq) %>% 
  select(mp, datum, naam, waarde = waarde_totaal) %>% 
  mutate(naam = increase_taxonlevel(naam, "Species")) %>% 
  group_by(mp, datum, naam) %>%
  summarise(waarde = sum(waarde)) %>%
  ungroup() %>% 
  add_jaar() 

bz_veg_totaal <- # voor het toevoegen van de totale bedekking aan het plot
  bz_veg %>% 
  # bind_rows(bz_2021) %>% 
  # filter(jaar > 2012) %>% 
  filter(!str_detect(naam, "Vaucheria"),
         !str_detect(naam, "Sagittaria"),
         !str_detect(naam, "Enteromorpha"),
         !str_detect(naam, "Lemna")) %>% 
  mutate(naam = case_when(
    naam == "Zannichellia" ~ "Zannichellia palustris [1]",
    TRUE ~ naam
  )) %>% 
  group_by(mp, datum, jaar) %>% 
  summarise(waarde = sum(waarde)) %>% 
  mutate(naam = "Totaal") %>% 
  mutate(waarde = ifelse(waarde > 100, 100, waarde))

pdf("veg_mult_bz.pdf", width = 14, height = 24)

bz_veg %>% 
  filter(!str_detect(naam, "Vaucheria"),
         !str_detect(naam, "Sagittaria"),
         !str_detect(naam, "Enteromorpha"),
         !str_detect(naam, "Spirodela"),
         !str_detect(naam, "Lemna")) %>% 
  mutate(naam = case_when(
    naam == "Zannichellia" ~ "Zannichellia palustris [1]",
    TRUE ~ naam
  )) %>% 
  bind_rows(bz_veg_totaal) %>% 
  inner_join(bz_mp) %>% 
  group_by(naam) %>% 
  filter(n() > 10) %>% 
  ungroup() %>% 
  # filter(jaar == 2014, naam == "Chara vulgaris") %>% 
  sf::st_as_sf(coords = c("x", "y"), crs = 28992) %>% 
  mutate(naam = fct_reorder(naam, naam, .fun = length, .desc = TRUE)) %>% 
  ggplot() + 
  geom_sf(fill = NA, colour = "grey60", data = bz) + 
  geom_sf(aes(fill = waarde), shape = 21, size = 2) +
  scale_fill_viridis_c(option = "B", direction = -1, guide = guide_legend(title = "Bedekkingspercentage"), limits = c(0.1,100), expand = FALSE) +
  hhskthema_kaart_oud() +
  facet_grid(cols = vars(jaar), rows = vars(naam), switch = "y", labeller = label_wrap_gen(15)) +
  labs(title = "Bedekking Bleiswijkse Zoom",
       caption = "Alleen soorten die meer dan 10 keer zijn aangetroffen") +
  theme(legend.position = "top")

dev.off()
