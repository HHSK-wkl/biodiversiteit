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

kriho <- sf::st_read("data/kriho.shp")

mp_sel <- meetpunten %>% 
  filter(str_detect(mp, "^VKRH")) %>% 
  sf::st_as_sf(coords = c("x", "y"), crs = 28992,  remove = FALSE) #%>% 
  # sf::st_intersection(edp)

veg_sel <- 
  bio %>% 
  filter(str_detect(mp, "^VKRH"), 
         #!str_detect(mp, "^VEDP_9"), 
         naam %in% planten_aq) %>% 
  select(mp, datum, naam, waarde = waarde_totaal) %>% 
  mutate(naam = increase_taxonlevel(naam, "Species")) %>% 
  group_by(mp, datum, naam) %>%
  summarise(waarde = sum(waarde)) %>%
  ungroup() %>% 
  add_jaar() 

pdf("veg_mult_kriho.pdf", width = 10, height = 10)

veg_sel %>% 
  filter(!str_detect(naam, "Vaucheria"),
         !str_detect(naam, "Sagittaria"),
         !str_detect(naam, "Enteromorpha"),
         !str_detect(naam, "Lemna"),
         !str_detect(naam, "Butomus")) %>% 
  mutate(naam = case_when(
    naam == "Zannichellia" ~ "Zannichellia palustris [1]",
    TRUE ~ naam
  )) %>% 
  inner_join(mp_sel) %>% 
  group_by(naam) %>% 
  # filter(n() > 20) %>% 
  ungroup() %>% 
  # filter(jaar == 2014, naam == "Chara vulgaris") %>% 
  sf::st_as_sf(coords = c("x", "y"), crs = 28992) %>% 
  mutate(naam = fct_reorder(naam, naam, .fun = length, .desc = TRUE)) %>% 
  ggplot() + 
  geom_sf(fill = NA, colour = "grey60", data = kriho) + 
  geom_sf(aes(fill = waarde), shape = 21) +
  scale_fill_viridis_c(option = "B", direction = -1, guide = guide_legend(title = "Bedekkingspercentage")) +
  hhskthema_kaart_oud() +
  facet_grid(cols = vars(jaar), rows = vars(naam), switch = "y", labeller = label_wrap_gen(15)) +
  labs(title = "Bedekking Krimpenerhout",
       caption = "Alleen soorten die meer dan 20 keer zijn aangetroffen")

dev.off()
