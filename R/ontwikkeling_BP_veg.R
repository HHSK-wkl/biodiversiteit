# Libraries
library(tidyverse)
library(lubridate)
library(HHSKwkl)
library(vegan)
library(twn)

# options(OutDec = ",")
theme_set(hhskthema())

## ---- load-data ----

download_data(c("biologie.rds", "meetpunten.rds"))
bio <- readRDS("data/biologie.rds")
meetpunten <- readRDS("data/meetpunten.rds")

planten_aq <- readxl::read_excel("data/planten_info.xlsx") %>% filter(aquatisch == "aquatisch") %>% pull(naam)
planten_submers <- readxl::read_excel("data/planten_info.xlsx") %>% filter(bedekkingslaag == "submers") %>% pull(naam)

bp <- sf::st_read("data/waterlichamen.gpkg") %>% filter(OWMNAAM == "Bergse Plassen")

bp_mp <- meetpunten %>% 
  filter(str_detect(mp, "^VBV|^VBA")) %>% 
  sf::st_as_sf(coords = c("x", "y"), crs = 28992,  remove = FALSE) %>% 
  sf::st_intersection(bp)

bp_veg <- 
  bio %>% 
  filter(str_detect(mp, "^VBV|^VBA"), !str_detect(mp, "^VBV_9|^VBA_9"), naam %in% planten_submers) %>% 
  select(mp, datum, naam, waarde = waarde_totaal) %>% 
  mutate(naam = increase_taxonlevel(naam, "Species")) %>% 
  group_by(mp, datum, naam) %>%
  summarise(waarde = sum(waarde)) %>%
  ungroup() %>% 
  add_jaar() 

bp_veg_totaal <- 
  bp_veg %>% 
  filter(jaar > 2012) %>% 
  filter(!str_detect(naam, "Vaucheria"),
         !str_detect(naam, "Sagittaria"),
         !str_detect(naam, "Enteromorpha")) %>% 
  mutate(naam = case_when(
    naam == "Zannichellia" ~ "Zannichellia palustris [1]",
    TRUE ~ naam
  )) %>% 
  group_by(mp, datum, jaar) %>% 
  summarise(waarde = sum(waarde)) %>% 
  mutate(naam = "Totaal") %>% 
  mutate(waarde = pmin(waarde, 100))

pdf("veg_mult_bp.pdf", width = 30, height = 30)



bp_veg %>% 
  filter(jaar > 2012) %>% 
  filter(!str_detect(naam, "Vaucheria"),
         !str_detect(naam, "Sagittaria"),
         !str_detect(naam, "Enteromorpha")) %>% 
  mutate(naam = case_when(
    naam == "Zannichellia" ~ "Zannichellia palustris [1]",
    TRUE ~ naam
  )) %>% 
  bind_rows(bp_veg_totaal) %>% 
  inner_join(bp_mp) %>% 
  group_by(naam) %>% 
  filter(n() > 30) %>% 
  ungroup() %>% 
  # filter(jaar == 2014, naam == "Chara vulgaris") %>% 
  sf::st_as_sf(coords = c("x", "y"), crs = 28992) %>% 
  mutate(naam = fct_reorder(naam, naam, .fun = length, .desc = TRUE)) %>% 
  ggplot() + 
  geom_sf(fill = NA, colour = "grey60", data = bp) + 
  geom_sf(aes(fill = waarde), shape = 21) +
  scale_fill_viridis_c(option = "B", direction = -1, guide = guide_legend(title = "Bedekkingspercentage")) +
  hhskthema_kaart_oud() +
  facet_grid(cols = vars(jaar), rows = vars(naam), labeller = label_wrap_gen(15)) +
  labs(title = "Bedekking Bergse Plassen",
       caption = "Alleen soorten die meer dan 30 keer zijn aangetroffen") +
  theme(legend.position = "top")

dev.off()


# bp_veg_totaal <- 
#   bp_veg %>% 
#   filter(jaar <= 2012) %>% 
#   filter(!str_detect(naam, "Vaucheria"),
#          !str_detect(naam, "Sagittaria"),
#          !str_detect(naam, "Enteromorpha")) %>% 
#   mutate(naam = case_when(
#     naam == "Zannichellia" ~ "Zannichellia palustris [1]",
#     TRUE ~ naam
#   )) %>% 
#   group_by(mp, datum, jaar) %>% 
#   summarise(waarde = sum(waarde)) %>% 
#   mutate(naam = "Totaal") %>% 
#   mutate(waarde = ifelse(waarde > 100, 100, waarde))
# 
# pdf("veg_mult_bp_tm_2012.pdf", width = 20, height = 20)
# 
# 
# 
# bp_veg %>% 
#   filter(jaar <= 2012) %>% 
#   filter(!str_detect(naam, "Vaucheria"),
#          !str_detect(naam, "Sagittaria"),
#          !str_detect(naam, "Enteromorpha")) %>% 
#   mutate(naam = case_when(
#     naam == "Zannichellia" ~ "Zannichellia palustris [1]",
#     TRUE ~ naam
#   )) %>% 
#   bind_rows(bp_veg_totaal) %>% 
#   inner_join(bp_mp) %>% 
#   group_by(naam) %>% 
#   filter(n() > 30) %>% 
#   ungroup() %>% 
#   # filter(jaar == 2014, naam == "Chara vulgaris") %>% 
#   sf::st_as_sf(coords = c("x", "y"), crs = 28992) %>% 
#   mutate(naam = fct_reorder(naam, naam, .fun = length, .desc = TRUE)) %>% 
#   ggplot() + 
#   geom_sf(fill = NA, colour = "grey60", data = bp) + 
#   geom_sf(aes(fill = waarde), shape = 21) +
#   scale_fill_viridis_c(option = "B", direction = -1, guide = guide_legend(title = "Bedekkingspercentage")) +
#   hhskthema_kaart() +
#   facet_grid(cols = vars(jaar), rows = vars(naam), labeller = label_wrap_gen(15)) +
#   labs(title = "Bedekking Bergse Plassen",
#        caption = "Alleen soorten die meer dan 30 keer zijn aangetroffen") +
#   theme(legend.position = "top")
# 
# dev.off()

bp_veg_totaal %>% 
  inner_join(bp_mp) %>% 
  sf::st_as_sf(coords = c("x", "y"), crs = 28992) %>% 
  ggplot() + 
  geom_sf(fill = NA, colour = "grey60", data = bp) + 
  geom_sf(aes(fill = waarde), shape = 21, size = 2) +
  scale_fill_viridis_c(option = "B", direction = -1, guide = guide_legend(title = "Bedekkingspercentage")) +
  hhskthema_kaart_oud() +
  facet_wrap(vars(fct_rev(as.character(jaar))), labeller = label_wrap_gen(15)) +
  labs(title = "Bedekking Bergse Plassen") +
  theme(legend.position = "top")

bp_veg_totaal_excl_chara <- 
  bp_veg %>% 
  filter(jaar > 2009) %>%
  filter(!str_detect(naam, "Vaucheria"),
         !str_detect(naam, "Sagittaria"),
         !str_detect(naam, "Enteromorpha"),
         !str_detect(naam, "Lemna"),
         !str_detect(naam, "Spirodela"),
         !str_detect(naam, "FLAB"),
         !str_detect(naam, "Wolffia"),
         !str_detect(naam, "Hydrodictyon"),
         !str_detect(naam, "Chara"),
         !str_detect(naam, "Nitellopsis"),
         !str_detect(naam, "Nitella")) %>% 
  mutate(naam = case_when(
    naam == "Zannichellia" ~ "Zannichellia palustris [1]",
    TRUE ~ naam
  )) %>% 
  group_by(mp, datum, jaar) %>% 
  summarise(waarde = sum(waarde)) %>% 
  mutate(naam = "Totaal") %>% 
  mutate(waarde = pmin(waarde, 100))  

bp_veg_totaal_excl_chara %>% 
  inner_join(bp_mp) %>% 
  sf::st_as_sf(coords = c("x", "y"), crs = 28992) %>% 
  ggplot() + 
  geom_sf(fill = NA, colour = "grey60", data = bp) + 
  geom_sf(aes(fill = waarde), shape = 21, size = 2) +
  scale_fill_viridis_c(option = "B", direction = -1, guide = guide_legend(title = "Bedekkingspercentage")) +
  hhskthema_kaart_oud() +
  facet_wrap(vars(fct_rev(as.character(jaar))), labeller = label_wrap_gen(15)) +
  labs(title = "Bedekking Bergse Plassen - zonder kranswieren") +
  theme(legend.position = "top")

bp_veg %>% 
  filter(jaar > 2009) %>%
  filter(str_detect(naam, "Chara") | str_detect(naam, "Nitellopsis") | str_detect(naam, "Nitella")) %>% 
  group_by(mp, datum, jaar) %>% 
  summarise(waarde = sum(waarde)) %>% 
  mutate(naam = "Totaal") %>% 
  mutate(waarde = pmin(waarde, 100))  %>% 
  inner_join(bp_mp) %>% 
  sf::st_as_sf(coords = c("x", "y"), crs = 28992) %>% 
  ggplot() + 
  geom_sf(fill = NA, colour = "grey60", data = bp) + 
  geom_sf(aes(fill = waarde), shape = 21, size = 2) +
  scale_fill_viridis_c(option = "B", direction = -1, guide = guide_legend(title = "Bedekkingspercentage")) +
  hhskthema_kaart_oud() +
  facet_wrap(vars(jaar), labeller = label_wrap_gen(15)) +
  labs(title = "Bedekking Bergse Plassen - kranswieren") +
  theme(legend.position = "top")
