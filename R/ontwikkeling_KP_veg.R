# Libraries
library(tidyverse)
library(lubridate)
library(HHSKwkl)
library(vegan)
library(twn)

theme_set(hhskthema())

## ---- load-data ----

# bio <- data_online("biologie.rds")
# meetpunten <- data_online("meetpunten.rds")
# download_data(c("biologie.rds", "meetpunten.rds"))
bio <- readRDS("data/biologie.rds")
meetpunten <- readRDS("data/meetpunten.rds")

kp <- sf::st_read("data/waterlichamen.gpkg") %>% filter(OWMNAAM == "Kralingse Plas")

planten_aq <- readxl::read_excel("data/planten_info.xlsx") %>% filter(aquatisch == "aquatisch") %>% pull(naam)

kp_veg <- 
  bio %>% 
  filter(str_detect(mp, "^VKP"), !str_detect(mp, "^VKP_9"), naam %in% planten_aq) %>% 
  select(mp, datum, naam, waarde = waarde_totaal) %>% 
  mutate(naam = increase_taxonlevel(naam, "Species")) %>% 
  add_jaar() 

kp_veg_totaal <- # voor het toevoegen van de totale bedekking aan het plot
  kp_veg %>% 
  filter(!str_detect(naam, "Vaucheria"),
         !str_detect(naam, "Sagittaria"),
         !str_detect(naam, "Enteromorpha"),
         !str_detect(naam, "Lemna"),
         !str_detect(naam, "Spirodela")) %>% 
  group_by(mp, datum, jaar) %>% 
  summarise(waarde = sum(waarde)) %>% 
  mutate(naam = "Totaal") %>% 
  mutate(waarde = pmin(100, waarde))


kp_veg_totaal_excl_chara <- # voor het toevoegen van de totale bedekking aan het plot
  kp_veg %>% 
  filter(!str_detect(naam, "Vaucheria"),
         !str_detect(naam, "Sagittaria"),
         !str_detect(naam, "Enteromorpha"),
         !str_detect(naam, "Lemna"),
         !str_detect(naam, "Spirodela")) %>% 
  filter(twn::increase_taxonlevel(naam, "Familia") != "Characeae") %>% 
  group_by(mp, datum, jaar) %>% 
  summarise(waarde = sum(waarde)) %>% 
  ungroup() %>% 
  mutate(naam = "Totaal zonder kranswieren") %>% 
  mutate(waarde = pmin(100, waarde))


kp_veg_kranswieren <- # voor het toevoegen van de totale bedekking aan het plot
  kp_veg %>% 
  filter(twn::increase_taxonlevel(naam, "Familia") == "Characeae") %>% 
  group_by(mp, datum, jaar) %>% 
  summarise(waarde = sum(waarde)) %>% 
  ungroup() %>% 
  mutate(naam = "Kranswieren") %>% 
  mutate(waarde = pmin(100, waarde))


# Plot totaal -------------------------------------------------------------

# pdf("veg_mult_kp_v2.pdf", width = 27, height = 27)
pdf("output/vegetatie ontwikkeling KP 2010-2026.pdf", width = 27, height = 27)

kp_veg %>% 
  # bind_rows(kp_2021) %>% 
  filter(naam %in% planten_aq) %>% 
  filter(!str_detect(naam, "Cladophora"),
         !str_detect(naam, "Vaucheria"),
         !str_detect(naam, "Sagittaria"),
         !str_detect(naam, "Enteromorpha"),
         !str_detect(naam, "Lemna"),
         !str_detect(naam, "Spirodela")) %>% 
  mutate(naam = case_when(
    naam == "Zannichellia" ~ "Zannichellia palustris [1]",
    TRUE ~ naam
  )) %>% 
  group_by(mp, datum, naam, jaar) %>% 
  summarise(waarde = sum(waarde)) %>% 
  ungroup() %>% 
  bind_rows(kp_veg_totaal) %>% 
  bind_rows(kp_veg_totaal_excl_chara) %>% 
  group_by(naam) %>% 
  filter(n() > 10) %>% 
  ungroup() %>% 
  mutate(naam = fct_reorder(naam, naam, .fun = length, .desc = TRUE)) %>% 
  filter(!(year(datum) == 2022 & month(datum) %in% c(5, 9))) %>% 
  # mutate(jaar = case_when(
  #   jaar == 2022 ~ paste0(jaar, " - ", lubridate::month(datum, label = FALSE)),
  #   TRUE ~ as.character(jaar)
  # )) %>% 
  left_join(meetpunten) %>% 
  sf::st_as_sf(coords = c("x", "y"), crs = 28992) %>% 
  ggplot() + 
  geom_sf(fill = NA, colour = "grey60", data = kp) + 
  geom_sf(aes(fill = waarde), shape = 21) +
  scale_fill_viridis_c(option = "B", direction = -1, guide = guide_legend(title = "Bedekkingspercentage")) +
  hhskthema_kaart_oud() +
  facet_grid(cols = vars(jaar), rows = vars(naam), labeller = label_wrap_gen(15)) +
  labs(title = "Bedekking Kralingse Plas",
       caption = "Alleen soorten die meer dan 10 keer zijn aangetroffen") +
  theme(legend.position = "top")

dev.off()


# Plots per soort ---------------------------------------------------------

pdf("output/vegetatie ontwikkeling KP 2010-2026 - per soort.pdf", width = 10, height = 10)

# KP totaal

kp_veg_totaal %>% 
  left_join(meetpunten) %>% 
  sf::st_as_sf(coords = c("x", "y"), crs = 28992) %>% 
  ggplot() + 
  geom_sf(fill = NA, colour = "grey60", data = kp) + 
  geom_sf(aes(fill = waarde, color = waarde), shape = 21, size = 3, stroke = 0.3) +
  scale_color_continuous(palette = c("grey30", "grey30"), na.value = NA, guide = guide_none()) +
  scale_fill_viridis_c(option = "B", direction = -1, guide = guide_legend(title = "Bedekkingspercentage")) +
  hhskthema_kaart_oud() +
  facet_wrap(vars(jaar), labeller = label_wrap_gen(15)) +
  labs(title = "Bedekking Kralingse Plas - Totaal"
  ) +
  theme(legend.position = "top")

# KP zonder kranswieren

kp_veg_totaal_excl_chara %>% 
  left_join(meetpunten) %>% 
  sf::st_as_sf(coords = c("x", "y"), crs = 28992) %>% 
  ggplot() + 
  geom_sf(fill = NA, colour = "grey60", data = kp) + 
  geom_sf(aes(fill = waarde, color = waarde), shape = 21, size = 3, stroke = 0.3) +
  scale_color_continuous(palette = c("grey30", "grey30"), na.value = NA, guide = guide_none()) +
  scale_fill_viridis_c(option = "B", direction = -1, guide = guide_legend(title = "Bedekkingspercentage")) +
  hhskthema_kaart_oud() +
  facet_wrap(vars(jaar), labeller = label_wrap_gen(15)) +
  labs(title = "Bedekking Kralingse Plas - Totaal zonder kranswieren"
  ) +
  theme(legend.position = "top")

# KP kranswieren

kp_veg_kranswieren %>% 
  left_join(meetpunten) %>% 
  sf::st_as_sf(coords = c("x", "y"), crs = 28992) %>% 
  ggplot() + 
  geom_sf(fill = NA, colour = "grey60", data = kp) + 
  geom_sf(aes(fill = waarde, color = waarde), shape = 21, size = 3, stroke = 0.3) +
  scale_color_continuous(palette = c("grey30", "grey30"), na.value = NA, guide = guide_none()) +
  scale_fill_viridis_c(option = "B", direction = -1, guide = guide_legend(title = "Bedekkingspercentage")) +
  hhskthema_kaart_oud() +
  facet_wrap(vars(jaar), labeller = label_wrap_gen(15)) +
  labs(title = "Bedekking Kralingse Plas - Kranswieren"
  ) +
  theme(legend.position = "top")

# KP soorten

kp_veg_plot <- 
  kp_veg %>% 
  filter(!str_detect(naam, "Cladophora"),
         !str_detect(naam, "Enteromorpha"),
         !str_detect(naam, "Hydrodictyon"),
         !str_detect(naam, "Lemna"),
         !str_detect(naam, "Sagittaria"),
         !str_detect(naam, "Spirodela"),
         !str_detect(naam, "Vaucheria"),) %>% 
  mutate(naam = case_when(
    str_detect(naam, "Callitriche") ~ "Callitriche", 
    str_detect(naam, "Nitella") ~ "Nitella", 
    .default = naam
  )) %>% 
  filter(!(year(datum) == 2022 & month(datum) %in% c(5, 9))) %>% 
  summarise(waarde = sum(waarde), .by = c(mp, jaar, naam)) %>% 
  complete(mp, jaar, naam)

namen <- kp_veg_plot %>% pull(naam) %>% unique() %>%  sort()

for (naam_sel in namen){
  
  p <- 
    kp_veg_plot %>% 
    complete(mp, jaar, naam) %>% 
    filter(naam == naam_sel) %>% 
    left_join(meetpunten) %>% 
    sf::st_as_sf(coords = c("x", "y"), crs = 28992) %>% 
    ggplot() + 
    geom_sf(fill = NA, colour = "grey60", data = kp) + 
    geom_sf(aes(fill = waarde, color = waarde), shape = 21, size = 3, stroke = 0.3) +
    scale_color_continuous(palette = c("grey30", "grey30"), na.value = NA, guide = guide_none()) +
    scale_fill_viridis_c(option = "B", direction = -1, guide = guide_legend(title = "Bedekkingspercentage"), na.value = NA, limits = c(0,100)) +
    hhskthema_kaart_oud() +
    facet_wrap(vars(jaar), labeller = label_wrap_gen(15)) +
    labs(title = paste("Bedekking Kralingse Plas -", naam_sel)
    ) +
    theme(legend.position = "top")
  
  print(p)
  
}

dev.off()
