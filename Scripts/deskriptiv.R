#############################
# PROJEKT: Floraväktardata – försvunna arter
# DEL 1: Bearbetning av data
#############################

## 0. Paket ----

# install.packages(c("tidyverse", "lubridate", "sf", "janitor", "stringr", "readxl"))

library(tidyverse)
library(lubridate)
library(sf)
library(janitor)
library(stringr)
library(readxl)

# Koordinatsystem – vi antar att Ost/Nord är SWEREF 99 TM
crs_selector <- "EPSG:3021"

## 1. Läs in data ----
# Exempel: din fil "example.xlsx" med bladet "Knarot_combined"
# På sikt kan du läsa in en större export med många arter, men strukturen blir samma.

fn_florav <- "C:/Users/cako2866/Documents/FV_Analys/Data/Knarot/FV_95_2025/Knarot_combined.csv"
florav_raw <- read.csv(fn_florav) |>
  clean_names()   # gör t.ex. "Publik kommentar" -> "publik_kommentar"

glimpse(florav_raw)
# Viktiga kolumner efter clean_names():
# - projektnamn
# - ej_atterfunnen
# - startdatum
# - ost, nord
# - noggrannhet
# - artnamn, vetenskapligt_namn, taxon_id
# - publik_kommentar, privat_kommentar


## 2. Filtrera till Floraväktarna, år, och "Ej återfunnen" ----
florav<-florav_raw
# florav <- florav_raw |>
#   mutate(
#     year = year(startdatum)
#   ) |>
#   filter(
#     projektnamn == "Floraväkteri Sverige",
#     between(year, 1995, 2025),
#     ej_aterfunnen == TRUE   # logisk kolumn i ditt exempel
#   )
# 
# count(florav, year)
# count(florav, artnamn)


## 3. Skapa sf-objekt och buffertar (Ost/Nord) ----

# Ta bort saknade koordinater
florav_sf <- florav |>
  filter(!is.na(ost), !is.na(nord)) |>
  st_as_sf(coords = c("ost", "nord"), crs = crs_selector)

# florav_sf <- st_transform(florav_sf, 3006)
# st_write(florav_sf, "C:/Users/cako2866/Documents/FV_Analys/Data/Knarot/florav_sf_test.gpkg", delete_layer = TRUE)

# Bestäm buffertstorlek
# Om noggrannhet finns: använd den, annars t.ex. 50 m
florav_sf <- florav_sf |>
  mutate(
    buffer_radius_m = case_when(
      !is.na(noggrannhet) ~ pmax(noggrannhet, 20),  # minst 20 m
      TRUE ~ 50
    )
  )

florav_buff<-st_buffer(florav_sf,dist=florav_sf$buffer_radius_m)
st_write(florav_buff, "C:/Users/cako2866/Documents/FV_Analys/Data/Knarot/florav_buff_test.gpkg",delete_layer=T, overwrite=T)

# Behåll också punktgeometrin separat om du vill
florav_buff$geometry_point <- st_geometry(florav_sf)
florav_buff <-  dplyr::select(florav_buff,-geometry_point)
st_write(florav_buff, "C:/Users/cako2866/Documents/FV_Analys/Data/Knarot/florav_buff_test.gpkg", delete_layer = TRUE)

## 4. Gruppindelning: "lokaler" via överlappande buffertar per art ----

# Hjälpfunktion: komponent-ID baserat på st_intersects
get_component_id <- function(sf_obj) {
  mat <- st_intersects(sf_obj)
  comp_id <- rep(NA_integer_, length(mat))
  current_id <- 0L
  
  for (i in seq_along(mat)) {
    if (!is.na(comp_id[i])) next
    current_id <- current_id + 1L
    stack <- i
    comp_id[i] <- current_id
    while (length(stack) > 0) {
      j <- stack[1]
      stack <- stack[-1]
      neigh <- mat[[j]]
      new_neigh <- neigh[is.na(comp_id[neigh])]
      comp_id[new_neigh] <- current_id
      stack <- c(stack, new_neigh)
    }
  }
  comp_id
}

# Skapa lokal-ID per art (Artnamn)
florav_grouped <- florav_buff |>
  group_by(artnamn) |>
  mutate(
    lokal_id = get_component_id(cur_data())
  ) |>
  ungroup()

# Globalt ID för art x lokal
florav_grouped <- florav_grouped |>
  mutate(
    art_lokal_id = paste(artnamn, lokal_id, sep = "_")
  )

# En rad per "art x lokal"
florav_lokaler <- florav_grouped |>
  group_by(artnamn, art_lokal_id) |>
  summarise(
    n_rapporter = n(),
    first_year = min(year, na.rm = TRUE),
    last_year  = max(year, na.rm = TRUE),
    kommentar_publik = paste(unique(publik_kommentar), collapse = " | "),
    kommentar_privat = paste(unique(privat_kommentar), collapse = " | "),
    # sammanfoga buffertgeometrierna
    geometry = st_union(geometry),
    .groups = "drop"
  ) |>
  st_as_sf(crs = crs_selector)

# Lägg till sammanslagen kommentarkolumn (kan användas för orsaks-nyckelord senare)
florav_lokaler <- florav_lokaler |>
  mutate(
    kommentar_all = str_squish(
      paste(kommentar_publik, kommentar_privat, sep = " | ")
    )
  )


## 5. Läs in "alla observationer" för att hitta återfynd ----
# Här antar vi en *annan* export från Artportalen med samma struktur (alla obs, ej bara Ej återfunnen).
# Justera filnamn/bladsnamn efter din verkliga fil.

fn_all <- "data/all_observations_1995_2023.xlsx"
all_obs_raw <- read_excel(fn_all, sheet = 1) |>
  clean_names()

all_obs <- all_obs_raw |>
  mutate(
    year = year(startdatum)
  ) |>
  filter(
    between(year, 1995, 2023)
  )

# Skapa sf-objekt även här
all_obs_sf <- all_obs |>
  filter(!is.na(ost), !is.na(nord)) |>
  st_as_sf(coords = c("ost", "nord"), crs = crs_selector)

all_obs_sf$obs_id <- dplyr::row_number()


## 6. Kontrollera återfynd inom "försvunna lokaler" ----

# Anta att sista året i florav_lokaler motsvarar "försvinnandeår"
florav_lokaler <- florav_lokaler |>
  mutate(extinction_year = last_year)

# Spatial join: varje obs inom en "försvunnen"-lokal
join_res <- st_join(
  all_obs_sf,
  florav_lokaler,
  join = st_within,
  left = FALSE
)

# join_res har kolumner från både all_obs_sf (x) och florav_lokaler (y)
# Namnen blir t.ex. artnamn.x och artnamn.y om båda har kolumnen "artnamn"

# Identifiera återfynd: samma art, senare år än extinction_year
refound <- join_res |>
  filter(
    artnamn.x == artnamn.y,
    year.x > extinction_year
  ) |>
  distinct(art_lokal_id)

length(unique(refound$art_lokal_id))

# Filtrera bort dessa lokaler – kvar blir "troligen verkligen försvunna"
florav_lokaler_clean <- florav_lokaler |>
  filter(!art_lokal_id %in% refound$art_lokal_id)


## 7. Sammanställning: antal lokaler per art ----

sammanfattning_arter <- florav_lokaler_clean |>
  st_drop_geometry() |>
  count(artnamn, name = "antal_lokaler_forvunna") |>
  arrange(desc(antal_lokaler_forvunna))

# Spara för vidare analys/rapport
write_csv(sammanfattning_arter,
          "output/antal_lokaler_forvunna_per_art.csv")

# Geopackage för vidare GIS-analys i t.ex. ArcMap/QGIS
st_write(florav_lokaler_clean,
         "output/floravaktare_forvunna_lokaler_clean.gpkg",
         delete_dsn = TRUE)


## 8. Enkel nyckelordskodning av orsaker (från kommentarer) ----

keywords <- tribble(
  ~kategori,         ~pattern,
  "igenväxning",     "igenv|busk|träduppslag|förbuskning",
  "avverkning",      "avverk|kalhug|gallring|skogsmaskin",
  "upphört_bete",    "bete.*upphört|slutat betas|uteblivet bete",
  "odling_intensiv", "plöjd|åker|gödsl|dikning|dränering",
  "exploatering",    "väg|hus|parkering|byggt|exploater"
)

orsaker_per_lokal <- florav_lokaler_clean |>
  st_drop_geometry() |>
  select(art_lokal_id, artnamn, kommentar_all) |>
  rowwise() |>
  mutate(
    orsaks_kategori = paste(
      keywords$kategori[
        str_detect(
          tolower(kommentar_all),
          keywords$pattern
        )
      ],
      collapse = ", "
    ),
    orsaks_kategori = ifelse(orsaks_kategori == "", NA, orsaks_kategori)
  ) |>
  ungroup()

write_csv(orsaker_per_lokal,
          "output/orsaker_forvunna_lokaler_per_artlokal.csv")
