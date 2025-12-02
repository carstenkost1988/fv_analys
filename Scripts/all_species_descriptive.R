###############################################################
#  FLORAVÄKTARNA ANALYS — DISAPPEARED LOCALITIES (RELIABLE)
###############################################################

library(tidyverse)
library(lubridate)
library(sf)
library(igraph)
library(janitor)
library(readxl)
library(ggplot2)

#------------------------------------------
# 1. LOAD ALL FLORAVÄKTAR-DATA
#------------------------------------------

folder_path <- "C:/Users/carst/Documents/fv_analys/Data/Floravaktarna/all"

files <- list.files(path = folder_path,
                    pattern = "\\.xls[x]?$",
                    full.names = TRUE)

df_list <- lapply(files, read_excel)

florav_raw <- bind_rows(df_list, .id = "source_file")

florav <- florav_raw |>
  clean_names() |>
  mutate(
    startdatum = as.Date(startdatum),
    year = year(startdatum),
    ej_aterfunnen_log = case_when(
      ej_aterfunnen %in% c("Ja","ja","JA") ~ TRUE,
      ej_aterfunnen %in% c("Nej","nej","NEJ") ~ FALSE,
      ej_aterfunnen %in% c("TRUE","True","true") ~ TRUE,
      ej_aterfunnen %in% c("FALSE","False","false") ~ FALSE,
      TRUE ~ NA
    )
  )

#------------------------------------------
# 2. CONVERT TO SF + BUFFER
#------------------------------------------

crs_sweref <- 3006

florav_sf <- florav |>
  filter(!is.na(sweref99tm_x_ost), !is.na(sweref99tm_y_nord)) |>
  st_as_sf(coords = c("sweref99tm_x_ost", "sweref99tm_y_nord"), crs = crs_sweref)

florav_sf <- florav_sf |>
  mutate(
    buffer_radius_m = case_when(
      !is.na(koordinatnoggrannhet_m) ~ pmax(koordinatnoggrannhet_m, 25),
      TRUE ~ 50
    )
  )

florav_buff <- florav_sf |>
  mutate(geometry_buffer = st_buffer(geometry, dist = buffer_radius_m)) |>
  st_set_geometry("geometry_buffer")

# Optional: Save for debugging
st_write(florav_buff,
         "C:/Users/carst/Documents/FV_Analys/Data/Floravaktarna/florav_buff_test_multispecies.gpkg",
         delete_layer = TRUE, overwrite = TRUE)

#------------------------------------------
# 3. CLUSTER TO LOCAL_ID
#------------------------------------------

get_clusters_simple <- function(sf_obj) {
  adj <- st_intersects(sf_obj)
  g <- graph_from_adj_list(adj)
  as.integer(components(g)$membership)
}

florav_grouped <- florav_buff |>
  group_by(svenskt_namn) |>
  mutate(local_id = get_clusters_simple(st_as_sf(cur_data()))) |>
  ungroup()

#------------------------------------------
# 4. ALL LOCALITIES PER SPECIES
#------------------------------------------

florav_lokaler_all <- florav_grouped |>
  group_by(svenskt_namn, local_id) |>
  summarise(
    n_besok = n(),
    first_date = min(startdatum, na.rm = TRUE),
    last_date  = max(startdatum, na.rm = TRUE),
    geometry = st_union(geometry),
    .groups = "drop"
  )

#------------------------------------------
# 5. ADD VISIT-QUALITY & RELIABILITY
#------------------------------------------

florav_lokaler_all <- florav_lokaler_all |>
  mutate(
    visit_quality = case_when(
      n_besok == 1 ~ "very_low",
      n_besok == 2 ~ "low",
      n_besok <= 4 ~ "medium",
      n_besok >= 5 ~ "high"
    ),
    is_reliable_locality = n_besok >= 2   # <- REQUIRE 2+ VISITS
  )

#------------------------------------------
# 6. RELIABLE DISAPPEARANCES ONLY
#------------------------------------------

florav_lokaler_slutligt_forsvunna <- florav_grouped |>
  group_by(svenskt_namn, local_id) |>
  arrange(startdatum, .by_group = TRUE) |>
  summarise(
    n_besok = n(),
    first_date = first(startdatum),
    last_date  = last(startdatum),
    ever_ej = any(ej_aterfunnen_log, na.rm = TRUE),
    last_is_ej = last(ej_aterfunnen_log),
    geometry = st_union(geometry),
    .groups = "drop"
  ) |>
  mutate(is_reliable_locality = n_besok >= 2) |>    # Require at least 2 visits
  filter(
    ever_ej,
    last_is_ej %in% TRUE,
    is_reliable_locality
  )

#------------------------------------------
# 7. SUMMARIES PER SPECIES
#------------------------------------------

antal_lokaler_totalt <- florav_lokaler_all |>
  st_drop_geometry() |>
  count(svenskt_namn, name = "antal_lokaler_totalt")

antal_lokaler_forsvunna <- florav_lokaler_slutligt_forsvunna |>
  st_drop_geometry() |>
  count(svenskt_namn, name = "antal_lokaler_forsvunna")

art_sammanfattning <- antal_lokaler_totalt |>
  left_join(antal_lokaler_forsvunna, by = "svenskt_namn") |>
  mutate(
    antal_lokaler_forsvunna = if_else(is.na(antal_lokaler_forsvunna), 0L, antal_lokaler_forsvunna),
    andel_forsvunna = antal_lokaler_forsvunna / antal_lokaler_totalt,
    andel_forsvunna_procent = 100 * andel_forsvunna
  ) |>
  arrange(desc(antal_lokaler_forsvunna))

#------------------------------------------
# 8. PLOT (PERCENTAGE DISAPPEARED)
#------------------------------------------

plot_data <- art_sammanfattning |>
  filter(antal_lokaler_totalt >= 5) |>
  slice_max(order_by = andel_forsvunna_procent, n = 100) |>
  mutate(
    label = paste0(svenskt_namn, " (", antal_lokaler_forsvunna, "/", antal_lokaler_totalt, ")"),
    label = reorder(label, andel_forsvunna_procent)
  )

ggplot(plot_data, aes(x = label, y = andel_forsvunna_procent)) +
  geom_col() +
  coord_flip() +
  labs(
    x = "Art (antal lokaler: försvunna/totalt)",
    y = "Procent lokaler ej återfunnen",
    title = "Andel lokaler där arten ej återfunnits (kräver ≥2 besök)"
  ) +
  theme_minimal(base_size = 13)

#------------------------------------------
# 9. QC DIAGNOSTIC — HOW MANY LOW-QUALITY DISAPPEARANCES REMOVED?
#------------------------------------------

raw_disappearances <- florav_grouped |>
  group_by(svenskt_namn, local_id) |>
  summarise(
    n_besok = n(),
    last_is_ej = last(ej_aterfunnen_log),
    .groups = "drop"
  ) |>
  filter(last_is_ej)

qc_disappearances <- tibble(
  total_ej_aterfunnen = nrow(raw_disappearances),
  reliable_disappearances = nrow(florav_lokaler_slutligt_forsvunna),
  dropped_due_to_low_quality = nrow(raw_disappearances) - nrow(florav_lokaler_slutligt_forsvunna),
  percent_dropped = round(100 * (dropped_due_to_low_quality / total_ej_aterfunnen), 1)
)

print(qc_disappearances)

###############################################################
# END OF SCRIPT
###############################################################
