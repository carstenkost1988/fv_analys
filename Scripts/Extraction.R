# =============================
# 0) PACKAGES
# =============================
library(data.table)
library(dplyr)
library(readr)
library(sf)
library(terra)
library(stringr)
library(broom)
library(ggplot2)
library(forcats)

# =============================
# 1) USER INPUTS
# =============================
csv_path     <- "C:/Users/carst/Documents/FV_Analys/Data/Knarot/FV_95_2025/Knarot_combined.csv"    # with Artnamn column
raster1_path <- "C:/Users/carst/Documents/FV_Analys/Data/Land_use/SMD_Raster/SMD_Raster/Marktacke_raster.tif"
raster2_path <- "C:/Users/carst/Documents/FV_Analys/Data/Land_use/NMD2023_basskikt_v0_2/NMD2023bas_v0_2.tif"

# Path to your 8-bit→SMD crosswalk CSV
smd_gridcode_lookup_csv <- "C:/Users/carst/Documents/FV_Analys/Data/Land_use/SMD_Raster/smd_gridcode_lookup_from_table.csv"

crs_points <- 3021  # Sites are RT90 2.5 gon V (EPSG:3021)
use_buffer <- TRUE
buffer_m   <- 50

# =============================
# 2) LOAD SPECIES DATA
# =============================
dt <- fread(csv_path, encoding = "UTF-8")
stopifnot(all(c("Lokalnamn","Ost","Nord","Antal","Artnamn") %in% names(dt)))

# dt <- dt %>%
#   mutate(ej_aterfunnen2 = str_to_lower(trimws(as.character(Antal))) == "ej återfunnen")

# summary(dt$`Ej återfunnen`)
# summary(dt$`ej_aterfunnen2`)

# % Ej återfunnen (robust)
100 * mean(dt$ej_aterfunnen, na.rm = TRUE)

# =============================
# 3) UNIQUE SITE COORDS (for raster extraction)
# =============================
sites_dt <- dt %>%
  filter(!is.na(Lokalnamn), !is.na(Ost), !is.na(Nord)) %>%
  distinct(Lokalnamn, Ost, Nord) %>%
  mutate(site_id = row_number())

sites_sf <- st_as_sf(sites_dt, coords = c("Ost","Nord"), crs = crs_points)

# =============================
# 4) LOAD & PREP RASTERS
# =============================
r1_raw <- rast(raster1_path)  # SMD
r2_raw <- rast(raster2_path)  # NMD
# If rasters lack CRS metadata, uncomment the next two lines:
# terra::crs(r1_raw) <- "EPSG:3006"
# terra::crs(r2_raw) <- "EPSG:3006"

# helper for quick sampling
sample_raster_values <- function(r, n = 100000) {
  df <- terra::spatSample(r, size = n, method = "random", na.rm = TRUE, as.raster = FALSE)
  as.vector(df[[1]])
}

# --- SMD class dictionary (labels)
smd_classes <- tribble(
  ~smd_code, ~smd_label,
  111, "Tät stadsstruktur",
  11211, "Orter >200 inv, mindre grönområden",
  11212, "Orter >200 inv, större grönområden",
  1122, "Orter <200 inv",
  1123, "Landortsbebyggelse",
  121, "Industri",
  122, "Väg- och järnvägsnät",
  123, "Hamnområden",
  124, "Flygplats",
  1311, "Grus- och sandtag",
  1312, "Övriga mineralextraktionsplatser",
  132, "Deponier",
  133, "Byggplatser",
  141, "Urbana grönområden",
  1421, "Idrottsanläggning",
  1422, "Flygfält (gräs)",
  1423, "Skidpist",
  1424, "Golfbana",
  1425, "Ej urban park",
  1426, "Campingplats",
  211, "Åkermark",
  222, "Frukt- och bärodling",
  231, "Betesmarker",
  3111, "Lövskog ej myr/berg",
  3112, "Lövskog myr",
  3113, "Lövskog berg",
  31211, "Barrskog lavmark",
  312121, "Barrskog ej lav 5–15 m",
  312122, "Barrskog ej lav >15 m",
  3122, "Barrskog på myr",
  3123, "Barrskog berg",
  3131, "Blandskog ej myr/berg",
  3132, "Blandskog myr",
  3133, "Blandskog berg",
  321, "Naturlig gräsmark",
  322, "Hedmark",
  3241, "Busksnår",
  3242, "Hygge",
  3243, "Ungskog",
  331, "Stränder",
  332, "Berg i dagen",
  333, "Sparsam vegetation",
  334, "Brandfält",
  335, "Glaciärer",
  3211, "Gräshed",
  3212, "Örtäng",
  411, "Limnogena våtmarker",
  4121, "Blöt myr",
  4122, "Övrig myr",
  4123, "Torvtäkt",
  421, "Saltpåverkade våtmarker",
  511, "Vattendrag",
  5121, "Sjöar/dammar, öppen yta",
  5122, "Sjöar/dammar, vegetation",
  521, "Kustlagun",
  522, "Estuarier",
  5231, "Kusthav öppen",
  5232, "Kusthav vegetation",
  99, "SMD-klass saknas"
)

# --- NMD lookup table
nmd_lookup <- tribble(
  ~nmd_code, ~nmd_label,
  110, "Skogsmark fastmark",
  118, "Temporärt ej skog fastmark",
  120, "Skogsmark våtmark",
  128, "Temporärt ej skog våtmark",
  20,  "Öppen våtmark",
  23,  "Låg fjällskog våtmark",
  3,   "Åkermark",
  41,  "Öppen fastmark utan vegetation",
  42,  "Öppen fastmark med vegetation",
  43,  "Låg fjällskog fastmark",
  51,  "Byggnad",
  52,  "Anlagd mark",
  53,  "Väg/järnväg",
  54,  "Torvtäkt",
  61,  "Inlandsvatten",
  62,  "Hav"
)

# Detect SMD value system and build reclass
vals_r1 <- sample_raster_values(r1_raw)
looks_like_smd <- all(vals_r1 %in% smd_classes$smd_code, na.rm = TRUE)

if (looks_like_smd) {
  rcl_smd <- smd_classes %>% transmute(from = smd_code, to = smd_code) %>% as.matrix()
} else {
  xwalk8 <- readr::read_csv(smd_gridcode_lookup_csv, show_col_types = FALSE) %>%
    mutate(gridcode8 = as.numeric(gridcode8), smd_code = as.numeric(smd_code))
  rcl_smd <- xwalk8 %>% transmute(from = gridcode8, to = smd_code) %>% as.matrix()
}

r1_h <- classify(r1_raw, rcl = rcl_smd, include.lowest = TRUE, others = NA)
levels(r1_h) <- smd_classes %>% transmute(ID = smd_code, label = smd_label)
levels(r2_raw) <- nmd_lookup %>% transmute(ID = nmd_code, label = nmd_label)

# =============================
# 5) HARMONIZE TO BROAD CLASSES
# =============================
smd_to_group <- function(code, label) {
  code <- as.numeric(code)
  lbl  <- tolower(ifelse(is.na(label), "", label))
  built_codes <- c(111,11211,11212,1122,1123,121,122,123,124,141,1421,1422,1423,1424,1425,1426,132,133)
  agri_codes  <- c(211,222,231)
  forest_wet_codes <- c(3112,3122,3132)
  forest_firm_codes <- c(3111,3113,31211,312121,312122,3123,3131,3133)
  temp_nonforest_codes <- c(3242,3243)
  open_wetland_codes <- c(411,4121,4122,421)
  water_inland_codes <- c(511,5121,5122)
  sea_codes <- c(521,522,5231,5232)
  open_firm_codes <- c(321,322,3241,331,332,333,334,335,3211,3212)
  dplyr::case_when(
    code %in% built_codes ~ "built",
    code %in% agri_codes ~ "agriculture",
    code %in% forest_wet_codes ~ "forest_wet",
    code %in% forest_firm_codes ~ "forest_firm",
    code %in% temp_nonforest_codes ~ "temp_nonforest",
    code == 4123 | grepl("torvtäkt", lbl) ~ "built",
    code %in% open_wetland_codes ~ "open_wetland",
    code %in% water_inland_codes ~ "inland_water",
    code %in% sea_codes ~ "sea",
    code %in% open_firm_codes ~ "open_firm",
    grepl("vatt", lbl) ~ "inland_water",
    grepl("hav|kust", lbl) ~ "sea",
    grepl("åkermark|odling", lbl) ~ "agriculture",
    grepl("skog", lbl) & grepl("myr", lbl) ~ "forest_wet",
    grepl("skog", lbl) ~ "forest_firm",
    grepl("hygge|ungskog", lbl) ~ "temp_nonforest",
    grepl("våtmark|myr", lbl) ~ "open_wetland",
    grepl("hed|gräs|busk|berg i dagen|sand|sparsam", lbl) ~ "open_firm",
    TRUE ~ NA_character_
  )
}

nmd_to_group <- function(code, label) {
  code <- as.numeric(code)
  dplyr::case_when(
    code %in% c(51,52,53,54) ~ "built",
    code == 3 ~ "agriculture",
    code %in% c(110,43) ~ "forest_firm",
    code == 120 ~ "forest_wet",
    code %in% c(118,128) ~ "temp_nonforest",
    code == 20 ~ "open_wetland",
    code %in% c(41,42) ~ "open_firm",
    code == 61 ~ "inland_water",
    code == 62 ~ "sea",
    TRUE ~ NA_character_
  )
}

# =============================
# 6) EXTRACT RASTER VALUES ONCE
# =============================
extract_majority <- function(r, geom, buf_m = 0) {
  target_crs <- sf::st_crs(terra::crs(r, proj = TRUE))  # WKT -> sf crs
  g2 <- st_transform(geom, target_crs)
  if (buf_m > 0) {
    poly <- st_buffer(g2, dist = buf_m)
    terra::extract(r, vect(poly), fun = function(v, ...) {
      if (length(v) == 0) return(NA)
      ux <- unique(v); ux[which.max(tabulate(match(v, ux)))]
    }, na.rm = TRUE)
  } else {
    terra::extract(r, vect(g2))
  }
}

r1_vals <- extract_majority(r1_h, sites_sf, if (use_buffer) buffer_m else 0)
r2_vals <- extract_majority(r2_raw, sites_sf, if (use_buffer) buffer_m else 0)

col_r1 <- names(r1_h)[1]; col_r2 <- names(r2_raw)[1]

site_landuse <- sites_dt %>%
  arrange(site_id) %>%
  bind_cols(
    smd_code = as.numeric(r1_vals[[col_r1]]),
    nmd_code = as.numeric(r2_vals[[col_r2]])
  ) %>%
  left_join(smd_classes, by = "smd_code") %>%   # add smd_label
  left_join(nmd_lookup,  by = "nmd_code") %>%   # add nmd_label
  mutate(
    group_smd = smd_to_group(smd_code, smd_label),
    group_nmd = nmd_to_group(nmd_code, nmd_label),
    landuse_change = if_else(is.na(group_smd) | is.na(group_nmd), NA, group_smd != group_nmd),
    change_dir = if_else(is.na(group_smd) | is.na(group_nmd),
                         NA_character_,
                         paste0(group_smd, " → ", group_nmd))
  )

# =============================
# 7) JOIN BACK TO SPECIES DATA
# =============================
dt_enriched <- dt %>%
  left_join(site_landuse, by = c("Lokalnamn","Ost","Nord"))

# =============================
# 8) PER-SPECIES SUMMARY & TESTS
# =============================
species_summary <- dt_enriched %>%
  group_by(Artnamn) %>%
  summarise(
    n_sites = n_distinct(Lokalnamn),
    n_obs = n(),
    prop_ej = mean(ej_aterfunnen),
    prop_landuse_change = mean(landuse_change, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(n_sites))

cat("\n=== Species summary ===\n")
print(species_summary)

# Logistic regression per species (if 2x2 table valid)
species_tests <- dt_enriched %>%
  filter(!is.na(landuse_change)) %>%
  group_by(Artnamn) %>%
  do({
    tab <- table(.$landuse_change, .$ej_aterfunnen)
    if (all(dim(tab) == c(2, 2))) {
      fit <- glm(ej_aterfunnen ~ landuse_change, data = ., family = binomial())
      tidy(fit)
    } else {
      tibble(term = NA, estimate = NA, p.value = NA)
    }
  }) %>%
  ungroup()

cat("\n=== Species logistic regression results ===\n")
print(species_tests)

# =============================
# 9) FACETED PLOT (Ej vs change) BY SPECIES
# =============================
min_sites <- 15
keep_species <- dt_enriched %>%
  group_by(Artnamn) %>% summarise(n_sites = n_distinct(Lokalnamn), .groups = "drop") %>%
  filter(n_sites >= min_sites) %>% pull(Artnamn)

plot_dat <- dt_enriched %>%
  filter(Artnamn %in% keep_species, !is.na(landuse_change)) %>%
  group_by(Artnamn, landuse_change) %>%
  summarise(p_ej = mean(ej_aterfunnen), n = n(), .groups = "drop") %>%
  left_join(
    dt_enriched %>% count(Artnamn, name = "n_total"),
    by = "Artnamn"
  ) %>%
  mutate(Artnamn = forcats::fct_reorder(Artnamn, n_total, .desc = TRUE))

ggplot(plot_dat, aes(x = landuse_change, y = p_ej)) +
  geom_col() +
  geom_text(aes(label = paste0("n=", n)), vjust = -0.5, size = 3) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Land-use changed between SMD (older) & NMD (newer)?",
       y = "Share 'Ej återfunnen'",
       title = "‘Ej återfunnen’ vs. land-use change per species") +
  theme_minimal(base_size = 12) +
  facet_wrap(~ Artnamn, scales = "free_y")



library(dplyr)
library(ggplot2)

top_k <- 15

ej_transitions <- dt_enriched %>%
  filter(
    ej_aterfunnen == TRUE,
    landuse_change == TRUE,
    !is.na(change_dir)
  ) %>%
  count(change_dir, name = "n") %>%
  mutate(
    pct = 100 * n / sum(n)
  ) %>%
  arrange(desc(pct))

if (nrow(ej_transitions) == 0) {
  message("No changed sites among 'Ej återfunnen' — nothing to plot.")
} else {
  ej_top <- ej_transitions %>% slice_head(n = top_k)
  
  ggplot(ej_top, aes(x = reorder(change_dir, pct), y = pct)) +
    geom_col() +
    coord_flip() +
    geom_text(
      aes(label = paste0(round(pct, 1), "% (n=", n, ")")),
      hjust = -0.1, size = 3.2
    ) +
    expand_limits(y = max(ej_top$pct) * 1.15) +
    labs(
      title = "Most common land-use transitions among 'Ej återfunnen' sites",
      x = "Transition (SMD → NMD)",
      y = "Percent of changed sites in 'Ej återfunnen'"
    ) +
    theme_minimal(base_size = 12)
}


cmp <- dt_enriched %>%
  filter(landuse_change == TRUE, !is.na(change_dir)) %>%
  mutate(group = ifelse(ej_aterfunnen, "Ej återfunnen", "Återfunnen")) %>%
  count(group, change_dir, name = "n") %>%
  group_by(group) %>%
  mutate(pct = 100 * n / sum(n)) %>%
  ungroup()

top_k <- 10

cmp_top <- cmp %>%
  group_by(group) %>%
  slice_max(pct, n = top_k, with_ties = FALSE) %>%
  ungroup()

ggplot(cmp_top, aes(x = reorder(change_dir, pct), y = pct, fill = group)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~ group, scales = "free_y") +
  geom_text(
    aes(label = paste0(round(pct, 1), "% (n=", n, ")")),
    hjust = -0.1, size = 3
  ) +
  labs(
    title = "Land-use transitions (percent) among changed sites",
    x = "Transition (SMD → NMD)",
    y = "Percent within group"
  ) +
  theme_minimal(base_size = 12)

# {
# 
#   "dataProvider": {
# 
#     "ids": [
# 
#       1
# 
#     ]
# 
#   },
# 
#   "taxon": {
# 
#     "ids": [
# 
#       6000506
# 
#     ],
# 
#     "includeUnderlyingTaxa": true
# 
#   },
# 
#   "projectIds": [
# 
#     4102
# 
#   ],
#   "date": { "startDate": "2025-01-01", "endDate": "2025-12-31" }
#   }