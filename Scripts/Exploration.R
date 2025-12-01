library(dplyr)
library(stringr)
library(ggplot2)

combined_df<-read.csv(file="C:/Users/carst/Documents/FV_Analys/Data/Knarot/FV_95_2025/Knarot_combined.csv")

arkiv_df<-read.csv("C:/Users/carst/Documents/FV_Analys/Data/Knarot/FV_Arkiv/FV_arkiv.csv")

combined_df %>%
  count(Ost, Nord) %>%
  filter(n > 1) #There are rows that have the same coordinates, despite all ID's being unique

#Assign new ID to all sites that have the same coordinates:
combined_df <- combined_df %>%
  group_by(Ost, Nord) %>%
  mutate(PlaceID = cur_group_id()) %>%
  ungroup()

summary(duplicated(combined_df$PlaceID)) #Number of unqiue and duplicated places

#Finding rows where Lokalnamn is not same for PlaceID
#This are problematic localities, some are just slight variations in name, some are completely different. Let's exclude them for now.
combined_df %>%
  group_by(PlaceID) %>%
  summarize(unique_names = n_distinct(Lokalnamn[!is.na(Lokalnamn)])) %>%
  filter(unique_names > 1)

prob<-combined_df %>%
  group_by(PlaceID) %>%
  filter(n_distinct(Lokalnamn[!is.na(Lokalnamn)]) > 1) %>%
  arrange(PlaceID, Lokalnamn)


combined_df_clean <- combined_df %>%
  group_by(PlaceID) %>%
  filter(n_distinct(Lokalnamn[!is.na(Lokalnamn)]) <= 1) %>%
  ungroup()

nrow(combined_df) - nrow(combined_df_clean) #How many localities were removed?

#Plotting where clear cuts and thinning mentioned

# ---- Set your column names here ----
status_col  <- "Ej.återfunnen"     # e.g. "Present" / "Disappeared"
comment_col <- "Publik.kommentar"    # text field with notes/comments
# ------------------------------------

# Build categorized labels from comment text
df_categorized <- combined_df %>%
  mutate(.comment_txt = as.character(.data[[comment_col]])) %>%
  mutate(
    comment_category = case_when(
      # --- Clear-cut & final felling ---
      # English + Swedish: avverkning, slutavverkning, föryngringsavverkning,
      # kalhygge/kalhuggen/hygge, clear-cut, trakthyggesbruk
      !is.na(.comment_txt) & str_detect(
        .comment_txt,
        stringr::regex(
          "(clear[ -]?cut|\\bavverk\\w*|slutavverk\\w*|f?[oö]ryngrings?avverk\\w*|kalhygge\\w*|kalhug\\w*|\\bhygge\\b|trakthyggesbruk)",
          ignore_case = TRUE
        )
      ) ~ "Clear-cut",
      
      # --- Thinning ---
      # English + Swedish: thinning, gallring/gallrad/gallrat
      !is.na(.comment_txt) & str_detect(
        .comment_txt,
        stringr::regex("(thinn\\w*|gallr\\w*)", ignore_case = TRUE)
      ) ~ "Thinned",
      
      # --- Clearing / Pre-commercial thinning ---
      # Swedish: röjning/röjt/röjd, allow 'roj' for missing diacritics
      !is.na(.comment_txt) & str_detect(
        .comment_txt,
        stringr::regex("([rR][öo]j\\w*)", ignore_case = TRUE)
      ) ~ "Clearing/Pre-thin",
      
      # --- Regeneration / Planting (often follows clear-cuts) ---
      !is.na(.comment_txt) & str_detect(
        .comment_txt,
        stringr::regex("(planterad\\w*|plantering\\w*)", ignore_case = TRUE)
      ) ~ "Regeneration/Planting",
      
      # --- Other disturbances / forestry context (optional bucket) ---
      !is.na(.comment_txt) & str_detect(
        .comment_txt,
        stringr::regex("(stormfäll\\w*|vindfäll\\w*|barkborre\\w*|brand\\w*)", ignore_case = TRUE)
      ) ~ "Disturbance/Other",
      
      TRUE ~ "None"
    ),
    # Make a nice ordering for the legend
    comment_category = factor(
      comment_category,
      levels = c("Clear-cut", "Thinned", "Clearing/Pre-thin",
                 "Regeneration/Planting", "Disturbance/Other", "None")
    )
  )

# ---- Counts by Status x Category ----
counts_cat <- df_categorized %>%
  count(!!sym(status_col), comment_category, name = "n")

print(counts_cat)

# ---- Plot counts (bars by Status, split by category) ----
ggplot(counts_cat, aes(x = .data[[status_col]], y = n, fill = comment_category)) +
  geom_col(position = "dodge") +
  labs(
    x = "Ej återfunnen",
    y = "Number of rows",
    fill = "Comment category",
    title = "Comment categories vs Status"
  ) +
  theme_minimal(base_size = 12)

# ---- (Optional) Plot within-Status proportions ----
props_cat <- counts_cat %>%
  group_by(!!sym(status_col)) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

ggplot(props_cat, aes(x = .data[[status_col]], y = prop, fill = comment_category)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x = "Ej återfunnen",
    y = "Share within Status",
    fill = "Comment category",
    title = "Share of comment categories within each Status"
  ) +
  theme_minimal(base_size = 12)
