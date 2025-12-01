library(tidyverse)
library(terra)
knarot<-read.csv("C:/Users/carst/Documents/FV_Analys/Data/Knarot/FV_95_2025/Knarot_combined.csv")
avv<-vect("C:/Users/carst/Documents/FV_Analys/Data/Avverkningar/sksUtfordAvverk.gpkg")
# knarot_vect <- st_as_sf(sites_dt, coords = c("Ost","Nord"), crs = crs_points)
knarot_vect  <- vect(knarot, geom=c("Ost", "Nord"))
crs(knarot_vect)<-"EPSG:3021"

#Important: Harmonize CRS
avv <- terra::project(avv, "EPSG:3006")
knarot_vect <- terra::project(knarot_vect, "EPSG:3006")


  # knarot_vect<-terra::project(knarot_vect, crs(avv,proj=T))
  #writeVector(knarot_vect,"C:/Users/carst/Documents/FV_Analys/Data/Knarot/FV_95_2025/vect_test.gpkg",overwrite=T)

  # knarot_vect_buf<-buffer(knarot_vect, width = knarot_vect$Noggrannhet)
  # writeVector(knarot_vect_buf,"C:/Users/carst/Documents/FV_Analys/Data/Knarot/FV_95_2025/vect_buf_test.gpkg",overwrite=T)
knarot_vect_buf<-vect("C:/Users/carst/Documents/FV_Analys/Data/Knarot/FV_95_2025/vect_buf_test.gpkg")

unique(knarot_vect_buf$Lokalnamn)
knarot_vect_buf$Lokalnamn[duplicated(knarot_vect_buf$Lokalnamn)]
knarot$Lokalnamn[duplicated(knarot$Lokalnamn)]
knarot[knarot$Lokalnamn %in% knarot$Lokalnamn[duplicated(knarot$Lokalnamn)], ]
table(knarot$Lokalnamn)[ table(knarot$Lokalnamn) > 1 ]

# intersections<-intersect(knarot_vect_buf,avv)
# writeVector(intersections,"C:/Users/carst/Documents/FV_Analys/Data/Knarot/FV_95_2025/intersections_test.gpkg",overwrite=T)
intersections<-vect("C:/Users/carst/Documents/FV_Analys/Data/Knarot/FV_95_2025/intersections_test.gpkg")


# df_intersections<-as.data.frame(intersections)
# df_m<-merge(knarot,subset(df_intersections,select=c("Id","Avvdatum","AnmaldHa")),all.x=T,by="Id")
# head(df_m)
# nrow(df_m)
# nrow(knarot_vect_buf)
# nrow(intersections)

intersections$site_date <- as.Date(intersections$Startdatum)  # survey / last visit date
intersections$cut_date  <- as.Date(intersections$Avvdatum)   # clear-cut date

intersections_df <- as.data.frame(intersections)

intersections_df <- intersections_df %>%
  mutate(
    cut_before = cut_date <= site_date,
    cut_after  = cut_date >  site_date,
    # time difference (positive = cut before survey)
    days_diff  = as.numeric(difftime(site_date, cut_date, units = "days")),
    years_diff = days_diff / 365.25 #.25 to account for leap years
  )

site_clearcut_summary <- intersections_df %>%
  group_by(Id) %>%   # or Lokalnamn?
  summarise(
    any_cut_overlap      = n() > 0,
    any_cut_before       = any(cut_before, na.rm = TRUE),
    any_cut_after        = any(cut_after,  na.rm = TRUE),
    min_years_since_cut  = if (any(cut_before, na.rm = TRUE))
      min(years_diff[cut_before], na.rm = TRUE)
    else NA_real_,
    n_cuts_overlapping   = n(),
    .groups = "drop"
  )

knarot_vect_buf2 <- knarot_vect_buf

knarot_vect_buf2 <- terra::merge(
  knarot_vect_buf2,
  site_clearcut_summary,
  by = "Id",   # or "Lokalnamn"
  all.x = TRUE
)

df_sites <- as.data.frame(knarot_vect_buf2)
df_sites$ej_aterfunnen_num <- as.integer(df_sites$Ej.återfunnen == "True")
fit <- glm(ej_aterfunnen_num ~ any_cut_before + min_years_since_cut,
           data = df_sites, family = binomial())
summary(fit)


fit <- glm(ej_aterfunnen_num ~ any_cut_before,
           data = df_sites, family = binomial())
summary(fit)
plogis(-3.36) #Probability of a ej_återfunnen without clear-cut
plogis(-3.36 + 2.3452) #Probability of a ej_återfunnen wit clear-cut

exp(coef(fit))

newdat <- data.frame(any_cut_before = c(FALSE, TRUE))
newdat$pred <- predict(fit, newdat, type = "response")

ggplot(newdat, aes(x = any_cut_before, y = pred)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = round(pred, 3)), vjust = -0.5, size = 5) +
  ylim(0, 1) +
  labs(x = "Clearcut before observation",
       y = "Predicted probability of 'ej återfunnen'",
       title = "Effect of clear-cuts on probability of not being refound") +
  theme_minimal(base_size = 14)

df_sites %>%
  group_by(any_cut_before) %>%
  summarise(prop_ej = mean(ej_aterfunnen_num),
            n = n()) %>%
  ggplot(aes(x = any_cut_before, y = prop_ej)) +
  geom_col(fill = "orange") +
  geom_text(aes(label = paste0("n=", n)), vjust = -0.5) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Observed probabilities of 'Ej återfunnen'",
    x = "Any cut before visit?",
    y = "Observed probability"
  ) +
  theme_minimal(14)

