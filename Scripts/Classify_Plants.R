library(readxl)
library(tidyverse)
library(skimr)

plantClasses<-tibble(read_xlsx("C:/Users/carst/Documents/FV_Analys/Material/ScienceDirect_files_11Nov2025_08-27-27.778/1-s2.0-S1470160X20308621-mmc1.xlsx"))
head(plantClasses)
names(plantClasses)
plantClasses <- plantClasses %>% select(1,36:73)
summary(plantClasses)
skim(plantClasses)


# plantClasses_long <- plantClasses %>%
#   pivot_longer(
#     cols = !`Scientific name`,
#     names_to = "Trait",
#     values_to = "Score"
#   )
# 
# ggplot(plantClasses_long, aes(x = Trait, y = `Scientific name`, fill = Score)) +
#   geom_tile() +
#   scale_fill_viridis_c() +
#   theme_minimal() +
#   theme(axis.text.y = element_blank()) +
#   labs(title = "Trait Profiles per Species",
#        x = "Trait", y = "Species")

plantClasses %>%
  mutate(sd_score = apply(select(., where(is.numeric)), 1, sd, na.rm = TRUE)) %>%
  ggplot(aes(x = sd_score)) +
  geom_histogram(bins = 30, fill = "steelblue") +
  labs(title = "Distribution of SD Scores per Species",
       x = "SD", y = "Count of Species")

plantClasses_flagged <- plantClasses %>%
  mutate(
    sd_score = apply(select(., where(is.numeric)), 1, sd, na.rm = TRUE),
    specialist = sd_score >= 1.5
  )

table(plantClasses_flagged$specialist)
# plantClasses_specialists<-plantClasses_flagged[plantClasses_flagged$specialist==T,]
# plantClasses_generalists<-plantClasses_flagged[plantClasses_flagged$specialist==F,]

# names(plantClasses_flagged)
# max.col(plantClasses_flagged[,c(2:39)])
# colnames(plantClasses_flagged[,c(2:39)])[max.col(plantClasses_flagged[,c(2:39)])]
plantClasses_flagged$specialisation<-colnames(plantClasses_flagged[,c(2:39)])[max.col(plantClasses_flagged[,c(2:39)])]
plantClasses_flagged$specialisation[plantClasses_flagged$specialist==F]<-"None"

# plantClasses_flagged$specialisation<-"None"
# for(i in 1:nrow(plantClasses_flagged)){
#   if(plantClasses_flagged$specialist[i]==T){
#     # print(max(plantClasses_flagged[i,c(2:39)]))  
#    maxValue<-max(plantClasses_flagged[i,c(2:39)])
#    colnames()[max.col(plantClasses_flagged[i,c(2:39)])]
#    plantClasses_flagged$specialisation[i]<-colnames(plantClasses_flagged[i,maxValue])
#    print(colnames(plantClasses_flagged[i,maxValue]))
#   }
# }
# {
#   "dataProvider": {"ids": [1]},              
#   "taxon": {
#     "ids": [6000506],                      
#     "includeUnderlyingTaxa": true
#   },
#   "projectIds": [4102],          
# }
