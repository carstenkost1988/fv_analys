library(readxl)
library(tidyverse)
plantClasses<-read_xlsx("C:/Users/carst/Documents/FV_Analys/Material/ScienceDirect_files_11Nov2025_08-27-27.778/1-s2.0-S1470160X20308621-mmc1.xlsx")
fv1<-read_xlsx("C:/Users/carst/Documents/FV_Analys/Data/Floravaktarna/ZendTo-JdYToBTxZ6PHThjC/ZendTo-JdYToBTxZ6PHThjC/Observations 2025-11-12 12.09 SOS export.xlsx")
fv2<-read_xlsx("C:/Users/carst/Documents/FV_Analys/Data/Floravaktarna/ZendTo-JdYToBTxZ6PHThjC/ZendTo-JdYToBTxZ6PHThjC/Observations 2025-11-12 12.09 SOS export (2).xlsx")
fv<-bind_rows(fv1,fv2)
names(fv) #DyntaxaTaxonId
names(plantClasses) #Dyntaxa ID number

summary(unique(fv$DyntaxaTaxonId) %in% unique(plantClasses$"Dyntaxa ID number"))
missingID<-unique(fv$DyntaxaTaxonId[(fv$DyntaxaTaxonId  %in% plantClasses$"Dyntaxa ID number"==F)])

missing_fv<-fv[fv$DyntaxaTaxonId %in% missingID,]
missing_fv_unique<-distinct(missing_fv, DyntaxaTaxonId, .keep_all = TRUE)
write.csv2(missing_fv_unique,"C:/Users/carst/Documents/FV_Analys/Material/fv_missing_taxons.csv")
