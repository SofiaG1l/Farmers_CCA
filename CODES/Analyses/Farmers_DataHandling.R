
##################################
# 
# Author: Dr. Sofia Gil-Clavel
# 
# Date: October 1st, 2024.
# 
# Description: Code to data handle the network data frame 
#   "FarmersDataFrame_Phase1.csv" stored in DANS:
# Gil-Clavel, Sofia; Filatova, Tatiana, 2024, "Interrelated Climate Change 
#   Adaptation Measures and Factors", https://doi.org/10.17026/SS/PYZCXK, 
#   DANS Data Station Social Sciences and Humanities, DRAFT VERSION; 
#   FarmersDataFrame_Phase1.csv [fileName] 
# 
# Computer Environment:
#   - Windows 
#   - R - 4.3.3 (2024-02-29 ucrt) -- "Angel Food Cake"
#   - Rstudio (2023.12.1)
#   - Microsoft Windows 10 Enterprise
# 
# R Packages:
#   - tidyverse (2.0.0)
# 
##################################

rm(list=ls())
gc()

library(tidyverse)

#### Opening Data ####

FACTORS=read.csv(".\\FarmersDataFrame_Phase1.csv")

#### Opening the farmers' measures dictionary ####

ADAPT=read.csv(".\\ADAPT_farmers.csv")
row.names(ADAPT)=ADAPT$Adaptation_Strategy

#### Keeping only the MEASURE - FACTOR connections ####

FACTORS2=FACTORS[,c("prism.doi","ID_ART","STD","SIGN",
                    "source","source_type","target","target_type")]

FACTORS2=FACTORS2[FACTORS2$source_type!=FACTORS2$target_type,]

FACTORS2=FACTORS2%>%
  mutate(Adaptation=ifelse(source_type=="ADAPT",source,target))%>%
  mutate(Factor=ifelse(source_type=="FACT",source,target))

FACTORS2$TypeAdaptation=ADAPT[FACTORS2$Adaptation,"Type_Adaptation"]
FACTORS2$TypeAdaptation[FACTORS2$TypeAdaptation=="Both"]="Incremental"
FACTORS2$TypeAdaptation[FACTORS2$TypeAdaptation=="Maladaptation"]="Incremental"
FACTORS2=FACTORS2%>%relocate(TypeAdaptation, .after = Adaptation)

FACTORS2$AdaptationCommonName=ADAPT[FACTORS2$Adaptation,"Category"]
FACTORS2=FACTORS2%>%
  relocate(AdaptationCommonName, .after = Adaptation)%>%
  group_by(prism.doi,ID_ART,STD,Adaptation,TypeAdaptation,Factor)%>%
  mutate(value=ifelse(SIGN=="+",1,ifelse(SIGN=="-",-1,0)))%>%
  select(-SIGN)%>%
  summarise(value = sum(value))

FACTORS2=distinct(FACTORS2)

FACTORS2=FACTORS2%>%spread(Factor,value,fill = NA)

FACTORS2=FACTORS2%>%
  separate_longer_delim(STD, delim = "/")


#### Studied Places ####

FACTORS2<-FACTORS2%>%
  tidyr::separate_rows(STD, sep = "/")%>%
  separate(col=STD,sep=":", into = c("Country", "STD"))%>%
  mutate(STD=ifelse(is.na(STD),Country,STD))%>%
  distinct()

#### Saving new database ####

FACTORS2%>%
  filter(prism.doi!="")%>%
  write.csv(".\\FarmersDataFrame_Phase2_OnlyMeasureFactor.csv")

#### Keeping all connections ####

A=FACTORS[,c("prism.doi","ID_ART","STD","source","source_type")]
colnames(A)[4:5]=c("node","node_type")
B=FACTORS[,c("prism.doi","ID_ART","STD","target","target_type")]
colnames(B)[4:5]=c("node","node_type")

FACTORS3=rbind(A,B)%>%
  group_by(prism.doi,ID_ART,node_type) %>% 
  mutate(node_by_type = paste0(node, collapse = "/"))

FACTORS3=FACTORS3[,c("prism.doi","ID_ART","STD","node_by_type","node_type")]
FACTORS3=distinct(FACTORS3)

FACTORS3=FACTORS3%>%spread(node_type,node_by_type)

FACTORS3<-FACTORS3%>%tidyr::separate_rows(FACT, sep = "/")%>%
  filter(FACT != "")%>%
  mutate(value = 1)%>%
  distinct()%>%
  spread(FACT,value,fill=0)

FACTORS3<-FACTORS3%>%
  tidyr::separate_rows(ADAPT, sep = "/")%>%
  distinct()%>%
  mutate(ADAPT=ifelse(is.na(ADAPT),"Climate Change Adaptation",ADAPT))

FACTORS3$TypeAdaptation=ADAPT[FACTORS3$ADAPT,"Type_Adaptation"]
FACTORS3$TypeAdaptation[FACTORS3$TypeAdaptation=="Both"]="Incremental"
FACTORS3$TypeAdaptation[FACTORS3$TypeAdaptation=="Maladaptation"]="Incremental"
FACTORS3=FACTORS3%>%relocate(TypeAdaptation, .after = ADAPT)

FACTORS3$AdaptationCommonName=ADAPT[FACTORS3$ADAPT,"Category"]
FACTORS3=FACTORS3%>%relocate(AdaptationCommonName, .after = ADAPT)

#### Studied Places ####

FACTORS3<-FACTORS3%>%
  tidyr::separate_rows(STD, sep = "/")%>%
  separate(col=STD,sep=":", into = c("Country", "STD"))%>%
  mutate(STD=ifelse(is.na(STD),Country,STD))%>%
  distinct()

#### Saving the Data ####

FACTORS3%>%
  filter(prism.doi!="")%>%
  write.csv(".\\FarmersDataFrame_Phase2_AllConnections.csv")
