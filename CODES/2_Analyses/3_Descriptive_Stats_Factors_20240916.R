
##################################
# 
# Author: Dr. Sofia Gil-Clavel
# 
# Last update: October 1st, 2024.
# 
# Description: Code to generate Figures 4-6 from: 
#   - Gil-Clavel, S., Wagenblast, T., & Filatova, T. (2023, November 24). Incremental
#       and Transformational Climate Change Adaptation Factors in Agriculture Worldwide:
#       A Natural Language Processing Comparative Analysis. 
#       https://doi.org/10.31235/osf.io/3dp5e
# 
# Computer Environment:
#   - Windows 
#   - R - 4.3.3 (2024-02-29 ucrt) -- "Angel Food Cake"
#   - Rstudio (2023.12.1)
#   - Microsoft Windows 10 Enterprise
# 
# R Packages:
#   - tidyverse (2.0.0)
#   - tidyr (1.3.1)
#   - ggplot2 (3.5.1)
#   - ggforce (0.4.2)
# 
##################################

# General packages
library(tidyverse)
library(tidyr)
library(ggplot2)
# For parallel set
library(ggforce)

# Cleaning the environment
rm(list=ls())
gc()

#### Using Factors2 Database ####
# FACTORS2 comes from script "2_GLM_Models_AllConnections_20240923.R"

DATA0<-read.csv("@SofiaG1L/Farmers_CCA/PROCESSED/FACTORS2.csv")

DATA0$AdaptationCommonName[DATA0$ADAPT=="Farm Infrastructure"]="Physical Infrastructure Management"

N_Arts=length(unique(DATA0$ID_ART))
N_Arts_STD=DATA0%>%
  group_by(ID_ART,STD,Country)%>%
  summarise(one=1)%>%
  group_by(STD)%>%
  summarise(Total=sum(one))
N_Arts_STD<-data.frame(N_Arts_STD)
row.names(N_Arts_STD)<-N_Arts_STD$STD


FA<-read.csv("@SofiaG1L/Farmers_CCA/PROCESSED/Factors_Dictionary_python_SGCTF.csv")

# Shorting some levels
DATA0$AdaptationName=factor(DATA0$ADAPT)
levels(DATA0$AdaptationName)[c(6,7,8,9,10)]=
  c("Borrow - Formal sources",# "Borrow - Informal sources",
    "Incremental","Incremental","Incremental","Transformational")

DATA0$AdaptationCommonName=factor(DATA0$AdaptationCommonName,
                          levels = c("General","crop management",
                                     "farm management","financial management",
                                     "irrigation and water management",
                                     "Physical Infrastructure Management",
                                     "Information Management "),
                          labels = c("CCA General Terms","Crop\nManagement",
                                     "Farm\nManagement","Financial\nManagement",
                                     "Irrigation & Water\nManagement",
                                     "Physical Infrastructure\nManagement",
                                     "Information\nManagement"))
  

#### Adaptation Descriptive Statistics ####

DATA0%>%
  mutate(N_Arts=N_Arts)%>%
  add_count(ID_ART,Country)%>%
  group_by(ID_ART,Country,AdaptationCommonName,AdaptationName,TypeAdaptation,n,N_Arts)%>%
  summarise(value=1/n)%>%
  group_by(AdaptationCommonName,AdaptationName,TypeAdaptation)%>%
  summarise(Total=sum(value))%>%
  mutate(per=Total/N_Arts)%>%
  filter(AdaptationCommonName!="CCA General Terms")%>%
  ggplot(aes(x=reorder(AdaptationName,-per), y=per,
             fill=TypeAdaptation))+
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Dark2")+
  scale_y_continuous(labels = scales::percent,expand = c(0,0))+ # ,limits = c(0,0.12)
  facet_wrap(.~AdaptationCommonName,scales = "free_x",ncol = 3)+
  theme_light(base_size = 15)+
  theme(axis.text.x = element_text(angle=90,
                                   vjust = 0.5,hjust = 1),
        legend.position = "bottom",axis.title = element_blank(),
        strip.text = element_text(colour = 'black'))+
  labs(fill="Adaptation Type:")

# ggsave("@SofiaG1L/Farmers_CCA/IMAGES/AdaptationType_20240927.png",
#        units = "cm",height = 20,width =30)

#### Factors Descriptive Statistics ####

DATA<-DATA0%>%
  select(-ADAPT,-AdaptationCommonName)%>%
  gather(key="Driver",value="value",-ID_ART,-Country,-STD,-TypeAdaptation)%>%
  left_join(FA[,c("Tatiana_Factor_Strategy","Tatiana_Category")],
            by=c("Driver"="Tatiana_Factor_Strategy"))%>%
  distinct()%>%
  mutate(Umbrella_Terms=
           ifelse(Driver=="TypeAdaptation","TypeAdaptation",Tatiana_Category),
         Driver=ifelse(Driver=="TypeAdaptation","Transformational",Driver),
         value=ifelse(value=="0",0,1))%>%
  filter(value>0)%>%
  add_count(ID_ART,Country)%>%
  mutate(value=1/n)%>%
  select(-n)

#### Parallel Sets ####

# Function from:
# https://stackoverflow.com/questions/74284095/in-ggforce-geom-parallel-sets-labels-how-to-add-more-information-to-the-bar-l?rq=1
helper_after_stat <- function(x, value, label) {
  data.frame(x = x, value = value, label = label) %>% 
    group_by(x) %>% 
    mutate(value_p = value / sum(value),
           label = scales::percent(value_p,accuracy = 1)) %>%
    pull(label)
}



### Parallel Sets Graph

data2<-DATA0%>%
  select(-ADAPT,-AdaptationName)%>%
  distinct()%>%
  gather(key="Driver",value="value",-ID_ART,-Country,-STD,
         -AdaptationCommonName,-TypeAdaptation)%>%
  left_join(FA[,c("Tatiana_Factor_Strategy","Tatiana_Category")],
            by=c("Driver"="Tatiana_Factor_Strategy"))%>%
  select(-Driver)%>%
  mutate(Umbrella_Terms=Tatiana_Category)%>%
  group_by(ID_ART,Country,STD,Umbrella_Terms,
           AdaptationCommonName,TypeAdaptation)%>%
  distinct()%>%
  ungroup()%>%
  filter(value>0,Umbrella_Terms!="TCCA-specific ")

levels(data2$AdaptationCommonName)=
  c("CCA General Terms","Crop Management","Farm Management",
    "Financial Management",
    "Irrigation & Water Management",
    "Physical Infrastructure Management",
    "Information Management")

data2<-data2%>%
  add_count(ID_ART,Country)%>% # This adds "n"
  group_by(ID_ART,Country,STD,Umbrella_Terms,AdaptationCommonName,TypeAdaptation)%>%
  summarise(value=sum(value)/n)%>%
  ungroup()

data2[,3:6] <- lapply(data2[,3:6], factor)
data2=as.data.frame(data2)
summary(data2)

data2<-gather_set_data(data2, 3:6)

data2$x=factor(data2$x,
               levels = c(3,4,5,6),
               labels = c("Region","Factor","Adaptation\nMeasure","Adaptation\nType"))

data2%>%
  ggplot(aes(x=x, id = id, split = y, value = value)) +
  geom_parallel_sets(aes(fill = TypeAdaptation), alpha = 0.3,axis.width = 0.1) +
  geom_parallel_sets_axes(axis.width = 0.1, color = "lightgrey", fill = "white")  +
  geom_parallel_sets_labels(
    aes(label = after_stat(helper_after_stat(x, value, label))),
                            colour = 'black')+
  geom_parallel_sets_labels(colour = 'black', 
                            angle = 0, nudge_x = 0.1, hjust = 0)+
  theme_light(base_size = 14)+
  scale_fill_brewer(type = "qual",palette = "Dark2")+
  scale_x_discrete(expand = c(0,0),
                   limits=c("Region","Factor","Adaptation\nMeasure","Adaptation\nType"))+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  guides(fill=guide_legend(title = "Adaptation Type",nrow = 1))

# ggsave("@SofiaG1L/Farmers_CCA/IMAGES/ParallelSets_20240927.png",
#        width = 30,height = 20,units = "cm")

### Type of Factor ####

is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}


### This calculation gives the percentage of times the type of adaptation appeared
### relative to the number of articles and regions
TypeADAPT=DATA0%>%ungroup()%>%
  select(ID_ART,STD,TypeAdaptation)%>%
  distinct()%>%
  mutate(n=1)%>%
  group_by(STD)%>%
  mutate(Total=sum(n))%>%
  select(-ID_ART)%>%
  group_by(STD,TypeAdaptation)%>%
  mutate(Count=sum(n))%>%
  filter(TypeAdaptation=="Transformational")%>%
  mutate(per=Count/Total)%>%
  distinct()%>%
  ungroup()%>%
  select(-TypeAdaptation,-n,-Total,-Count)%>%
  mutate(Factor="Transformational",Tatiana_Category="Type of Adaptation")

### This calculation gives the percentage of times the factors appeared
### relative to the number of articles and regions
pers=DATA0%>%
  filter(STD!="")%>%
  select(-AdaptationName,-Country,-ADAPT,-AdaptationCommonName,-TypeAdaptation)%>%
  distinct()%>%
  gather(key="Factor",value="value",-ID_ART,-STD)%>%
  left_join(FA[,c("Tatiana_Factor_Strategy","Tatiana_Category")],
            by=c("Factor"="Tatiana_Factor_Strategy"))%>%
  group_by(STD,Factor)%>%
  add_count(name = "Total")%>%
  mutate(Count=sum(value))%>%
  ungroup()%>%
  select(-ID_ART)%>%
  filter(value>0)%>%
  distinct()%>%
  mutate(per=Count/Total)%>%
  select(-value,-Total,-Count)

pers=bind_rows(TypeADAPT,pers)

pers$Factor=factor(pers$Factor,
                   level=rev(c("Transformational",
                               "Climate-Change Related Hazard Experience",
                           "Damage Experience",
                           "Non-Climate Related Hazard",
                           "Access To Infrastructure",
                           "Access To Irrigation",
                           "Access To Market Input Output",
                           "Adaptive Capacity",
                           "Age",
                           "Assets",
                           "Demographic Factors",
                           "Economic Factors",
                           "Education",
                           "Farm Characteristics",
                           "Farm Size",
                           "Farming Experience",
                           "Gender",
                           "Health",
                           "Income",
                           "Local Context",
                           "Owner",
                           "Access To Finance",
                           "Government Support",
                           "Cultural Social Norms",
                           "Network",
                           "Social Capital",
                           "Access To Information",
                           "Psychological Barriers",
                           "Psychological Drivers",
                           "TCCA-specific ",
                           "Access To Technology")))

pers$Tatiana_Category=factor(pers$Tatiana_Category,
                             level=rev(c("Type of Adaptation",
                                         "Hazard Experience",
                                     "Individual AC",
                                     "Institutional AC - Formal Institutions",
                                     "Institutional AC - Informal Institutions",
                                     "Knowledge and Information",
                                     "Psychological factors",
                                     "TCCA-specific ",
                                     "Technology")))

pers <- pers %>% 
  rownames_to_column(var="outlier") %>% 
  group_by(Factor) %>% 
  mutate(is_outlier=ifelse(is_outlier(per), per, as.numeric(NA)))
pers$outlier=NA
pers$outlier[!is.na(pers$is_outlier)] <- pers$STD[!is.na(pers$is_outlier)]

### Adding more colors to the palette
colourCount = length(unique(pers$Tatiana_Category))
getPalette = grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Dark2"))

pers%>%
  filter(!Factor%in%c("Adaptive Capacity","Health",
                      "Network","Local Context","TCCA-specific "))%>%
  ggplot(aes(Factor,per,fill=Tatiana_Category))+
  guides(fill=guide_legend("Umbrella Category",reverse = TRUE,
                           position = "bottom"))+
  scale_fill_manual(values = getPalette(colourCount)) +
  geom_boxplot(outlier.colour = "red")+
  annotate("rect",xmin = 1.5, xmax = 2.5, ymin = 0, ymax = 1,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 3.5, xmax = 4.5, ymin = 0, ymax = 1,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 5.5, xmax = 6.5, ymin = 0, ymax = 1,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 7.5, xmax = 8.5, ymin = 0, ymax = 1,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 9.5, xmax = 10.5, ymin = 0, ymax = 1,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 11.5, xmax = 12.5, ymin = 0, ymax = 1,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 13.5, xmax = 14.5, ymin = 0, ymax = 1,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 15.5, xmax = 16.5, ymin = 0, ymax = 1,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 17.5, xmax = 18.5, ymin = 0, ymax = 1,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 19.5, xmax = 20.5, ymin = 0, ymax = 1,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 21.5, xmax = 22.5, ymin = 0, ymax = 1,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 23.5, xmax = 24.5, ymin = 0, ymax = 1,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 25.5, xmax = 26.5, ymin = 0, ymax = 1,
           alpha = .1,fill = "gray")+
  labs(y = "Percentage",x="Control Variable")+
  scale_y_continuous(expand = c(0, 0),limits = c(0,1.05))+
  theme(panel.spacing = unit(1, "lines"),legend.byrow = TRUE,
        text = element_text(size=24),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_line(size = 0.50, 
                                          linetype = 'dashed',
                                          colour = "gray"),
        panel.grid.minor.x = element_line(size = 0.50, 
                                          linetype = 'dashed',
                                          colour = "gray"),
        strip.background = element_rect(color = NA,
                                        fill = NA, size = 1),
        axis.line = element_line(color = 'black'),
        legend.justification.bottom = "right")+
  geom_text(aes(label=outlier),color="black",na.rm=TRUE,
            nudge_y=-0.01,nudge_x = 0.05,size=5)+
  coord_flip()

# ggsave("@SofiaG1L/Farmers_CCA/IMAGES/FactorsBoxPlots_20240927.png",
#        units = "cm",height = 30,width =50,dpi = 300)

# This is table D2:
# write.csv(pers%>%spread(STD,per),
#           "@SofiaG1L/Farmers_CCA/PROCESSED/FactorsByRegion_20240927.csv")



























