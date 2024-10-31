
##################################
# 
# Author: Dr. Sofia Gil-Clavel
# 
# Last update: October 1st, 2024.
# 
# Description: Code to perform the statistical analyses in:
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
# 
##################################

library(tidyverse)
library(tidyr)
library(ggplot2)

options(scipen=999)

# Cleaning the environment
rm(list=ls())
gc()

#### Opening Data ####
#   "2_Farmers_NetworkData.csv" stored in DANS:
#   Gil-Clavel, Sofia; Filatova, Tatiana, 2024, "Interrelated Climate Change 
#   Adaptation Measures and Factors", https://doi.org/10.17026/SS/PYZCXK, 
#   DANS Data Station Social Sciences and Humanities, DRAFT VERSION; 
#   2_Farmers_NetworkData.csv [fileName] 

FACTORS=read.csv("2_Farmers_NetworkData.csv")

#   "2_Farmers_AdaptationDictionary.csv" stored in DANS:
#   Gil-Clavel, Sofia; Filatova, Tatiana, 2024, "Interrelated Climate Change 
#   Adaptation Measures and Factors", https://doi.org/10.17026/SS/PYZCXK, 
#   DANS Data Station Social Sciences and Humanities, DRAFT VERSION; 
#   2_Farmers_AdaptationDictionary.csv [fileName] 

ADAPT=read.csv("2_Farmers_AdaptationDictionary.csv")
row.names(ADAPT)=ADAPT$Adaptation_Strategy

CTY_NAMES=read.csv("@SofiaG1L/Farmers_CCA/DATA/CountryByRegion.csv")
CTY_NAMES$alpha.2[is.na(CTY_NAMES$alpha.2)]="NA"
row.names(CTY_NAMES)=CTY_NAMES$alpha.2

CTY_NAMES<-CTY_NAMES%>%
  mutate(region=ifelse(region=="Americas",sub.region,region))

### For the other regions:
REGS<-c("AF"="Africa","AS"="Asia","EU"="Europe",
        "SA"="LAC","NA"="N.America","OC"="Oceania",
        'sub-saharan africa'="Africa",
        'latin america and the caribbean'="LAC",
        "Latin America and the Caribbean"="LAC",
        'africa'="Africa",'asia'="Asia",'eastern asia'="Asia",
        'northern africa'="Africa",'south-eastern asia'="Asia",
        'southern asia'="Asia","mediterranean"="Europe",
        "Asia"="Asia","Europe"="Europe","Africa"="Africa",
        "asia"="Asia","europe"="Europe","africa"="Africa",
        "Oceania"="Oceania","oceania"="Oceania",
        "Northern America"="N.America")

#### Cleaning Countries Variables
### Researcher Affiliation ###

FACTORS$AFF=""

N=dim(FACTORS)[1]

II=1
for(II in 1:N){
  la=FACTORS$affiliation_Continent[II]
  la=regmatches(la,gregexpr("'[^']*[^']*'",la))
  la=la[[1]]
  la=gsub("'","",la)
  
  N2=length(la)
  
  if(N2>0){
    REG=c()
    for(JJ in la){
      REG=c(REG,REGS[JJ])
    }
    REG=unique(REG)
    FACTORS[II,]$AFF=paste(REG, collapse = "/")
  }
}


### Studied Area ###
FACTORS$STD=""

N=dim(FACTORS)[1]

II=1
for(II in 1:N){
  la=FACTORS$StudiedPlace_ISO2[II]
  la=regmatches(la,gregexpr("'[^']*[^']*'",la))
  la=la[[1]]
  la=gsub("'","",la)
  
  N2=length(la)
  
  if(N2>0){
    REG=c()
    for(JJ in la){
      if(nchar(JJ)==2){
        REG=c(REG,paste0(JJ,":",REGS[CTY_NAMES[JJ,"region"]]))
      }else{
        REG=c(REG,paste0(JJ,":",REGS[JJ]))
      }
    }
    REG=unique(REG)
    FACTORS[II,]$STD=paste(REG, collapse = "/")
  }else{
    la=FACTORS$StudiedPlace_Continent[II]
    la=regmatches(la,gregexpr("'[^']*[^']*'",la))
    la=la[[1]]
    la=gsub("'","",la)
    
    N2=length(la)
    
    if(N2>0){
      REG=c()
      for(JJ in la){
        if(nchar(JJ)==2){
          REG=c(REG,paste0(JJ,":",REGS[CTY_NAMES[JJ,"region"]]))
        }else{
          REG=c(REG,paste0(JJ,":",REGS[JJ]))
        }
      }
      REG=unique(REG)
      FACTORS[II,]$STD=paste(REG, collapse = "/")
    }
  }
}


### Article Identifier into number

IDS=data.frame(ID_ART=unique(FACTORS$dc.identifier))
IDS$NUMBER=c(1:dim(IDS)[1])
row.names(IDS)=IDS$ID_ART

FACTORS$ID_ART=IDS[FACTORS$dc.identifier,"NUMBER"]

#### Keeping all connections ####

A=FACTORS[,c("ID_ART","STD","source","source_type")]
colnames(A)[3:4]=c("node","node_type")
B=FACTORS[,c("ID_ART","STD","target","target_type")]
colnames(B)[3:4]=c("node","node_type")

FACTORS2=rbind(A,B)%>%
  group_by(ID_ART,node_type) %>% 
  mutate(node_by_type = paste0(node, collapse = "/"))

FACTORS2=FACTORS2[,c("ID_ART","STD","node_by_type","node_type")]
FACTORS2=distinct(FACTORS2)

FACTORS2=FACTORS2%>%spread(node_type,node_by_type)

FACTORS2<-FACTORS2%>%tidyr::separate_rows(FACT, sep = "/")%>%
  filter(FACT != "")%>%
  mutate(value = 1)%>%
  distinct()%>%
  spread(FACT,value,fill=0)

FACTORS2<-FACTORS2%>%
  tidyr::separate_rows(ADAPT, sep = "/")%>%
  distinct()%>%
  mutate(ADAPT=ifelse(is.na(ADAPT),"Climate Change Adaptation",ADAPT))

FACTORS2$TypeAdaptation=ADAPT[FACTORS2$ADAPT,"Type_Adaptation"]
FACTORS2$TypeAdaptation[FACTORS2$TypeAdaptation=="Both"]="Incremental"
FACTORS2$TypeAdaptation[FACTORS2$TypeAdaptation=="Maladaptation"]="Incremental"
FACTORS2=FACTORS2%>%relocate(TypeAdaptation, .after = ADAPT)

FACTORS2$AdaptationCommonName=ADAPT[FACTORS2$ADAPT,"Category"]
FACTORS2=FACTORS2%>%relocate(AdaptationCommonName, .after = ADAPT)

#### Studied Places ####

FACTORS2<-FACTORS2%>%
  tidyr::separate_rows(STD, sep = "/")%>%
  separate(col=STD,sep=":", into = c("Country", "STD"))%>%
  mutate(STD=ifelse(is.na(STD),Country,STD))%>%
  distinct()

FACTORS2=FACTORS2[FACTORS2$STD!="NA",]

# write.csv(FACTORS2,"@SofiaG1L/Farmers_CCA/PROCESSED/FACTORS2.csv")

### CommonName

FACTORS3=FACTORS2

## Some of the variables have too few cases
## so, I aggregated them with other variables
# This:
# Access to infrastructure:
#   access to infrastructure
#   access to irrigation
#   access to market input output
FACTORS3=FACTORS3%>%
  mutate(`Access To Infrastructure`=
           ifelse(`Access To Infrastructure`+
                    `Access To Irrigation`+
                    `Access To Market Input Output`>=1,1,0))%>%
  select(-`Access To Irrigation`,-`Access To Market Input Output`)
# Hazard Experience:
#   Climate-change related hazard experience
#   Non-Climate related hazard
#   Damage experience
FACTORS3=FACTORS3%>%
  mutate(`Hazard Experience`=
           ifelse(`Climate-Change Related Hazard Experience`+
                    `Non-Climate Related Hazard`+
                    `Damage Experience`>=1,1,0))%>%
  select(-`Climate-Change Related Hazard Experience`,
         -`Non-Climate Related Hazard`,
         -`Damage Experience`)
# Psychological factors:
#   Psychological Barriers
#   Psychological Drivers
FACTORS3=FACTORS3%>%
  mutate(`Psychological factors`=
           ifelse(`Psychological Barriers`+
                    `Psychological Drivers`>=1,1,0))%>%
  select(-`Psychological Barriers`,-`Psychological Drivers`)

# `farm characteristics`:
#   `farm characteristics`
#   `farm size`
FACTORS3=FACTORS3%>%
  mutate(`Farm Characteristics`=
           ifelse(`Farm Characteristics`+
                    `Farm Size`>=1,1,0))%>%
  select(-`Farm Size`)

FACTORS3<-FACTORS3%>%select(-"Health",-"Adaptive Capacity",-"Network",
                            -"Local Context",-"TCCA-specific ") #
FACTORS3=FACTORS3[rowSums(FACTORS3[,7:25])>0,]

FACTORS3%>%
  count(STD,ID_ART)%>%
  mutate(ONE=1)%>%
  group_by(STD)%>%
  summarise(sum(ONE))

FACTORS3%>%
  count(STD,ID_ART)%>%
  mutate(ONE=1)%>%
  group_by(ID_ART)%>%
  summarise(sum(ONE))

FACTORS3%>%
  mutate(weights=1)%>%
  group_by(STD)%>%
  mutate(total=sum(sum(weights)))%>%
  group_by(STD,total,TypeAdaptation)%>%
  summarise(n=sum(weights))%>%
  mutate(per=n/total)

## Common Name
FACTORS3$STD<-factor(FACTORS3$STD,
                     levels=c("Europe","Africa","Asia","LAC",
                                "Oceania","N.America"),
                     labels=c("Europe","Africa","Asia","LAC",
                               "Oceania","N.America"))

FACTORS3$STD <- relevel(FACTORS3$STD, ref = "Europe")

FACTORS3[,c(6:25)] <- lapply(FACTORS3[,c(6:25)], factor)
FACTORS3=as.data.frame(FACTORS3)
summary(FACTORS3)

#### Labels and Levels ####

LABELS = rev(c("(Intercept)","Africa","Asia",
               "Europe","LAC","N.America","Oceania",
               "Transformational","Hazard Experience",
               "Access to Infrastructure",
               "Age","Assets","Demographic Factors",
               "Economic Factors","Education",
               "Farm Characteristics",
               "Farming Experience","Gender",
               "Income","Owner",
               "Access to Finance","Government Support",
               "Cultural&Social Norms",
               "Social Capital","Access to Information",
               "Psychological Factors",
               "TCCA-specific ","Access to Technology"))
LEVELS = rev(c("(Intercept)","STDAfrica","STDAsia",
               "STDEurope","STDLAC",
               "STDN.America","STDOceania",
               "TypeAdaptationTransformational","`Hazard Experience`1",
               "`Access To Infrastructure`1",
               "Age1","Assets1","`Demographic Factors`1",
               "`Economic Factors`1","Education1",
               "`Farm Characteristics`1",
               "`Farming Experience`1", "Gender1",
               "Income1","Owner1",
               "`Access To Finance`1","`Government Support`1",
               "`Cultural Social Norms`1",
               "`Social Capital`1","`Access To Information`1",
               "`Psychological factors`1",
               "`TCCA-specific `1","`Access To Technology`1"))

#### Multinomial ####

library(nnet)

fit<-multinom(STD~.,data=FACTORS3[,-c(2,4,5)]) #
(SUM=summary(fit))
t(SUM$coefficients)

# Showing Results as Image #
Coef<-as.data.frame(SUM$coefficients)
Coef$Coef=row.names(Coef)

Coef<-Coef%>%
  tidyr::gather("STD", "Mean",-Coef)

# Confidence Interval
conf=confint(fit,level = 0.90)

CONF=Coef

CONF$lower=""
CONF$upper=""

for(ii in unique(Coef$STD)){
  for(jj in unique(Coef$Coef)){
    CONF[CONF$STD==ii&CONF$Coef==jj,c("lower","upper")]=conf[ii,,jj]
  }
}

CONF$lower=as.double(CONF$lower)
CONF$upper=as.double(CONF$upper)

CONF<-CONF%>%
  mutate(color=ifelse(Mean>0&lower>0&upper>0,"More Likely",
                      ifelse(Mean<0&lower<0&upper<0,"Less Likely","Equally Likely")))

CONF$Category<-factor(CONF$STD,
                      labels = LABELS,
                      levels = LEVELS)


CONF$Coef<-factor(CONF$Coef,
                  levels=c("Africa","Asia","LAC",
                           "N.America","Oceania"),
                  labels=c("Africa","Asia","LAC",
                           "N.America","Oceania"))


CONF%>%
  filter(!STD%in%c("(Intercept)","ID_ART"))%>%
  ggplot(aes(x=Category, y=exp(Mean),color=color,group=Category)) +  # ,shape=names
  geom_errorbar(aes(ymin=exp(lower), ymax=exp(upper)), 
                width=.6,position = position_dodge(1)) +
  scale_color_manual(values=c("More Likely"="#117733",
                              "Less Likely"="#882255","Equally Likely"="black"))+
  coord_flip()+
  facet_grid(.~Coef,scales = "free")+
  geom_point(size=1.5,position = position_dodge(1))+
  annotate("rect",xmin = 1.5, xmax = 2.5, ymin = 0.01, ymax = 30,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 3.5, xmax = 4.5, ymin = 0.01, ymax = 30,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 5.5, xmax = 6.5, ymin = 0.01, ymax = 30,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 7.5, xmax = 8.5, ymin = 0.01, ymax = 30,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 9.5, xmax = 10.5, ymin = 0.01, ymax = 30,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 11.5, xmax = 12.5, ymin = 0.01, ymax = 30,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 13.5, xmax = 14.5, ymin = 0.01, ymax = 30,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 15.5, xmax = 16.5, ymin = 0.01, ymax = 30,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 17.5, xmax = 18.5, ymin = 0.01, ymax = 30,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 19.5, xmax = 20.5, ymin = 0.01, ymax = 30,
           alpha = .1,fill = "gray")+
  scale_y_continuous(trans="log10",expand = c(0, 0),
                     limits = c(0.01,35),
                     breaks = c(0.01,0.03,0.1,0.3,1.0,3.0,10.0,30.0),
                     labels = function(x)ifelse(x<1,format(x,digits = 4),
                                                format(x,digits=1))) +
  geom_hline(yintercept = 1, linetype="dashed")+
  theme(panel.spacing = unit(1, "lines"),
        legend.position = "bottom",
        text = element_text(size = 20),
        axis.text.x = element_text(angle = 60,hjust = 1,vjust = 1),
        strip.text = element_text(size = 18),
        axis.title.y = element_blank(),
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
        plot.caption = element_text(hjust = 0.5,size = 18))+
  guides(color=guide_legend("Likelihood"))+
  labs(y = "Odds Ratio")

# ggsave("@SofiaG1L\Farmers_CCA\IMAGES\RegionsDiff.png",dpi = 600,
#        width = 7.79*1.9,height = 4.03*1.8,units = "in")

#### glm ####
fit<-glm(TypeAdaptation~., #+Psychological,
         family = binomial(),
         data=FACTORS3[,-c(2,4,5)])
(SUM=summary(fit))

# Showing Results as Image #
Coef<-SUM$coefficients[,1]
# Confidence Interval
conf=confint(fit,level = 0.90)
CI1<-data.frame(lower=round(conf[,1],4),
                mean=round(Coef,4),
                upper=round(conf[,2],4))
# SE
CI1$SE<-SUM$coefficients[,2]
# p-values
p <- SUM$coefficients[,4]
CI1$NAMES=row.names(CI1)
CI<-rbind(CI1)

CI<-CI%>%
  mutate(color=ifelse(mean>0&lower>0&upper>0,"More Likely",
                      ifelse(mean<0&lower<0&upper<0,"Less Likely","Equally Likely")))

CI$NAMES<-factor(CI$NAMES,
                 labels = LABELS,
                 levels = LEVELS)

#### Plotting the result ####
CI%>%
  filter(!str_detect(NAMES,"Intercept"),NAMES!="ID_ART")%>%
  ggplot(aes(x=NAMES, y=exp(mean),color=color)) +  # ,shape=names
  geom_errorbar(aes(ymin=exp(lower), ymax=exp(upper)), 
                width=.6,position = position_dodge(1)) +
  scale_color_manual(values=c("More Likely"="#117733",
                              "Less Likely"="#882255","Equally Likely"="black"))+
  coord_flip()+
  geom_point(size=1.5,position = position_dodge(1))+
  annotate("rect",xmin = 1.5, xmax = 2.5, ymin = 0.3, ymax = 3.,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 3.5, xmax = 4.5, ymin = 0.3, ymax = 3.,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 5.5, xmax = 6.5, ymin = 0.3, ymax = 3.,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 7.5, xmax = 8.5, ymin = 0.3, ymax = 3.,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 9.5, xmax = 10.5, ymin = 0.3, ymax = 3.,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 11.5, xmax = 12.5, ymin = 0.3, ymax = 3.,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 13.5, xmax = 14.5, ymin = 0.3, ymax = 3.,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 15.5, xmax = 16.5, ymin = 0.3, ymax = 3.,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 17.5, xmax = 18.5, ymin = 0.3, ymax = 3.,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 19.5, xmax = 20.5, ymin = 0.3, ymax = 3.,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 21.5, xmax = 22.5, ymin = 0.3, ymax = 3.,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 23.5, xmax = 24.5, ymin = 0.3, ymax = 3.,
           alpha = .1,fill = "gray")+
  scale_y_continuous(trans="log10",expand = c(0, 0),
                     limits = c(0.3,3.0),
                     breaks = c(0.3,1.0,3.0),
                     labels = function(x)ifelse(x<1,format(x,digits = 4),
                                                format(x,digits=1))) +
  geom_hline(yintercept = 1, linetype="dashed")+
  theme(panel.spacing = unit(1, "lines"),
        legend.position = "right",
        text = element_text(size = 14),
        axis.title.y = element_blank(),
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
        plot.caption = element_text(hjust = 0.5,size = 14))+
  guides(color=guide_legend("Likelihood"))+
  labs(y = "Odds Ratio")

# ggsave("@SofiaG1L\Farmers_CCA\IMAGES\TransIncrDiff.png",dpi = 600,
#        width = 7.79*1.9,height = 4.03*1.8,units = "in")


