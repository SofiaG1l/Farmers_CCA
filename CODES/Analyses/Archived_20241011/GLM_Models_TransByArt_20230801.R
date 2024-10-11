
library(tidyverse)
library(tidyr)
library(ggplot2)
library(ggridges)
library(ggExtra)
# Following packages are for maps
library(sf)
library(tmap)
library(cowplot)
# Following packages are for networks
library(igraph)       # To create a Network Object.
library(ggraph)       # To create the network visualization.
# Color blind pallet
library(rcartocolor)
# display_carto_all(colorblind_friendly = TRUE)

options(scipen=999)

rm(list=ls())
gc()

#### Opening Data ####

FACTORS=read.csv("PROCESSED/FACTORS2_notUsed.csv")

VARIABLES=read.csv("PROCESSED/Factors_Dictionary2_20230801.csv")

CTY_NAMES=read.csv("DATA/CountryByRegion.csv")
CTY_NAMES$alpha.2[is.na(CTY_NAMES$alpha.2)]="NA"
row.names(CTY_NAMES)=CTY_NAMES$alpha.2

CTY_NAMES<-CTY_NAMES%>%
  mutate(region=ifelse(region=="Americas",sub.region,region))

### For the other regions:
REGS<-c("AF"="Africa","AS"="Asia","EU"="Europe",
        "SA"="S.America","NA"="Northern America","OC"="Oceania")
REGS
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
      REG=c(REG,paste0(la,":",CTY_NAMES[la,"region"]))
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
        REG=c(REG,REGS[JJ])
      }
      REG=unique(REG)
      FACTORS[II,]$STD=paste(REG, collapse = "/")
    }
  }
}

# Checking
FACTORS=FACTORS%>%
  mutate(STD=ifelse(STD!="",STD,AFF))

### Analysis of Adaptation ####
# VARIABLES$Row.Labels<-str_trim(VARIABLES$Row.Labels,"both")

FACTORS$source=gsub("\n","",FACTORS$source)
FACTORS$target=gsub("\n","",FACTORS$target)

FACTORS<-FACTORS%>%gather("EdgeType","Strings",source, target)

FACTORS$TypeAdaptation=""
FACTORS$AdaptationName=""
FACTORS$Driver=""

N=dim(FACTORS)[1]

for(ii in 1:N){
  
  SUBSET=try(
    #VARIABLES[VARIABLES$Row.Labels==FACTORS$Strings[ii],],
    VARIABLES[str_detect(VARIABLES$Row.Labels,FACTORS$Strings[ii]),],
    TRUE)

  if(class(SUBSET)!="try-error"){  
    if(dim(SUBSET)[1]==1){
      if(SUBSET$Adaptation==1){
        FACTORS$TypeAdaptation[ii]=SUBSET$Transformational 
        FACTORS$AdaptationName[ii]=SUBSET$CommonName 
      }
      if(SUBSET$DriverAdaptation==1){
        FACTORS$Driver[ii]=SUBSET$CommonName 
      }
    }else{
      SUBSET=VARIABLES[VARIABLES$Row.Labels==FACTORS$Strings[ii],]
      if(dim(SUBSET)[1]==1){
        if(SUBSET$Adaptation==1){
          FACTORS$TypeAdaptation[ii]=SUBSET$Transformational 
          FACTORS$AdaptationName[ii]=SUBSET$CommonName 
        }
        if(SUBSET$DriverAdaptation==1){
          FACTORS$Driver[ii]=SUBSET$CommonName 
        }
      }
    }
  }

}

#### Checking 10% sample ####
# set.seed(45461)
# IDS=unique(FACTORS$ID_ART)
# SAMPLE_IDS=sample(IDS,0.1*length(IDS))
# View(FACTORS%>%
#        filter(ID_ART%in%SAMPLE_IDS & (AdaptationName!=""|Driver!=""))%>%
#        arrange(ID_ART))

### Control Variables ####

length(unique(FACTORS$ID_ART))
FACTORS2=FACTORS[FACTORS$TypeAdaptation!="" | FACTORS$Driver!="",
                 c("ID_ART","doi","STD","ADAPT","AdaptationName","TypeAdaptation",
                   "Driver")]# "AFF",
length(unique(FACTORS2$ID_ART))

FACTORS2<-FACTORS2%>%
  group_by(ID_ART,STD)%>% # AFF,
  mutate(Driver=paste(unique(Driver),collapse = "/"))

unique(FACTORS2$ID_ART)

FACTORS2<-FACTORS2%>%distinct()

IND=c()

N=dim(FACTORS2)[1]
for(ii in 1:N){
  
  check=strsplit(FACTORS2$Driver[ii],"/")
  
  if(length(check[[1]])>=1){
    ere=FACTORS2[ii,] # the row to copy
    for(jj in check[[1]]){
      ere$Driver=jj
      FACTORS2=rbind(FACTORS2,ere)
    }
    IND=c(IND,ii)
  }
}

length(unique(FACTORS2$ID_ART))
FACTORS2=FACTORS2[-IND,]
length(unique(FACTORS2$ID_ART))

#### Studied Places ####

FACTORS2$ONE=1
FACTORS2<-FACTORS2%>%spread(Driver,ONE)
FACTORS2[is.na(FACTORS2)] <- 0

# Removing the empty extra column created by spliting by "/"
FACTORS2=FACTORS2%>%
  select(-V1)

# Creating extra rows for the extra regions 

sum(FACTORS2$STD=="")

# FACTORS2$STD=ifelse(FACTORS2$STD=="",FACTORS2$AFF,FACTORS2$STD)

IND=c()

N=dim(FACTORS2)[1]
for(ii in 1:N){
  
  check=strsplit(FACTORS2$STD[ii],"/")
  
  if(length(check[[1]])>=1){
    ere=FACTORS2[ii,] # the row to copy
    for(jj in check[[1]]){
      ere$STD=jj
      FACTORS2=rbind(FACTORS2,ere)
    }
    IND=c(IND,ii)
  }
}

FACTORS2=FACTORS2[-IND,]

#### Adding all the adaptation options ####

FACTORS2=FACTORS2%>%
  mutate(AdaptationName=ADAPT)

FACTORS2<-FACTORS2%>%distinct()

IND=c()

N=dim(FACTORS2)[1]
ii=1
for(ii in 1:N){
  
  check=strsplit(FACTORS2$AdaptationName[ii],"/")
  check=unique(check[[1]])
  
  if(length(check)>=1){
    ere=FACTORS2[ii,] # the row to copy
    for(jj in check){
      ere$AdaptationName=jj
      FACTORS2=rbind(FACTORS2,ere)
    }
    IND=c(IND,ii)
  }
}

FACTORS2=FACTORS2[-IND,]
length(unique(FACTORS2$ID_ART))

FACTORS2=FACTORS2%>%
  mutate(AdaptationName=ifelse(AdaptationName=="","General",AdaptationName))

### Adding inc. vs tra. ####

FACTORS2$AdaptationCommonName=""

N=dim(FACTORS2)[1]

for(ii in 1:N){
  
  SUBSET=try(
    #VARIABLES[VARIABLES$Row.Labels==FACTORS2$Strings[ii],],
    VARIABLES[str_detect(VARIABLES$Row.Labels,FACTORS2$AdaptationName[ii]),],
    TRUE)
  
  if(class(SUBSET)!="try-error"){  
    if(dim(SUBSET)[1]==1){
      if(SUBSET$Adaptation==1){
        FACTORS2$TypeAdaptation[ii]=SUBSET$Transformational 
        FACTORS2$AdaptationCommonName[ii]=SUBSET$CommonName 
      }
    }
  }
  
}


FACTORS2=FACTORS2%>%
  mutate(TypeAdaptation=ifelse(TypeAdaptation=="","0",TypeAdaptation))

FACTORS2=FACTORS2%>%
  mutate(AdaptationCommonName=
           ifelse(AdaptationName=="General","General",AdaptationCommonName))

FACTORS2<-FACTORS2%>%
  separate(col=STD,sep=":", into = c("Country", "STD"))%>%
  mutate(STD=ifelse(is.na(STD),Country,STD))%>%
  distinct()

#### Running the Model ####
# FACTORS3=FACTORS2%>%
#   group_by(ID_ART,Country)%>%
#   mutate(CHECK=ifelse(any(TypeAdaptation=="1"),1,0))%>%
#   select(-AdaptationName,-AdaptationCommonName,-ADAPT,-TypeAdaptation)%>%
#   filter(!STD%in%c(""))%>%
#   distinct()

FACTORS3=FACTORS2%>%
  group_by(ID_ART,Country)%>%
  mutate(TypeAdaptation=ifelse(any(TypeAdaptation=="1"),1,0))%>%
  ungroup()%>%
  select(-AdaptationName,-AdaptationCommonName,-ADAPT)%>%
  group_by_all()%>%
  distinct()%>%
  ungroup()

## Some of the variables have too few cases
## so, I aggregated them with other variables
# This:
# Access to infrastructure:
#   access to infrastructure
#   access to irrigation
#   access to market input output
FACTORS3=FACTORS3%>%
  mutate(`access to infrastructure`=
           ifelse(`access to infrastructure`+
           `access to irrigation`+
           `access to market input output`>=1,1,0))%>%
  select(-`access to irrigation`,-`access to market input output`)
# Hazard Experience:
#   Climate-change related hazard experience
#   Non-Climate related hazard
#   Damage experience
FACTORS3=FACTORS3%>%
  mutate(`Hazard Experience`=
           ifelse(`Climate-change related hazard experience`+
           `Non-Climate related hazard`+
           `Damage experience`>=1,1,0))%>%
  select(-`Climate-change related hazard experience`,
           -`Non-Climate related hazard`,
           -`Damage experience`)
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
  mutate(`farm characteristics`=
           ifelse(`farm characteristics`+
                    `farm size`>=1,1,0))%>%
  select(-`farm size`)

# FACTORS3<-FACTORS3%>%distinct()

# FACTORS3$CHECK<-factor(FACTORS3$CHECK)

# FACTORS3<-FACTORS3%>%distinct()

FACTORS3%>%
  mutate(weights=1)%>%
  group_by(STD)%>%
  mutate(total=sum(sum(weights)))%>%
  group_by(STD,total,TypeAdaptation)%>%
  summarise(n=sum(weights))%>%
  mutate(per=n/total)

## CommonName
# colnames(FACTORS3)[c(8,11,12,15)]=c("AdaptiveCapacity","CulturalSocialNorms",
#                                  "DemographicFactors","FarmManagement")
FACTORS3$STD<-factor(FACTORS3$STD,
                     levels = c("Africa","Asia","Europe",
                                "Latin America and the Caribbean",
                                "Northern America",
                                "Oceania"),
                     labels = c("Africa","Asia","Europe",
                                "LAC","N.America",
                                "Oceania"))

FACTORS3[,c(5:25)] <- lapply(FACTORS3[,c(5:25)], factor)

FACTORS3=as.data.frame(FACTORS3)
summary(FACTORS3)

### CommonName2
# FACTORS3[,5:40] <- lapply(FACTORS3[,5:40], factor)
# 
# FACTORS3=as.data.frame(FACTORS3)
# summary(FACTORS3)

#### The Models ####

library(nnet)

fit<-multinom(STD~.,data=FACTORS3[,-c(2:3)]) #
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

CONF$STD<-factor(CONF$STD,
                 levels = rev(c("(Intercept)","ID_ART",
                                "TypeAdaptation1",
                                "`Access to finance`1",
                                "`Access to information`1",
                                "`access to infrastructure`1",
                                "`Access to Technology`1",    
                                "age1",   
                                "assets1",
                                "`cultural social norms`1",
                                "`demographic factors`1",
                                "`economic factors`1",
                                "education1",
                                "`farm characteristics`1",
                                "`farming experience`1",
                                "gender1",
                                "`Government Support`1",
                                "income1",
                                "owner1",
                                "`social capital`1",
                                "`TCCA-specific `1",
                                "`Hazard Experience`1",
                                "`Psychological factors`1")),
                 labels = rev(c("(Intercept)","ID_ART",
                                "Transformational",
                                "Access to Finance",
                                "Access to Information",
                                "Access to Infrastructure",
                                "Access to Technology",    
                                "Age",   
                                "Assets",
                                "Cultural-Social Norms",
                                "Demographic Factors",
                                "Economic Factors",
                                "Education",
                                "Farm Characteristics",
                                "Farming Experience",
                                "Gender",
                                "Government Support",
                                "Income",
                                "Owner",
                                "Social Capital",
                                "TCCA-Specific",
                                "Hazard Experience",
                                "Psychological Factors")))

CONF$Category<-factor(CONF$STD,
                 levels = rev(c("(Intercept)","ID_ART",
                                "Transformational",
                                "Access to finance",
                                "Access to information",
                                "Access to infrastructure",
                                "Access to Technology",    
                                "Age",   
                                "Assets",
                                "Cultural-Social norms",
                                "Demographic factors",
                                "Economic factors",
                                "Education",
                                "Farm Characteristics",
                                "Farming Experience",
                                "Gender",
                                "Government Support",
                                "Income",
                                "Owner",
                                "Social Capital",
                                "TCCA-specific",
                                "Hazard Experience",
                                "Psychological factors")),
                 labels = rev(c("(Intercept)","ID_ART",
                                "Adaptation Type",
                                "Access to finance",
                                "Knowledge and Information",
                                "Individual AC",
                                "Technology",    
                                "Individual AC",   
                                "AssetsIndividual AC",
                                "Institutional AC\nInformal Institutions",
                                "Individual AC",
                                "Individual AC",
                                "Individual AC",
                                "Individual AC",
                                "Individual AC",
                                "Individual AC",
                                "Institutional AC\nFormal Institutions",
                                "Individual AC",
                                "Individual AC",
                                "Institutional AC\nInformal Institutions",
                                "TCCA-specific",
                                "Hazard Experience",
                                "Psychological factors")))



CONF%>%
  filter(!STD%in%c("(Intercept)","ID_ART"))%>%
  ggplot(aes(x=STD, y=exp(Mean),color=color,group=Category)) +  # ,shape=names
  geom_errorbar(aes(ymin=exp(lower), ymax=exp(upper)), 
                width=.6,position = position_dodge(1)) +
  scale_color_manual(values=c("More Likely"="#117733",
                              "Less Likely"="#882255","Equally Likely"="black"))+
  coord_flip()+
  facet_grid(.~Coef,scales = "free")+
  geom_point(size=1.5,position = position_dodge(1))+
  annotate("rect",xmin = 1.5, xmax = 2.5, ymin = 0.003, ymax = 100,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 3.5, xmax = 4.5, ymin = 0.003, ymax = 100,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 5.5, xmax = 6.5, ymin = 0.003, ymax = 100,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 7.5, xmax = 8.5, ymin = 0.003, ymax = 100,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 9.5, xmax = 10.5, ymin = 0.003, ymax = 100,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 11.5, xmax = 12.5, ymin = 0.003, ymax = 100,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 13.5, xmax = 14.5, ymin = 0.003, ymax = 100,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 15.5, xmax = 16.5, ymin = 0.003, ymax = 100,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 17.5, xmax = 18.5, ymin = 0.003, ymax = 100,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 19.5, xmax = 20.5, ymin = 0.003, ymax = 100,
           alpha = .1,fill = "gray")+
  scale_y_continuous(trans="log10",expand = c(0, 0),
                     limits = c(0.005,150),
                     breaks = c(0.01,0.03,0.1,0.3,1.0,3.0,10.0,30.0,100),
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

# ggsave("IMAGES//RegionsDiff.png",dpi = 600,
#        width = 7.79*1.9,height = 4.03*1.8,units = "in")

#### glm ####
fit<-glm(TypeAdaptation~., #+Psychological,
        family = binomial(),
        data=FACTORS3[,-c(2:3)])
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
                 levels = rev(c("(Intercept)","ID_ART",
                                "STDAsia",
                                "STDEurope",
                                "STDLAC",
                                "STDN.America",
                                "STDOceania",
                                "`Access to finance`1",
                                "`Access to information`1",
                                "`access to infrastructure`1",
                                "`Access to Technology`1",    
                                "age1",   
                                "assets1",
                                "`cultural social norms`1",
                                "`demographic factors`1",
                                "`economic factors`1",
                                "education1",
                                "`farm characteristics`1",
                                "`farming experience`1",
                                "gender1",
                                "`Government Support`1",
                                "income1",
                                "owner1",
                                "`social capital`1",
                                "`TCCA-specific `1",
                                "`Hazard Experience`1",
                                "`Psychological factors`1")),
                 labels = rev(c("(Intercept)","ID_ART",
                                "Asia",
                                "Europe",
                                "LAC",
                                "N.America",
                                "Oceania",
                                "Access to Finance",
                                "Access to Information",
                                "Access to Infrastructure",
                                "Access to Technology",    
                                "Age",   
                                "Assets",
                                "Cultural-Social Norms",
                                "Demographic Factors",
                                "Economic Factors",
                                "Education",
                                "Farm Characteristics",
                                "Farming Experience",
                                "Gender",
                                "Government Support",
                                "Income",
                                "Owner",
                                "Social Capital",
                                "TCCA-Specific",
                                "Hazard Experience",
                                "Psychological Factors")))

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
  annotate("rect",xmin = 1.5, xmax = 2.5, ymin = 0.09, ymax = 20,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 3.5, xmax = 4.5, ymin = 0.09, ymax = 20,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 5.5, xmax = 6.5, ymin = 0.09, ymax = 20,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 7.5, xmax = 8.5, ymin = 0.09, ymax = 20,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 9.5, xmax = 10.5, ymin = 0.09, ymax = 20,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 11.5, xmax = 12.5, ymin = 0.09, ymax = 20,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 13.5, xmax = 14.5, ymin = 0.09, ymax = 20,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 15.5, xmax = 16.5, ymin = 0.09, ymax = 20,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 17.5, xmax = 18.5, ymin = 0.09, ymax = 20,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 19.5, xmax = 20.5, ymin = 0.09, ymax = 20,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 21.5, xmax = 22.5, ymin = 0.09, ymax = 20,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 23.5, xmax = 24.5, ymin = 0.09, ymax = 20,
           alpha = .1,fill = "gray")+
  scale_y_continuous(trans="log10",expand = c(0, 0),
                     limits = c(0.09,20),
                     breaks = c(0.1,0.3,1.0,3.0,10.0),
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

ggsave("IMAGES/TransIncrDiff.png",dpi = 600,
       width = 15,height = 15,units = "cm")

# Multilevel
library(lme4)

fit <- glmer(TypeAdaptation ~.+(1|ID_ART),
                data = FACTORS3[,-c(2:3)],
             family = binomial()) #,
              # control = glmerControl(
              #   optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))

(SUM=summary(fit))

# Showing Results as Image #
Coef<-SUM$coefficients[,1]
# Confidence Interval
conf=confint(fit,level = 0.90,method="Wald")
CI1<-data.frame(lower=round(conf[-1,1],4),
                mean=round(Coef,4),
                upper=round(conf[-1,2],4))
# SE
CI1$SE<-SUM$coefficients[,2]
# p-values
p <- SUM$coefficients[,4]
CI1$NAMES=row.names(CI1)
CI<-rbind(CI1)
CI<-CI%>%
  mutate(color=ifelse(mean>0&lower>0&upper>0,"Green",
                      ifelse(mean<0&lower<0&upper<0,"Red","Black")))

CI$NAMES<-factor(CI$NAMES,
                 levels = rev(c("(Intercept)","ID_ART",
                                "STDAsia",
                                "STDEurope",
                                "STDLAC",
                                "STDN.America",
                                "STDOceania",
                                "`Access to finance`1",
                                "`Access to information`1",
                                "`access to infrastructure`1",
                                "`Access to Technology`1",    
                                "age1",   
                                "assets1",
                                "`cultural social norms`1",
                                "`demographic factors`1",
                                "`economic factors`1",
                                "education1",
                                "`farm characteristics`1",
                                "`farming experience`1",
                                "gender1",
                                "`Government Support`1",
                                "income1",
                                "owner1",
                                "`social capital`1",
                                "`TCCA-specific `1",
                                "`Hazard Experience`1",
                                "`Psychological factors`1")),
                 labels = rev(c("(Intercept)","ID_ART",
                                "Asia",
                                "Europe",
                                "LAC",
                                "N.America",
                                "Oceania",
                                "Access to finance",
                                "Access to information",
                                "Access to infrastructure",
                                "Access to Technology",    
                                "Age",   
                                "Assets",
                                "Cultural-Social norms",
                                "Demographic factors",
                                "Economic factors",
                                "Education",
                                "Farm Characteristics",
                                "Farming Experience",
                                "Gender",
                                "Government Support",
                                "Income",
                                "Owner",
                                "Social Capital",
                                "TCCA-specific",
                                "Hazard Experience",
                                "Psychological factors")))
# row.names(CI)<-NULL

#### Plotting the result ####
CI%>%
  filter(!str_detect(NAMES,"Intercept"),NAMES!="ID_ART")%>%
  ggplot(aes(x=NAMES, y=exp(mean),color=color)) +  # ,shape=names
  geom_errorbar(aes(ymin=exp(lower), ymax=exp(upper)), 
                width=.6,position = position_dodge(1)) +
  scale_color_manual(values=c("Green"="#117733","Red"="#882255","Black"="black"))+
  coord_flip()+
  geom_point(size=1.5,position = position_dodge(1))+
  annotate("rect",xmin = 1.5, xmax = 2.5, ymin = 0.0003, ymax = 3000,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 3.5, xmax = 4.5, ymin = 0.0003, ymax = 3000,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 5.5, xmax = 6.5, ymin = 0.0003, ymax = 3000,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 7.5, xmax = 8.5, ymin = 0.0003, ymax = 3000,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 9.5, xmax = 10.5, ymin = 0.0003, ymax = 3000,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 11.5, xmax = 12.5, ymin = 0.0003, ymax = 3000,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 13.5, xmax = 14.5, ymin = 0.0003, ymax = 3000,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 15.5, xmax = 16.5, ymin = 0.0003, ymax = 3000,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 17.5, xmax = 18.5, ymin = 0.0003, ymax = 3000,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 19.5, xmax = 20.5, ymin = 0.0003, ymax = 3000,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 21.5, xmax = 22.5, ymin = 0.0003, ymax = 3000,
           alpha = .1,fill = "gray")+
  scale_y_continuous(trans="log10",expand = c(0, 0),
                     # limits = c(0.0003,3000),
                     # breaks = c(0.001,0.003,0.01,0.03,0.1,0.3,1.0,3.0,10.0,30,100,300,1000),
                     labels = function(x)ifelse(x<1,format(x,digits = 4),
                                                round(x,digits = 1))) +
  geom_hline(yintercept = 1, linetype="dashed")+
  theme(panel.spacing = unit(1, "lines"),
        text = element_text(size = 14),
        axis.text.x = element_text(angle = 60,hjust = 0.5,vjust = 0.7),
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
        plot.caption = element_text(hjust = 0.5,size = 14),
        legend.position = "bottom")+
  labs(y = "Odds Ratio")

# ggsave("IMAGES//TransIncrDiff.png",dpi = 600,
#        width = 15,height = 20,units = "cm")

#### Using MCMC: Transformational ####

# FACTORS3<-FACTORS3[FACTORS3$STD=="",]

STDS=paste0(unique(FACTORS3$TypeAdaptation))
VARS=c("(Intercept)","ID_ART",
       "STDAsia",
       "STDEurope",
       "STDLAC",
       "STDN.America",
       "STDOceania",
       "`Access to finance`1",
       "`Access to information`1","`access to infrastructure`1",
       "`Access to Technology`1","age1",
       "assets1","`cultural social norms`1",
       "`demographic factors`1","`economic factors`1",
       "education1","`farm characteristics`1",
       "gender1","`farming experience`1",
       "`Government Support`1","income1",
       "owner1","`social capital`1",
       "`TCCA-specific `1",
       "`Hazard Experience`1","`Psychological factors`1")

VARS_NAMES<-data.frame(
  NAMES = c("(Intercept)",
            paste(rep(VARS[-1],times=length(STDS[-1])),sep = "")),
  VARIABLES = c(rep(VARS[1],1),rep(VARS[-1],times=length(STDS[-1]))),
  focus=c(STDS[-1],rep(STDS[-1],each=length(VARS[-1]))))

row.names(VARS_NAMES)<-VARS_NAMES$NAMES

library(doParallel)
library(MCMCglmm)

k <- length(levels(FACTORS3$TypeAdaptation))
I <- diag(k-1) 
J <- matrix(rep(1, (k-1)^2), c(k-1, k-1))

R = list(fix=1, V=(I + J), nu=0.002, n = k)
G = list(G1 = list(V = diag(k-1),nu=0.002, n = k-1))#,
# G2=list(V = diag(1),nu=0.002, n = 1))
# G = list(G1 = list(V = diag(k-1),nu=0.002, n = k-1))

detectCores()

cls<-makeCluster(10)
registerDoParallel(cls)

(T0<-Sys.time())

bla=foreach(i=1:2000,.combine='cbind',
            .packages = c('MCMCglmm','tidyverse'))%dopar%{
              Adaptation<-try(MCMCglmm(TypeAdaptation~STD+`Access to finance`+
                                                  `Access to information`+`access to infrastructure`+
                                                  `Access to Technology`+`age`+
                                                  `assets`+`cultural social norms`+
                                                  `demographic factors`+`economic factors`+
                                                  `education`+`farm characteristics`+
                                                  `gender`+`farming experience`+
                                                  `Government Support`+`income`+
                                                  `owner`+`social capital`+
                                                  `TCCA-specific `+
                                                  `Hazard Experience`+`Psychological factors`,
                                       random = ~ idh(1):ID_ART,
                                       rcov = ~ us(1):units,
                                       prior = list(R=R,G=G),
                                       burnin = 15000,
                                       nitt = 30000,
                                       thin=50,
                                       singular.ok=FALSE,
                                       data = as.data.frame(FACTORS3),
                                       family = "categorical"))
              if(class(Adaptation)!="try-error"){
                bla=summary(Adaptation)
                return(bla$solutions)
              }
            }

T1<-Sys.time() 
T1-T0

stopCluster(cls)

#### MCMC Logit: Using the CI ####

SOLS=as.data.frame(bla)
EMO_MEAN=data.frame("post.mean"=bla[,which(names(SOLS)=="post.mean")],
                    "u-95% CI"=bla[,which(names(SOLS)=="u-95% CI")],
                    "l-95% CI"=bla[,which(names(SOLS)=="l-95% CI")])
EMO_MEAN$NAMES=rownames(EMO_MEAN)
EMO_MEAN<-EMO_MEAN%>%
  gather(key = "variable", value = "value",-NAMES)%>%
  mutate(variable=str_remove_all(variable,"[0-9]"))%>%
  mutate(variable=str_remove_all(variable,"\\."))
  
EMO_MEAN<-EMO_MEAN%>%
  group_by(variable,NAMES)%>%
  summarise(MEAN=median(value))%>%
  spread(key=variable,value = MEAN)

colnames(EMO_MEAN)<-c("NAMES","l-95% CI","post.mean","u-95% CI")

FOCUS2<-cbind(EMO_MEAN,VARS_NAMES[EMO_MEAN$NAMES,-1])

FOCUS2$VARIABLES<-factor(FOCUS2$VARIABLES,
                         levels = rev(c("(Intercept)","ID_ART",
                                        "TypeAdaptation1",
                                        "STDAsia",
                                        "STDEurope",
                                        "STDLAC",
                                        "STDN.America",
                                        "STDOceania",
                                        "`Access to finance`1",
                                        "`Access to information`1",
                                        "`access to infrastructure`1",
                                        "`Access to Technology`1",    
                                        "age1",   
                                        "assets1",
                                        "`cultural social norms`1",
                                        "`demographic factors`1",
                                        "`economic factors`1",
                                        "education1",
                                        "`farm characteristics`1",
                                        "`farming experience`1",
                                        "gender1",
                                        "`Government Support`1",
                                        "income1",
                                        "owner1",
                                        "`social capital`1",
                                        "`TCCA-specific `1",
                                        "`Hazard Experience`1",
                                        "`Psychological factors`1")),
                         labels = rev(c("(Intercept)","ID_ART",
                                        "Transformative",
                                        "Asia",
                                        "Europe",
                                        "LAC",
                                        "N.America",
                                        "Oceania",
                                        "Access to finance",
                                        "Access to information",
                                        "Access to infrastructure",
                                        "Access to Technology",    
                                        "Age",   
                                        "Assets",
                                        "Cultural-Social norms",
                                        "Demographic factors",
                                        "Economic factors",
                                        "Education",
                                        "Farm Characteristics",
                                        "Farming Experience",
                                        "Gender",
                                        "Government Support",
                                        "Income",
                                        "Owner",
                                        "Social Capital",
                                        "TCCA-specific",
                                        "Hazard Experience",
                                        "Psychological factors")))

FOCUS2=EMO_MEAN
FOCUS2<-FOCUS2%>%
  group_by(NAMES)%>%
  mutate(color=ifelse(`l-95% CI`>0& `u-95% CI`>0,"Green",
                      ifelse(`l-95% CI`<0& `u-95% CI`<0,"Red","Black")))

FOCUS2<-FOCUS2%>%
  mutate(focus="Transformative")

FOCUS2%>%
  filter(NAMES!="(Intercept)")%>%
  ggplot(aes(x=NAMES, y=exp(post.mean),color=color)) +  # ,shape=names
  geom_errorbar(aes(ymin=exp(`l-95% CI`), ymax=exp(`u-95% CI`)),
                width=.6,position = position_dodge(1)) +
  coord_flip()+
  geom_point(size=3,position = position_dodge(1))+
  geom_hline(yintercept = 1, linetype="dashed")+
  geom_vline(aes(xintercept=0.4),size=1.2)+
  facet_grid(.~focus)+
  scale_color_manual(values=c("Green"="#117733","Red"="#882255","Black"="black"))+
  geom_point(size=1.5,position = position_dodge(1))+
  # annotate("rect",xmin = 1.5, xmax = 2.5, ymin = -60, ymax = 60,
  #          alpha = .1,fill = "gray")+
  # annotate("rect",xmin = 3.5, xmax = 4.5, ymin = -60, ymax = 60,
  #          alpha = .1,fill = "gray")+
  # annotate("rect",xmin = 5.5, xmax = 6.5, ymin = -60, ymax = 60,
  #          alpha = .1,fill = "gray")+
  # annotate("rect",xmin = 7.5, xmax = 8.5, ymin = -60, ymax = 60,
  #          alpha = .1,fill = "gray")+
  # annotate("rect",xmin = 9.5, xmax = 10.5, ymin = -60, ymax = 60,
  #          alpha = .1,fill = "gray")+
  # annotate("rect",xmin = 11.5, xmax = 12.5, ymin = -60, ymax = 60,
  #          alpha = .1,fill = "gray")+
  # annotate("rect",xmin = 13.5, xmax = 14.5, ymin = -60, ymax = 60,
  #          alpha = .1,fill = "gray")+
  # annotate("rect",xmin = 15.5, xmax = 16.5, ymin = -60, ymax = 60,
  #          alpha = .1,fill = "gray")+
  # scale_y_continuous(expand = c(0, 0),limits = c(-60,60)) +
scale_y_continuous(trans="log10",expand = c(0, 0))+#,
theme(panel.spacing = unit(1, "lines"),
        axis.title.x = element_blank(),
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
  guides(color=guide_legend(title = "Emotion",reverse = TRUE))

#### MCMC Logit: Using the Boxplots ####

SOLS=as.data.frame(bla)
EMO_MEAN=data.frame("post.mean"=bla[,which(names(SOLS)=="post.mean")])
EMO_MEAN$NAMES=rownames(EMO_MEAN)
EMO_MEAN<-EMO_MEAN%>%
  gather("post.mean","value",contains("post.mean"))

FOCUS2<-cbind(EMO_MEAN,VARS_NAMES[EMO_MEAN$NAMES,-1])

FOCUS2$VARIABLES<-factor(FOCUS2$VARIABLES,
                         levels = rev(c("(Intercept)","ID_ART",
                                        "TypeAdaptation1",
                                        "STDAsia",
                                        "STDEurope",
                                        "STDLAC",
                                        "STDN.America",
                                        "STDOceania",
                                        "`Access to finance`1",
                                        "`Access to information`1",
                                        "`access to infrastructure`1",
                                        "`Access to Technology`1",    
                                        "age1",   
                                        "assets1",
                                        "`cultural social norms`1",
                                        "`demographic factors`1",
                                        "`economic factors`1",
                                        "education1",
                                        "`farm characteristics`1",
                                        "`farming experience`1",
                                        "gender1",
                                        "`Government Support`1",
                                        "income1",
                                        "owner1",
                                        "`social capital`1",
                                        "`TCCA-specific `1",
                                        "`Hazard Experience`1",
                                        "`Psychological factors`1")),
                         labels = rev(c("(Intercept)","ID_ART",
                                        "Transformative",
                                        "Asia",
                                        "Europe",
                                        "LAC",
                                        "N.America",
                                        "Oceania",
                                        "Access to finance",
                                        "Access to information",
                                        "Access to infrastructure",
                                        "Access to Technology",    
                                        "Age",   
                                        "Assets",
                                        "Cultural-Social norms",
                                        "Demographic factors",
                                        "Economic factors",
                                        "Education",
                                        "Farm Characteristics",
                                        "Farming Experience",
                                        "Gender",
                                        "Government Support",
                                        "Income",
                                        "Owner",
                                        "Social Capital",
                                        "TCCA-specific",
                                        "Hazard Experience",
                                        "Psychological factors")))

FOCUS2<-FOCUS2%>%
  group_by(VARIABLES,focus)%>%
  mutate(Q1=quantile(value)[2],Q2=quantile(value)[3],Q3=quantile(value)[4])%>%
  mutate(color=ifelse(Q1>0&Q3>0,"More Likely",
                      ifelse(Q1<0&Q3<0,"Less Likely","Equally Likely")))

FOCUS2$color=factor(FOCUS2$color,
                    level=c("More Likely","Equally Likely","Less Likely"))

FOCUS2<-FOCUS2%>%
  mutate(focus="Transformative")

FOCUS2%>%
  filter(VARIABLES!="(Intercept)")%>%
  ggplot(aes(x=VARIABLES, y=exp(value),color=color)) +  # ,shape=names
  coord_flip()+
  scale_colour_brewer(type= "qual",palette = "Dark2")+
  geom_boxplot(position = position_dodge(1),outlier.shape = NA)+
  stat_summary(aes(group=focus),
               position = position_dodge(1),
               fun=mean, colour="darkred",size=0.6, 
               shape=18, show.legend=FALSE)+
  geom_hline(yintercept = 1, linetype="dashed")+
  geom_vline(aes(xintercept=0.4),size=1.2)+
  facet_grid(.~focus)+
  scale_color_manual(values=c("More Likely"="#117733",
                              "Less Likely"="#882255",
                              "Equally Likely"="black"))+
  annotate("rect",xmin = 1.5, xmax = 2.5, ymin = -25, ymax = 25,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 3.5, xmax = 4.5, ymin = -25, ymax = 25,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 5.5, xmax = 6.5, ymin = -25, ymax = 25,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 7.5, xmax = 8.5, ymin = -25, ymax = 25,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 9.5, xmax = 10.5, ymin = -25, ymax = 25,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 11.5, xmax = 12.5, ymin = -25, ymax = 25,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 13.5, xmax = 14.5, ymin = -25, ymax = 25,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 15.5, xmax = 16.5, ymin = -25, ymax = 25,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 17.5, xmax = 18.5, ymin = -25, ymax = 25,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 19.5, xmax = 20.5, ymin = -25, ymax = 25,
           alpha = .1,fill = "gray")+
  scale_y_continuous(trans="log10",expand = c(0, 0))+#,
                     # limits = c(0.001,100),
                     # breaks = c(0.001,0.003,0.01,0.03,0.1,0.3,1.0,3.0,10.0,30.0,100),
                     # labels = function(x)ifelse(x<1,format(x,digits = 4),
                     #                            format(x,digits=1))) +
  theme(panel.spacing = unit(1, "lines"),
        text = element_text(size = 11),
        axis.text.x = element_text(angle = 60,hjust = 1,vjust = 1),
        strip.text = element_text(size = 13),
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
  labs(y = "Odds Ratio")+
  guides(color=guide_legend("Likelihood"))

# ggsave("PROCESSED/RegionsDiff_Bayesian_byArt.png",dpi = 600,
#        width = 7.79*2,height = 4.03*2,units = "in")

#### Using MCMC: Regions ####

# FACTORS3<-FACTORS3[FACTORS3$STD=="",]

STDS=paste0("STD.",levels(FACTORS3$STD))
VARS=c("(Intercept)","ID_ART",
       "TypeAdaptation1",
       "Asia",
       "Europe",
       "LAC",
       "N.America",
       "Oceania",
       "`Access to finance`1",
       "`Access to information`1","`access to infrastructure`1",
       "`Access to Technology`1","age1",
       "assets1","`cultural social norms`1",
       "`demographic factors`1","`economic factors`1",
       "education1","`farm characteristics`1",
       "gender1","`farming experience`1",
       "`Government Support`1","income1",
       "owner1","`social capital`1",
       "`TCCA-specific `1",
       "`Hazard Experience`1","`Psychological factors`1")

VARS_NAMES<-data.frame(
  NAMES = c(rep(paste0("trait",STDS[-1]),each=length(1)),
            paste(rep(paste0("trait",STDS[-1]),each=length(VARS[-1])),":",
                  rep(VARS[-1],times=length(STDS[-1])),sep = "")),
  VARIABLES = c(rep(VARS[1],5),rep(VARS[-1],times=length(STDS[-1]))),
  focus=c(STDS[-1],rep(STDS[-1],each=length(VARS[-1]))))

row.names(VARS_NAMES)<-VARS_NAMES$NAMES

library(doParallel)
library(MCMCglmm)

k <- length(levels(FACTORS3$STD))
I <- diag(k-1)
J <- matrix(rep(1, (k-1)^2), c(k-1, k-1))

R = list(fix=1, V=1/k*(I + J), nu=0.002, n = k)
# G = list(G1 = list(V = diag(21),nu=0.002, n = 21), # length of covariates + 1
#          G2 = list(V = diag(k-1),nu=0.002, n = k-1))
G = list(G1 = list(V = diag(k-1),nu=0.002, n = k-1))

detectCores()

cls<-makeCluster(10)
registerDoParallel(cls)

(T0<-Sys.time())

bla=foreach(i=1:100,.combine='cbind',
            .packages = c('MCMCglmm','tidyverse'))%dopar%{
              Adaptation<-try(MCMCglmm(STD~trait-1+
                               trait:(TypeAdaptation+`Access to finance`+
                              `Access to information`+`access to infrastructure`+
                              `Access to Technology`+`age`+
                              `assets`+`cultural social norms`+
                              `demographic factors`+`economic factors`+
                              `education`+`farm characteristics`+
                              `gender`+`farming experience`+
                              `Government Support`+`income`+
                              `owner`+`social capital`+
                              `TCCA-specific `+
                              `Hazard Experience`+`Psychological factors`),
                                       random = ~ idh(trait):ID_ART,
                                       rcov = ~ us(trait):units,
                                       prior = list(R=R,G=G),
                                       burnin = 15000,
                                       nitt = 30000,
                                       thin=50,
                                       singular.ok=FALSE,
                                       data = as.data.frame(FACTORS3),
                                       family = "categorical"))
              if(class(Adaptation)!="try-error"){
                bla=summary(Adaptation)
                return(bla$solutions)
              }
            }

T1<-Sys.time() 
T1-T0

stopCluster(cls)

# save(bla,file = "PROCESSED/MCMC_REGIONS_2000it_20230921.RData")

#### MCMC Multinomial: Using the CI ####

SOLS=as.data.frame(bla)
EMO_MEAN=data.frame("post.mean"=bla[,which(names(SOLS)=="post.mean")],
                    "u-95% CI"=bla[,which(names(SOLS)=="u-95% CI")],
                    "l-95% CI"=bla[,which(names(SOLS)=="l-95% CI")])
EMO_MEAN$NAMES=rownames(EMO_MEAN)
EMO_MEAN<-EMO_MEAN%>%
  gather(key = "variable", value = "value",-NAMES)%>%
  mutate(variable=str_remove_all(variable,"[0-9]"))%>%
  mutate(variable=str_remove_all(variable,"\\."))

EMO_MEAN<-EMO_MEAN%>%
  group_by(variable,NAMES)%>%
  summarise(MEAN=median(value))%>%
  spread(key=variable,value = MEAN)

colnames(EMO_MEAN)<-c("NAMES","l-95% CI","post.mean","u-95% CI")

FOCUS2<-cbind(EMO_MEAN,VARS_NAMES[EMO_MEAN$NAMES,-1])

FOCUS2$VARIABLES<-factor(FOCUS2$VARIABLES,
                         levels = rev(c("(Intercept)","ID_ART",
                                        "TypeAdaptation1",
                                        "STDAsia",
                                        "STDEurope",
                                        "STDLAC",
                                        "STDN.America",
                                        "STDOceania",
                                        "`Access to finance`1",
                                        "`Access to information`1",
                                        "`access to infrastructure`1",
                                        "`Access to Technology`1",    
                                        "age1",   
                                        "assets1",
                                        "`cultural social norms`1",
                                        "`demographic factors`1",
                                        "`economic factors`1",
                                        "education1",
                                        "`farm characteristics`1",
                                        "`farming experience`1",
                                        "gender1",
                                        "`Government Support`1",
                                        "income1",
                                        "owner1",
                                        "`social capital`1",
                                        "`TCCA-specific `1",
                                        "`Hazard Experience`1",
                                        "`Psychological factors`1")),
                         labels = rev(c("(Intercept)","ID_ART",
                                        "Transformative",
                                        "Asia",
                                        "Europe",
                                        "LAC",
                                        "N.America",
                                        "Oceania",
                                        "Access to finance",
                                        "Access to information",
                                        "Access to infrastructure",
                                        "Access to Technology",    
                                        "Age",   
                                        "Assets",
                                        "Cultural-Social norms",
                                        "Demographic factors",
                                        "Economic factors",
                                        "Education",
                                        "Farm Characteristics",
                                        "Farming Experience",
                                        "Gender",
                                        "Government Support",
                                        "Income",
                                        "Owner",
                                        "Social Capital",
                                        "TCCA-specific",
                                        "Hazard Experience",
                                        "Psychological factors")))

FOCUS2<-FOCUS2%>%
  group_by(NAMES)%>%
  mutate(color=ifelse(`l-95% CI`>0& `u-95% CI`>0,"More Likely",
                      ifelse(`l-95% CI`<0& `u-95% CI`<0,"Less Likely","Equally Likely")))

FOCUS2<-FOCUS2%>%
  mutate(focus=ifelse(focus=="STD.Asia","Asia",
                      ifelse(focus=="STD.Europe","Europe",
                             ifelse(focus=="STD.LAC","LAC",
                                    ifelse(focus=="STD.N.America","N. America",
                                           "Oceania")))))

FOCUS2%>%
  ggplot(aes(x=VARIABLES, y=exp(post.mean),color=color)) +  # ,shape=names
  geom_errorbar(aes(ymin=exp(`l-95% CI`), ymax=exp(`u-95% CI`)),
                width=.6,position = position_dodge(1)) +
  coord_flip()+
  geom_point(size=3,position = position_dodge(1))+
  geom_hline(yintercept = 1, linetype="dashed")+
  geom_vline(aes(xintercept=0.4),size=1.2)+
  facet_grid(.~focus)+
  scale_color_manual(values=c("More Likely"="#117733",
                              "Less Likely"="#882255",
                              "Equally Likely"="black"))+
  annotate("rect",xmin = 1.5, xmax = 2.5, ymin = -25, ymax = 25,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 3.5, xmax = 4.5, ymin = -25, ymax = 25,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 5.5, xmax = 6.5, ymin = -25, ymax = 25,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 7.5, xmax = 8.5, ymin = -25, ymax = 25,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 9.5, xmax = 10.5, ymin = -25, ymax = 25,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 11.5, xmax = 12.5, ymin = -25, ymax = 25,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 13.5, xmax = 14.5, ymin = -25, ymax = 25,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 15.5, xmax = 16.5, ymin = -25, ymax = 25,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 17.5, xmax = 18.5, ymin = -25, ymax = 25,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 19.5, xmax = 20.5, ymin = -25, ymax = 25,
           alpha = .1,fill = "gray")+
  scale_y_continuous(trans="log10",expand = c(0, 0),
  limits = c(0.00001,1000),
  breaks = c(0.001,0.003,0.01,0.03,0.1,0.3,1.0,3.0,10.0,30.0,100),
  labels = function(x)ifelse(x<1,format(x,digits = 4),
                             format(x,digits=1))) +
  theme(panel.spacing = unit(1, "lines"),
        text = element_text(size = 11),
        axis.text.x = element_text(angle = 60,hjust = 1,vjust = 1),
        strip.text = element_text(size = 13),
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
  labs(y = "Odds Ratio")+
  guides(color=guide_legend("Likelihood"))


#### MCMC Multinomial - Boxplots ####

SOLS=as.data.frame(bla)
EMO_MEAN=data.frame("post.mean"=bla[,which(names(SOLS)=="post.mean")])
EMO_MEAN$NAMES=rownames(EMO_MEAN)
EMO_MEAN<-EMO_MEAN%>%
  gather("post.mean","value",contains("post.mean"))

FOCUS2<-cbind(EMO_MEAN,VARS_NAMES[EMO_MEAN$NAMES,-1])

FOCUS2$VARIABLES<-factor(FOCUS2$VARIABLES,
                         levels = rev(c("(Intercept)","ID_ART",
                                        "TypeAdaptation1",
                                        "STDAsia",
                                        "STDEurope",
                                        "STDLAC",
                                        "STDN.America",
                                        "STDOceania",
                                        "`Access to finance`1",
                                        "`Access to information`1",
                                        "`access to infrastructure`1",
                                        "`Access to Technology`1",    
                                        "age1",   
                                        "assets1",
                                        "`cultural social norms`1",
                                        "`demographic factors`1",
                                        "`economic factors`1",
                                        "education1",
                                        "`farm characteristics`1",
                                        "`farming experience`1",
                                        "gender1",
                                        "`Government Support`1",
                                        "income1",
                                        "owner1",
                                        "`social capital`1",
                                        "`TCCA-specific `1",
                                        "`Hazard Experience`1",
                                        "`Psychological factors`1")),
                         labels = rev(c("(Intercept)","ID_ART",
                                        "Transformative",
                                        "Asia",
                                        "Europe",
                                        "LAC",
                                        "N.America",
                                        "Oceania",
                                        "Access to finance",
                                        "Access to information",
                                        "Access to infrastructure",
                                        "Access to Technology",    
                                        "Age",   
                                        "Assets",
                                        "Cultural-Social norms",
                                        "Demographic factors",
                                        "Economic factors",
                                        "Education",
                                        "Farm Characteristics",
                                        "Farming Experience",
                                        "Gender",
                                        "Government Support",
                                        "Income",
                                        "Owner",
                                        "Social Capital",
                                        "TCCA-specific",
                                        "Hazard Experience",
                                        "Psychological factors")))

FOCUS2<-FOCUS2%>%
  group_by(VARIABLES,focus)%>%
  mutate(Q1=quantile(value)[2],Q2=quantile(value)[3],Q3=quantile(value)[4])%>%
  mutate(color=ifelse(Q1>0&Q3>0,"More Likely",
                      ifelse(Q1<0&Q3<0,"Less Likely","Equally Likely")))

FOCUS2$color=factor(FOCUS2$color,
                    level=c("More Likely","Equally Likely","Less Likely"))

FOCUS2<-FOCUS2%>%
  mutate(focus=ifelse(focus=="STD.Asia","Asia",
                      ifelse(focus=="STD.Europe","Europe",
                             ifelse(focus=="STD.LAC","LAC",
                                    ifelse(focus=="STD.N.America","N. America",
                                           "Oceania")))))

FOCUS2%>%
  filter(VARIABLES!="(Intercept)")%>%
  ggplot(aes(x=VARIABLES, y=exp(value),color=color)) +  # ,shape=names
  coord_flip()+
  scale_colour_brewer(type= "qual",palette = "Dark2")+
  geom_boxplot(position = position_dodge(1),outlier.shape = NA)+
  stat_summary(aes(group=focus),
               position = position_dodge(1),
               fun=mean, colour="darkred",size=0.6, 
               shape=18, show.legend=FALSE)+
  geom_hline(yintercept = 1, linetype="dashed")+
  geom_vline(aes(xintercept=0.4),size=1.2)+
  facet_grid(.~focus)+
  scale_color_manual(values=c("More Likely"="#117733",
                              "Less Likely"="#882255",
                              "Equally Likely"="black"))+
  annotate("rect",xmin = 1.5, xmax = 2.5, ymin = -25, ymax = 25,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 3.5, xmax = 4.5, ymin = -25, ymax = 25,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 5.5, xmax = 6.5, ymin = -25, ymax = 25,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 7.5, xmax = 8.5, ymin = -25, ymax = 25,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 9.5, xmax = 10.5, ymin = -25, ymax = 25,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 11.5, xmax = 12.5, ymin = -25, ymax = 25,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 13.5, xmax = 14.5, ymin = -25, ymax = 25,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 15.5, xmax = 16.5, ymin = -25, ymax = 25,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 17.5, xmax = 18.5, ymin = -25, ymax = 25,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 19.5, xmax = 20.5, ymin = -25, ymax = 25,
           alpha = .1,fill = "gray")+
  scale_y_continuous(trans="log10",expand = c(0, 0),
                     limits = c(0.0001,300),
                     breaks = c(0.0001,0.003,0.01,0.03,0.1,0.3,1.0,3.0,10.0,30.0,100,300),
                     labels = function(x)ifelse(x<1,format(x,digits = 4),
                                                format(x,digits=1))) +
  theme(panel.spacing = unit(1, "lines"),
        text = element_text(size = 11),
        axis.text.x = element_text(angle = 60,hjust = 1,vjust = 1),
        strip.text = element_text(size = 13),
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
  labs(y = "Odds Ratio")+
  guides(color=guide_legend("Likelihood"))

# ggsave("PROCESSED/RegionsDiff_Bayesian_byArt_2.png",dpi = 600,
#        width = 17*2,height = 12*2,units = "cm")

# save(FOCUS2, file = "PROCESSED/RData/RegionsDiff_Bayesian_byArt.RData")


SOLS=as.data.frame(summary(Adaptation)$solutions)
SOLS$NAMES=rownames(SOLS)
FOCUS2<-cbind(SOLS,VARS_NAMES[SOLS$NAMES,-1])

FOCUS2$VARIABLES<-factor(FOCUS2$VARIABLES,
                         levels = rev(c("(Intercept)","ID_ART",
                                        "TypeAdaptation1","STDAsia",
                                        "STDEurope","STDLAC",
                                        "STDN.America","STDOceania",
                                        "`Access to finance`1","`Access to information`1",
                                        "`access to infrastructure`1","`Access to Technology`1",    
                                        "age1","assets1",
                                        "`cultural social norms`1","`demographic factors`1",
                                        "`economic factors`1","education1",
                                        "`farm characteristics`1",                                        "`farming experience`1",
                                        "gender1","`Government Support`1",
                                        "income1","owner1",
                                        "`social capital`1",                                        "`TCCA-specific `1",
                                        "`Hazard Experience`1",
                                        "`Psychological factors`1")),
                         labels = rev(c("(Intercept)","ID_ART",
                                        "Transformative",
                                        "Asia",
                                        "Europe",
                                        "LAC",
                                        "N.America",
                                        "Oceania",
                                        "Access to finance",
                                        "Access to information",
                                        "Access to infrastructure",
                                        "Access to Technology",    
                                        "Age",   
                                        "Assets",
                                        "Cultural-Social norms",
                                        "Demographic factors",
                                        "Economic factors",
                                        "Education",
                                        "Farm Characteristics",
                                        "Farming Experience",
                                        "Gender",
                                        "Government Support",
                                        "Income",
                                        "Owner",
                                        "Social Capital",
                                        "TCCA-specific",
                                        "Hazard Experience",
                                        "Psychological factors")))


FOCUS2<-FOCUS2%>%
  group_by(VARIABLES,focus)%>%
  mutate(color=ifelse(`l-95% CI`>0& `u-95% CI`>0,"More Likely",
                      ifelse(`l-95% CI`<0& `u-95% CI`<0,"Less Likely","Equally Likely")))

FOCUS2%>%
  filter(!VARIABLES%in%c("(Intercept)"))%>%
  ggplot(aes(x=VARIABLES, y=exp(post.mean),color=color)) +  # ,shape=names
  geom_errorbar(aes(ymin=exp(`l-95% CI`), ymax=exp(`u-95% CI`)), 
                width=.6,position = position_dodge(1)) +
  scale_color_manual(values=c("More Likely"="#117733",
                              "Less Likely"="#882255",
                              "Equally Likely"="black"))+
  coord_flip()+
  facet_grid(.~focus)+
  geom_point(size=1.5,position = position_dodge(1))+
  annotate("rect",xmin = 1.5, xmax = 2.5, ymin = 0.01, ymax = 100,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 3.5, xmax = 4.5, ymin = 0.01, ymax = 100,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 5.5, xmax = 6.5, ymin = 0.01, ymax = 100,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 7.5, xmax = 8.5, ymin = 0.01, ymax = 100,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 9.5, xmax = 10.5, ymin = 0.01, ymax = 100,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 11.5, xmax = 12.5, ymin = 0.01, ymax = 100,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 13.5, xmax = 14.5, ymin = 0.01, ymax = 100,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 15.5, xmax = 16.5, ymin = 0.01, ymax = 100,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 17.5, xmax = 18.5, ymin = 0.01, ymax = 100,
           alpha = .1,fill = "gray")+
  annotate("rect",xmin = 19.5, xmax = 20.5, ymin = 0.01, ymax = 100,
           alpha = .1,fill = "gray")+
  scale_y_continuous(trans="log10",expand = c(0, 0),
                     # limits = c(0.01,100),
                     # breaks = c(0.01,0.03,0.1,0.3,1.0,3.0,10.0,30.0,100),
                     labels = function(x)ifelse(x<1,format(x,digits = 4),
                                                format(x,digits=1))) +
  geom_hline(yintercept = 1, linetype="dashed")+
  theme(panel.spacing = unit(1, "lines"),
        text = element_text(size = 11),
        axis.text.x = element_text(angle = 60,hjust = 1,vjust = 1),
        strip.text = element_text(size = 13),
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
  guides(color="none")+
  labs(y = "Odds Ratio")








