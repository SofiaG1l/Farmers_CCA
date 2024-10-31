
##################################
# 
# Author: Dr. Sofia Gil-Clavel
# 
# Last update: October 1st, 2024.
# 
# Description: Code to draw the map in: 
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
#   - ggplot2 (3.5.1)
#   - egg (0.4.5)
#   - sf (1.0-18)
#   - tmap (3.3-4)
#   - cowplot (1.1.3)
# 
##################################

library(tidyverse)
library(ggplot2)
library(egg) 
# Following packages are for maps
library(sf)
library(tmap)
library(cowplot)

## Cleaning the Environment
rm(list=ls())
gc()

#### Opening the Data ####
#   "2_Farmers_NetworkData.csv" stored in DANS:
#   Gil-Clavel, Sofia; Filatova, Tatiana, 2024, "Interrelated Climate Change 
#   Adaptation Measures and Factors", https://doi.org/10.17026/SS/PYZCXK, 
#   DANS Data Station Social Sciences and Humanities, DRAFT VERSION; 
#   2_Farmers_NetworkData.csv [fileName] 

DT1=read.csv("2_Farmers_NetworkData.csv")
DT1<-DT1%>%filter(period=="<Aug,2022")
DT1<-DT1[,c("dc.identifier",
                    "affiliation_ISO2","StudiedPlace_ISO2",
                    "affiliation_Continent","StudiedPlace_Continent")]
DT1<-DT1%>%distinct()

# To extract values between single quotes:

AFF=data.frame("Country_Code"="US","Affiliation"=0)
row.names(AFF)<-AFF$Country_Code

N=dim(DT1)[1]

for(II in 1:N){
  # ISO2 CODE
  la=DT1$affiliation_ISO2[II]
  la=regmatches(la,gregexpr("'[^']*[^']*'",la))
  la=la[[1]]
  la=gsub("'","",la)
  
  N2=length(la)
  if(N2>0){
    for(j in 1:N2){
      
      if(la[j]%in%row.names(AFF)){
        AFF[la[j],"Affiliation"]=as.integer(AFF[la[j],"Affiliation"])+1
      }
      else{
        AFF=rbind(AFF,c(la[j],1))
        row.names(AFF)<-AFF$Country_Code
      }
      
    }
  }
}


STD=data.frame("Country_Code"="US","Studied"=0)
row.names(STD)<-STD$Country_Code

for(II in DT1$StudiedPlace_ISO2){
  la=II
  la=regmatches(la,gregexpr("'[^']*[^']*'",la))
  la=la[[1]]
  la=gsub("'","",la)
  for(j in la){
    if(j%in%row.names(STD)){
      STD[j,"Studied"]=as.integer(STD[j,"Studied"])+1
    }
    else{
      STD=rbind(STD,c(j,1))
      row.names(STD)<-STD$Country_Code
    }
    
  }
}


BOTH<-merge(AFF,STD, all = TRUE)%>%
  filter(nchar(Country_Code)==2)

BOTH<-BOTH%>%
  mutate(Affiliation=as.double(Affiliation),
         Studied=as.double(Studied))

#### Merging with country names ####
CY_NAME=read.table("@SofiaG1L/Farmers_CCA/DATA/Countries_Coordinates.txt",
                   sep=",",header = TRUE)

CY_NAME[is.na(CY_NAME$ISO.3166.Country.Code),]
CY_NAME[is.na(CY_NAME$ISO.3166.Country.Code),"ISO.3166.Country.Code"]="NA"

row.names(CY_NAME)<-CY_NAME$ISO.3166.Country.Code

BOTH$Country=CY_NAME[BOTH$Country_Code,]$Country

#### Making Map ####

# My Data
DT=BOTH

DT$region=DT$Country

DT$region[DT$region=="Iran, Islamic Republic of"]="Iran"
DT$region[DT$region=="Russian Federation"]="Russia"
DT$region[DT$region=="Tanzania, United Republic of"]="Tanzania"
DT$region[DT$region=="Viet Nam"]="Vietnam"
DT$region[DT$region=="Lao People's Democratic Republic"]="Lao PDR"

DT$Studied[is.na(DT$Studied)]=0
DT$Affiliation[is.na(DT$Affiliation)]=0

# Original Map Data
# world_map <- map_data("world")
data("World")
st_crs(World)
world_rob<-st_transform(World, "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
st_crs(world_rob)   

# world_map$region[world_map$subregion=="Hong Kong"]="Hong Kong"

# Checking names that do not match
DT_C=unique(DT$region)
# world_map_C=unique(world_map$region)
world_map_C=unique(World$name)

DT_C[!DT_C%in%world_map_C]

#### Bivariate Color Map ####
# https://timogrossenbacher.ch/2019/04/bivariate-maps-with-ggplot2-and-sf/#create-a-bivariate-choropleth

# create 3 buckets for gini
DT%>%
  count(Affiliation)%>%
  mutate(sum=cumsum(n))%>%
  mutate(per=sum/sum(n))

quantiles_AFF <- DT %>%
  pull(Affiliation) %>%
  quantile(probs = c(0, 0.40, 0.80, 0.99, 1),na.rm=TRUE)
(quantiles_AFF<-ceiling(quantiles_AFF))

# quantiles_AFF<-seq(min(DT$Affiliation),12,length.out=4)
# (quantiles_AFF<-c(ceiling(quantiles_AFF),max(DT$Affiliation)))

# create 3 buckets for STD income
DT%>%
  count(Studied)%>%
  mutate(sum=cumsum(n))%>%
  mutate(per=sum/sum(n))

quantiles_STD <- DT %>%
  pull(Studied) %>%
  quantile(probs = c(0, 0.40, 0.80, 0.99, 1),na.rm=TRUE)
(quantiles_STD<-ceiling(quantiles_STD))

# quantiles_STD<-seq(min(DT$Studied),12,length.out=4)
# (quantiles_STD<-c(ceiling(quantiles_STD),max(DT$Studied)))

# Generated the colors using:
# https://observablehq.com/@benjaminadk/bivariate-choropleth-color-generator

COLORS= c("#d3d3d3", "#accaca", "#81c1c1", "#52b6b6", "#c6acc1", "#a2a5b9", 
          "#799db0", "#4d94a6", "#ba85b0", "#977fa8", "#7279a0", "#487397", 
          "#ad5b9c", "#8d5796", "#6a538f", "#434e87")
NAMES= paste0(rep(1:4,4)," - ",rep(1:4,each=4))

names(COLORS)<-NAMES
cbbPalette=COLORS

bivariate_color_scale <- tibble(group=NAMES,fill=COLORS) %>%
  gather("group", "fill")

DT %<>%
  mutate(
    AFF_quantiles = cut(
      Affiliation,
      breaks = quantiles_AFF,
      include.lowest = TRUE
    ),
    STD_quantiles = cut(
      Studied,
      breaks = quantiles_STD,
      include.lowest = TRUE
    ),
    # by pasting the factors together as numbers we match the groups defined
    # in the tibble bivariate_color_scale
    group = paste(
      as.numeric(AFF_quantiles), "-",
      as.numeric(STD_quantiles)
    )
  ) %>%
  # we now join the actual hex values per "group"
  # so each municipality knows its hex value based on the his AFF and avg
  # income value
  left_join(bivariate_color_scale, by = "group")


world_map_DT <- right_join(DT, world_rob, by = c("region"="name"))


# separate the groups
bivariate_color_scale <-DT%>%
  group_by(Affiliation,Studied,group,fill)%>%
  # summarise()
  separate(group, into = c("Affiliation", "Studied"), sep = " - ") %>%
  mutate(Affiliation = as.integer(Affiliation),
         Studied = ifelse(is.na(Studied),"",as.integer(Studied)))


map <- ggplot()+
  geom_sf(data = world_map_DT ,  
          mapping = aes(geometry=geometry,fill = group),
          color = "mistyrose2", size = 0.1, show.legend = F) +
  scale_fill_manual(values=cbbPalette,na.value = "white")+
  theme_minimal()+
  theme(axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(),
        axis.line = element_blank())


p1 <- bivariate_color_scale%>%
  ggplot() +
  geom_tile(aes(x = AFF_quantiles,
                y = STD_quantiles,fill = fill)) +
  geom_rug()+
  scale_fill_identity() +
  labs(x = "Affiliated Researchers",
       y = "Cases Studied") +
  theme_minimal() +
  # make font small enough
  theme(plot.margin = unit(c(0,0,0,0), 'lines'),
        axis.title = element_text(size = 8),
        axis.text = element_text(size = 8),
        axis.text.x = element_text(angle = 90,
                                   hjust = 1,vjust = 0.35)) +
  # quadratic tiles
  coord_fixed()
# 
# thm = list(theme_void(),
#            guides(fill=FALSE),
#            theme(plot.margin=unit(rep(0,4), "lines")))
# 
# p1 = p1 + thm[-1]

p2 = bivariate_color_scale%>%
  group_by(AFF_quantiles)%>%
  count(wt=Affiliation)%>%
  ungroup()%>%
  mutate(per=n/sum(n))%>%
  ggplot(aes(x=AFF_quantiles,weight=per)) +
  geom_histogram(stat="count") +
  scale_y_continuous(breaks = c(0,0.20,0.40),
                     labels = scales::percent_format())+
  theme_minimal(base_size = 8)+
  theme(plot.margin = unit(c(0,0,0,0.5), 'lines'),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())#  +

p3 = bivariate_color_scale%>%
  group_by(STD_quantiles)%>%
  count(wt=Studied)%>%
  ungroup()%>%
  mutate(per=n/sum(n))%>%
  ggplot(aes(x=STD_quantiles,weight=per)) +
  geom_histogram(stat="count") +
  scale_y_continuous(breaks = c(0,0.20,0.40),
                     labels = scales::percent_format())+
  theme_minimal(base_size = 8)+
  theme(plot.margin = unit(c(0,0,0.5,0), 'lines'),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90,
                                   hjust = 1,vjust = 0.35))+
  coord_flip()


## Joining the graphs

pobj = ggarrange(p2, ggplot()+theme_minimal(), p1, p3, 
                 ncol=2, widths=c(4,1), heights=c(1,4))

ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(pobj, x=0, y=0.175, 
            width = 0.3,height = 0.4)
# 
# ggsave("IMAGES/Map_Farmers_20240926.png",
#        width = 15,height = 10,units = "cm")







