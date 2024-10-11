
library(tidyverse)
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
display_carto_all(colorblind_friendly = TRUE)

rm(list=ls())
gc()

#### Labels ####

HZD_DICT=read.csv("DATA/Hazards_Dict.csv",sep = ";")

COLOR_TXT=c("climate OR climate change"="#D95F02",
            "social change"="#081D58",
            "state shift"="#E6AB02",
            "critical transition AND [society OR social]"="#1B9E77",
            "structural change AND [society OR social]"="#1B9E77",
            "threshold AND [social OR econom* OR behavio* OR policy OR political]"="#1B9E77",
            "transformation* AND [society OR social]"="#1B9E77",
            "transformative AND [society OR social]"="#1B9E77",
            "transition* AND [society OR social]"="#1B9E77",
            "dike"="#666666",
            "drought"="#A6761D",
            "earthquake"="#A6761D",
            "flood*"="#A6761D",
            "hurricane"="#A6761D",
            "storm"="#A6761D",
            "typhoon"="#A6761D",
            "landslide"="#A6761D"
)


TOPIC=c("climate OR climate change"="CC",
        "social change"="SOCIAL",
        "society OR social"="SOCIAL",
        "social OR econom* OR behavio* OR policy OR political"="SOCIAL",
        "state shift"="SHIFT",
        "critical transition AND [society OR social]"="SHIFT",
        "structural change AND [society OR social]"="SHIFT",
        "threshold AND [social OR econom* OR behavio* OR policy OR political]"="SHIFT",
        "transformation* AND [society OR social]"="SHIFT",
        "transformative AND [society OR social]"="SHIFT",
        "transition* AND [society OR social]"="SHIFT",
        "dike"="SOLUTION",
        "drought"="HAZARD",
        "earthquake"="HAZARD",
        "flood*"="HAZARD",
        "hurricane"="HAZARD",
        "storm"="HAZARD",
        "typhoon"="HAZARD",
        "landslide"="HAZARD"
)

TOPIC=data.frame(TOPIC)

TOPIC$Term=row.names(TOPIC)

#### Opening the Data ####
# DT1
DT1=read.csv("PROCESSED/DT1_FACTORS_20240913.csv")
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
CY_NAME=read.table("C:\\Dropbox\\TU_Delft\\External_Data\\Countries_Coordinates.txt",
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


library(egg) 

pobj = ggarrange(p2, ggplot()+theme_minimal(), p1, p3, 
                 ncol=2, widths=c(4,1), heights=c(1,4))

ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(pobj, x=0, y=0.175, 
            width = 0.3,height = 0.4)
# 
# ggsave("IMAGES/Map_Farmers_20240926.png",
#        width = 15,height = 10,units = "cm")



#### Stats from the articles ####

HZD2=data.frame("HZD"="drought","Count"=0)
row.names(HZD2)<-HZD2$HZD

N=dim(DT1)[1]

for(II in 1:N){
  # ISO2 CODE
  la=DT1$HAZARD[II]
  la=str_split(la,"/")
  la=la[[1]]
  
  N2=length(la)
  if(N2>0){
    for(j in 1:N2){
      
      if(la[j]%in%row.names(HZD2)){
        HZD2[la[j],"Count"]=as.integer(HZD2[la[j],"Count"])+1
      }
      else{
        HZD2=rbind(HZD2,c(la[j],1))
        row.names(HZD2)<-HZD2$HZD
      }
      
    }
  }
}

HZD2=HZD2[!is.na(HZD2$Count),]
HZD2=HZD2%>%mutate(Count=as.double(Count))

#### Hazards ####

HZD2%>%
  ggplot(aes(x=reorder(HZD,-Count),y=Count))+
  geom_bar(stat = "identity", position = "stack")+
  theme_minimal()+
  theme(axis.text.x = 
          element_text(size = 8,angle = 60,hjust = 0.85,vjust = 1))+
  labs(x=NULL)

# ggsave("IMAGES/HZD_Farmers.png",
#        width = 15,height = 5,units = "cm")

##### Extracting search terms to create network of terms ####

STD=data.frame("Term"="flood*","Count"=0)
row.names(STD)<-STD$Term

for(II in DT1$query_y){
  la=II
  la=regmatches(II,
                gregexpr("TITLE-ABS-KEY\\((.*?)\\)",II))
  la=la[[1]]
  la=gsub("TITLE-ABS-KEY","",la)
  la=gsub("[()]", "", la)
  
  for(j in la){
    if(j%in%row.names(STD)){
      STD[j,"Count"]=as.integer(STD[j,"Count"])+1
    }
    else{
      STD=rbind(STD,c(j,1))
      row.names(STD)<-STD$Term
    }
    
  }
}

STD$Count<-as.double(STD$Count)

# Network Matrix

COLOR_TXT=c("climate OR climate change"="#D95F02", #
            "social change"="#081D58", #
            "state shift"="#E6AB02", #
            "regime shift AND [society OR social]"="#1B9E77", #
            "critical transition AND [society OR social]"="#1B9E77", #
            "structural change AND [society OR social]"="#1B9E77", #
            "threshold AND [social OR econom* OR behavio* OR policy OR political]"="#1B9E77", #
            "transformation* AND [society OR social]"="#1B9E77", #
            "transformative AND [society OR social]"="#1B9E77", #
            "transition* AND [society OR social]"="#1B9E77", # 
            "tipping point AND [society OR social]", #
            # "dike"="#666666",
            "drought"="#A6761D", #
            "earthquake"="#A6761D", #
            "flood*"="#A6761D", #
            "hurricane"="#A6761D", #
            "storm"="#A6761D", #
            "typhoon"="#A6761D", #
            "landslide"="#A6761D" #
)

# TERMS<-matrix(0,ncol = dim(STD)[1],nrow = dim(STD)[1])
# 
# colnames(TERMS)<-sort(STD$Term)
# rownames(TERMS)<-sort(STD$Term)

EDGES<-data.frame(matrix(ncol=4,nrow=0, 
           dimnames=list(NULL, c("Source","Target","Distance","Color"))))
NODES<-data.frame(matrix(ncol=3,nrow=0, 
           dimnames=list(NULL, c("Label","Type","Color"))))

for(II in DT1$query_y){
  la=II
  la=regmatches(II,
                gregexpr("TITLE-ABS-KEY\\((.*?)\\)",II))
  la=la[[1]]
  la=gsub("TITLE-ABS-KEY","",la)
  la=gsub("[()]", "", la)
  
  n=length(la)
  if(n>1){
    for(j in 1:(n-1)){
      # TERMS[la[j],la[j+1]]=TERMS[la[j],la[j+1]]+1
      EDGES<-rbind(EDGES,c(la[j],la[j+1],j,COLOR_TXT[la[j]]))
      NODES<-rbind(NODES,c(la[j],"In",COLOR_TXT[la[j]]))
      NODES<-rbind(NODES,c(la[j+1],"Out",COLOR_TXT[la[j+1]]))
    }
  }else{
    EDGES<-rbind(EDGES,c(la[j],la[j],j,COLOR_TXT[la[j]]))
    NODES<-rbind(NODES,c(la[j],"Loop",COLOR_TXT[la[j]]))
  }
}

names(EDGES)<-c("Source","Target","Distance","Color")
names(NODES)<-c("Id","Type","Color")

EDGES<-EDGES%>%
  group_by(Source,Target,Distance,Color)%>%
  summarise(Weight=n())

NODES<-NODES%>%
  group_by(Id,Type,Color)%>%
  summarise(Weight=n())

NODES<-NODES%>%
  group_by(Id,Color)%>%
  summarise(Weight=sum(Weight))

# write.csv(NODES,"PROCESSED/NODES_SCH.csv",row.names = FALSE)
# write.csv(EDGES,"PROCESSED/EDGES_SCH.csv",row.names = FALSE)

# Transform it in a graph format
network=graph_from_data_frame(EDGES,vertices = NODES, directed = TRUE)

# Make the graph
ggraph(network) + 
  geom_edge_link(edge_alpha=0.3, # edge_colour="#69b3a2", 
                 aes(edge_width=E(network)$Weight,
                     edge_colour=E(network)$Color) , 
                 arrow=arrow(type = "closed", length = unit(3, 'mm')),
                 check_overlap = TRUE, angle_calc = "rot") +
  scale_edge_width(range=c(1,3)) +
  geom_node_point(aes(color=V(network)$Color),size=3) +
  geom_node_text( aes(label=name), repel = TRUE,
                  size=4, nudge_x = 0.05) + # color="#69b3a2",
  theme_void() +
  theme(
    legend.position="none",
    plot.margin=unit(rep(1,4), "cm")
  ) 


plot(network, layout=layout_as_tree(network, circular=TRUE))


#### Stats about the journals and fields ####

AREA=DT1%>%
  select(prism.coverDate_Year,SUBJAREA_2)%>%
  group_by(SUBJAREA_2)%>%
  mutate(total=n())%>%
  group_by(prism.coverDate_Year,SUBJAREA_2,total)%>%
  summarise(value=n())%>%
  arrange(desc(total))

AREA_F=unique(AREA$SUBJAREA_2)

AREA=AREA%>%
  mutate(SUBJAREA_2 = factor(SUBJAREA_2, levels = AREA_F))

AREA%>%
  ggplot(aes(x=prism.coverDate_Year,y=value,fill=SUBJAREA_2))+
  geom_bar(stat="identity")+
  scale_fill_carto_d(palette = "ArmyRose")+
  theme_minimal()+
  theme(legend.position = "bottom")+
  labs(x="Year",y="Count")+
  guides(fill=guide_legend(title="SCOPUS Area",byrow = TRUE))

# ggsave("IMAGES/SCOPUS_Area.png",
#        width = 18,height = 8,units = "cm")

DT1%>%
  select(SUBJAREA_2)%>%
  group_by(SUBJAREA_2)%>%
  summarise(value=n())%>%
  ungroup()%>%
  summarise(SUBJAREA_2=SUBJAREA_2,perc=value/sum(value))%>%
  arrange(perc)

#### Adding Hzd to Data ####
# 
# DT1=DT1%>%
#   mutate(avalanch1=grepl(HZD_DICT$Hazard[3],sentenceINT,perl=TRUE),
#          landslide1=grepl(HZD_DICT$Hazard[4],sentenceINT,perl=TRUE),
#          drought1=grepl(HZD_DICT$Hazard[5],sentenceINT,perl=TRUE),
#          famine1=grepl(HZD_DICT$Hazard[6],sentenceINT,perl=TRUE),
#          extreme_temperature1=grepl(HZD_DICT$Hazard[7],sentenceINT,perl=TRUE),
#          flood1=grepl(HZD_DICT$Hazard[8],sentenceINT,perl=TRUE),
#          fire1=grepl("fire",sentenceINT,perl=TRUE),
#          windstorm1=grepl(HZD_DICT$Hazard[11],sentenceINT,perl=TRUE),
#          epidemic1=grepl(HZD_DICT$Hazard[12],sentenceINT,perl=TRUE))
# 
# rowSums(DT1[,31:39])
# 
# 
# DT1=DT1%>%
#   mutate(avalanch2=grepl(HZD_DICT$Hazard[3],query_y,perl=TRUE),
#          landslide2=grepl(HZD_DICT$Hazard[4],query_y,perl=TRUE),
#          drought2=grepl(HZD_DICT$Hazard[5],query_y,perl=TRUE),
#          famine2=grepl(HZD_DICT$Hazard[6],query_y,perl=TRUE),
#          extreme_temperature2=grepl(HZD_DICT$Hazard[7],query_y,perl=TRUE),
#          flood2=grepl(HZD_DICT$Hazard[8],query_y,perl=TRUE),
#          fire2=grepl("fire",query_y,perl=TRUE),
#          windstorm2=grepl(HZD_DICT$Hazard[11],query_y,perl=TRUE),
#          epidemic2=grepl(HZD_DICT$Hazard[12],query_y,perl=TRUE))
# 
# rowSums(DT1[,96:104])
# 
# 
# DT1=DT1%>%
#   mutate(avalanch=ifelse(avalanch1+avalanch2>0,TRUE,FALSE),
#          landslide=ifelse(landslide1+landslide2>0,TRUE,FALSE),
#          drought=ifelse(drought1+drought2>0,TRUE,FALSE),
#          famine=ifelse(famine1+famine2>0,TRUE,FALSE),
#          extreme_temperature=ifelse(extreme_temperature1+extreme_temperature2>0,TRUE,FALSE),
#          flood=ifelse(flood1+flood2>0,TRUE,FALSE),
#          fire=ifelse(fire1+fire2>0,TRUE,FALSE),
#          windstorm=ifelse(windstorm1+windstorm2>0,TRUE,FALSE),
#          epidemic=ifelse(epidemic1+epidemic2>0,TRUE,FALSE))
# 
# sum(rowSums(DT1[,105:113])>0)
# 
# DT1$None=rowSums(DT1[,105:113])==0
# 
# DT_HZD=DT1%>%
#   select(avalanch:None)%>%
#   gather(key="Hazard","Count",avalanch:None)%>%
#   group_by(Hazard)%>%
#   summarise(Total=sum(Count))
# 
# DT_HZD%>%
#   ggplot(aes(x=reorder(Hazard,-Total),y=Total))+
#   geom_bar(stat = "identity", position = "stack")+
#   theme_minimal()+
#   theme(axis.text.x = element_text(size = 8))+
#   labs(x=NULL)


#### Type of research ####

DT1$qualitative2=0
DT1$quantitative2=0

QUAL=read.csv("DATA/Qualitative.csv",sep=";")

N=dim(DT1)[1]

for(ii in 1:N){
  # Check Qualitative
  DT1[ii,"qualitative2"]=ifelse(sum(apply(cbind(QUAL[QUAL$Type=="qualitative","Vocabulary"]),
                              1,grepl,DT1$sentenceABS[ii],perl=FALSE))>1,1,0)
  # Check Quantitative
  DT1[ii,"quantitative2"]=ifelse(sum(apply(cbind(QUAL[QUAL$Type=="quantitative","Vocabulary"]),
                               1,grepl,DT1$sentenceABS[ii],perl=FALSE))>1,1,0)
}

DT1[,"qualitative2"]=as.double(DT1[,"qualitative2"])
DT1[,"quantitative2"]=as.double(DT1[,"quantitative2"])
DT1[,"both2"]=ifelse(DT1["qualitative2"]+DT1["quantitative2"]==2,1,0)

DT1=DT1%>%
  mutate(qualitative2=ifelse(both2==1,0,qualitative2),
         quantitative2=ifelse(both2==1,0,quantitative2))

IDS=which(DT1["qualitative2"]+DT1["quantitative2"]+DT1["both2"]==0)

DT1[IDS,"qualitative2"]=ifelse(DT1[IDS,"qualitative"]!="",1,0)
DT1[IDS,"quantitative2"]=as.double(DT1[IDS,"quantitative"]!="",1,0)
DT1[,"both2"]=ifelse(DT1["qualitative2"]+DT1["quantitative2"]==2,1,0)

DT1=DT1%>%
  mutate(qualitative2=ifelse(both2==1,0,qualitative2),
         quantitative2=ifelse(both2==1,0,quantitative2))

DT_HZD=DT1%>%
  select(qualitative2,quantitative2,both2)%>%
  gather(key="Type_Study","Count",qualitative2:both2)%>%
  group_by(Type_Study)%>%
  summarise(Total=sum(Count))%>%
  ungroup()%>%
  mutate(per=Total/sum(Total))

DT_HZD%>%
  ggplot(aes(x="Type",y=per,fill=Type_Study))+
  geom_bar(stat = "identity", position = "stack")+
  coord_flip()+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 8))+
  labs(x=NULL)






