
### These values represent the % of times the names were 
### mentioned in the articles.

#### Using Factors2 Database ####
DATA0<-FACTORS2

N_Arts=length(unique(DATA0$ID_ART))
N_Arts_STD=DATA0%>%
  group_by(ID_ART,STD,Country)%>%
  summarise(one=1)%>%
  group_by(STD)%>%
  summarise(Total=sum(one))
N_Arts_STD<-data.frame(N_Arts_STD)
row.names(N_Arts_STD)<-N_Arts_STD$STD


FA<-read.csv("PROCESSED/FactorsAdaptation_20230712.csv")
FA<-FA%>%filter(Researcher=="Tatiana")%>%select(-Researcher)


# Shorting some levels
DATA0$AdaptationName=factor(DATA0$AdaptationName)
levels(DATA0$AdaptationName)[c(5,6,7,8)]=
  c("Borrow - Formal sources","Borrow - Informal sources",
    "Incremental","Transformational")

DATA0$AdaptationCommonName=factor(DATA0$AdaptationCommonName,
                          levels = c("General","crop management",
                                     "farm management","financial management",
                                     "irrigation and water management",
                                     "physical infrastructure management",
                                     "social activities"),
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
  mutate(per=Total/N_Arts,
         TypeAdaptation=ifelse(TypeAdaptation=="0","Incremental","Transformational"))%>%
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

# ggsave("IMAGES/AdaptatioType2.png",
#        units = "cm",height = 20,width =30)

#### Factors Descriptive Statistics ####
# DATA0%>%
#   mutate(N_Arts=N_Arts)%>%
#   add_count(ID_ART,Country)%>%
#   group_by(ID_ART,Country,STD,Umbrella_Terms,Driver,n,N_Arts)%>%
#   summarise(value=1/n)%>%
#   group_by(STD,Country,Umbrella_Terms,Driver)%>%
#   summarise(Total=sum(value))%>%
#   mutate(per=Total/N_Arts)

DATA<-DATA0%>%
  select(-ADAPT,-doi,-AdaptationName,-AdaptationCommonName)%>%
  gather(key="Driver",value="value",-ID_ART,-Country,-STD)%>%
  left_join(FA,by=c("Driver"="Factors"))%>%
  distinct()%>%
  mutate(Umbrella_Terms=
           ifelse(Driver=="TypeAdaptation","TypeAdaptation",Umbrella_Terms),
         Driver=ifelse(Driver=="TypeAdaptation","Transformational",Driver),
         value=ifelse(value=="0",0,1))%>%
  filter(value>0)%>%
  add_count(ID_ART,Country)%>%
  mutate(value=1/n)%>%
  select(-n)

#### Parallel Sets ####

library(ggforce)

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
  gather(key="Driver",value="value",-ID_ART,-Country,-STD,-doi,
         -AdaptationCommonName,-TypeAdaptation)%>%
  left_join(FA,by=c("Driver"="Factors"))%>%
  select(-Driver)%>%
  # mutate(Umbrella_Terms=AdaptationCommonName)%>%
  group_by(ID_ART,Country,STD,Umbrella_Terms,
           AdaptationCommonName,TypeAdaptation)%>%
  distinct()%>%
  ungroup()%>%
  filter(value>0)

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

data2$TypeAdaptation=factor(data2$TypeAdaptation,
                            levels = c("0","1"),
                            labels = c("Incremental","Transformational"))

data2$x=factor(data2$x,
               levels = c(3,4,5,6),
               labels = c("Region","Factor","Adaptation","Adaptation\nType"))

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
                   limits=c("Region","Factor","Adaptation","Adaptation\nType"))+
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

# ggsave("IMAGES/ParallelSets2.png",
#        width = 30,height = 20,units = "cm")

## Type of Adaptation
data2%>%
  filter(x=="Region")%>%
  select(STD,TypeAdaptation,value)%>%
  group_by(STD)%>%
  mutate(Total=sum(value))%>%
  group_by(STD,TypeAdaptation)%>%
  summarise(value=sum(value),Total=Total)%>%
  distinct()%>%
  mutate(per=value/Total)%>%
  filter(TypeAdaptation=="Transformational")

## Driver
data2%>%
  filter(x=="Region")%>%
  select(STD,Umbrella_Terms,value)%>%
  group_by(STD)%>%
  mutate(Total=sum(value))%>%
  group_by(STD,Umbrella_Terms)%>%
  summarise(value=sum(value),Total=Total)%>%
  distinct()%>%
  mutate(per=value/Total)%>%
  select(-value,-Total)%>%
  spread(STD,per)


### Following the same logic for the other percentages

data2<-DATA0%>%
  select(-ADAPT,-doi)%>%
  distinct()%>%
  gather(key="Driver",value="value",-ID_ART,-Country,-STD,
         -AdaptationCommonName,-AdaptationName,-TypeAdaptation)%>%
  left_join(FA,by=c("Driver"="Factors"))%>%
  group_by(ID_ART,Country,STD,Umbrella_Terms,Driver,
           AdaptationCommonName,AdaptationName,TypeAdaptation)%>%
  distinct()%>%
  ungroup()%>%
  filter(value>0)

levels(data2$AdaptationCommonName)=
  c("General","Crop Management","Farm Management",
    "Financial Management",
    "Irrigation & Water Management",
    "Physical Infrastructure Management",
    "Social Activities")

data2<-data2%>%
  select(-AdaptationCommonName,
         -AdaptationName,-TypeAdaptation)%>%
  distinct()%>%
  add_count(ID_ART,Country)%>% # This adds "n"
  group_by(STD,Country,Umbrella_Terms,Driver)%>%
  mutate(value=1/n)%>%
  ungroup()%>%
  select(-n,-ID_ART,-Country)
  
pers=data2%>%
  group_by(STD,Umbrella_Terms,Driver)%>%
  summarise(value=sum(value),Total=n())%>%
  mutate(per=value/Total)%>%
  select(STD,Umbrella_Terms,Driver,per)%>%
  spread(STD,per)


View(pers)
# write.csv(pers,"PROCESSED/FactorsByRegion.csv")



