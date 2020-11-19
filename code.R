library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
Accident_19 <- read_excel("C:/Users/user/Desktop/Data Set/TF_ACCIDENTS_VICTIMS_2019.xlsx")
Accident_18 <- read_excel("C:/Users/user/Desktop/Data Set/TF_ACCIDENTS_VICTIMS_2018.xlsx")

Accident_19<-Accident_19%>%mutate(year=2019)
Accident_18 <- Accident_18 %>% mutate(year=2018)%>%
  mutate( TX_PROV_DESCR_FR= case_when(TX_PROV_DESCR_FR== "Province de Luxembourg" ~  "Province du Luxembourg", TRUE~ TX_PROV_DESCR_FR  )  )

Accident_19_18<- Accident_19%>%full_join(Accident_18)


Accident_19_18<-
  
  Accident_19_18%>%
  select(DT_DAY,DT_HOUR,TX_DAY_OF_WEEK_DESCR_FR, MS_VICT, MS_SLY_INJ ,MS_SERLY_INJ,
         MS_DEAD_30_DAYS,  TX_BUILD_UP_AREA_DESCR_FR,  TX_VICT_TYPE_DESCR_FR,
         TX_ROAD_USR_TYPE_DESCR_FR,  TX_ROAD_TYPE_DESCR_FR,  TX_LIGHT_COND_DESCR_FR, TX_AGE_CLS_DESCR_FR ,
         TX_MUNTY_DESCR_FR,TX_PROV_DESCR_FR,TX_RGN_DESCR_FR,TX_SEX_DESCR_FR,year
  )%>%
  
  rename(Number_Of_Victim=MS_VICT,  Number_Slight_Injured =MS_SLY_INJ, Number_Seriously_Injured=MS_SERLY_INJ,
         Dead_30days=MS_DEAD_30_DAYS,  Localisation=TX_BUILD_UP_AREA_DESCR_FR, VictimType=TX_VICT_TYPE_DESCR_FR ,
         RoadUserType=TX_ROAD_USR_TYPE_DESCR_FR,  RoadType=TX_ROAD_TYPE_DESCR_FR, LightType=TX_LIGHT_COND_DESCR_FR,
         Age=TX_AGE_CLS_DESCR_FR , Municipality = TX_MUNTY_DESCR_FR , Province= TX_PROV_DESCR_FR , Region =TX_RGN_DESCR_FR,
         Sex=TX_SEX_DESCR_FR
         
  )%>%
  
  group_by(Province,year) %>% 
  summarise(Number_Of_Victim= sum(Number_Of_Victim),
            Number_Slight_Injured = sum(Number_Slight_Injured ),
            Dead_30days= sum(Dead_30days), Number_Slight_Injured= sum(Number_Slight_Injured),
            Number_Seriously_Injured=sum(Number_Seriously_Injured)
            
            
  )%>%
  mutate( prop_Slightly= Number_Slight_Injured/Number_Of_Victim,
          prop_serious=  Number_Seriously_Injured/Number_Of_Victim,
          prop_dead=Dead_30days/Number_Of_Victim)
  


Accident_19_18<- Accident_19_18 %>% ungroup()%>% arrange(Province,year)


glimpse(Accident_19)

Accident<-Accident_19%>%
  select(DT_DAY,DT_HOUR,TX_DAY_OF_WEEK_DESCR_FR, MS_VICT, MS_SLY_INJ ,MS_SERLY_INJ,
         MS_DEAD_30_DAYS,  TX_BUILD_UP_AREA_DESCR_FR,  TX_VICT_TYPE_DESCR_FR,
         TX_ROAD_USR_TYPE_DESCR_FR,  TX_ROAD_TYPE_DESCR_FR,  TX_LIGHT_COND_DESCR_FR, TX_AGE_CLS_DESCR_FR ,
         TX_MUNTY_DESCR_FR,TX_PROV_DESCR_FR,TX_RGN_DESCR_FR,TX_SEX_DESCR_FR,year
         )%>%
  
  rename(Number_Of_Victim=MS_VICT,  Number_Slight_Injured =MS_SLY_INJ, Number_Seriously_Injured=MS_SERLY_INJ,
         Dead_30days=MS_DEAD_30_DAYS,  Localisation=TX_BUILD_UP_AREA_DESCR_FR, VictimType=TX_VICT_TYPE_DESCR_FR ,
         RoadUserType=TX_ROAD_USR_TYPE_DESCR_FR,  RoadType=TX_ROAD_TYPE_DESCR_FR, LightType=TX_LIGHT_COND_DESCR_FR,
         Age=TX_AGE_CLS_DESCR_FR , Municipality = TX_MUNTY_DESCR_FR , Province= TX_PROV_DESCR_FR , Region =TX_RGN_DESCR_FR,
         Sex=TX_SEX_DESCR_FR
         
         )





Province_19<-Accident%>% group_by(Province,year) %>% 
                summarise(Number_Of_Victim= sum(Number_Of_Victim),
                          Number_Slight_Injured = sum(Number_Slight_Injured ),
                          Dead_30days= sum(Dead_30days), 
                          Number_Seriously_Injured=sum(Number_Seriously_Injured)
                         
                          
                          )%>%
                mutate( prop_Slightly= Number_Slight_Injured/Number_Of_Victim,
                        prop_serious=  Number_Seriously_Injured/Number_Of_Victim,
                        prop_dead=Dead_30days/Number_Of_Victim)


Total_19<- Accident%>% group_by(year) %>% 
              summarise(Number_Of_Victim= sum(Number_Of_Victim),
                        Number_Slight_Injured = sum(Number_Slight_Injured ),
                        Dead_30days= sum(Dead_30days), Number_Slight_Injured= sum(Number_Slight_Injured),
                        Number_Seriously_Injured=sum(Number_Seriously_Injured)
            
                                  
                        )%>%
                        mutate( prop_Slightly= Number_Slight_Injured/Number_Of_Victim,
                                prop_serious=  Number_Seriously_Injured/Number_Of_Victim,
                                prop_dead=Dead_30days/Number_Of_Victim, Province="Total 19")






Accident2<-Accident_18%>%
  select(DT_DAY,DT_HOUR,TX_DAY_OF_WEEK_DESCR_FR, MS_VICT, MS_SLY_INJ ,MS_SERLY_INJ,
         MS_DEAD_30_DAYS,  TX_BUILD_UP_AREA_DESCR_FR,  TX_VICT_TYPE_DESCR_FR,
         TX_ROAD_USR_TYPE_DESCR_FR,  TX_ROAD_TYPE_DESCR_FR,  TX_LIGHT_COND_DESCR_FR, TX_AGE_CLS_DESCR_FR ,
         TX_MUNTY_DESCR_FR,TX_PROV_DESCR_FR,TX_RGN_DESCR_FR,TX_SEX_DESCR_FR,year
  )%>%
  
  rename(Number_Of_Victim=MS_VICT,  Number_Slight_Injured =MS_SLY_INJ, Number_Seriously_Injured=MS_SERLY_INJ,
         Dead_30days=MS_DEAD_30_DAYS,  Localisation=TX_BUILD_UP_AREA_DESCR_FR, VictimType=TX_VICT_TYPE_DESCR_FR ,
         RoadUserType=TX_ROAD_USR_TYPE_DESCR_FR,  RoadType=TX_ROAD_TYPE_DESCR_FR, LightType=TX_LIGHT_COND_DESCR_FR,
         Age=TX_AGE_CLS_DESCR_FR , Municipality = TX_MUNTY_DESCR_FR , Province= TX_PROV_DESCR_FR , Region =TX_RGN_DESCR_FR,
         Sex=TX_SEX_DESCR_FR
         
  )



Total_18<- Accident2%>% group_by(year) %>% 
  summarise(Number_Of_Victim= sum(Number_Of_Victim),
            Number_Slight_Injured = sum(Number_Slight_Injured ),
            Dead_30days= sum(Dead_30days), Number_Slight_Injured= sum(Number_Slight_Injured),
            Number_Seriously_Injured=sum(Number_Seriously_Injured)
            
            
  )%>%
  mutate( prop_Slightly= Number_Slight_Injured/Number_Of_Victim,
          prop_serious=  Number_Seriously_Injured/Number_Of_Victim,
          prop_dead=Dead_30days/Number_Of_Victim, Province="Total 18")





Province_18<-Accident2%>% group_by(Province,year) %>% 
  summarise(Number_Of_Victim= -sum(Number_Of_Victim),
            Number_Slight_Injured = -sum(Number_Slight_Injured ),
            Dead_30days= -sum(Dead_30days), 
            Number_Seriously_Injured=-sum(Number_Seriously_Injured)
            
            
  )%>%
  mutate( prop_Slightly= Number_Slight_Injured/Number_Of_Victim,
          prop_serious=  Number_Seriously_Injured/Number_Of_Victim,
          prop_dead=Dead_30days/Number_Of_Victim)



water_fall<-Province_18%>%full_join(Province_19)%>%ungroup%>% select(-year)%>%      group_by(Province) %>% 
  summarise(Number_Of_Victim= sum(Number_Of_Victim),
            Number_Slight_Injured = sum(Number_Slight_Injured ),
            Dead_30days= sum(Dead_30days), 
            Number_Seriously_Injured=sum(Number_Seriously_Injured))
data<-Total_18%>%full_join(water_fall)%>%full_join(Total_19)%>%
  select(-prop_Slightly,-prop_serious, -prop_dead,-year)%>%replace_na(list(Province="Non spécifié"))

measure<- c("absolute", "relative", "relative", "relative", "relative","relative", "relative",
            "relative", "relative","relative", "relative", "relative","relative", "relative", "relative","relative",  "total")

fig<-plot_ly(
  data, type = "waterfall", measure = ~measure,
  x = ~as.list(data$Province), textposition = "outside", y= ~data$Number_Of_Victim, text =~data$Province
 ) 
 fig<-fig %>%
  layout(title = "Profit and loss statement 2018",
         xaxis = list(title = ""),
         yaxis = list(title = ""),
         autosize = TRUE)

fig

