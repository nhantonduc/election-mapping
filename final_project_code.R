library(tidyverse)
library(sf)
library(tidyverse)
library(janitor)
library(broom)

election_pres_12_20<-read_rds("election_data_president_2012_2020.rds")

unemployment_2012<-read_rds("unemployment_rate_by_county_2012.rds")

unemployment_2016<-read_rds("unemployment_rate_by_county_2016.rds")

state_map <- read_rds("state_shapefile.rds")

election_pres_2012<- election_pres_12_20 %>% filter(year=="2012")

election_pres_2016<- election_pres_12_20 %>% filter(year=="2016")

dat2012_unmp_pres<- left_join(election_pres_2012,unemployment_2012, by="fips")

dat2012_unmp_pres<- dat2012_unmp_pres %>% select(-state_name)
dat2012_unmp_pres<- dat2012_unmp_pres %>% select(-office)
dat2012_unmp_pres<- dat2012_unmp_pres %>% select(-county)
dat2012_unmp_pres<- dat2012_unmp_pres %>% select(-period)

dat2016_unmp_pres<- left_join(election_pres_2016,unemployment_2016, by="fips")

dat2016_unmp_pres<- dat2016_unmp_pres %>% select(-state_name)
dat2016_unmp_pres<- dat2016_unmp_pres %>% select(-office)
dat2016_unmp_pres<- dat2016_unmp_pres %>% select(-county)
dat2016_unmp_pres<- dat2016_unmp_pres %>% select(-period)

dat2012_unmp_pres<-dat2012_unmp_pres %>% 
  mutate(dem_pct_2012=dem_votes/total_vote,
         rep_pct_2012=rep_votes/total_vote)

dat2016_unmp_pres<-dat2016_unmp_pres %>% 
  mutate(dem_pct_2016=dem_votes/total_vote,
         rep_pct_2016=rep_votes/total_vote)

#----------------------------------------------------------------



#---------------------------------------------







#-----------------------------------------------------------------







#----------------

#WA OR NY































dat2012_unmp_pres<- dat2012_unmp_pres %>% 
  rename("unemployment_rate_2012" = "unemployment_rate")

dat2016_unmp_pres<- dat2016_unmp_pres %>% 
  rename("unemployment_rate_2016" = "unemployment_rate")

dat2012thru2016<- left_join(dat2012_unmp_pres,dat2016_unmp_pres, by="fips")

dat2012thru2016<- dat2012thru2016 %>% 
  mutate(dem_pct_change=dem_pct_2016-dem_pct_2012,
         rep_pct_change=rep_pct_2016-rep_pct_2012)

dat2012thru2016<- dat2012thru2016 %>% 
  mutate(unemployment_rate_change=unemployment_rate_2016-unemployment_rate_2012)







#notes from Bo,
#change selection to whether the state was democratic in 2012, not 2016
#create 2 null hypotheses for the two selection groups







dem_win_2012<-dat2012thru2016 %>% filter(
  state.x=="WA" || state.x=="OR" || state.x=="CA"|| state.x=="NV"|| state.x=="CO"
  || state.x=="NM"|| state.x=="MN"|| state.x=="IA"|| state.x=="WI"
  || state.x=="IL"|| state.x=="MI"|| state.x=="OH"|| state.x=="FL"
  || state.x=="VA"|| state.x=="PA"|| state.x=="MD"|| state.x=="DE"
  || state.x=="DC"|| state.x=="NJ"|| state.x=="NY"|| state.x=="CT"
  || state.x=="RI"|| state.x=="MA"|| state.x=="VT"|| state.x=="NH"
  || state.x=="ME"|| state.x=="HI")
ggplot(data=dem_win_2012, 
       mapping = aes(x=unemployment_rate_change, y=dem_pct_change))+
  geom_point()+
  geom_smooth(method="lm", se=F, color="red") +
  labs(x="Unemployment Rate Change", 
       y="Democratic Vote Percentage Change")+
  xlim(-10, 5)+
  ylim(-0.3,0.1)
ggsave("Democratic.png",width=6,height=4)

#--------------------------------------------------


rep_win_2012<-dat2012thru2016 %>% filter(
  state.x!="WA" && state.x!="OR" && state.x!="CA"&& state.x!="NV"&& state.x!="CO"
  && state.x!="NM"&& state.x!="MN"&& state.x!="IA"&& state.x!="WI"
  && state.x!="IL"&& state.x!="MI"&& state.x!="OH"&& state.x!="FL"
  && state.x!="VA"&& state.x!="PA"&& state.x!="MD"&& state.x!="DE"
  && state.x!="DC"&& state.x!="NJ"&& state.x!="NY"&& state.x!="CT"
  && state.x!="RI"&& state.x!="MA"&& state.x!="VT"&& state.x!="NH"
  && state.x!="ME"&& state.x!="HI")
ggplot(data=rep_win_2012, 
       mapping = aes(x=unemployment_rate_change, y=dem_pct_change))+
  geom_point()+
  geom_smooth(method="lm", se=F, color="red") +
  labs(x="Unemployment Rate Change", 
       y="Democratic Vote Percentage Change")+
  xlim(-10, 5)+
  ylim(-0.3,0.1)
ggsave("Republican.png",width=6,height=4)

election_pres_2012 <- election_pres_2012 %>% 
  group_by(state)%>% mutate(total_state=sum(total_vote))

election_pres_2012 <- election_pres_2012 %>% 
  group_by(state)%>% mutate(total_dem=sum(dem_votes))

election_pres_2012 <- election_pres_2012 %>% 
  group_by(state)%>% mutate(dem_pct=total_dem/total_state)

election_pres_2012 <- election_pres_2012 %>% 
  group_by(state)%>% mutate(dem_win=ifelse(dem_pct>=0.5,0,1))
election_pres_2012 <- election_pres_2012 %>% 
  group_by(state)%>% mutate(state_fips=state_code)

pres_2012_state <- election_pres_2012 %>% select(state_fips,dem_win)

state_map <- state_map %>% left_join(pres_2012_state, by="state_fips")

ggplot(state_map) +
  geom_sf(aes(fill=dem_win),lwd=.1, col="gray",show.legend=F) + 
  scale_fill_distiller(palette="RdBu",limits=c(0,1))+theme_void()+
  labs(title="2012 Presidential Election Result by State")
  
ggsave("PresidentialElection2012.png",width=6,height=4)