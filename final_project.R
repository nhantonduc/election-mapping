library(tidyverse)
library(sf)
library(glue)
library(dplyr)
#presidential election data for 2012-2020
president_data <- read_rds("election_data_president_2012_2020.rds")
#presidential election data for 2012
president_2012 <- president_data %>% filter(year=="2012")
#presidential election data for 2016
president_2016 <- president_data %>% filter(year=="2016")

#unemployment data for 2012
unem_2012 <- read_rds("unemployment_rate_by_county_2012.rds")
#unemployment data for 2016
unem_2016 <- read_rds("unemployment_rate_by_county_2016.rds")

elec2012 <- president_2012 %>% left_join(unem_2012, by="fips")
elec2012 <- select(elec2012,-period) %>% select(elec2012,-state_name) %>% 
  select(elec2012,-county) %>% select(elec2012,-office) %>%
elec2016 <- president_2016 %>% left_join(unem_2016, by="fips")
elec2016 <- select(elec2016,-state_name)
elec2012 <- select(elec2012,-state_name)
elec2012 <- select(elec2012,-period)
elec2012 <- select(elec2012,-county)
elec2012 <- select(elec2012,-office)
#create dem_pct and rep_pct for each year
elec2012 <- elec2012 %>% mutate(dem_pct_2012=dem_votes/total_vote,rep_pct_2012=rep_votes/total_vote)
elec2016 <- elec2016 %>% mutate(dem_pct_2016=dem_votes/total_vote,rep_pct_2016=rep_votes/total_vote)
a <- elec2012 %>% left_join(elec2016,by="fips")

e2012 <- lm((dem_pct_2016-dem_pct_2012) ~ (unemployment_rate.y-unemployment_rate.x),data=a)

a <- a %>% filter(state.x=="CA" || state.x=="MA" || state.x=="NY" || state.x=="WA" || state.x=="OR")

ggplot(a,mapping=aes(y=(dem_pct_2016-dem_pct_2012),x=(unemployment_rate.y-unemployment_rate.x))) + 
  geom_point() + geom_smooth(method="lm", se=F, color="red")+
  facet_wrap(~state.x) + scale_y_continuous(limits=c(-0.5, 0))


