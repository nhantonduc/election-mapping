library(tidyverse)
library(sf)
library(glue)
library(dplyr)

#unemployment data for 2012
unem_2012 <- read_rds("unemployment_rate_by_county_2012.rds")
#unemployment data for 2016
unem_2016 <- read_rds("unemployment_rate_by_county_2016.rds")
#governor election 
governor <- read_rds("election_data_governor_2016_2020.rds")
#calculate vote percentage for democrats
governor <- mutate(governor,dem_pct=dem_votes/total_vote)

governor_unem2016 <- governor %>% select(fips,dem_pct,state,geographic_name)
governor_unem2016 <- governor_unem2016 %>% left_join(unem_2016, by="fips")

gov_unem <- lm(dem_pct ~ unemployment_rate,data=governor_unem2016)

ggplot(governor_unem2016,mapping=aes(y=dem_pct,x=unemployment_rate)) + 
  geom_point() + geom_smooth(method="lm", se=F, color="red") +
  scale_y_continuous(limits=c(0,1))

ggplot(governor_unem2016,mapping=aes(y=dem_pct,x=unemployment_rate)) + 
  geom_point() + geom_smooth(method="lm", se=F, color="red") + 
  facet_wrap(~state)+scale_y_continuous(limits=c(0,1))

pres <- read_rds("election_data_president_2012_2020.rds")

pres2016 <- filter(pres, year=="2016")

pres2016 <- mutate(pres2016,dem_pct=dem_votes/total_vote)

pres_unem2016 <- pres2016 %>% select(fips,dem_pct,state,geographic_name)

pres_unem2016 <- pres_unem2016 %>% left_join(unem_2016, by="fips")

ggplot(pres_unem2016,mapping=aes(y=dem_pct,x=unemployment_rate)) + 
  geom_point() + geom_smooth(method="lm", se=F, color="red") +
  scale_y_continuous(limits=c(0,1))

ggplot(pres_unem2016,mapping=aes(y=dem_pct,x=unemployment_rate)) + 
  geom_point() + geom_smooth(method="lm", se=F, color="red") + 
  facet_wrap(~state)+scale_y_continuous(limits=c(0,1))
#WA
filter(pres_unem2016,state=="WA") %>%
  ggplot(pres_unem2016,mapping=aes(y=dem_pct,x=unemployment_rate)) + 
  geom_point() + geom_smooth(method="lm", se=F, color="red") + 
  scale_y_continuous(limits=c(0,1)) + labs(y="Democratic Vote Pct in 2016 Presidential Election in WA",x="Unemployment Rate in 2016")
ggsave("WA_pres_unem_2016.png", width=6, height=5)
#CA
filter(pres_unem2016,state=="CA") %>%
  ggplot(pres_unem2016,mapping=aes(y=dem_pct,x=unemployment_rate)) + 
  geom_point() + geom_smooth(method="lm", se=F, color="red") + 
  scale_y_continuous(limits=c(0,1)) + labs(y="Democratic Vote Pct in 2016 Presidential Election",x="Unemployment Rate in 2016")
ggsave("CA_pres_unem_2016.png", width=6, height=5)
#OR
filter(pres_unem2016,state=="OR") %>%
  ggplot(pres_unem2016,mapping=aes(y=dem_pct,x=unemployment_rate)) + 
  geom_point() + geom_smooth(method="lm", se=F, color="red") + 
  scale_y_continuous(limits=c(0,1))+ labs(y="Democratic Vote Pct in 2016 Presidential Election in OR",x="Unemployment Rate in 2016")
ggsave("OR_pres_unem_2016.png", width=6, height=5)
#IL
filter(pres_unem2016,state=="IL") %>%
  ggplot(pres_unem2016,mapping=aes(y=dem_pct,x=unemployment_rate)) + 
  geom_point() + geom_smooth(method="lm", se=F, color="red") + 
  scale_y_continuous(limits=c(0,1))+ labs(y="Democratic Vote Pct in 2016 Presidential Election",x="Unemployment Rate in 2016")
ggsave("IL_pres_unem_2016.png", width=6, height=5)
#NY
filter(pres_unem2016,state=="NY") %>%
  ggplot(pres_unem2016,mapping=aes(y=dem_pct,x=unemployment_rate)) + 
  geom_point() + geom_smooth(method="lm", se=F, color="red") + 
  scale_y_continuous(limits=c(0,1))+ labs(y="Democratic Vote Pct in 2016 Presidential Election in NY",x="Unemployment Rate in 2016")
ggsave("NY_pres_unem_2016.png", width=6, height=5)
#WY
filter(pres_unem2016,state=="WY") %>%
  ggplot(pres_unem2016,mapping=aes(y=dem_pct,x=unemployment_rate)) + 
  geom_point() + geom_smooth(method="lm", se=F, color="red") + 
  scale_y_continuous(limits=c(0,1))+ labs(y="Democratic Vote Pct in 2016 Presidential Election in WY",x="Unemployment Rate in 2016")
ggsave("WY_pres_unem_2016.png", width=6, height=5)
#UT
filter(pres_unem2016,state=="UT") %>%
  ggplot(pres_unem2016,mapping=aes(y=dem_pct,x=unemployment_rate)) + 
  geom_point() + geom_smooth(method="lm", se=F, color="red") + 
  scale_y_continuous(limits=c(0,1))+ labs(y="Democratic Vote Pct in 2016 Presidential Election",x="Unemployment Rate in 2016")
ggsave("UT_pres_unem_2016.png", width=6, height=5)
#OK
filter(pres_unem2016,state=="OK") %>%
  ggplot(pres_unem2016,mapping=aes(y=dem_pct,x=unemployment_rate)) + 
  geom_point() + geom_smooth(method="lm", se=F, color="red") + 
  scale_y_continuous(limits=c(0,1))+ labs(y="Democratic Vote Pct in 2016 Presidential Election in OK",x="Unemployment Rate in 2016")
ggsave("OK_pres_unem_2016.png", width=6, height=5)
#WV
filter(pres_unem2016,state=="WV") %>%
  ggplot(pres_unem2016,mapping=aes(y=dem_pct,x=unemployment_rate)) + 
  geom_point() + geom_smooth(method="lm", se=F, color="red") + 
  scale_y_continuous(limits=c(0,1))+ labs(y="Democratic Vote Pct in 2016 Presidential Election in WV",x="Unemployment Rate in 2016")
ggsave("WV_pres_unem_2016.png", width=6, height=5)
#ID
filter(pres_unem2016,state=="ID") %>%
  ggplot(pres_unem2016,mapping=aes(y=dem_pct,x=unemployment_rate)) + 
  geom_point() + geom_smooth(method="lm", se=F, color="red") + 
  scale_y_continuous(limits=c(0,1))+ labs(y="Democratic Vote Pct in 2016 Presidential Election",x="Unemployment Rate in 2016")
ggsave("ID_pres_unem_2016.png", width=6, height=5)


#WA
filter(governor_unem2016,state=="WA") %>%
  ggplot(governor_unem2016,mapping=aes(y=dem_pct,x=unemployment_rate)) + 
  geom_point() + geom_smooth(method="lm", se=F, color="red") + 
  scale_y_continuous(limits=c(0,1))+ labs(y="Democratic Vote Pct in Governor Election",x="Unemployment Rate in 2016")
ggsave("WA_gov_unem_2016.png", width=6, height=5)
#CA
filter(governor_unem2016,state=="CA") %>%
  ggplot(governor_unem2016,mapping=aes(y=dem_pct,x=unemployment_rate)) + 
  geom_point() + geom_smooth(method="lm", se=F, color="red") + 
  scale_y_continuous(limits=c(0,1))+ labs(y="Democratic Vote Pct in Governor Election",x="Unemployment Rate in 2016")
ggsave("CA_gov_unem_2016.png", width=6, height=5)
#OR
filter(governor_unem2016,state=="OR") %>%
  ggplot(governor_unem2016,mapping=aes(y=dem_pct,x=unemployment_rate)) + 
  geom_point() + geom_smooth(method="lm", se=F, color="red") + 
  scale_y_continuous(limits=c(0,1))+ labs(y="Democratic Vote Pct in Governor Election",x="Unemployment Rate in 2016")
ggsave("OR_gov_unem_2016.png", width=6, height=5)
#IL
filter(governor_unem2016,state=="IL") %>%
  ggplot(governor_unem2016,mapping=aes(y=dem_pct,x=unemployment_rate)) + 
  geom_point() + geom_smooth(method="lm", se=F, color="red") + 
  scale_y_continuous(limits=c(0,1))+ labs(y="Democratic Vote Pct in Governor Election",x="Unemployment Rate in 2016")
ggsave("IL_gov_unem_2016.png", width=6, height=5)
#NY
filter(governor_unem2016,state=="NY") %>%
  ggplot(governor_unem2016,mapping=aes(y=dem_pct,x=unemployment_rate)) + 
  geom_point() + geom_smooth(method="lm", se=F, color="red") + 
  scale_y_continuous(limits=c(0,1))+ labs(y="Democratic Vote Pct in Governor Election",x="Unemployment Rate in 2016")
ggsave("NY_gov_unem_2016.png", width=6, height=5)
#WY
filter(governor_unem2016,state=="WY") %>%
  ggplot(governor_unem2016,mapping=aes(y=dem_pct,x=unemployment_rate)) + 
  geom_point() + geom_smooth(method="lm", se=F, color="red") + 
  scale_y_continuous(limits=c(0,1))+ labs(y="Democratic Vote Pct in Governor Election",x="Unemployment Rate in 2016")
ggsave("WY_gov_unem_2016.png", width=6, height=5)
#UT
filter(governor_unem2016,state=="UT") %>%
  ggplot(governor_unem2016,mapping=aes(y=dem_pct,x=unemployment_rate)) + 
  geom_point() + geom_smooth(method="lm", se=F, color="red") + 
  scale_y_continuous(limits=c(0,1))+ labs(y="Democratic Vote Pct in Governor Election",x="Unemployment Rate in 2016")
ggsave("UT_gov_unem_2016.png", width=6, height=5)
#OK
filter(governor_unem2016,state=="OK") %>%
  ggplot(governor_unem2016,mapping=aes(y=dem_pct,x=unemployment_rate)) + 
  geom_point() + geom_smooth(method="lm", se=F, color="red") + 
  scale_y_continuous(limits=c(0,1))+ labs(y="Democratic Vote Pct in Governor Election",x="Unemployment Rate in 2016")
ggsave("OK_gov_unem_2016.png", width=6, height=5)
#WV
filter(governor_unem2016,state=="WV") %>%
  ggplot(governor_unem2016,mapping=aes(y=dem_pct,x=unemployment_rate)) + 
  geom_point() + geom_smooth(method="lm", se=F, color="red") + 
  scale_y_continuous(limits=c(0,1))+ labs(y="Democratic Vote Pct in Governor Election",x="Unemployment Rate in 2016")
ggsave("WV_gov_unem_2016.png", width=6, height=5)
#ID
filter(governor_unem2016,state=="ID") %>%
  ggplot(governor_unem2016,mapping=aes(y=dem_pct,x=unemployment_rate)) + 
  geom_point() + geom_smooth(method="lm", se=F, color="red") + 
  scale_y_continuous(limits=c(0,1))+ labs(y="Democratic Vote Pct in Governor Election",x="Unemployment Rate in 2016")
ggsave("ID_gov_unem_2016.png", width=6, height=5)