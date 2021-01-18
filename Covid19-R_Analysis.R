library(data.table)
library(ggplot2)
library(dplyr)
library(viridis)
library(countrycode)
library(hrbrthemes)
library(scales)
library(zoo)
library(ggpubr)
library(TSrepr)
library("tsmp")
library(lubridate)
library(forcats)
library(fpp2)
library(mapproj)
library(tmap)
library(grid)
library(rworldmap)
library(wpp2019)

################################################# ???????????????????????????? #################################################

rm(list = ls())

width = 30
height = 15

covid1 <- read.csv(file = 'time_series_covid19_confirmed_global.csv')
covid2 <- read.csv(file = 'time_series_covid19_deaths_global.csv')

covid_confirmed <- as.data.table(covid1)
covid_global <- as.data.table(covid2)

# ????????_1

covid_confirmed[, c("Province.State","Lat","Long"):=NULL]
covid_global[, c("Province.State","Lat","Long"):=NULL]

names(covid_confirmed) <- gsub("X", "", names(covid_confirmed), fixed = TRUE)
names(covid_global) <- gsub("X", "", names(covid_global), fixed = TRUE)



#????????_2
colsc <- colnames(covid_confirmed)
colsg <- colnames(covid_global)

covid_confirmed.m = melt(covid_confirmed, id.vars = colnames(covid_confirmed[,1]),
             measure.vars = colnames(covid_confirmed[,2:length(colsc)]))

covid_global.m = melt(covid_global, id.vars = colnames(covid_global[,1]),
                          measure.vars = colnames(covid_global[,2:length(colsg)]))

#????????_3

setnames(covid_confirmed.m, "Country.Region", "Country")
setnames(covid_global.m, "Country.Region", "Country")



#????????_4


setnames(covid_confirmed.m, "value", "confirmed")
setnames(covid_global.m, "value", "deaths")

#????????_5

setnames(covid_confirmed.m, "variable", "date")
setnames(covid_global.m, "variable", "date")

covid_confirmed.m$date <- as.Date(covid_confirmed.m$date, format = "%m.%d.%y")
covid_global.m$date <- as.Date(covid_global.m$date, format = "%m.%d.%y")

#????????_6

covid_confirmed.m <- covid_confirmed.m %>% group_by(Country,date) %>% summarise(confirmed = sum(confirmed))
covid_global.m <- covid_global.m %>% group_by(Country,date) %>% summarise(deaths = sum(deaths))

#BHMA_7

covid <- merge(covid_confirmed.m,covid_global.m,by=c("Country","date"))
covid <- as.data.table(covid)

#????????_8

world <- covid[covid$date==max(covid$date),]
world[, date:=NULL]

#???????? 9 - ???????? ?????????????????????????????? ?????? 

#????????_10

covid <- covid%>%group_by(Country)%>%mutate(confirmed.ind = confirmed - lag(confirmed))
covid <- covid%>%group_by(Country)%>%mutate(deaths.inc = deaths - lag(deaths))

covid[covid$date==min(covid$date),5]<-covid[covid$date==min(covid$date),3]
covid[covid$date==min(covid$date),6]<-covid[covid$date==min(covid$date),4]




################################################# ?????????????????? Dataset #################################################

num_countries<-nrow(world)
num_countries


world$continent <- countrycode(sourcevar = world$Country, origin = "country.name", destination = "continent")
world$continent[world$Country=='Diamond Princess']='Ship'
world$continent[world$Country=='MS Zaandam']='Ship'
world$continent[world$Country=='Kosovo']='Europe'

Europe<-world[continent=="Europe",.N]
Asia<-world[continent=="Asia",.N]
Africa<-world[continent=="Africa",.N]
Oceania<-world[continent=="Oceania",.N]
Americas<-world[continent=="Americas",.N]

countr <- data.table(
  group=c("Europe","Asia","Africa","Oceania","Americas"),
  value=c(Europe,Asia,Africa,Oceania,Americas)
)

countr <- countr %>%
  arrange(desc(group)) %>%
  mutate(lab = cumsum(value) - 0.5*value)

ggplot(countr, aes(x="", y=value, fill=group)) +
  geom_bar(stat="identity", width=1,color="white") +
  labs(fill="class", 
       x=NULL, 
       y=NULL, 
       title="Continent Participation")+
  geom_text(aes(y=lab,label = value),color="white")+
  coord_polar("y", start=0)+
  theme_void()

ggsave('R_images/pie_chart.png', width = width, height = height, units="cm")

world_sorted_deaths<-world[order(continent,-deaths)]

top5cases_per_continent<-world%>%
  group_by(continent) %>%
  arrange(desc(confirmed)) %>%
  slice(1:5)


topEuropecases<-top5cases_per_continent[top5cases_per_continent$continent=="Europe",]
topAfricacases<-top5cases_per_continent[top5cases_per_continent$continent=="Africa",]
topAsiacases<-top5cases_per_continent[top5cases_per_continent$continent=="Asia",]
topAmericascases<-top5cases_per_continent[top5cases_per_continent$continent=="Americas",]
topOceaniacases<-top5cases_per_continent[top5cases_per_continent$continent=="Oceania",]


meltAfrica<-melt.data.table(as.data.table(topAfricacases), id.vars = c("Country", "continent"), measure.vars = c("deaths", "confirmed"))
meltEurope<-melt.data.table(as.data.table(topEuropecases), id.vars = c("Country", "continent"), measure.vars = c("deaths", "confirmed"))
meltOceania<-melt.data.table(as.data.table(topOceaniacases), id.vars = c("Country", "continent"), measure.vars = c("deaths", "confirmed"))
meltAmericas<-melt.data.table(as.data.table(topAmericascases), id.vars = c("Country", "continent"), measure.vars = c("deaths", "confirmed"))
meltAsia<-melt.data.table(as.data.table(topAsiacases), id.vars = c("Country", "continent"), measure.vars = c("deaths", "confirmed"))

plotAfrica<-ggplot(meltAfrica, aes(fill=meltAfrica$variable, y=meltAfrica$value, x=meltAfrica$Country)) + 
  geom_bar(position="stack", stat="identity",alpha=.6, width=.4) +
  theme_bw() +
  xlab("")+
  labs(title = "Africa", 
       y = "", x = "", fill = "")+
  coord_flip() +
  theme(legend.position="none")

plotAfrica

ggsave('R_images/plot_Africa.png', width = width, height = height, units="cm")

plotEurope<-ggplot(meltEurope, aes(fill=meltEurope$variable, y=meltEurope$value, x=meltEurope$Country)) + 
  geom_bar(position="stack", stat="identity",alpha=.6, width=.4) +
  theme_bw() +
  xlab("")+
  labs(title = "Europe", 
       y = "", x = "", fill = "")+
  coord_flip() +
  theme(legend.position="none")

plotEurope

ggsave('R_images/plot_Europe.png', width = width, height = height, units="cm")

plotOceania<-ggplot(meltOceania, aes(fill=meltOceania$variable, y=meltOceania$value, x=meltOceania$Country)) + 
  geom_bar(position="stack", stat="identity",alpha=.6, width=.4) +
  theme_bw() +
  xlab("")+
  labs(title = "Oceania", 
       y = "", x = "", fill = "")+
  coord_flip() +
  theme(legend.position="none")

plotOceania

ggsave('R_images/plot_Oceania.png', width = width, height = height, units="cm")

plotAsia<-ggplot(meltAsia, aes(fill=meltAsia$variable, y=meltAsia$value, x=meltAsia$Country)) + 
  geom_bar(position="stack", stat="identity",alpha=.6, width=.4) +
  theme_bw() +
  xlab("")+
  labs(title = "Asia", 
       y = "", x = "", fill = "")+
  coord_flip() +
  theme(legend.position="none")

plotAsia

ggsave('R_images/plot_Asia.png', width = width, height = height, units="cm")

plotAmericas<-ggplot(meltAmericas, aes(fill=meltAmericas$variable, y=meltAmericas$value, x=meltAmericas$Country)) + 
  geom_bar(position="stack", stat="identity",alpha=.6, width=.4) +
  theme_bw() +
  xlab("")+
  labs(title = "Americas", 
       y = "", x = "", fill = "")+
  coord_flip() +
  theme(legend.position="bottom")

plotAmericas

ggsave('R_images/plot_Americas.png', width = width, height = height, units="cm")


################################################# ?????????????????? ?????????????? #################################################


agg <- covid %>% group_by(date) %>% summarize(confirmed.ind=sum(confirmed.ind), deaths.inc=sum(deaths.inc)) %>% mutate(days = date - first(date) + 1)
agg <- agg %>% mutate(rolling_conf = rollmean(confirmed.ind, k = 7, fill = NA))
agg <- agg %>% mutate(rolling_death = rollmean(deaths.inc, k = 7, fill = NA))

agg<-as.data.table(agg)

ggplot(mapping = aes()) +
  geom_line(data = agg, mapping = aes(x=date, y=deaths.inc,color="Deaths"),size=0.8) +
  geom_line(data = agg, mapping = aes(x=date, y=rolling_death,color="Trend"),size=0.8)+
  geom_point() + 
  labs(x = "date (month/year)",
       y = "deaths",
       color = "Legend",
       title = "Daily COVID Deaths") +
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%m/%y")) +
  scale_y_continuous(breaks = seq(0,max(agg$deaths.inc), by=1000)) +
  theme_minimal()

ggsave('R_images/World_death.png', width = width, height = height, units="cm")

ggplot(mapping = aes()) +
  geom_line(data = agg, mapping = aes(x=date, y=confirmed.ind,color="Confirmed"),size=0.8) +
  geom_line(data = agg, mapping = aes(x=date, y=rolling_conf,color="Trend"),size=0.8)+
  labs(x = "date (month/year)",
       y = "confirmed",
       color="Legend",
       title = "Daily COVID Confirmed Cases") +
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%m/%y")) +
  scale_y_continuous(breaks = seq(0,max(agg$confirmed.ind)+10000, by=100000)) +
  scale_color_manual(values=c("chocolate3", "springgreen2"))+
  theme_minimal()

ggsave('R_images/world_cases.png', width = width, height = height, units="cm")

################################################# ???????????? ################################################# 


worldMap <- getMap()


data(pop)
population<-pop[,c("name", "2020")]
setnames(population,"2020","twenty")
world$pop<-population$twenty[match(world$Country,population$name)]

world[Country=="Czechia"]$Country="Czech Rep."
world[Country=="Russia"]$pop=145934.460
world[Country=="Andorra"]$pop=77.006 
world[Country=="Czech Rep."]$pop=10708.982 
world[Country=="Kosovo"]$pop=1873.160
world[Country=="Moldova"]$pop=2640.438


world <- transform(world, c_ratio =confirmed /(1000*pop))
world <- transform(world, d_ratio =deaths /(1000*pop))
eu<-world[continent=="Europe"]$Country

indEU <- which(worldMap$NAME%in%eu)
europeCoords <- lapply(indEU, function(i){
  df <- data.frame(worldMap@polygons[[i]]@Polygons[[1]]@coords)
  df$region =as.character(worldMap$NAME[i])
  colnames(df) <- list("long", "lat", "region")
  return(df)
})

world[Country=="Czech Rep."]
world[Country=="Belgium"]


europeCoords <- do.call("rbind", europeCoords)
europeCoords$Deaths <- world$d_ratio[match(europeCoords$region,world$Country)]
europeCoords$Cases<-world$c_ratio[match(europeCoords$region,world$Country)]




P <- ggplot() + geom_polygon(data = europeCoords, aes(x = long, y = lat, group = region, fill = Deaths),
                             colour = "black", size = 0.1) +
  coord_map(xlim = c(-13, 35),  ylim = c(32, 71))+ 
  theme_minimal()+
  scale_fill_gradient(name = "Deaths", low = "#FFFF00FF", high = "#FF0000FF", na.value = "grey50")+
  theme(#panel.grid.minor = element_line(colour = NA), panel.grid.minor = element_line(colour = NA),
    #panel.background = element_rect(fill = NA, colour = NA),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(), axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(), axis.title = element_blank(),
    #rect = element_blank(),
    plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines"))+
  labs(title = "Deaths per Population ")

P
ggsave('R_images/death_ratio.png', width = width, height = height, units="cm")

P1<- ggplot() + geom_polygon(data = europeCoords, aes(x = long, y = lat, group = region, fill = Cases),
                             colour = "black", size = 0.1) +
  coord_map(xlim = c(-13, 35),  ylim = c(32, 71))+ 
  theme_minimal()+
  scale_fill_gradient(name = "Confirmed", low = "green", high = "purple", na.value = "white")+
  theme(#panel.grid.minor = element_line(colour = NA), panel.grid.minor = element_line(colour = NA),
    #panel.background = element_rect(fill = NA, colour = NA),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(), axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(), axis.title = element_blank(),
    #rect = element_blank(),
    plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines"))+
  labs(title = "Confirmed Cases per Population")
P1
ggsave('R_images/cases_ratio.png', width = width, height = height, units="cm")


Europe<-world[continent=="Europe"]
Europe<-Europe[order(confirmed)]

Europe_plot_cases<-Europe %>%
  arrange(confirmed) %>%
  mutate(state = factor(Country, unique(Country))) %>%
  ggplot() + aes(x=Country, y=confirmed) +
  geom_segment( aes(x=state, xend=state, y=0, yend=confirmed), color="dodgerblue4") +
  geom_point( color="dodgerblue4", size=4, alpha=0.6) +
  geom_text(aes(label = confirmed), hjust = -0.25,size=3) + 
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank()
  )+
  labs(y = "Confirmed Cases")

Europe_plot_cases
ggsave('R_images/Europe_cases_rank.png', width = width, height = height, units="cm")

Europe_plot_Deaths<-Europe %>%
  arrange(deaths) %>%
  mutate(state = factor(Country, unique(Country))) %>%
  ggplot() + aes(x=Country, y=deaths) +
  geom_segment( aes(x=state, xend=state, y=0, yend=deaths), color="#D55E00") +
  geom_point( color="#D55E00", size=4, alpha=0.6) +
  geom_text(aes(label = deaths), hjust = -0.25,size=3) + 
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank()
  )+
  labs(y = "Deaths")

Europe_plot_Deaths
ggsave('R_images/Europe_deaths_rank.png', width = width, height = height, units="cm")




Russia <- covid[covid$Country=="Russia",] %>% group_by(date) %>% summarize(confirmed.ind=sum(confirmed.ind), deaths.inc=sum(deaths.inc)) %>% mutate(days = date - first(date) + 1)
UK <- covid[covid$Country=="United Kingdom",] %>% group_by(date) %>% summarize(confirmed.ind=sum(confirmed.ind), deaths.inc=sum(deaths.inc)) %>% mutate(days = date - first(date) + 1)
France<-covid[covid$Country=="France",] %>% group_by(date) %>% summarize(confirmed.ind=sum(confirmed.ind), deaths.inc=sum(deaths.inc)) %>% mutate(days = date - first(date) + 1)
Italy<-covid[covid$Country=="Italy",] %>% group_by(date) %>% summarize(confirmed.ind=sum(confirmed.ind), deaths.inc=sum(deaths.inc)) %>% mutate(days = date - first(date) + 1)
Spain<-covid[covid$Country=="Spain",] %>% group_by(date) %>% summarize(confirmed.ind=sum(confirmed.ind), deaths.inc=sum(deaths.inc)) %>% mutate(days = date - first(date) + 1)
Greece<-covid[covid$Country=="Greece",] %>% group_by(date) %>% summarize(confirmed.ind=sum(confirmed.ind), deaths.inc=sum(deaths.inc)) %>% mutate(days = date - first(date) + 1)


Spain<-as.data.table(Spain)
Spain[, ConfiCum := cumsum(confirmed.ind)]
Spain[, DeathsCum := cumsum(deaths.inc)]

Russia<-as.data.table(Russia)
Russia[, ConfiCum := cumsum(confirmed.ind)]
Russia[, DeathsCum := cumsum(deaths.inc)]

UK<-as.data.table(UK)
UK[, ConfiCum := cumsum(confirmed.ind)]
UK[, DeathsCum := cumsum(deaths.inc)]

Italy<-as.data.table(Italy)
Italy[, ConfiCum := cumsum(confirmed.ind)]
Italy[, DeathsCum := cumsum(deaths.inc)]

France<-as.data.table(France)
France[, ConfiCum := cumsum(confirmed.ind)]
France[, DeathsCum := cumsum(deaths.inc)]

Greece<-as.data.table(Greece)
Greece[, ConfiCum := cumsum(confirmed.ind)]
Greece[, DeathsCum := cumsum(deaths.inc)]

S_lockdown1 = mdy("3/14/20")
S_lockdown2 = mdy("10/25/20")

S_Deaths_lockdown1<-Spain[date==S_lockdown1]$DeathsCum
S_Deaths_lockdown2<-Spain[date==S_lockdown2]$DeathsCum

Spain_Cases<-ggplot(mapping = aes()) +
  geom_area(data = Spain, mapping = aes(x=date, y=ConfiCum),fill = "lightblue") +
  geom_point() + 
  labs(x = "date (month)",
       y = "Cases",
       color = "Legend",
       title = "Cumulative Cases in Spain") +
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%m")) +
  geom_vline(size=0.8, mapping = aes(xintercept=as.numeric(S_lockdown1), color="National Lockdown"), show.legend = T) +
  geom_vline(size=0.8, mapping = aes(xintercept=as.numeric(S_lockdown2), color="National Curfew"), show.legend = T)+
  scale_y_continuous(breaks = seq(min(Spain$ConfiCum),max(Spain$ConfiCum), by=100000)) +
  theme_minimal()+
  annotate("label", x = S_lockdown1, y = 1500000, label = "Deaths = 195") +
  annotate("label", x = S_lockdown2, y = 1500000, label = "Deaths = 34752")

Spain_Cases
ggsave('R_images/Spain_cases.png', width = width, height = height, units="cm")


Spainlock1<-as.data.table(Spain)[date>S_lockdown1 & date<S_lockdown1+60]
Spainlock2<-as.data.table(Spain)[date>S_lockdown2 & date<S_lockdown2+60]

Spainlock1<-Spainlock1 %>% group_by(week = floor_date(date, unit="week")) %>% summarise(mean_cases = mean(confirmed.ind),weekly_cases=sum(confirmed.ind))
Spainlock2<-Spainlock2 %>% group_by(week = floor_date(date-1, unit="week")) %>% summarise(mean_cases = mean(confirmed.ind),weekly_cases=sum(confirmed.ind))

#Spainlock1<-Spainlock1 %>% group_by(week = cut(date, "week",start.on.monday = FALSE)) %>% summarise(mean_cases = mean(confirmed.ind),weekly_cases=sum(confirmed.ind))

Spainlock1<-Spainlock1[1:8,]

#Spainlock2<-Spainlock2 %>% group_by(week = cut(date, "week",start.on.monday = TRUE)) %>% summarise(mean_cases = mean(confirmed.ind),weekly_cases=sum(confirmed.ind))

Spainlock2<-Spainlock2[1:8,]

Spain_lock1<-ggplot(mapping = aes()) +
  geom_bar(data = Spainlock1, mapping = aes(x=as.Date(week), y=weekly_cases),'identity',fill = "grey70") +
  geom_line(data = Spainlock1, mapping = aes(x=as.Date(week), y=mean_cases,color="Mean Cases"),size=2.5)+
  geom_point( data=Spainlock1, aes(x= as.Date(week),y=mean_cases), size=3.0, colour="white" ) + 
  geom_text(data=Spainlock1,aes(x=as.Date(week), y=weekly_cases+700,label=weekly_cases),vjust=0)+
  geom_text(data=Spainlock1,aes(x=as.Date(week), y=mean_cases+1000,label=round(mean_cases)),vjust=0)+
  labs(x = "date (day/month)",
       y = "Covid Cases",
       color = "Legend",
       title = "Weekly Cases after Lockdown in Spain") +
  scale_x_date(breaks = seq(min(Spainlock1$week),max(Spainlock1$week),by=7), labels = date_format("%d/%m")) +
  scale_y_continuous(breaks = seq(0,max(Spainlock1$weekly_cases), by=3000)) +
  theme_minimal()

Spain_lock1
ggsave('R_images/Spain_lock1.png', width = width, height = height, units="cm")

Spain_lock2<-ggplot(mapping = aes()) +
  geom_bar(data = Spainlock2, mapping = aes(x=as.Date(week)+1, y=weekly_cases),'identity',fill = "grey70") +
  geom_line(data = Spainlock2, mapping = aes(x=as.Date(week)+1, y=mean_cases,color="Mean Cases"),size=2.5)+
  geom_point( data=Spainlock2, aes(x= as.Date(week)+1,y=mean_cases), size=3.0, colour="white" ) + 
  geom_text(data=Spainlock2,aes(x=as.Date(week)+1, y=weekly_cases+700,label=weekly_cases),vjust=0)+
  geom_text(data=Spainlock2,aes(x=as.Date(week), y=mean_cases+1000,label=round(mean_cases)),vjust=0)+
  labs(x = "date (day/month)",
       y = "Covid Cases",
       color = "Legend",
       title = "Weekly Cases after National Curfew in Spain") +
  scale_x_date(breaks = seq(min(Spainlock2$week)+1,max(Spainlock2$week)+1,by=7), labels = date_format("%d/%m")) +
  scale_y_continuous(breaks = seq(0,max(Spainlock2$weekly_cases), by=30000)) +
  theme_minimal()

Spain_lock2

ggsave('R_images/Spain.png', width = width, height = height, units="cm")

I_lockdown1 = mdy("3/9/20")
I_lockdown2 = mdy("11/4/20")

I_Deaths_lockdown1<-Italy[date==I_lockdown1]$DeathsCum
I_Deaths_lockdown2<-Italy[date==I_lockdown2]$DeathsCum

Italy_Cases<-ggplot(mapping = aes()) +
  geom_area(data = Italy, mapping = aes(x=date, y=ConfiCum),fill = "lightblue") +
  geom_point() + 
  labs(x = "date (month)",
       y = "Cases",
       color = "Legend",
       title = "Cumulative Cases in Italy") +
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%m")) +
  geom_vline(size=0.8, mapping = aes(xintercept=as.numeric(I_lockdown1), color="National Lockdown"), show.legend = T) +
  geom_vline(size=0.8, mapping = aes(xintercept=as.numeric(I_lockdown2), color="Three Zone Lockdown"), show.legend = T)+
  scale_y_continuous(breaks = seq(min(Italy$ConfiCum),max(Italy$ConfiCum), by=100000)) +
  theme_minimal()+
  annotate("label", x = I_lockdown1, y = 1500000, label = "Deaths = 195") +
  annotate("label", x = I_lockdown2, y = 1500000, label = "Deaths = 34752")

Italy_Cases
ggsave('R_images/Italy_plot.png', width = width, height = height, units="cm")

Italylock1<-as.data.table(Italy)[date>I_lockdown1 & date<I_lockdown1+60]
Italylock2<-as.data.table(Italy)[date>I_lockdown2 & date<I_lockdown2+60]

Italylock1<-Italylock1 %>% group_by(week = floor_date(date-2, unit="week")) %>% summarise(mean_cases = mean(confirmed.ind),weekly_cases=sum(confirmed.ind))
Italylock2<-Italylock2 %>% group_by(week = floor_date(date-4, unit="week")) %>% summarise(mean_cases = mean(confirmed.ind),weekly_cases=sum(confirmed.ind))


#Italylock1<-Italylock1 %>% group_by(week = cut(date, "week",start.on.monday=4)) %>% summarise(mean_cases = mean(confirmed.ind),weekly_cases=sum(confirmed.ind))

Italylock1<-Italylock1[1:8,]

#Italylock2<-Italylock2 %>% group_by(week = cut(date, "week",start.on.thursday = TRUE)) %>% summarise(mean_cases = mean(confirmed.ind),weekly_cases=sum(confirmed.ind))

Italylock2<-Italylock2[1:8,]

Italy_lock1<-ggplot(mapping = aes()) +
  geom_bar(data = Italylock1, mapping = aes(x=as.Date(week)+2, y=weekly_cases),'identity',fill = "grey70") +
  geom_line(data = Italylock1, mapping = aes(x=as.Date(week)+2, y=mean_cases,color="Mean Cases"),size=2.5)+
  geom_point( data=Italylock1, aes(x= as.Date(week)+2,y=mean_cases), size=3.0, colour="white" ) + 
  geom_text(data=Italylock1,aes(x=as.Date(week)+2, y=weekly_cases+700,label=weekly_cases),vjust=0)+
  geom_text(data=Italylock1,aes(x=as.Date(week)+2, y=mean_cases+1000,label=round(mean_cases)),vjust=0)+
  labs(x = "date (day/month)",
       y = "Covid Cases",
       color = "Legend",
       title = "Weekly Cases after Lockdown in Italy") +
  scale_x_date(breaks = seq(min(Italylock1$week)+2,max(Italylock1$week)+2,by=7), labels = date_format("%d/%m")) +
  scale_y_continuous(breaks = seq(0,max(Italylock1$weekly_cases), by=3000)) +
  theme_minimal()

Italy_lock1
ggsave('R_images/Italy_lock1.png', width = width, height = height, units="cm")

Italy_lock2<-ggplot(mapping = aes()) +
  geom_bar(data = Italylock2, mapping = aes(x=as.Date(week)+4, y=weekly_cases),'identity',fill = "grey70") +
  geom_line(data = Italylock2, mapping = aes(x=as.Date(week)+4, y=mean_cases,color="Mean Cases"),size=2.5)+
  geom_point( data=Italylock2, aes(x= as.Date(week)+4,y=mean_cases), size=3.0, colour="white" ) + 
  geom_text(data=Italylock2,aes(x=as.Date(week)+4, y=weekly_cases+700,label=weekly_cases),vjust=0)+
  geom_text(data=Italylock2,aes(x=as.Date(week)+4, y=mean_cases+1000,label=round(mean_cases)),vjust=0)+
  labs(x = "date (day/month)",
       y = "Covid Cases",
       color = "Legend",
       title = "Weekly Cases after Three Zone Lockdown in Italy") +
  scale_x_date(breaks = seq(min(Italylock2$week)+4,max(Italylock2$week)+4,by=7), labels = date_format("%d/%m")) +
  scale_y_continuous(breaks = seq(0,max(Italylock2$weekly_cases), by=15000)) +
  theme_minimal()

Italy_lock2
ggsave('R_images/Italy_lock2.png', width = width, height = height, units="cm")

U_lockdown1 = mdy("3/23/20")
U_lockdown2 = mdy("11/5/20")

U_Deaths_lockdown1<-UK[date==U_lockdown1]$DeathsCum
U_Deaths_lockdown2<-UK[date==U_lockdown2]$DeathsCum

UK_Cases<-ggplot(mapping = aes()) +
  geom_area(data = UK, mapping = aes(x=date, y=ConfiCum),fill = "lightblue") +
  geom_point() + 
  labs(x = "date (month)",
       y = "Cases",
       color = "Legend",
       title = "Cumulative Cases in UK") +
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%m")) +
  geom_vline(size=0.8, mapping = aes(xintercept=as.numeric(U_lockdown1), color="1st National Lockdown"), show.legend = T) +
  geom_vline(size=0.8, mapping = aes(xintercept=as.numeric(U_lockdown2), color="2nd National Lockdown"), show.legend = T)+
  scale_y_continuous(breaks = seq(min(UK$ConfiCum),max(UK$ConfiCum), by=300000)) +
  theme_minimal()+
  annotate("label", x = U_lockdown1, y = 1500000, label = "Deaths = 365") +
  annotate("label", x = U_lockdown2, y = 1500000, label = "Deaths = 48210")

UK_Cases
ggsave('R_images/UK_cases.png', width = width, height = height, units="cm")

UKlock1<-as.data.table(UK)[date>U_lockdown1 & date<U_lockdown1+60]
UKlock2<-as.data.table(UK)[date>U_lockdown2 & date<U_lockdown2+60]

UKlock1<-UKlock1 %>% group_by(week = floor_date(date-2, unit="week")) %>% summarise(mean_cases = mean(confirmed.ind),weekly_cases=sum(confirmed.ind))
UKlock2<-UKlock2 %>% group_by(week = floor_date(date-5, unit="week")) %>% summarise(mean_cases = mean(confirmed.ind),weekly_cases=sum(confirmed.ind))

#UKlock1<-UKlock1 %>% group_by(week = cut(date, "week",start.on.tuesday = TRUE)) %>% summarise(mean_cases = mean(confirmed.ind),weekly_cases=sum(confirmed.ind))

UKlock1<-UKlock1[1:8,]

#UKlock2<-UKlock2 %>% group_by(week = cut(date, "week",start.on.friday = TRUE)) %>% summarise(mean_cases = mean(confirmed.ind),weekly_cases=sum(confirmed.ind))

UKlock2<-UKlock2[1:8,]

UK_lock1<-ggplot(mapping = aes()) +
  geom_bar(data = UKlock1, mapping = aes(x=as.Date(week)+2, y=weekly_cases),'identity',fill = "grey70") +
  geom_line(data = UKlock1, mapping = aes(x=as.Date(week)+2, y=mean_cases,color="Mean Cases"),size=2.5)+
  geom_point( data=UKlock1, aes(x= as.Date(week)+2,y=mean_cases), size=3.0, colour="white" ) + 
  geom_text(data=UKlock1,aes(x=as.Date(week)+2, y=weekly_cases+700,label=weekly_cases),vjust=0)+
  geom_text(data=UKlock1,aes(x=as.Date(week)+2, y=mean_cases+1000,label=round(mean_cases)),vjust=0)+
  labs(x = "date (day/month)",
       y = "Covid Cases",
       color = "Legend",
       title = "Weekly Cases after First Lockdown in UK") +
  scale_x_date(breaks = seq(min(UKlock1$week)+2,max(UKlock1$week)+2,by=7), labels = date_format("%d/%m")) +
  scale_y_continuous(breaks = seq(0,max(UKlock1$weekly_cases), by=3000)) +
  theme_minimal()

UK_lock1
ggsave('R_images/UK_lock1.png', width = width, height = height, units="cm")

UK_lock2<-ggplot(mapping = aes()) +
  geom_bar(data = UKlock2, mapping = aes(x=as.Date(week)+5, y=weekly_cases),'identity',fill = "grey70") +
  geom_line(data = UKlock2, mapping = aes(x=as.Date(week)+5, y=mean_cases,color="Mean Cases"),size=2.5)+
  geom_point( data=UKlock2, aes(x= as.Date(week)+5,y=mean_cases), size=3.0, colour="white" ) + 
  geom_text(data=UKlock2,aes(x=as.Date(week)+5, y=weekly_cases+700,label=weekly_cases),vjust=0)+
  geom_text(data=UKlock2,aes(x=as.Date(week)+5, y=mean_cases+1000,label=round(mean_cases)),vjust=0)+
  labs(x = "date (day/month)",
       y = "Covid Cases",
       color = "Legend",
       title = "Weekly Cases after Second Lockdown in UK") +
  scale_x_date(breaks = seq(min(UKlock2$week)+5,max(UKlock2$week)+5,by=7), labels = date_format("%d/%m")) +
  scale_y_continuous(breaks = seq(0,max(UKlock2$weekly_cases), by=15000)) +
  theme_minimal()

UK_lock2
ggsave('R_images/UK_lock2.png', width = width, height = height, units="cm")


G_lockdown1 = mdy("3/22/20")
G_lockdown2 = mdy("11/5/20")

G_Deaths_lockdown1<-Greece[date==U_lockdown1]$DeathsCum
G_Deaths_lockdown2<-Greece[date==U_lockdown2]$DeathsCum

Greece_Cases<-ggplot(mapping = aes()) +
  geom_area(data = Greece, mapping = aes(x=date, y=ConfiCum),fill = "lightblue") +
  geom_point() + 
  labs(x = "date (month)",
       y = "Cases",
       color = "Legend",
       title = "Cumulative Cases in Greece") +
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%m")) +
  geom_vline(size=0.8, mapping = aes(xintercept=as.numeric(G_lockdown1), color="1st National Lockdown"), show.legend = T) +
  geom_vline(size=0.8, mapping = aes(xintercept=as.numeric(G_lockdown2), color="2nd National Lockdown"), show.legend = T)+
  scale_y_continuous(breaks = seq(min(Greece$ConfiCum),max(Greece$ConfiCum), by=30000)) +
  theme_minimal()+
  annotate("label", x = G_lockdown1, y = 90000, label = "Deaths = 17") +
  annotate("label", x = G_lockdown2, y = 90000, label = "Deaths = 702")

Greece_Cases
ggsave('R_images/Greece_plot.png', width = width, height = height, units="cm")

Greecelock1<-as.data.table(Greece)[date>G_lockdown1 & date<U_lockdown1+60]
Greecelock2<-as.data.table(Greece)[date>G_lockdown2 & date<U_lockdown2+60]

Greecelock1<-Greecelock1 %>% group_by(week = floor_date(date-1, unit="week")) %>% summarise(mean_cases = mean(confirmed.ind),weekly_cases=sum(confirmed.ind))
Greecelock2<-Greecelock2 %>% group_by(week = floor_date(date-5, unit="week")) %>% summarise(mean_cases = mean(confirmed.ind),weekly_cases=sum(confirmed.ind))

#Greecelock1<-Greecelock1 %>% group_by(week = cut(date, "week",start.on.monday = TRUE)) %>% summarise(mean_cases = mean(confirmed.ind),weekly_cases=sum(confirmed.ind))

Greecelock1<-Greecelock1[1:8,]

#Greecelock2<-Greecelock2 %>% group_by(week = cut(date, "week",start.on.friday = TRUE)) %>% summarise(mean_cases = mean(confirmed.ind),weekly_cases=sum(confirmed.ind))

Greecelock2<-Greecelock2[1:8,]

Greece_lock1<-ggplot(mapping = aes()) +
  geom_bar(data = Greecelock1, mapping = aes(x=as.Date(week)+1, y=weekly_cases),'identity',fill = "grey70") +
  geom_line(data = Greecelock1, mapping = aes(x=as.Date(week)+1, y=mean_cases,color="Mean Cases"),size=2.5)+
  geom_point( data=Greecelock1, aes(x= as.Date(week)+1,y=mean_cases), size=3.0, colour="white" ) + 
  geom_text(data=Greecelock1,aes(x=as.Date(week)+1, y=weekly_cases+30,label=weekly_cases),vjust=0)+
  geom_text(data=Greecelock1,aes(x=as.Date(week)+1, y=mean_cases+30,label=round(mean_cases)),vjust=0)+
  labs(x = "date (day/month)",
       y = "Covid Cases",
       color = "Legend",
       title = "Weekly Cases after First Lockdown in Greece") +
  scale_x_date(breaks = seq(min(Greecelock1$week)+1,max(Greecelock1$week)+1,by=7), labels = date_format("%d/%m")) +
  scale_y_continuous(breaks = seq(0,max(Greecelock1$weekly_cases), by=50)) +
  theme_minimal()

Greece_lock1
ggsave('R_images/Greece_lock1.png', width = width, height = height, units="cm")

Greece_lock2<-ggplot(mapping = aes()) +
  geom_bar(data = Greecelock2, mapping = aes(x=as.Date(week)+5, y=weekly_cases),'identity',fill = "grey70") +
  geom_line(data = Greecelock2, mapping = aes(x=as.Date(week)+5, y=mean_cases,color="Mean Cases"),size=2.5)+
  geom_point( data=Greecelock2, aes(x= as.Date(week)+5,y=mean_cases), size=3.0, colour="white" ) + 
  geom_text(data=Greecelock2,aes(x=as.Date(week)+5, y=weekly_cases+700,label=weekly_cases),vjust=0)+
  geom_text(data=Greecelock2,aes(x=as.Date(week)+5, y=mean_cases+1000,label=round(mean_cases)),vjust=0)+
  labs(x = "date (day/month)",
       y = "Covid Cases",
       color = "Legend",
       title = "Weekly Cases after Second Lockdown in Greece") +
  scale_x_date(breaks = seq(min(Greecelock2$week)+5,max(Greecelock2$week)+5,by=7), labels = date_format("%d/%m")) +
  scale_y_continuous(breaks = seq(0,max(Greecelock2$weekly_cases), by=3000)) +
  theme_minimal()

Greece_lock2
ggsave('R_images/Greece_lock2.png', width = width, height = height, units="cm")

Europe<-world[continent=="Europe"]
Europe<-Europe[order(confirmed)]

Spain$day <- weekdays(as.Date(Spain$date))
Italy$day <- weekdays(as.Date(Italy$date))
UK$day <- weekdays(as.Date(UK$date))
Greece$day <- weekdays(as.Date(Greece$date))

meltSpain<-melt.data.table(as.data.table(Spain), id.vars = c("date", "days","day","ConfiCum","DeathsCum"))

S_boxplot<-ggplot(meltSpain, aes(x=day, y=value, fill=variable)) + 
  geom_boxplot()+
  theme_minimal()+
  labs(y = "Confirmed/Deaths")+
  scale_fill_discrete(labels = c("confirmed_cases", "deaths"))+
  labs(title = "Cases/Deaths per Week Day in Spain")

S_boxplot

ggsave('R_images/Spain_box.png', width = width, height = height, units="cm")

meltItaly<-melt.data.table(as.data.table(Italy), id.vars = c("date", "days","day","ConfiCum","DeathsCum"))

I_boxplot<-ggplot(meltItaly, aes(x=day, y=value, fill=variable)) + 
  geom_boxplot()+
  theme_minimal()+
  labs(y = "Confirmed/Deaths")+
  scale_fill_discrete(labels = c("confirmed_cases", "deaths"))+
  labs(title = "Cases/Deaths per Week Day in Italy")

I_boxplot
ggsave('R_images/Italy_box.png', width = width, height = height, units="cm")

meltUK<-melt.data.table(as.data.table(UK), id.vars = c("date", "days","day","ConfiCum","DeathsCum"))

U_boxplot<-ggplot(meltUK, aes(x=day, y=value, fill=variable)) + 
  geom_boxplot()+
  theme_minimal()+
  labs(y = "Confirmed/Deaths")+
  scale_fill_discrete(labels = c("confirmed_cases", "deaths"))+
  labs(title = "Cases/Deaths per Week Day in UK")

U_boxplot
ggsave('R_images/UK_box.png', width = width, height = height, units="cm")

meltGreece<-melt.data.table(as.data.table(Greece), id.vars = c("date", "days","day","ConfiCum","DeathsCum"))

G_boxplot<-ggplot(meltUK, aes(x=day, y=value, fill=variable)) + 
  geom_boxplot()+
  theme_minimal()+
  labs(y = "Confirmed/Deaths")+
  scale_fill_discrete(labels = c("confirmed_cases", "deaths"))+
  labs(title = "Cases/Deaths per Week Day in Greece")

G_boxplot
ggsave('R_images/Greece_box.png', width = width, height = height, units="cm")

