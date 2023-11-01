# Replication: Sudan war fires
library(tidyverse)
library(lubridate)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(anytime)

country <- 'Sudan'
iso3c <- 'SDN'

show_comparisons <- F
# -----------------------------------------------------------------------
# 1. Fire map:
# April 15 2023 to present
# This plots fires from April 15th 2023 to July 18th 2023, with locations rounded to 0.1 lat/lng, and events in the same location summed in the 'num_fires" column
geo <- read_csv("output-data/SDN/fire_map_replication_with_comparison.csv")

# Load the country shapefile data
country_map <- ne_states(country = country, returnclass = "sf")

# This plot shows average population density at positions of fire events in the period
ggplot()+
  geom_sf(data = country_map, fill = "lightblue", color = "white")+
  geom_point(data = geo[geo$num_fires > 0, ],
             aes(x=lng, y=lat, col=cell_fire_pop, size=num_fires))+
  scale_color_binned(trans='log10')+
  scale_size(trans='log10')+ggtitle('Fires in inhabited areas of Sudan\nApril 15th 2023-July 18th 2023')

# This plot shows mode population density 'category" at positions of fire events in the period
ggplot()+
  geom_sf(data = country_map, fill = "lightblue", color = "white")+
  geom_point(data = geo,
             aes(x=lng, y=lat, col=cell_fire_pop_category_label, size=num_fires))+
  scale_size(trans='log10')+ggtitle('Fires in inhabited areas of Sudan\nApril 15th 2023-July 18th 2023')

# We can also do the same as a comparison to the same time one year ago:
ggplot()+
  geom_sf(data = country_map, fill = "lightblue", color = "white")+
  geom_point(data = geo,
             aes(x=lng, y=lat, col=cell_fire_pop, size=excess_fires))+ggtitle('Excess Fires in inhabited areas of Sudan\nApril 15th 2023-July 18th 2023')+
  xlab('Compared to same period last year')+
  scale_color_continuous(high='red', low='blue', trans='pseudo_log')

# This plot shows mode population density 'category" at positions of fire events in the period
ggplot()+
  geom_sf(data = country_map, fill = "lightblue", color = "white")+
  geom_point(data = geo[geo$num_fires > geo$num_fires_one_year_prior, ],
             aes(x=lng, y=lat, col=cell_fire_pop_category_label, size=excess_fires))+
  scale_size(trans='log10')+ggtitle('Excess Fires in inhabited areas of Sudan\nApril 15th 2023-July 18th 2023')+xlab('Compared to same period last year')+scale_size()

# ----------------------------------------------------------------------
# 2. ACLED map:
acled <- read_csv("output-data/SDN/acled_data_replication.csv")
ggplot()+
  geom_sf(data = country_map, fill = "lightblue", color = "white")+
  geom_point(data = acled,
             aes(x=longitude,
                 y=latitude,
                 size=fatalities,
                 col=event_type))

head(acled)

# ----------------------------------------------------------------------
# 3a. Charts of fire activity over time
fire_counts_small <- readRDS(paste0('output-data/', iso3c, '/fire_counts_small.RDS'))
fire_counts_small <- fire_counts_small[fire_counts_small$acq_date >= as.Date('2013-01-01'), ]

# By day
inhabited <- fire_counts_small[fire_counts_small$adm2_name != '_unknown_' &
                                 year(fire_counts_small$acq_date) > 2013, ]

ggplot(data = inhabited, aes(x=yday(acq_date),
                             col=as.factor(year(acq_date) == year(Sys.Date())),
                             group = as.factor(year(acq_date)),
                             y=num_fires))+
  geom_smooth(data = inhabited[year(inhabited$acq_date) != year(Sys.Date()), ],
              alpha= 0.25, aes(group=1), method = 'loess', span = 0.05)+
  geom_line(data = inhabited[year(inhabited$acq_date) != year(Sys.Date()), ],
            alpha= 0.25)+
  geom_line(data = inhabited[year(inhabited$acq_date) == year(Sys.Date()), ],
            alpha= 1)+
  xlab('Blue = 2023')+
  ggtitle(paste0('Number of fires in inhabited areas in ', country, ', by day of year'))+
  ylab('')+theme_minimal()+theme(legend.pos = 'none')

# By week
ggplot(data = inhabited, aes(x=week(acq_date),
                             col=as.factor(year(acq_date) == year(Sys.Date())),
                             group = as.factor(year(acq_date)),
                             y=num_fires_week_ave*7))+
  geom_smooth(data = inhabited[year(inhabited$acq_date) != year(Sys.Date()), ],
              alpha= 0.25, aes(group=1), method = 'loess', span = 0.05)+
  geom_line(data = inhabited[year(inhabited$acq_date) != year(Sys.Date()), ],
            alpha= 0.25)+
  geom_line(data = inhabited[year(inhabited$acq_date) == year(Sys.Date()), ],
            alpha= 1)+
  xlab('Week of year')+
  ggtitle(paste0('Number of fires in inhabited areas in ', country, ', by year'))+
  ylab('')+theme_minimal()+theme(legend.pos = 'none')

# NOTE: These can also be shown cumulatively

# FOR COMPARISON -- In uninhabited areas:
if(show_comparisons){
  uninhabited <- fire_counts_small[fire_counts_small$adm2_name == '_unknown_' &
                                     year(fire_counts_small$acq_date) > 2015, ]

  ggplot(data = uninhabited, aes(x=yday(acq_date),
                                 col=as.factor(year(acq_date) == year(Sys.Date())),
                                 group = as.factor(year(acq_date)),
                                 y=num_fires))+
    geom_smooth(data = uninhabited[year(uninhabited$acq_date) != year(Sys.Date()), ],
                alpha= 0.25, aes(group=1), method = 'loess', span = 0.05)+
    geom_line(data = uninhabited[year(uninhabited$acq_date) != year(Sys.Date()), ],
              alpha= 0.25)+
    geom_line(data = uninhabited[year(uninhabited$acq_date) == year(Sys.Date()), ],
              alpha= 1)+
    xlab('Day of year')+
    ggtitle(paste0('Number of fires in uninhabited areas in ', country, ', by year'))+
    ylab('')+theme_minimal()+theme(legend.pos = 'none')

  ggplot(data = uninhabited, aes(x=week(acq_date),
                                 col=as.factor(year(acq_date) == year(Sys.Date())),
                                 group = as.factor(year(acq_date)),
                                 y=num_fires_week_ave*7))+
    geom_smooth(data = uninhabited[year(uninhabited$acq_date) != year(Sys.Date()), ],
                alpha= 0.25, aes(group=1), method = 'loess', span = 0.05)+
    geom_line(data = uninhabited[year(uninhabited$acq_date) != year(Sys.Date()), ],
              alpha= 0.25)+
    geom_line(data = uninhabited[year(uninhabited$acq_date) == year(Sys.Date()), ],
              alpha= 1)+
    xlab('Week of year')+
    ggtitle(paste0('Number of fires in uninhabited areas in ', country, ', by year'))+
    ylab('')+theme_minimal()+theme(legend.pos = 'none')

}

# 3b. Charts of fire activity over time (excluding cement factories)
joined_data <- readRDS(paste0('output-data/', iso3c, '/joined_data.RDS'))

# Filter out events which are from factories and the like
joined_data$lat <- round(joined_data$lat, 2)
joined_data$lng <- round(joined_data$lng, 2)

# This has to be done manually (I went through top 30):
joined_data$id <- paste0(joined_data$lat, '_', joined_data$lng)
joined_data$exclude <- joined_data$id %in% c('11.04_28.5'   , # factory or refinery
                                             '11.32_28.47'    , # refinery/oil burning
                                             '11.6_28.24'   , # refinery/oil burning
                                             '10.92_28.57'    , # refinery
                                             '17.87_33.84'  , # cement plant
                                             '11.46_28.43'    , # refinery/oil burning
                                             '16.13_32.68'  , # petrochemical company
                                             '10.9_28.57'    , # oil extraction
                                             '10.9_28.56'   , # oil extraction
                                             '11.33_28.47'    , # refinery/oil burning
                                             '11.5_28.23'   , # oil extraction
                                             '16.13_32.69'    , # refinery
                                             '17.94_34.04'  , # cement factory
                                             '17.7_33.8'    , # cement factory
                                             '17.94_34.05'  , # cement factory
                                             '11.51_28.41'  , # oil extraction
                                             '17.71_33.8'  , # cement factory
                                             '16.12_32.69' , # power plant
                                             '16.12_32.68' , # power plant
                                             '10.92_28.56' , # oil refinery
                                             '11.31_28.47' , # oil extraction
                                             '17.93_34.05' , # cement factory
                                             '11.54_28.42' ) # oil extraction

joined_data <- joined_data[!joined_data$exclude & !is.na(joined_data$adm2_name), ]
fire_counts <- joined_data %>%
  group_by(adm2_name, acq_date) %>%
  summarise(num_fires = n())

fire_counts_small <- fire_counts[year(fire_counts$acq_date) >= 2013, c('adm2_name', 'acq_date', 'num_fires')]
fire_counts_small$num_fires <- ave(fire_counts_small$num_fires, paste0(fire_counts_small$adm2_name, '_', fire_counts_small$acq_date))
fire_counts_small <- unique(fire_counts_small)
temp <- expand.grid(as.Date(setdiff(min(fire_counts_small$acq_date):max(fire_counts_small$acq_date), unique(fire_counts_small$acq_date))), unique(fire_counts_small$adm2_name), num_fires = 0)
colnames(temp) <- c('acq_date', 'adm2_name', 'num_fires')
fire_counts_small <- merge(fire_counts_small, temp, all=T)
fire_counts_small$adm2_name[is.na(fire_counts_small$adm2_name)] <- '_unknown_'

# Generated expected fire counts using a weekly fixed-effects model:
lm_fit <- lm(num_fires ~ as.factor(week(acq_date))*as.numeric(year(acq_date)), data =fire_counts_small[year(fire_counts_small$acq_date) %in% (year(Sys.Date())-1):(year(Sys.Date())-5), ])
fire_counts_small$expected <- predict(newdata=fire_counts_small, lm_fit)

# Generate weekly and monthly counts
fire_counts_small$num_fires_week_ave <- ave(fire_counts_small$num_fires, paste0(year(fire_counts_small$acq_date), '_', week(fire_counts_small$acq_date), '_', fire_counts_small$adm2_name), FUN = function(x) sum(x)/length(x))
fire_counts_small$num_fires_month_ave <- ave(fire_counts_small$num_fires, paste0(year(fire_counts_small$acq_date), '_', month(fire_counts_small$acq_date), '_', fire_counts_small$adm2_name), FUN = function(x) sum(x)/length(x))

# Remove the last week:
fire_counts_small <- fire_counts_small[!(week(fire_counts_small$acq_date) ==
                      max(week(fire_counts_small$acq_date[year(fire_counts_small$acq_date) ==
                                                            year(Sys.Date())])) &
                      year(fire_counts_small$acq_date) == year(Sys.Date())), ]


write_csv(fire_counts_small, 'output-data/SDN/fires_by_week_excluding_factories.csv')
fire_counts_small <- read_csv('output-data/SDN/fires_by_week_excluding_factories.csv')
fire_counts_small <- fire_counts_small[fire_counts_small$acq_date >= as.Date('2013-01-01'), ]

fire_counts_small <- fire_counts_small[!duplicated(paste0(week(fire_counts_small$acq_date), '_', year(fire_counts_small$acq_date))), ]
ggplot(data = fire_counts_small, aes(x=week(acq_date),
                                     col=as.factor(year(acq_date) == year(Sys.Date())),
                                     group = as.factor(year(acq_date)),
                                     y=num_fires_week_ave*7))+
  geom_smooth(data = fire_counts_small[year(fire_counts_small$acq_date) != year(Sys.Date()), ],
              alpha= 0.25, aes(group=1), method = 'loess', span = 0.05)+
  geom_line(data = fire_counts_small[year(fire_counts_small$acq_date) != year(Sys.Date()), ],
            alpha= 0.25)+
  geom_line(data = fire_counts_small[year(fire_counts_small$acq_date) == year(Sys.Date()), ],
            alpha= 1)+
  xlab('Week of year')+
  ggtitle(paste0('Number of fires in inhabited areas in ', country, ', by week and year\n2010-2023 (blue = 2023)'))+
  ylab('')+theme_minimal()+theme(legend.pos = 'none')
ggsave('output-data/SDN/fires_by_week_excluding_factories.pdf')

# Below chart is to informtext
ggplot(data = fire_counts_small, aes(x=week(acq_date),
                                     col=as.factor(year(acq_date) == year(Sys.Date())),
                                     group = as.factor(year(acq_date)),
                                     y=num_fires_week_ave*7))+
  geom_smooth(data = fire_counts_small[year(fire_counts_small$acq_date) != year(Sys.Date()), ],
              alpha= 0.25, aes(group=1), method = 'loess', span = 0.05)+
  geom_line(data = fire_counts_small[year(fire_counts_small$acq_date) != year(Sys.Date()), ],
            alpha= 0.25)+
  geom_line(data = fire_counts_small[year(fire_counts_small$acq_date) == year(Sys.Date()), ],
            alpha= 1)+
  xlab('Week of year')+
  ggtitle(paste0('Number of fires in inhabited areas in ', country, ', by week and year\n2010-2023 (blue = 2023)'))+
  ylab('')+theme_minimal()+theme(legend.pos = 'none')+geom_hline(aes(yintercept = 5))+geom_hline(aes(yintercept = 80))+geom_vline(aes(xintercept=16))+geom_vline(aes(xintercept=26))

# Quote for text
ave_now <- sum(fire_counts_small$num_fires_week_ave[week(fire_counts_small$acq_date) %in% 16:28 & year(fire_counts_small$acq_date) == 2023]*7)
ave_past <- (sum(fire_counts_small$num_fires_week_ave[week(fire_counts_small$acq_date) %in% 16:28 & year(fire_counts_small$acq_date) != 2023]*7)/(length(unique(year(fire_counts_small$acq_date)))-1))

ave_now
ave_past
ave_now/ave_past

# ----------------------------------------------------------------------
# 4. Charts of fire activity over time and population density
joined_data <- readRDS(paste0('output-data/', iso3c, '/joined_data.RDS'))

# Filter out events which are from factories and the like
joined_data$lat <- round(joined_data$lat, 2)
joined_data$lng <- round(joined_data$lng, 2)

# This has to be done manually (I went through top 20):
fire_locations <- table(paste0(joined_data$lat, '_', joined_data$lng))
sort(fire_locations, decreasing = T)[1:30]

joined_data$id <- paste0(joined_data$lat, '_', joined_data$lng)
joined_data$exclude <- joined_data$id %in% c('11.04_28.5'   , # factory or refinery
                                             '11.32_28.47'    , # refinery/oil burning
                                             '11.6_28.24'   , # refinery/oil burning
                                             '10.92_28.57'    , # refinery
                                             '17.87_33.84'  , # cement plant
                                             '11.46_28.43'    , # refinery/oil burning
                                             '16.13_32.68'  , # petrochemical company
                                             '10.9_28.57'    , # oil extraction
                                             '10.9_28.56'   , # oil extraction
                                             '11.33_28.47'    , # refinery/oil burning
                                             '11.5_28.23'   , # oil extraction
                                             '16.13_32.69'    , # refinery
                                             '17.94_34.04'  , # cement factory
                                             '17.7_33.8'    , # cement factory
                                             '17.94_34.05'  , # cement factory
                                             '11.51_28.41'  , # oil extraction
                                             '17.71_33.8'  , # cement factory
                                             '16.12_32.69' , # power plant
                                             '16.12_32.68' , # power plant
                                             '10.92_28.56' , # oil refinery
                                             '11.31_28.47' , # oil extraction
                                             '17.93_34.05' , # cement factory
                                             '11.54_28.42' ) # oil extraction

# By population density:
ggplot(joined_data[!is.na(joined_data$population) &
                     year(joined_data$acq_date) >= 2015 &
                     !joined_data$exclude, ],
       aes(y=population, x=acq_date, col=population))+geom_point()

# As simplified heat-map
ggplot(joined_data[!is.na(joined_data$population) &
                     year(joined_data$acq_date) >= 2018 &
                     !joined_data$exclude, ],
       aes(y=population, x=acq_date))+
  geom_bin_2d(bins = 25)+scale_fill_continuous(trans = "log10", low = 'white', high ='darkred')+theme_bw()+ylab('')+xlab('')
  ggtitle('Fires by date and population density at location*\n*Excluding highly active cement factories, power generators, and oil-related facilities')

table(year(joined_data$acq_date)[joined_data$exclude & !is.na(joined_data$population)])
table(year(joined_data$acq_date)[!is.na(joined_data$population)])
# ----------------------------------------------------------------------
# 5a. ACLED summaries by year
acled <- read_csv("output-data/SDN/acled_events_by_year.csv")

# Neater labels:
acled$event_label <- NA
acled$event_label[acled$sub_event_type %in% c('Excessive force against protesters')] <- 'Excessive force against protestors'
acled$event_label[acled$sub_event_type %in% c('Air/drone strike', 'Armed clash', 'Attack', 'Remote explosive/landmine/IED', 'Shelling/artillery/missile attack', 'Grenade', 'Chemical weapon', "Government regains territory", "Non-state actor overtakes territory")] <- 'Armed clashes and explosions'
acled$event_label[acled$sub_event_type %in% c('Sexual violence',
                                              'Abduction/forced disappearance')] <- 'Abductions and sexual violence'
acled$event_label[acled$sub_event_type %in% c('Mob violence', 'Looting/property destruction',
                                              'Violent demonstration')] <- 'Looting, property destruction and mob violence'

write_csv(acled, 'output-data/SDN/acled_chart_by_year_replication.csv')

# Chart:
acled <- read_csv('output-data/SDN/acled_chart_by_year_replication.csv')
ggplot(acled[acled$total_violent_events > 0, ], aes(x=year, y=total_violent_events, col=event_label, fill=event_label))+geom_col()+
  ggtitle('Number of violent events, past years and year to date\n(ACLED)')+xlab('')+ylab('')
ggsave('output-data/SDN/acled_chart_by_year.pdf')


# ----------------------------------------------------------------------
# 5b ACLED summaries by month
acled <- read_csv("output-data/SDN/acled_events_by_month.csv")

# Neater labels:
acled$event_label <- NA
acled$event_label[acled$sub_event_type %in% c('Excessive force against protesters')] <- 'Excessive force against protestors'
acled$event_label[acled$sub_event_type %in% c('Air/drone strike', 'Armed clash', 'Attack', 'Remote explosive/landmine/IED', 'Shelling/artillery/missile attack', 'Grenade', 'Chemical weapon', "Government regains territory", "Non-state actor overtakes territory")] <- 'Armed clashes and explosions'
acled$event_label[acled$sub_event_type %in% c('Sexual violence',
                                              'Abduction/forced disappearance')] <- 'Abductions and sexual violence'
acled$event_label[acled$sub_event_type %in% c('Mob violence', 'Looting/property destruction',
                                              'Violent demonstration')] <- 'Looting, property destruction and mob violence'

write_csv(acled, 'output-data/SDN/acled_chart_by_month_replication.csv')

# Chart:
acled <- read_csv('output-data/SDN/acled_chart_by_month_replication.csv')
ggplot(acled[acled$total_violent_events > 0, ], aes(x=date, y=total_violent_events, col=event_label, fill=event_label))+geom_col()+
  ggtitle('Number of violent events, past years and year to date\n(ACLED)')+xlab('')+ylab('')+theme_minimal()
ggsave('output-data/SDN/acled_chart_by_month.pdf')


# ----------------------------------------------------------------------
# 5c. ACLED summaries by week
acled <- read_csv("output-data/SDN/acled_events_by_week.csv")

# Neater labels:
acled$event_label <- NA
acled$event_label[acled$sub_event_type %in% c('Excessive force against protesters')] <- 'Excessive force against protestors'
acled$event_label[acled$sub_event_type %in% c('Air/drone strike', 'Armed clash', 'Attack', 'Remote explosive/landmine/IED', 'Shelling/artillery/missile attack', 'Grenade', 'Chemical weapon', "Government regains territory", "Non-state actor overtakes territory")] <- 'Armed clashes and explosions'
acled$event_label[acled$sub_event_type %in% c('Sexual violence',
                                              'Abduction/forced disappearance')] <- 'Abductions and sexual violence'
acled$event_label[acled$sub_event_type %in% c('Mob violence', 'Looting/property destruction',
                                              'Violent demonstration')] <- 'Looting, property destruction and mob violence'

write_csv(acled, 'output-data/SDN/acled_chart_by_week_replication.csv')

# Chart:
acled <- read_csv('output-data/SDN/acled_chart_by_week_replication.csv')
ggplot(acled[acled$total_violent_events > 0, ], aes(x=date, y=total_violent_events, col=event_label, fill=event_label))+geom_col()+
  ggtitle('Number of violent events, past years and year to date\n(ACLED)')+xlab('')+ylab('')
ggsave('output-data/SDN/acled_chart_by_week.pdf')


# ----------------------------------------------------------------------
# 5c. Numbers for text:
geo <- read_csv("output-data/SDN/fire_map_replication_with_comparison.csv")

geo$id <- paste0(round(geo$lat, 1), '_', round(geo$lng, 1))

# How many ACLED events have unknown casualties?
acled <- read_csv("output-data/SDN/acled_data_replication.csv")
acled$violent <- acled$sub_event_type %in% c("Armed clash",
                                             "Abduction/forced disappearance",
                                             "Air/drone strike",
                                             "Attack",
                                             "Sexual violence",
                                             "Looting/property destruction",
                                             "Shelling/artillery/missile attack",
                                             "Remote explosive/landmine/IED",
                                             "Mob violence",
                                             "Excessive force against protesters",
                                             "Violent demonstration",
                                             "Grenade",
                                             "Chemical weapon",
                                             "Government regains territory",
                                             "Non-state actor overtakes territory")

length(grep('Casualties unknown', acled$notes[acled$violent]))
nrow(acled[acled$violent, ])
