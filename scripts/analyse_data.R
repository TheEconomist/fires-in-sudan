# 0. Import packages
library(tidyverse)
library(countrycode)
library(sf)
library(lubridate)
library(rnaturalearth)
library(rnaturalearthdata)
library(readr)
library(ISOweek)

# To replicate for a new country
# Give country name
# Download archive of old fires (FIRMS) and place them in folder "paste0('source-data/fire-archive-data/', iso3c, '/')
# Download settlement data (GRID3) and place them in folder "paste0('source-data/settlement-data/', iso3c, '/')"
# Download population data and place them in folder "paste0('source-data/population-density-data/', iso3c, '/')"
# Download acled data and place them in folder "paste0('source-data/acled-data/', iso3c, '/')"
# ACLED data can be downloaded using the data export tool at acledata.org
# Note: the below script requires that the script "scripts/combine_data.R" has been run already.

# 0. Select country and options---------------------
country <- 'Sudan'
iso3c <- countrycode(country, 'country.name', 'iso3c')
buffer <- F
temporal_focus <- as.Date('2023-04-15'):Sys.Date()
temporal_focus <- as.Date(temporal_focus, origin = '1970-01-01')
comparison <- T

# 1. Import data ---------------------
if(!file.exists(paste0('output-data/', iso3c, "/"))){
  print(paste0('No data to plot. Have you successfully run combine_data.R for -- ', iso3c, ' -- ?'))
} else {
  joined_data <- readRDS(paste0('output-data/', iso3c, '/joined_data.RDS'))
  fire_counts_small <- readRDS(paste0('output-data/', iso3c, '/fire_counts_small.RDS'))
  geo_full <- readRDS(paste0('output-data/', iso3c, '/geo_simplified_data.RDS'))
  acled_full <- read_csv('source-data/acled-data/SDN/1997-01-01-2023-07-31-Sudan.csv')
  acled_full$event_date <- as.Date(acled_full$event_date, format = "%d %b %Y")
}

# Load the country shapefile data
country_map <- ne_states(country = country, returnclass = "sf")

# Plot the map using ggplot2
ggplot() +
  geom_sf(data = country_map, fill = "lightblue", color = "white") +
  theme_void()

# 2. Plot over time ---------------------

# A: cloud cover
clouds <- read_csv(paste0('output-data/', iso3c, '/cloud_cover_by_day.csv'))
ggplot(clouds, aes(x=date, y=cloud_cover_in_country))+geom_line()+ylim(c(0,1))+ylab('')+xlab('')+ggtitle("Average cloud cover in country by day")

# In inhabited areas
inhabited <- fire_counts_small[fire_counts_small$adm2_name != '_unknown_' &
                                 year(fire_counts_small$acq_date) > 2015, ]

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
  xlab('Day of year')+
  ggtitle(paste0('Number of fires in inhabited areas in ', country, ', by year'))+
  ylab('')+theme_minimal()+theme(legend.pos = 'none')

# In uninhabited areas:
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

# Weekly
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
  xlab('Day of year')+
  ggtitle(paste0('Number of fires in inhabited areas in ', country, ', by year'))+
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
  xlab('Day of year')+
  ggtitle(paste0('Number of fires in uninhabited areas in ', country, ', by year'))+
  ylab('')+theme_minimal()+theme(legend.pos = 'none')

# By population density:
ggplot(joined_data[!is.na(joined_data$population) &
                          year(joined_data$acq_date) >= 2015, ],
       aes(y=lat, x=acq_date, col=population))+geom_point()


# 3. Plot geographically ---------------------

# Restrict to temporal focus
geo <- geo_full[geo_full$acq_date %in% temporal_focus, ]

# Simplify from cell-day to cell dataset:
geo$num_fires <- ave(as.numeric(geo$cell_fire), geo$cell, FUN = function(x) sum(x))
geo$cell_fire_pop <- as.numeric(ave(geo[, c('cell_fire', 'cell_fire_pop')], geo$cell, FUN = function(x){
  if(nrow(x) > 1){
  weighted.mean(w=unlist(x[, 1]),
                x=unlist(x[, 2]))
  } else {x[, 2]}
})[, 1])
geo$cell_fire <- NULL

geo$cell_fire_pop_category <- ave(geo$cell_fire_pop_category, geo$cell, FUN = function(x) paste0(unique(x), collapse = "_"))

# Get easier labels
geo$cell_fire_pop_category_label <- NA
geo$cell_fire_pop_category_label[grep('Rural', geo$cell_fire_pop_category)] <- 'Village'
geo$cell_fire_pop_category_label[grep('Urban Cluster', geo$cell_fire_pop_category)] <- 'Urban Cluster'
geo$cell_fire_pop_category_label[grep('Urban Centre', geo$cell_fire_pop_category)] <- 'Urban Centre'

geo <- geo[!duplicated(geo$cell), ]
geo$acq_date <- NULL
geo$start_date <- min(temporal_focus)
geo$end_date <- max(temporal_focus)


# Save to ease replication:
write_csv(geo[, c('lat', 'lng', 'num_fires', 'cell_fire_pop', 'cell_fire_pop_category', 'cell_fire_pop_category_label',
                         'start_date', 'end_date')],
                 paste0('output-data/', iso3c, '/fire_map_replication.csv'))

# Geographically
ggplot()+
  geom_sf(data = country_map, fill = "lightblue", color = "white")+
  geom_point(data = geo,
             aes(x=lng, y=lat, col=cell_fire_pop, size=num_fires))+scale_color_binned(trans='log10')

# By population category
ggplot()+
  geom_sf(data = country_map, fill = "lightblue", color = "white")+
  geom_point(data = geo,
             aes(x=lng, y=lat, col=cell_fire_pop_category, size=num_fires))

if(comparison){

  # Restrict to temporal focus
  geo <- geo_full[geo_full$acq_date %in% c(temporal_focus-365), ]

  # Simplify from cell-day to cell dataset:
  geo$num_fires <- ave(as.numeric(geo$cell_fire), geo$cell, FUN = function(x) sum(x))
  geo$cell_fire_pop <- as.numeric(ave(geo[, c('cell_fire', 'cell_fire_pop')], geo$cell, FUN = function(x){
    if(nrow(x) > 1){
      weighted.mean(w=unlist(x[, 1]),
                    x=unlist(x[, 2]))
    } else {x[, 2]}
  })[, 1])
  geo$cell_fire <- NULL

  geo$cell_fire_pop_category <- ave(geo$cell_fire_pop_category, geo$cell, FUN = function(x) paste0(unique(x), collapse = "_"))

  # Get easier labels
  geo$cell_fire_pop_category_label <- NA
  geo$cell_fire_pop_category_label[grep('Rural', geo$cell_fire_pop_category)] <- 'Village'
  geo$cell_fire_pop_category_label[grep('Urban Cluster', geo$cell_fire_pop_category)] <- 'Urban Cluster'
  geo$cell_fire_pop_category_label[grep('Urban Centre', geo$cell_fire_pop_category)] <- 'Urban Centre'

  geo <- geo[!duplicated(geo$cell), ]
  geo$acq_date <- NULL
  geo$start_date <- min(temporal_focus-365)
  geo$end_date <- max(temporal_focus-365)

  # Save to ease replication:
  write_csv(geo[, c('lat', 'lng', 'num_fires', 'cell_fire_pop', 'cell_fire_pop_category', 'cell_fire_pop_category_label',
                    'start_date', 'end_date')],
            paste0('output-data/', iso3c, '/fire_map_replication_one_year_prior.csv'))

  # Geographically
  ggplot()+
    geom_sf(data = country_map, fill = "lightblue", color = "white")+
    geom_point(data = geo,
               aes(x=lng, y=lat, col=cell_fire_pop, size=num_fires))+scale_color_binned(trans='log10')

  # By population category
  ggplot()+
    geom_sf(data = country_map, fill = "lightblue", color = "white")+
    geom_point(data = geo,
               aes(x=lng, y=lat, col=cell_fire_pop_category, size=num_fires))

  current <- read_csv(paste0('output-data/', iso3c, '/fire_map_replication.csv'))

  geo$num_fires_one_year_prior <- geo$num_fires
  geo <- merge(current, geo[, c('num_fires_one_year_prior', 'lat', 'lng')],
               by=c('lat', 'lng'), all = T)
  geo$num_fires[is.na(geo$num_fires)] <- 0
  geo$num_fires_one_year_prior[is.na(geo$num_fires_one_year_prior)] <- 0

  geo$excess_fires <- geo$num_fires - geo$num_fires_one_year_prior

  # Save to ease replication:
  write_csv(geo[, c('lat', 'lng', 'num_fires', 'excess_fires',  'num_fires_one_year_prior', 'cell_fire_pop', 'cell_fire_pop_category', 'cell_fire_pop_category_label',
                    'start_date', 'end_date')],
            paste0('output-data/', iso3c, '/fire_map_replication_with_comparison.csv'))

}


# 4. Plot ACLED data geographically ---------------------
acled <- acled_full[acled_full$event_date %in% temporal_focus, ]

ggplot()+
  geom_sf(data = country_map, fill = "lightblue", color = "white")+
  geom_point(data = acled,
             aes(x=longitude,
                 y=latitude,
                 size=fatalities,
                 col=event_type))
write_csv(acled, paste0('output-data/', iso3c, '/acled_data_replication.csv'))

# 4. Plot ACLED data over time ---------------------
acled <- acled_full
acled$year <- lubridate::year(acled$event_date)
acled$month <- lubridate::month(acled$event_date)
acled$week <- lubridate::week(acled$event_date)

# Make violent event category:
violent_event <- c("Armed clash",
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
acled$violent_event <- acled$sub_event_type %in%  violent_event

if(length(setdiff(acled$sub_event_type, c("Armed clash", "Government regains territory", "Abduction/forced disappearance", "Air/drone strike", "Attack",  "Non-state actor overtakes territory", "Other", "Non-violent transfer of territory", "Change to group/activity", "Sexual violence", "Looting/property destruction", "Peaceful protest", "Shelling/artillery/missile attack", "Arrests", "Protest with intervention",  "Disrupted weapons use", "Agreement", "Remote explosive/landmine/IED", "Headquarters or base established", "Mob violence", "Excessive force against protesters", "Violent demonstration", "Grenade", "Chemical weapon"))) > 0){
  print("New event type, please check")

  acled_mean_fatalities <- acled %>%
    group_by(sub_event_type) %>%
    summarise(mean_fatalities = mean(fatalities, na.rm = TRUE))

  # Print the resulting data frame
  data.frame(acled_mean_fatalities)
    stop()
}

# Make yearly to reduce noise
acled_agg <- acled %>%
  group_by(year, sub_event_type) %>%
  summarise(total_fatalities = sum(fatalities),
            total_events = length(fatalities),
            total_violent_events = sum(violent_event),
            event_type = unique(event_type)[1])

ggplot(acled_agg, aes(x=year, y=total_fatalities, col=sub_event_type, fill=sub_event_type))+geom_col()
ggplot(acled_agg, aes(x=year, y=total_events, col=sub_event_type, fill=sub_event_type))+geom_col()
ggplot(acled_agg[acled_agg$sub_event_type %in% violent_event, ], aes(x=year, y=total_violent_events, col=sub_event_type, fill=sub_event_type))+geom_col()+
  ggtitle('Number of violent events, past years and year to date\n(ACLED)')

# Export
write_csv(acled_agg, paste0('output-data/SDN/acled_events_by_year.csv'))

# Show monthly:
acled_agg <- acled %>%
  group_by(year, month, sub_event_type) %>%
  summarise(total_fatalities = sum(fatalities),
            total_events = length(fatalities),
            total_violent_events = sum(violent_event))
acled_agg$date <- as.Date(paste0(acled_agg$year, '-', acled_agg$month, '-01'))
ggplot(acled_agg, aes(x=date, y=total_violent_events, fill=sub_event_type))+geom_col()

# Export
write_csv(acled_agg, paste0('output-data/SDN/acled_events_by_month.csv'))


# Show weekly:
acled_agg <- acled %>%
  group_by(year, week, sub_event_type) %>%
  summarise(total_fatalities = sum(fatalities),
            total_events = length(fatalities),
            total_violent_events = sum(violent_event))
acled_agg$date <- as.Date(paste0(acled_agg$year, '-01-01'), format = "%Y-%m-%d")+(acled_agg$week-1)*7
ggplot(acled_agg, aes(x=date, y=total_violent_events, fill=sub_event_type))+geom_col()

# Export
write_csv(acled_agg, paste0('output-data/SDN/acled_events_by_week.csv'))
