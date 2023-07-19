# 0. Import packages
library(tidyverse)
library(countrycode)
library(sf)
library(lubridate)
library(readr)
library(httr)
library(jsonlite)
library(rnaturalearth)
library(rnaturalearthdata)
library(anytime)

# To replicate for a new country
# Give country name
# Download archive of old fires (FIRMS) and place them in folder "paste0('source-data/fire-archive-data/', iso3c, '/')
# Download settlement data (GRID3) and place them in folder "paste0('source-data/settlement-data/', iso3c, '/')"
# Download population data and place them in folder "paste0('source-data/population-density-data/', iso3c, '/')"
# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 0. Select country and options---------------------
country <- 'Sudan'
iso3c <- countrycode(country, 'country.name', 'iso3c')
buffer <- F

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 1. Import data ---------------------
library(readr)
if(!file.exists(paste0('output-data/', iso3c))){
  print(paste0('Please create an output-data folder called -- ', iso3c, ' --'))
}
if(!file.exists(paste0('source-data/', iso3c))){
  print(paste0('Please create an source-data folder called -- ', iso3c, ' --'))
}

# A. Import FIRMS data

# These lines generates an archive of fires, downloaded from here country-by-country: https://firms.modaps.eosdis.nasa.gov/download/list.php

# First checks if this has already been done
if(!file.exists(paste0('output-data/', iso3c, '/firms_data.csv'))){
    fire_archive <- data.frame()
    ind <- 0
    for(i in dir(paste0('source-data/fire-archive-data/', iso3c, '/'))){
      ind <- ind + 1
      shp_name <- grep('.shp', dir(paste0('source-data/fire-archive-data/', iso3c, '/', i)), value = T)
        for(j in shp_name){
        temp <- st_read(paste0('source-data/fire-archive-data/', iso3c, '/', i, '/', j))
        if(ind == 1){
          fire_archive <- temp
        } else {
          fire_archive <- rbind(fire_archive[, base::intersect(colnames(fire_archive), colnames(temp))], temp[, base::intersect(colnames(fire_archive), colnames(temp))])
        }
      }
    }

  # Remove duplicates
  fire_archive <- unique(fire_archive)
  colnames(fire_archive) <- tolower(colnames(fire_archive))
  write_csv(fire_archive, paste0('output-data/', iso3c, '/firms_data.csv'))
  write_csv(data.frame(dates = min(fire_archive$ACQ_DATE):max(fire_archive$ACQ_DATE)), paste0('output-data/', iso3c, '/dates_of_successfully_acquired_fire_data.csv'))

} else {
  fire_archive <- read_csv(paste0('output-data/', iso3c, '/firms_data.csv'))
}
print(paste0('Data in archive from ', range(fire_archive$acq_date)[1], ' to ', range(fire_archive$acq_date)[2]))

# This script downloads the latest data from FIRMS and then add them to the file "output-data/**iso3c**/firms_data.csv"

# Define map key and sat systems used
map_key <- as.character(read_csv('firms_api_key.csv')[2])
sat_systems <- c("VIIRS_SNPP_NRT", "MODIS_NRT", "VIIRS_NOAA20_NRT", "VIIRS_SNPP_NRT") # This excludes standard processing systems "MODIS_SP", and "VIIRS_SNPP_SP"
days <- 10

fires <- data.frame()
for(i in sat_systems){
  link <- paste0("https://firms.modaps.eosdis.nasa.gov/api/country/csv/",
                 map_key, "/",
                 i,
                 "/", iso3c,"/", days)
  temp <- read_csv(link, show_col_types = F)

  if(nrow(fires) > 0 & nrow(temp) > 0){
    fires <- rbind(fires[, intersect(colnames(fires), colnames(temp))], temp[, intersect(colnames(fires), colnames(temp))])
  } else {
    fires <- temp}

  cat(paste0('Loaded most recent ', days, " days of data from ", i, ". New fires = ", nrow(fires), "\n"))
}

# Remove duplicates:
fires <- unique(fires)

if(nrow(fires) > 0){
  # 4. Ensure full coverage:
  updated_data <- unique(c(Sys.Date():(Sys.Date()-days+1), read_csv(paste0('output-data/', iso3c, '/dates_of_successfully_acquired_fire_data.csv'))$dates))
  temp <- setdiff(as.Date('2023-03-03', origin = '1970-01-01'):Sys.Date(), updated_data)
  if(length(temp) > 0){
    stop(paste0("Error: Missing fire data for: ", as.Date(temp, origin = '1970-01-01'), ' --- aborting update.\n'))
  }
  # Record successful update:
  write_csv(data.frame(dates = updated_data), paste0('output-data/', iso3c, '/dates_of_successfully_acquired_fire_data.csv'))

  # Append to data and save:
  old_fires <- read_csv(paste0('output-data/', iso3c, '/firms_data.csv'))
  colnames(old_fires) <- tolower(colnames(old_fires))
  fires <- rbind(fires[, base::intersect(colnames(old_fires), colnames(fires))], old_fires[, base::intersect(colnames(old_fires), colnames(fires))])
  fires <- fires[!duplicated(paste0(fires$latitude, '-', fires$longitude, '-', fires$acq_date, '-', fires$acq_time, '-', fires$satellite)), ]
  write_csv(fires, paste0('output-data/', iso3c, '/firms_data.csv')) } else {
    stop('Update failed - check rate limits.')
  }

# Remove the latest day (for which we do not yet have full data)
fires <- fires[fires$acq_date < max(fires$acq_date), ]

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# B. Import settlement data.
# Source: https://data.grid3.org/datasets/GRID3::south-sudan-settlement-extents-version-01-01/explore?location=-0.022655%2C53.286129%2C5.22 (in this example, South Sudan)
# Remember to download Shapefile, and place it in an "source-data/settlement-data/**iso3c**/" folder

shp_name <- grep('.shp', dir(paste0('source-data/settlement-data/', iso3c, '/')), value = T)
settlements <- st_read(paste0('source-data/settlement-data/', iso3c, '/', shp_name))

# This sets the grouping variable, by default adm2_name, and if that does not exist, it is set as uninhabited
settlements$adm2_name <- ifelse(is.null(settlements$adm2_name), 'inhabited', settlements$adm2_name)

# Calculate overlap per day.
sf_use_s2(FALSE)

# If buffer desired:
if(buffer){
  # First create buffer to settlements, as the fires data is imprecise
  # This creates a 500m buffer around each feature
  # Project to an appropriate coordinate reference system
  settlements <- st_transform(settlements, "+proj=utm +zone=36 +datum=WGS84 +units=m +no_defs")
  settlements <- st_buffer(settlements, dist = 500)

  # Combine overlapping buffers into single features
  settlements <- st_union(settlements)
  settlements <- st_transform(settlements, "+proj=longlat +datum=WGS84")
}

# Convert fires to an sf object
fires_sf <- st_as_sf(fires, coords = c("longitude", "latitude"), crs = st_crs(settlements))

# Perform spatial join between settlements and fires
joined_data <- st_join(fires_sf, settlements, join = st_within)

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# C. Import population density data:
# Source: https://data.humdata.org/dataset/kontur-population-sudan (using the example of Sudan)
pop_density <- st_read(paste0(paste0('source-data/population-density-data/', iso3c, '/'), (dir(paste0('source-data/population-density-data/', iso3c, '/')))))
pop_density <- st_transform(pop_density, crs = st_crs(joined_data))
joined_data <- st_join(joined_data, pop_density, join = st_within)

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# D. Export this object to output file:
joined_data$lng <- st_coordinates(joined_data)[, 1]
joined_data$lat <- st_coordinates(joined_data)[, 2]
saveRDS(joined_data, paste0('output-data/', iso3c, '/joined_data.RDS'))

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# E. Generate data set by day and area
fire_counts <- joined_data %>%
  group_by(adm2_name, acq_date) %>%
  summarise(num_fires = n())

# Print the fire event counts
saveRDS(fire_counts, paste0('output-data/', iso3c, '/fire_counts.RDS'))

fire_counts_small <- fire_counts[, c('adm2_name', 'acq_date', 'num_fires')]
fire_counts_small$num_fires <- ave(fire_counts_small$num_fires, paste0(fire_counts_small$adm2_name, '_', fire_counts_small$acq_date))
fire_counts_small <- unique(fire_counts_small)
temp <- expand.grid(as.Date(setdiff(min(fire_counts_small$acq_date):max(fire_counts_small$acq_date), unique(fire_counts_small$acq_date))), unique(fire_counts_small$adm2_name), num_fires = 0)
colnames(temp) <- c('acq_date', 'adm2_name', 'num_fires')
fire_counts_small <- merge(fire_counts_small, temp, all=T)
fire_counts_small$adm2_name[is.na(fire_counts_small$adm2_name)] <- '_unknown_'

# Generated expected fire counts using a monthly fixed-effects model:
lm_fit <- lm(num_fires ~ as.factor(week(acq_date))*adm2_name*as.numeric(year(acq_date)), data =fire_counts_small[year(fire_counts_small$acq_date) %in% (year(Sys.Date())-1):(year(Sys.Date())-5), ])
fire_counts_small$expected <- predict(newdata=fire_counts_small, lm_fit)

# Generate weekly and monthly counts
fire_counts_small$num_fires_week_ave <- ave(fire_counts_small$num_fires, paste0(year(fire_counts_small$acq_date), '_', week(fire_counts_small$acq_date), '_', fire_counts_small$adm2_name), FUN = function(x) sum(x)/length(x))
fire_counts_small$num_fires_month_ave <- ave(fire_counts_small$num_fires, paste0(year(fire_counts_small$acq_date), '_', month(fire_counts_small$acq_date), '_', fire_counts_small$adm2_name), FUN = function(x) sum(x)/length(x))

# Export
saveRDS(fire_counts_small, paste0('output-data/', iso3c, '/fire_counts_small.RDS'))

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# F. Get shape of country
# Load the country shapefile data
country_map <- ne_states(country = country, returnclass = "sf")

# Plot the map using ggplot2
ggplot() +
  geom_sf(data = country_map, fill = "lightblue", color = "white") +
  theme_void()

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# G. Get daily cloud cover
# This script gets day-to-day cloud cover open-meteo

# Step 1: Get a grid of country
country_bbox <- st_bbox(country_map)
country_grid <- expand.grid('lng' = seq(from = round(country_bbox[1], 1),
                                        to = round(country_bbox[3], 1), by = 1),
                            'lat' = seq(from = round(country_bbox[2], 1),
                                        to = round(country_bbox[4], 1), by = 1))

# Check if the points are within Sudan
country_grid <- country_grid[unlist(st_contains(country_map,
                                         st_as_sf(country_grid,
                                                  coords = c("lng", "lat"), crs = 4326))), ]

# Step 2: Get dates to get forecast for:
# clouds <- read_csv('output-data/cloud_cover_in_ukraine_by_day.csv')
start_date <- min(joined_data$acq_date)
end_date <- Sys.Date()

# Step 3: Query API to get data, simplify, and save
# Define function to get data from api:
get_weather_forecast_by_lat_lng <- function(url){
  web_content <- httr::GET(url)
  web_content <- content(web_content,"text")
  json_data <- fromJSON(web_content, flatten = TRUE)
  df <- as.data.frame(json_data)
  return(df)
}

# Loop through locations and save results:
df <- data.frame()
for(i in 1:nrow(country_grid)){
  url <- paste0('https://archive-api.open-meteo.com/v1/archive?latitude=', country_grid$lat[i], '&longitude=', country_grid$lng[i], '&hourly=temperature_2m,cloudcover,relativehumidity_2m,precipitation,soil_moisture_0_to_7cm&start_date=', start_date, '&end_date=', end_date)
  df<- bind_rows(df,get_weather_forecast_by_lat_lng(url))
  cat('.')
}

# Simplify
df$date <- anydate(df$hourly.time)
df$hourly.cloudcover <- df$hourly.cloudcover/100
df$cloud_cover_in_country <- ave(df$hourly.cloudcover, df$date, FUN = function(x) mean(x, na.rm = T))
df$temp_in_country <- ave(df$hourly.temperature_2m, df$date, FUN = function(x) mean(x, na.rm = T))
df$relativehumidity_in_country <- ave(df$hourly.relativehumidity_2m, df$date, FUN = function(x) mean(x, na.rm = T))
df$precipitation_in_country <- ave(df$hourly.precipitation, df$date, FUN = function(x) mean(x, na.rm = T))
df$soil_moisture_in_country <- ave(df$hourly.soil_moisture_0_to_7cm, df$date, FUN = function(x) mean(x, na.rm = T))

df <- unique(df[, c('date', 'cloud_cover_in_country', 'temp_in_country',
                    'relativehumidity_in_country',
                    'precipitation_in_country',
                    'soil_moisture_in_country')])

ggplot(df, aes(x=yday(date), y=cloud_cover_in_country, col=as.factor(year(date))))+geom_line(alpha = 0.2)+geom_line(data = df[year(df$date) == 2023, ])+geom_vline(aes(xintercept = 105, col='War begins in 2023\n(April 15th)'), col = 'black')

ggplot(df, aes(x=yday(date), y=temp_in_country, col=as.factor(year(date))))+geom_line(alpha = 0.2)+geom_line(data = df[year(df$date) == 2023, ])+geom_vline(aes(xintercept = 105, col='War begins in 2023\n(April 15th)'))+geom_vline(aes(xintercept = 105, col='War begins in 2023\n(April 15th)'), col = 'black')

ggplot(df, aes(x=yday(date), y=relativehumidity_in_country, col=as.factor(year(date))))+geom_line(alpha = 0.2)+geom_line(data = df[year(df$date) == 2023, ])+geom_vline(aes(xintercept = 105, col='War begins in 2023\n(April 15th)'))+geom_vline(aes(xintercept = 105, col='War begins in 2023\n(April 15th)'), col = 'black')

ggplot(df, aes(x=yday(date), y=precipitation_in_country, col=as.factor(year(date))))+geom_line(alpha = 0.2)+geom_line(data = df[year(df$date) == 2023, ])+geom_vline(aes(xintercept = 105, col='War begins in 2023\n(April 15th)'))+scale_y_continuous(trans='log10')+geom_vline(aes(xintercept = 105, col='War begins in 2023\n(April 15th)'), col = 'black')

ggplot(df, aes(x=yday(date), y=soil_moisture_in_country, col=as.factor(year(date))))+geom_line(alpha = 0.2)+geom_line(data = df[year(df$date) == 2023, ])+geom_vline(aes(xintercept = 105, col='War begins in 2023\n(April 15th)'), col = 'black')

# Save
write_csv(df, paste0('output-data/', iso3c, '/cloud_cover_by_day.csv'))


# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# H. Generate a simplified geographical data frame of fires in populated areas:
geo <- joined_data[!is.na(joined_data$adm2_name), ]
geo$population[is.na(geo$population)] <- 1 # If population density is unknown but area known to be inhabited, set to 1 (lowest in population density data)
geo$lat <- round(geo$lat, 1)
geo$lng <- round(geo$lng, 1)
geo$cell <- paste0(geo$lat, '_', geo$lng)
geo$id <- paste0(geo$lat, '_', geo$lng, '_', date(geo$acq_date))
geo$cell_fire <- as.numeric(ave(geo$id, geo$id, FUN = length))
geo$cell_fire_pop <- as.numeric(ave(geo$population, geo$id, FUN = mean))

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

geo$cell_fire_pop_category <- ave(geo$dou_level1, geo$id, FUN = Mode)
geo <- geo[!duplicated(geo$id), ]
geo$geometry <- NULL
geo <- data.frame(geo[, c('lat', 'lng', 'acq_date', 'cell', 'id', 'cell_fire', 'cell_fire_pop', 'cell_fire_pop_category')])

# Save
saveRDS(geo, paste0('output-data/', iso3c, '/geo_simplified_data.RDS'))

