
library(magrittr)
library(dplyr)
library(lubridate)
library(sf)
library(feather)


# load the data from earthquakes.csv and create an RDS file
eq_data <- read.csv("data/earthquakes.csv") %>%
  dplyr::distinct() %>%
  dplyr::mutate(origintime = as.POSIXct(origintime, format = "%Y-%m-%dT%H:%M:%OSZ")) %>%
  dplyr::mutate(year = lubridate::year(origintime)) %>%
  dplyr::mutate(month = lubridate::month(origintime)) %>%
  dplyr::mutate(year_month_numeric = (year - min(year))*12 + month) %>%
  dplyr::mutate(month_label = format(origintime, "%b %Y")) %>%
  dplyr::mutate(hour = lubridate::hour(origintime)) %>%
  dplyr::mutate(time = format(origintime, "%H:%M:%S")) %>%
  dplyr::mutate(longitude = abs(longitude)) %>%
  dplyr::arrange(origintime)

# set origin to 'land' if the lat/long intersects with the NZ geometry
nz_shapes <- readRDS('data/regional_council_shapes.RDS') %>%
  st_union()

eq_data$origin <-  sapply(seq_len(nrow(eq_data)), function(i) {
  # message progress of total
  # if (i %% 1000 == 0) {
    message(paste0("Processing row ", i, " of ", nrow(eq_data)))
  # }
  ifelse(st_intersects(st_point(cbind(eq_data$longitude[i], eq_data$latitude[i])), nz_shapes),
         'land', 'sea')
})
# replace NA origin with 'sea'
eq_data$origin[is.na(eq_data$origin)] <- 'sea'


# set region based on which shape the lat/long intersects with
nz_shapes <- readRDS('data/regional_council_shapes.RDS')

eq_data$region <-  sapply(seq_len(nrow(eq_data)), function(i) {
  # message progress of total
  # if (i %% 1000 == 0) {
    message(paste0("Processing row ", i, " of ", nrow(eq_data)))
  # }
  region <- NA
    
  intersects <- unlist(st_intersects(st_point(cbind(eq_data$longitude[i], eq_data$latitude[i])), nz_shapes))
  
  if (length(intersects) > 0) {
    region <- nz_shapes$REGC2023_1[intersects[1]]
  }
  
  return(region)
})

# create a date column using origintime
eq_data$date <- as.Date(eq_data$origintime)

# remove columns we don't need
eq_data <- eq_data %>%
  dplyr::select(-publicid, -magnitudetype, -depthtype, -evaluationstatus, -evaluationmode)

# save eq_data to a feather file
feather::write_feather(eq_data, "data/earthquakes.feather")
