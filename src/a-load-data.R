# A script to load the various data sources
library(tidyverse)
library(readxl)
library(sf)

df.recon <- read_excel(path = "dat/invasion-front-reconnaissance-data.xlsx", sheet = "recon_data")

df.recon <- df.recon |>
  mutate(hour = hour(time), 
         minute = minute(time), 
         date = as.Date(paste(year, month, day, sep = "-")),
         ttd.censored = ifelse(toad.present==1, person.minutes, NA), # only report ttd for positive sighting
         censored = as.numeric(toad.present==0)) |> # censored, or not?
  st_as_sf(coords = c("lon", "lat")) |>
  st_set_crs(value = 4283) |> # set crs (GDA94/GRS 1980)
  st_transform(crs = 3857) # transform to web mercator

st_write(df.recon, "out/recon_sites.kml", append = FALSE)

# # get missing temperature data from SILO
# fetch_temps <- function(df){
#   for (nn in 1:nrow(df)){
#     if (!is.na(df$temperature[nn])) next
#     wd <- weatherOz::get_data_drill(
#       latitude = df$lat[nn],
#       longitude = df$lon[nn],
#       start_date = df$date[nn],
#       end_date = df$date[nn],
#       values = c(
#         "max_temp",
#         "min_temp",
#         "rh_tmax"
#       ),
#       api_key = Sys.getenv("SILO_API_KEY")
#     )
#   }
#   }




