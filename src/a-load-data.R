# A script to load the various data sources
library(tidyverse)
library(readxl)
library(sf)

# 2025 visual survey data (from Fulcrum)
df.fulcrum <- st_read(dsn = "dat/2025/2025_visual-surveys", layer = "toad_surveys") |>
  select(-(2:5), -photos, - audio, -X_latitude, -X_longitude) |>
  mutate(year = year(date),
         month = month(date),
         day = day(date)
  )

#2023-4 visual survey data
df.recon <- read_excel(path = "dat/invasion-front-reconnaissance-data.xlsx", sheet = "recon_data") |>
  mutate(hour = hour(time), 
         minute = minute(time), 
         date = as.Date(date),
         ttd = how_many_p*search_tim,
         ttd.censored = ifelse(any_cane_t=="yes", ttd, NA), # only report ttd for positive sighting
         censored = as.numeric(any_cane_t=="no")) |> # censored, or not?
  st_as_sf(coords = c("X_longitude", "X_latitude")) |>
  st_set_crs(value = 4283) |> # set crs (GDA94/GRS 1980)
  st_transform(crs = st_crs(df.fulcrum)) # transform to whatever comes from fulcrum

# merge the datasets
df <- bind_rows(df.fulcrum, df.recon)
rm(df.recon, df.fulcrum)

# st_write(df.recon, "out/recon_sites.kml", append = FALSE)




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




