# A script to load the various data sources
library(tidyverse)
library(readxl)
library(sf)

# Define local directory containing data files
data_dir <- file.path(Sys.getenv("DATA_PATH"), "Toads/invasion-front-monitoring")

# check there is an output directory, and make one if it doesn't exist
if (!dir.exists("out")) system("mkdir out")

# 2025 visual survey data (from Fulcrum)
fpath.fulcrum <- "https://web.fulcrumapp.com/shares/31b7089958c7ed6d.geojson"
df.fulcrum <- st_read(fpath.fulcrum) |>
  select(-(2:8), -photos, - audio, -latitude, -longitude, -project, -assigned_to) |>
  mutate(date = as.Date(date),
         time_of_day = hms::hms(hms(time_of_day)),
         hour = hour(time_of_day), 
         minute = minute(time_of_day), 
         year = year(date),
         month = month(date),
         day = day(date),
         temperature = as.numeric(temperature),
         water_available = "available",
         how_many_people_are_searching = as.numeric(how_many_people_are_searching),
         search_time_mins = as.numeric(search_time_mins),
         survey_type = ifelse(how_many_people_are_searching !=0, "nocturnal", "interview")
  ) |> 
  rename(how_many_p = how_many_people_are_searching,
         any_cane_t = any_cane_toads_found,
         time = time_of_day)

#2023-4 visual survey data
fpath.recon <- file.path(data_dir, "invasion-front-reconnaissance-data.xlsx")
df.recon <- read_excel(path = fpath.recon, sheet = "recon_data") |>
  mutate(date = ymd(date),
         time = hms::as_hms(time * 86400)) |> 
  st_as_sf(coords = c("X_longitude", "X_latitude")) |>
  st_set_crs(value = 4283) |> # set crs (GDA94/GRS 1980)
  st_transform(crs = st_crs(df.fulcrum)) # transform to whatever comes from fulcrum

# non-surveyed points
## These added to fill out the alpha hull, but not actually surveyed
X.ns <- c(-921351, -888286, -876303) 
Y.ns <- c(-1891416, -1877100, -1865818)
df.ns <- data.frame(X_longitude = X.ns, X_latitude = Y.ns, any_cane_t = "yes") |> 
  st_as_sf(coords = c("X_longitude", "X_latitude")) |> 
  st_set_crs(value = 3577) |> # set crs (Albers)
  st_transform(crs = st_crs(df.fulcrum))

# merge the datasets
df <- bind_rows(df.fulcrum, df.recon, df.ns) |> 
  mutate(hour = hour(time), 
         minute = minute(time), 
         search_time_mins = ifelse(search_time_mins==0, 0.5, search_time_mins), #ensure even small times are positive
         person.minutes = how_many_p*search_time_mins,
         p.m.positive = ifelse(any_cane_t=="yes", person.minutes, NA), # only report person.minutes for positive sighting
         censored = as.numeric(any_cane_t=="no"), # censored, or not?
         toad.present = 1-censored) # toad present, or not 
rm(df.recon, df.fulcrum, df.ns)

# st_write(df.recon, "out/recon_sites.kml", append = FALSE)
save(df, file = "out/merged-visual-surveys.RData")



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




