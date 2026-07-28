# A script to load the various data sources
library(tidyverse)
library(readxl)
library(sf)

# Define local directory containing data files
is_windows <- Sys.info()[["sysname"]] == "Windows" # are we on windows machine, or not?
if (is_windows) {
  data_dir <- file.path(Sys.getenv("DATA_PATH"), "invasion-front-monitoring")
} else {
  data_dir <- file.path(Sys.getenv("DATA_PATH"), "Toads/invasion-front-monitoring")
} # workaround for Windows demands for shortcut


# check there is an output directory, and make one if it doesn't exist
if (!dir.exists("out")) system("mkdir out")

# Clulow's data (2022-2024)
fpath.clulow <- file.path(data_dir, "clulow-data") # for the Clulow data
  ## All surveyed sites summarised into presence/absence
all.summ <- read_excel(path = file.path(fpath.clulow, "cane_toad_pr_ab_surveys.xlsx")) |> 
  rename(present = 'presence/absence',
         site.name = site) |> 
  mutate(present = as.numeric(present == "yes"),
         date = as.Date(date),
         month = month(date)) 

  ## Subset of surveyed sites with encounter rates
sub.23 <- read_excel(path = file.path(fpath.clulow, "2023_Honours_allSurvey_siteLevel_cleaned.xlsx"),
                     skip = 1) |> 
  select(contains("site"), lat, long, date = dt_start_vis, survey_length, temp = air_temp_vis, total_count, eDNA = PCRrep_prop_detect) |> 
  mutate(date = as.Date(date),
         year = year(date),
         month = month(date)) |> 
  filter(!(eDNA>0 & total_count == 0)) |>  # remove records with only positive eDNA
  select(-eDNA)
names(sub.23) <- gsub(pattern = "_", replacement = ".", names(sub.23))

sub.24.meta <- read_excel(path = file.path(fpath.clulow, "2024_Rmar24_visual-surveys_cleaned.xlsx"),
                     skip = 1, sheet = "site metadata") |> 
  select(contains("Site"), lat = Latitude, long = Longitude, date = Date) |> 
  rename(site = Site, site.name = 'Site name') |> 
  mutate(date = as.Date(date),
         year = year(date),
         month = month(date)) |> 
  select(-contains("comments"))
names(sub.24.meta) <- tolower(make.names(names(sub.24.meta)))

sub.24 <- read_excel(path = file.path(fpath.clulow, "2024_Rmar24_visual-surveys_cleaned.xlsx"),
                          skip = 1, sheet = "site survey data") |> 
  select(contains("Site"), 
         'Time of Day (start)', 
         'Surey length (numeric minutes)', 
         'Air Temp (°C)', 
         Total_count) |> 
  rename(site = 'Site code', 
         site.name = 'Site Name', 
         time = 'Time of Day (start)',
         survey.length = 'Surey length (numeric minutes)',
         temp = 'Air Temp (°C)',
         total.count = Total_count) |> 
  select(-contains("minute"))
names(sub.24) <- tolower(gsub("_", ".", make.names(names(sub.24))))

sub.24 <- left_join(sub.24, sub.24.meta); rm(sub.24.meta)
sub.all <- bind_rows(sub.23, sub.24); rm(sub.23, sub.24)

df.clulow <- full_join(all.summ, sub.all) |> 
  filter(!is.na(date)) |> 
  mutate(search_time_minutes = ifelse(is.na(survey.length), 15, survey.length), # default survey length for
         person.minutes = 2*search_time_minutes) |> # two observers in all surveys, walking separately
  rename(toad.present = present) |> 
  st_as_sf(crs = 4326, coords = c('long', 'lat')) # assume WGS84
rm(all.summ, sub.all)

# 2026 visual survey data from Harry's work
fpath.fulcrum.hrc <- "https://web.fulcrumapp.com/shares/8e29186100065353.geojson"
df.hrc <- st_read(fpath.fulcrum.hrc) |> 
  select(fulcrum_id, date, time_of_day, temperature, how_many_people_are_searching, any_cane_toads_found) |>
  mutate(date = as.Date(date),
         time_of_day = hms::hms(hms(time_of_day)),
         hour = hour(time_of_day),
         minute = minute(time_of_day),
         year = year(date),
         month = month(date),
         day = day(date),
         temperature = as.numeric(temperature),
         how_many_people_are_searching = as.numeric(how_many_people_are_searching)
  ) |>
  rename(how_many_p = how_many_people_are_searching,
         any_cane_t = any_cane_toads_found,
         time = time_of_day)

# 2025 visual survey data (from Fulcrum)
fpath.fulcrum <- "https://web.fulcrumapp.com/shares/31b7089958c7ed6d.geojson"
df.fulcrum <- st_read(fpath.fulcrum) |>
  select(-(2:8), -contains("photos"), -contains("audio"), -contains("gps"), -latitude, -longitude, -project, -assigned_to) |>
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
         time = time_of_day,
         location_n = location_namedescription)

#2023-4 visual survey data (Ben and Tim)
fpath.recon <- file.path(data_dir, "invasion-front-reconnaissance-data.xlsx")

df.recon <- read_excel(path = fpath.recon, sheet = "recon_data") |>
  mutate(date = ymd(date),
         time = hms::as_hms(time)) |> 
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
df <- bind_rows(df.fulcrum, df.recon) |> 
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

source("src/map-toad-presence.R")


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




