# load survey data
source("src/a-load-data.R")
library(alphahull)
library(ggplot2)

# load waterpoint data
df.wp <- read.csv(file = "../spread-model/dat/waterpoint-data_LaGrange.csv") |>
  select(X, Y, ndays_1, colonised, TCZ, origin_des) |>
  mutate(origin_des = as.numeric(as.factor(origin_des))-1,
         colonised = 0) |> # set all colonised to 0
  st_as_sf(coords = c("X", "Y"), remove = FALSE) |>
  st_set_crs(value = 3577) |> # set source crs (Australian albers)
  st_transform(crs = st_crs(df)) # transform to whatever comes from fulcrum

# load additional waterpoints from Tim
d_extra <- st_read("../spread-model/dat/tims_points.kml") |> 
  st_transform(crs = 3577) #|> 
  #st_zm(drop = TRUE, what = "Z") # drop Z dimension
d_extra <- cbind(d_extra, st_coordinates(d_extra)) |>
  st_transform(crs = st_crs(df)) |> # convert to fulcrum crs
  select(X, Y, geometry) |> 
  mutate(origin_des = 0, ndays_1 = 34, colonised = 0, TCZ = 0)


# bind the two sets
df.wp <- bind_rows(df.wp, d_extra)

# Make an alpha hull from survey positive records
ahull.coords <- df |>
  filter(any_cane_t == "yes") |>
  st_transform(crs = 3577) |> # Albers for correct distances
  st_coordinates() |>
  as.data.frame() |> 
  distinct() |> 
  as.matrix()

ah <- ahull(ahull.coords, alpha = 100000)

# cast alpha hull to polygon
## Extract edges using endpoint indices
edges <- ah$arcs
line_list <- lapply(1:nrow(edges), function(i) {
  p1 <- ahull.coords[edges[i, "end1"], ]
  p2 <- ahull.coords[edges[i, "end2"], ]
  st_linestring(rbind(p1, p2))
})

## Combine lines into an sf object
lines_sf <- st_sfc(line_list, crs = 3577)  |>  
  st_sf() 

## Union lines and polygonize
lines_union <- st_union(lines_sf)
polygon <- st_polygonize(lines_union)

## Convert to sf object
alpha_polygon_sf <- st_sf(geometry = polygon) |> 
  st_transform(crs = st_crs(df.wp)) # take back to same crs as waterpoints

# Find waterpoints inside the alpha hull
df.wp$colonised[st_within(df.wp, alpha_polygon_sf, sparse = FALSE)] <- 1

# plot to check
ggplot() +
  geom_sf(data = df.wp, color = "red", size = 2) +
  geom_sf(data = alpha_polygon_sf, fill = "lightblue", color = "blue", alpha = 0.5) +
  theme_minimal() +
  labs(title = "Spatial Points and Alpha Hull Polygon")

# Create a buffered polygon to select points on which to run the model
buffered.polygon <- alpha_polygon_sf |> 
  st_transform(crs = 3577) |> 
  st_buffer(dist = 2e5) |> # 200km buffer
  st_transform(st_crs(df.wp))

# Select points
df.model.pts <-  filter(df.wp, st_within(df.wp, buffered.polygon, sparse = FALSE)) |> 
  st_transform(crs = 3577)

