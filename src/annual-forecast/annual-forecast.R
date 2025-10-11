######## load functions and libraries ########
source("src/annual-forecast/annual-forecast-data-prep.R")
source("../spread-model/src/pprocess_functions.R")

######## load data ########
load("../spread-model/dat/Posteriors.RData")

######## annual forecast scenario ########
scen.name <- "annual-forecast"
proj.wd <- getwd() # switch working directories for a moment
setwd("../spread-model")
setup(point.data = df.model.pts, 
      X.id = "X", 
      Y.id = "Y",
      present.id = "colonised",
      artificial.natural.id = "origin_des",
      rain.id = "ndays_1",
      threshold = 100,
      constant.rain = NULL,
      trunc.dist = TRUE,
      TCZ = FALSE)
# write out points for basemapping
write.csv(spread.table, file = file.path(proj.wd, "out/basemap_points.csv"), row.names = FALSE)
# run sims..
sim_out <- run_sims(n.sims = 100, gens = 1, plot = FALSE, rollup = FALSE)
setwd(proj.wd) # switch back to project wd

######## summarise output ########
# extract popmatrices
pop.out <- lapply(sim_out, function(x){x$popmatrix}) 

# summarise and write to shapefile
pop.out <- do.call("rbind", pop.out) |> 
  as.data.frame() |>
  filter(X < mean(X[age==2]) & age < 2) |> # remove already colonised points and points to the east
  group_by(ID) |> 
  summarise(X = mean(X), Y = mean(Y), Prob.colonised = mean(Pres)) |> 
  filter(Prob.colonised > 0) |>  # remove never colonised points
  st_as_sf(coords = c("X", "Y"), remove = FALSE) |>
  st_set_crs(value = 3577) |> 
  st_transform(crs = st_crs(df.wp)) |> 
  st_write(dsn = paste0("out/", year(Sys.Date()), "-forecast.shp"), 
           driver = "ESRI Shapefile",
           append = FALSE)

