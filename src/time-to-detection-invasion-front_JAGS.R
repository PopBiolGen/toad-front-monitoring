# to estimate the current position of the invasion front
# uses latest year's data + presences from previous years
source("src/a-load-data.R")
library(rjags)

library(maptiles)
library(stars)
library(tidyterra)


# select the relevant data
in.dat <- filter(df, water_available == "available" & survey_type %in% c("nocturnal", "interview")) |> 
  filter(year == max(year) | toad.present == 1) |> # get latest year's data + any positive records from previous years
  mutate(is.interview = as.integer(survey_type == "interview"),
         person.minutes = if_else(survey_type == "interview", 0, person.minutes))

# get x, y coordinates for each record, in metres and add to dataframe
proj.coords <- in.dat |> 
  st_transform(crs = 3577) |> # project to albers
  st_coordinates()

mean.coord <- colMeans(proj.coords) # make centre of study site the reference point
proj.coords.centred <- sweep(proj.coords, 2, mean.coord, FUN = "-") |>
  as.data.frame() |> 
  rename(X.c = X, Y.c = Y)

in.dat <- cbind(in.dat, proj.coords, proj.coords.centred)

# make lists of data to feed JAGS
data.list <- list(ttd = in.dat$p.m.positive, # time to detection data (NA's where toads not found)
                  is.detected = in.dat$toad.present, 
                  tmax.i = in.dat$person.minutes,
                  is.interview = in.dat$is.interview,
                  n.obs = nrow(in.dat),
                  x = in.dat$X.c,
                  y = in.dat$Y.c)
init.list <- list(lambda = 1/10, 
                  a = -3,
                  b = -100000)

# the model
ttd.mod <- jags.model(file = "src/model-files/time-to-detection-invasion-front_JAGS.txt", 
           data = data.list, 
           inits = init.list,
           n.chains = 3)
update(ttd.mod, n.iter = 5000) # burn in
ttd.samp<-coda.samples(ttd.mod, 
                variable.names = c("lambda",
                                   "a",
                                   "b"), 
                n.iter = 10000, 
                thin = 5)
gelman.diag(ttd.samp)

(mod1 <- summary(ttd.samp))
coda::densplot(ttd.samp)

# save parameters out
save(mod1, mean.coord, file = file.path(Sys.getenv("DATA_PATH"), "invasion-front-parameters.Rdata"))

# make a plot of estimated line
# Calculate samples
ttd.samp.mat <- as.matrix(ttd.samp) # get samples
x.seq <- seq(from = min(in.dat$X.c), to = max(in.dat$X.c), length.out = 100) # define a vecotr ox x-values
out.mat <- matrix(NA, ncol = length(x.seq), nrow = nrow(ttd.samp.mat))
for (ii in 1: nrow(ttd.samp.mat)){
  out.mat[ii,] <- ttd.samp.mat[ii, "a"]*x.seq + ttd.samp.mat[ii, "b"]
}
# get quantiles
preds <- apply(out.mat, 2, quantile, p = c(0.025, 0.5, 0.975)) |> 
  t() |> 
  bind_cols(x.seq = x.seq) |> 
  as.data.frame() |> 
  rename(x = x.seq, y = '50%', y_lower = '2.5%', y_upper = '97.5%')


ggplot() +
  geom_point(data = in.dat, aes(x = X.c, y = Y.c, col = toad.present)) +
  geom_ribbon(data = preds, 
              aes(x = x, 
                  ymin = y_lower, 
                  ymax = y_upper),
                  fill = "lightblue", 
                  alpha = 0.3) +
  geom_line(data = preds, 
            aes(x = x, 
                y = y)) +
  coord_cartesian(ylim = range(in.dat$Y.c)) +
  labs(x = "Easting", y = "Northing")
ggsave(filename = "out/front-location.pdf")

# make a map of the estimated line
## predictions as sf
pred_to_sf <- function(X, Y, re.centre){
  data.frame(X = X, Y = Y) |> 
    sweep(2, re.centre, FUN = "+") |> #move back to original centre
    st_as_sf(coords = c("X", "Y"),
             crs = 3577) |> 
    st_transform(crs = 4326)
}
# line and upper/lower
predn <- pred_to_sf(preds$x, preds$y, mean.coord) |> st_combine() |> st_cast("LINESTRING")
upp <- pred_to_sf(preds$x, preds$y_upper, mean.coord)
low <- pred_to_sf(preds$x, preds$y_lower, mean.coord)
# survey points
points.p <- st_transform(in.dat, crs = 4326)

# base map tile - bbox focused on survey points only
bbox <- st_bbox(points.p)

sat.raster <- get_tiles(
  x = st_as_sfc(bbox),
  provider = "Esri.WorldImagery",
  zoom = 11
)

# build 95% CI band polygon from upper and lower boundary points
upp_coords <- st_coordinates(upp)
low_coords  <- st_coordinates(low)
front.band <- st_polygon(list(
  rbind(upp_coords, low_coords[nrow(low_coords):1, ], upp_coords[1, ])
)) |>
  st_sfc(crs = 4326) |>
  st_sf()

ggplot() +
  geom_spatraster_rgb(data = sat.raster) +
  geom_sf(data = front.band, fill = "red", colour = NA, alpha = 0.25) +
  geom_sf(data = predn, colour = "red", linetype = "solid", linewidth = 0.8) +
  geom_sf(data = points.p, aes(colour = factor(toad.present)), size = 2) +
  scale_colour_manual(
    values = c("0" = "#4575b4", "1" = "#d73027"),
    labels = c("0" = "Absent", "1" = "Present"),
    name   = "Toad presence"
  ) +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
           ylim = c(bbox["ymin"], bbox["ymax"]),
           expand = TRUE) +
  labs(x = "Longitude", y = "Latitude") +
  theme_bw() +
  theme(panel.grid = element_blank())

ggsave(filename = "out/current-front.pdf", width = 200, height = 180, units = "mm")
