source("src/a-load-data.R")
library(rjags)
library(dplyr)
library(sf)
library(ggplot2)
library(maptiles)
library(tidyterra)

# 1. Data preparation -------------------------------------------------------

in.dat <- filter(df, water_available == "available" &
                   survey_type %in% c("nocturnal", "interview")) |>
  mutate(is.interview   = as.integer(survey_type == "interview"),
         person.minutes = if_else(survey_type == "interview", 0, person.minutes),
         p.m.positive   = if_else(survey_type == "interview", NA_real_, p.m.positive))

years   <- sort(unique(in.dat$year))
n.years <- length(years)
year.idx <- match(in.dat$year, years)

# project to Albers, scale to km, centre on grand mean
proj.coords <- in.dat |>
  st_transform(crs = 3577) |>
  st_coordinates() / 1000  # metres -> km

mean.coord <- colMeans(proj.coords)
proj.coords.centred <- sweep(proj.coords, 2, mean.coord, FUN = "-") |>
  as.data.frame() |>
  rename(X.c = X, Y.c = Y)

in.dat <- cbind(in.dat, proj.coords, proj.coords.centred)

# 2. JAGS data and inits ----------------------------------------------------

data.list <- list(
  ttd          = in.dat$p.m.positive,
  is.detected  = in.dat$toad.present,
  tmax.i       = in.dat$person.minutes,
  is.interview = in.dat$is.interview,
  year.idx     = year.idx,
  n.years      = n.years,
  n.obs        = nrow(in.dat),
  x            = in.dat$X.c,
  y            = in.dat$Y.c
)

init.list <- list(
  lambda = 1/10,
  p.int  = 0.5,
  a      = 0,
  b      = rep(min(in.dat$Y.c) - 1, n.years)
)

# 3. Run JAGS ---------------------------------------------------------------

ttd.mod <- jags.model(
  file    = "src/model-files/time-to-detection-multi-year_JAGS.txt",
  data    = data.list,
  inits   = init.list,
  n.chains = 3
)
update(ttd.mod, n.iter = 5000) # burn in

ttd.samp <- coda.samples(
  ttd.mod,
  variable.names = c("lambda", "p.int", "a", "b", "delta"),
  n.iter = 10000,
  thin   = 5
)

gelman.diag(ttd.samp)
(mod.multi <- summary(ttd.samp))
coda::densplot(ttd.samp)

save(mod.multi, mean.coord, years,
     file = file.path(Sys.getenv("DATA_PATH"), "invasion-front-parameters-multi-year.Rdata"))

# 4. Generate per-year predictions ------------------------------------------

ttd.samp.mat <- as.matrix(ttd.samp)
x.seq <- seq(from = min(in.dat$X.c), to = max(in.dat$X.c), length.out = 100)

# helper: predictions for one year (X, Y in km), returned as sf with a year label
pred_to_sf <- function(X, Y, re.centre, yr) {
  data.frame(X = X, Y = Y) |>
    sweep(2, re.centre, FUN = "+") |>
    (\(d) d * 1000)() |>        # km -> metres for Albers CRS
    st_as_sf(coords = c("X", "Y"), crs = 3577) |>
    st_transform(crs = 4326) |>
    mutate(year = yr)
}

lines.list <- vector("list", n.years)
bands.list <- vector("list", n.years)

for (tt in seq_len(n.years)) {
  yr <- years[tt]
  b.col <- paste0("b[", tt, "]")

  out.mat <- matrix(NA, ncol = length(x.seq), nrow = nrow(ttd.samp.mat))
  for (ii in seq_len(nrow(ttd.samp.mat))) {
    out.mat[ii, ] <- ttd.samp.mat[ii, "a"] * x.seq + ttd.samp.mat[ii, b.col]
  }

  preds.yr <- apply(out.mat, 2, quantile, p = c(0.025, 0.5, 0.975)) |>
    t() |>
    as.data.frame() |>
    setNames(c("y_lower", "y", "y_upper")) |>
    mutate(x = x.seq)

  # median line as LINESTRING
  line_geom <- pred_to_sf(preds.yr$x, preds.yr$y, mean.coord, yr) |>
    st_geometry() |>
    st_combine() |>
    st_cast("LINESTRING")
  lines.list[[tt]] <- st_sf(year = as.numeric(yr), geometry = line_geom)

  # CI band as polygon
  upp_coords <- pred_to_sf(preds.yr$x, preds.yr$y_upper, mean.coord, yr) |> st_coordinates()
  low_coords  <- pred_to_sf(preds.yr$x, preds.yr$y_lower, mean.coord, yr) |> st_coordinates()
  band_geom <- st_sfc(
    st_polygon(list(rbind(upp_coords, low_coords[nrow(low_coords):1, ], upp_coords[1, ]))),
    crs = 4326
  )
  bands.list[[tt]] <- st_sf(year = as.numeric(yr), geometry = band_geom)
}

front.lines <- do.call(rbind, lines.list)
front.bands <- do.call(rbind, bands.list)

# 5. Faceted map ------------------------------------------------------------

points.p <- st_transform(in.dat, crs = 4326) |>
  mutate(year = years[year.idx])

bbox <- st_bbox(points.p)

sat.raster <- get_tiles(
  x        = st_as_sfc(bbox),
  provider = "Esri.WorldImagery",
  zoom     = 11
)
# aggregate to reduce cell count before faceted rendering
sat.raster <- terra::aggregate(sat.raster, fact = 3)

print(
  ggplot() +
    geom_spatraster_rgb(data = sat.raster) +
    geom_sf(data = front.bands, fill = "red", colour = NA, alpha = 0.25) +
    geom_sf(data = front.lines, colour = "red", linetype = "dashed", linewidth = 0.8) +
    geom_sf(data = points.p, aes(colour = factor(toad.present)), size = 1.8) +
    scale_colour_manual(
      values = c("0" = "#4575b4", "1" = "#d73027"),
      labels = c("0" = "Absent", "1" = "Present"),
      name   = "Toad presence"
    ) +
    coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
             ylim = c(bbox["ymin"], bbox["ymax"]),
             expand = TRUE) +
    facet_wrap(~year) +
    labs(x = "Longitude", y = "Latitude") +
    theme_bw() +
    theme(panel.grid = element_blank())
)

ggsave("out/multi-year-front.pdf", width = 280, height = 130, units = "mm")

# 6. Posterior of front movement between consecutive years ------------------

delta.df <- ttd.samp.mat[, grep("^delta", colnames(ttd.samp.mat)), drop = FALSE] |>
  as.data.frame() |>
  setNames(as.character(years[-n.years])) |>  # starting year of each interval
  tidyr::pivot_longer(everything(), names_to = "starting_year", values_to = "distance_km") |>
  dplyr::mutate(starting_year = as.integer(starting_year),
                distance_km   = abs(distance_km))

print(
  ggplot(delta.df, aes(x = factor(starting_year), y = distance_km)) +
    geom_violin(fill = "steelblue", alpha = 0.6, colour = NA) +
    geom_boxplot(width = 0.08, outlier.shape = NA) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "red") +
    labs(x = "Starting year",
         y = "Perpendicular front movement (km)") +
    theme_bw()
)

ggsave("out/front-movement-posteriors.pdf", width = 140, height = 120, units = "mm")
