# to estimate the current position of the invasion front
# uses latest year's data + presences from previous years
source("src/a-load-data.R")
library(rjags)

# select the relevant data
in.dat <- filter(df, water_available == "available" & survey_type == "nocturnal") |> 
  filter(year == max(year) | toad.present == 1) # get latest year's data + any positive records from previous years

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
  as.data.frame()


ggplot() +
  geom_point(data = in.dat, aes(x = X.c, y = Y.c, col = toad.present)) +
  geom_ribbon(data = preds, 
              aes(x = x.seq, 
                  ymin = `2.5%`, 
                  ymax = `97.5%`),
                  fill = "lightblue", 
                  alpha = 0.3) +
  geom_line(data = preds, 
            aes(x = x.seq, 
                y = `50%`)) +
  coord_cartesian(ylim = range(in.dat$Y.c))
  labs(x = "Easting", y = "Northing")
ggsave(filename = "out/front-location.pdf")
