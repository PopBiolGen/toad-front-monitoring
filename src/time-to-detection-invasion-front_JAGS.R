source("src/a-load-data.R")
library(rjags)

# select the relevant data
in.dat <- filter(df, water_available == "available" & survey_type == "nocturnal")

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
                  y = in.dat$Y.c,
                  t = in.dat$year-min(in.dat$year))
init.list <- list(lambda = 1/10, 
                  occ = ifelse(data.list$is.detected == 1, 1, rbinom(length(data.list$is.detected), 1, 0.2)),
                  beta = 0)

# the model
ttd.mod <- jags.model(file = "src/model-files/time-to-detection-invasion_JAGS.txt", 
           data = data.list, 
           inits = init.list,
           n.chains = 3)
update(ttd.mod, n.iter = 5000) # burn in
ttd.samp<-coda.samples(ttd.mod, 
                variable.names = c("lambda",
                                   "beta",
                                   "a",
                                   "b",
                                   "c"), 
                n.iter = 10000, 
                thin = 5)
gelman.diag(ttd.samp)

(mod1 <- summary(ttd.samp))
coda::densplot(ttd.samp)


# plot cumulative probability of detection with time
cum.exp <- function(t, lam){
  1-exp(-lam*t)
}
time <- seq(0, 30, 0.2) # 0-30 minutes
y.mean <- cum.exp(time, mod1$quantiles["lambda", "50%"])
y.upp <- cum.exp(time, mod1$quantiles["lambda", "2.5%"])
y.low <- cum.exp(time, mod1$quantiles["lambda", "97.5%"])

pdat <- data.frame(Time = time, y.mean, y.low, y.upp) #|> 

ggplot(data = pdat, aes(x = Time)) +
    geom_ribbon(
    aes(ymin = y.low,
        ymax = y.upp),
    fill = "lightblue", alpha = 0.3
  ) +
  geom_line(aes(y = y.mean)) +
  labs(x = "Person minutes", y = "Detection probability")
ggsave(filename = "out/detection-plot.pdf") 

ggplot(data = in.dat) +
  geom_histogram(aes(x = p.m.positive))
