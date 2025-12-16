source("src/a-load-data.R")
library(rjags)

# select the relevant data
in.dat <- filter(df, water_available == "available" & survey_type == "nocturnal")

# make lists of data to feed JAGS
data.list <- list(ttd = in.dat$p.m.positive, # time to detection data (NA's where toads not found)
                  is.detected = in.dat$toad.present, 
                  tmax.i = in.dat$person.minutes,
                  n.obs = nrow(in.dat))
init.list <- list(p.occ = 0.2, 
                  lambda = 1/10, 
                  occ = ifelse(data.list$is.detected == 1, 1, rbinom(length(data.list$is.detected), 1, 0.2)))

# the model
ttd.mod <- jags.model(file = "src/model-files/time-to-detection_JAGS.txt", 
           data = data.list, 
           inits = init.list,
           n.chains = 3)
update(ttd.mod, n.iter = 5000) # burn in
ttd.samp<-coda.samples(ttd.mod, 
                variable.names = c("lambda",
                                   "p.occ"), 
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
