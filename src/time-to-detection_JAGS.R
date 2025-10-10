source("src/a-load-data.R")
library(rjags)

# select the relevant data
in.dat <- filter(df.recon, water.status == "available" & survey.type == "nocturnal")

# make lists of data to feed JAGS
data.list <- list(ttd = in.dat$ttd.censored, # time to detection data (NA's where toads not found)
                  censored = in.dat$censored, 
                  tmax.i = in.dat$person.minutes,
                  n.obs = nrow(in.dat))
init.list <- list(p.occ = 0.2, lambda = 1/20, occ = in.dat$toad.present, ttd = ifelse(in.dat$toad.present==1, NA, in.dat$ttd.censored))

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

summary(ttd.samp)
coda::densplot(ttd.samp)
