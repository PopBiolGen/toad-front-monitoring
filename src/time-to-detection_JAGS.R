source("src/a-load-data.R")
library(rjags)

# select the relevant data
in.dat <- filter(df.recon, water.status == "available" & survey.type == "nocturnal")

# make lists of data to feed JAGS
data.list <- list(obs = in.dat$toad.present, 
                  time = in.dat$person.minutes,
                  n.obs = nrow(in.dat))
init.list <- list(p.occ = 0.3, lambda = 1/10, occ = in.dat$toad.present)

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
