# to simulate data and test recovery of parameters

library(AHMbook)
Tmax <- 60
sim.dat <- simOccttd(M = 100, mean.psi = 0.3, mean.lambda = 1/5, beta1 = 0, alpha1 = 0, Tmax = Tmax, show.plot = FALSE)

# make lists of data to feed JAGS
data.list <- list(obs = sim.dat$z, 
                  time = ifelse(is.na(sim.dat$ttd), Tmax, sim.dat$ttd),
                  n.obs = sim.dat$M)
init.list <- list(p.occ = 0.3, lambda = 1/10, occ = sim.dat$z)

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
