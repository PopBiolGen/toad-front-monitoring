source("src/a-load-data.R")
library(nimble)

t.to.det.code <- nimbleCode(
  {
    # priors
    p.occ ~ dbeta(1, 1)
    lambda ~ dunif(0, 1)
    
    for (ii in 1:n.obs){
      p.p[ii] <- 1-exp(-1*lambda*time[ii]) # probability observed if present
      occ[ii] ~ dbern(p.occ) # present at site?
      obs[ii] ~ dbern(occ[ii]*p.p[ii]) # observed present with this probability
    }
  }
)

# select the relevant data
in.dat <- filter(df.recon, water.status == "available" & survey.type == "nocturnal")

# make lists of data to feed nimble
constant.list <- list(n.obs = nrow(in.dat))
data.list <- list(obs = in.dat$toad.present, time = in.dat$person.minutes)
init.list <- list(p.occ = 0.3, lambda = 1/10, occ = rep(0, nrow(in.dat)))

# the model
ttd.mod <- nimbleModel(code = t.to.det.code, 
                  name = "ttd.mod",
                  constants = constant.list,
                  data = data.list,
                  inits = init.list)

ttd.mod$getNodeNames()
ttd.mod$plotGraph()
ttd.mod$simulate("lambda")
ttd.mod$lambda
ttd.mod$logProb_obs

# parameters to monitor
params <- c("lambda", "p.occ")

# mcmc settings
nb <- 500
ni <- 1000
nc = 3

a.n <- nimbleMCMC(code = t.to.det.code,
                  monitors = params, 
                  constants = constant.list,
                  data = data.list,
                  inits = init.list,
                  niter = ni, 
                  nburnin = nb, 
                  nchains = nc,
                  check = FALSE,
                  samplesAsCodaMCMC = TRUE)

coda::gelman.diag(a.n, multivariate = FALSE)
summary(a.n)
