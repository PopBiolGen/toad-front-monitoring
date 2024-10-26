source("src/a-load-data.R")
library(nimble)

t.to.det.code <- nimbleCode(
  {
    # priors
    p.occ ~ dbeta(1, 1)
    lambda ~ dunif(0, 1)
    
    for (ii in 1:n.obs){
      p.p[ii] <- 1-exp(-lambda*time[ii]) # probability observed if present
      occ[ii] ~ dbern(p.occ) # present at site?
      obs[ii] ~ dbern(occ[ii]*p.p[ii]) # observed present with this probability
    }
  }
)

# select the relevant data
in.dat <- filter(df.recon, water.status == "available" & survey.type == "nocturnal")

# make lists of data to feed nimble
constant.list <- list(n.obs = nrow(in.dat),
                      time = in.dat$person.minutes)
data.list <- list(obs = in.dat$toad.present)
init.list <- list(p.occ = 0.3, lambda = 1/10)

# the model
ttd.mod <- nimbleModel(code = t.to.det.code, 
                  name = "ttd.mod",
                  constants = constant.list,
                  data = data.list)

ttd.mod$getNodeNames()
ttd.mod$plotGraph()
ttd.mod$simulate("obs")
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
                  niter = ni, 
                  nburnin = nb, 
                  nchains = nc,
                  check = FALSE,
                  samplesAsCodaMCMC = TRUE)

coda::gelman.diag(a.n, multivariate = FALSE)
summary(a.n)