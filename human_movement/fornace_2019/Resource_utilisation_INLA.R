#################################################################################
################## Resource utilisation using hurdle model INLA #################
#################################################################################

library(fields)
library(INLA)

## Read data 
# Y = PDF of BRB
# Z = Binomial 0/1 for presence/ absence
# Environmental covariates (in this example: ohs, fr, rd, dem, slp, hs)
# Individual ID and covariates (e.g. farm, grp, id)
# Spatial coordinates

## Reset y zeros as NAs
y[y==0]<- NA

## Set intercepts
z.b0 <- rep(1, length(z))
y.b0 <- rep(1, length(y))

## Set random effect
z.ind <- df$id
y.ind <- df$id

## Subset covariates
covar <- df[c("ohs", "fr", "rd", "dem", "slp", "hs")]

## Extract covariates
z.ohs <- covar$ohs
y.ohs <- covar$ohs

z.lu <- factor(df$lu)
y.lu <- factor(df$lu)

z.fr <- covar$fr
y.fr <- covar$fr

z.rd <- covar$rd
y.rd <- covar$rd

z.dem <- covar$dem
y.dem <- covar$dem

z.slp <- covar$slp
y.slp <- covar$slp

z.hs <- covar$hs
y.hs <- covar$hs

z.farm <- df$farm
z.grp <- factor(df$grp)

### Model separately

## Binomial model

# Data
dat.z <- list(z=z, z.b0=z.b0, z.ohs=z.ohs, z.lu=z.lu, z.fr=z.fr, z.rd=z.rd, z.farm=z.farm, z.grp=z.grp, 
              z.dem=z.dem, z.slp=z.slp, z.hs=z.hs, z.ind=z.ind, link = 1)

# Priors
fixed.priors <- list(mean = list(z.b0=0, z.ohs=0, z.lu=0, z.fr=0, z.rd= 0, z.dem=0, z.slp=0, z.hs=0, z.grp=0, z.farm=0), 
                     prec=list(z.b0 = 1/100, z.ohs=1/100, z.lu=1/100, z.fr=1/100, z.rd= 1/100, z.dem=1/100, z.slp=1/100, 
                               z.hs=1/100, z.farm = 1/100, z.grp = 1/100))

# Model
form.z <- z ~ 0 + z.b0 + z.ohs + z.fr + z.rd + z.dem + z.slp + z.hs + z.farm + f(z.ind, model="iid", hyper = list(prec=list(initial=1.32)))

res.z <- inla(form.z, family = list("binomial"), data = dat.z, verbose = TRUE, 
              control.compute = list(openmp.strategy="huge",dic = TRUE),
              control.family = list(link = "logit"), control.fixed = fixed.priors,
              control.predictor = list(compute=TRUE, link = dat.z$link),
              Ntrials = 1, control.inla = list(strategy="gaussian", int.strategy="eb"))
summary(res.z)

## Gamma model

# Data
dat.y <- list(y=y, y.b0=y.b0, y.ohs=y.ohs, y.lu=y.lu, y.fr=y.fr, y.rd=y.rd, 
              y.dem=y.dem, y.slp=y.slp, y.hs=y.hs, y.ind=y.ind, link = 1)

# Model
form.y <- y ~ 0 + y.b0 + y.ohs + y.fr + y.rd + y.dem + y.slp + y.hs
res.y <- inla(form.y, family = list("gamma"), data = dat.y, 
              verbose = TRUE, control.compute = list(openmp.strategy="huge", dic = TRUE),
              control.predictor = list(compute=TRUE, link = dat.y$link),
              control.family = list(hyper=list(theta=list(initial=0.236))),
              control.fixed = fixed.priors,Ntrials = 1, num.threads = 2,
              control.inla = list(strategy="gaussian", int.strategy="eb"), debug=TRUE)
summary(res.y)

### Model jointly - hurdle model

# Data
dat <- list(Y=rbind(cbind(z, NA), cbind(NA, y)))
n <- length(z)
dat$z.b0 <- rep(1:0, c(n, n))
dat$y.b0 <- rep(0:1, c(n, n))
dat$link <- rep(1:2, c(n, n))
dat$z.ohs <- c(z.ohs, rep(0, n))
dat$y.ohs <- c(rep(0, n), y.ohs)
dat$z.lu <- c(factor(z.lu), rep(0, n))
dat$y.lu <- c(rep(0, n), factor(y.lu))
dat$z.fr <- c(z.fr, rep(0, n))
dat$y.fr <- c(rep(0, n), y.fr)
dat$z.rd <- c(z.rd, rep(0, n))
dat$y.rd <- c(rep(0, n), y.rd)
dat$z.dem <- c(z.dem, rep(0, n))
dat$y.dem <- c(rep(0, n), y.dem)
dat$z.slp <- c(z.slp, rep(0, n))
dat$y.slp <- c(rep(0, n), y.slp)
dat$z.hs <- c(z.hs, rep(0, n))
dat$y.hs <- c(rep(0, n), y.hs)
dat$z.ind <- c(z.ind, rep(0, n))

# Priors
fixed.priors <- list(mean = list(z.b0=0, z.ohs=0, z.lu=0, z.fr=0, z.rd= 0, z.dem=0, z.slp=0, z.hs=0, y.b0=0, 
                                 y.ohs=0, y.lu=0, y.fr=0, y.rd= 0, y.dem=0, y.slp=0, y.hs=0), 
                     prec=list(z.b0 = 1/100, z.ohs=1/100, z.lu=1/100, z.fr=1/100, z.rd= 1/100, z.dem=1/100, z.slp=1/100, z.hs=1/100, 
                               y.b0 = 1/100, y.ohs=1/100, y.lu=1/100, y.fr=1/100, y.rd= 1/100, y.dem=1/100, y.slp=1/100, y.hs=1/100))

# Model
form.zy <- Y ~ 0 + z.b0 + z.ohs + z.fr + z.rd + z.dem + z.slp + z.hs + 
          f(z.ind, model="iid", hyper=list(theta=list(prior="loggamma",param=c(1,0.01)))) +
          y.b0 + y.ohs + y.fr + y.rd + y.dem + y.slp + y.hs

res.zy <- inla(form.zy, family = list("binomial", "gamma"), data =dat, verbose=TRUE,
              control.compute = list(openmp.strategy="huge",dic = TRUE),
              Ntrials = 1, control.fixed = fixed.priors, control.family=list(list(), 
                                                                             list(hyper=list(theta=list(initial=0)))),
              control.predictor = list(compute=TRUE, link = dat$link), 
              control.inla = list(strategy="gaussian", int.strategy="eb"))
summary(res.zy)


