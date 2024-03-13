###################################################################################
################ Negative binomial model of mosquito biting rates #################
###################################################################################

require(plyr)
require(raster)
require(rgdal)
require(INLA)
require(gridExtra)
require(dplyr)
require(lubridate)
require(lattice)
require(fields)
require(raster)

################# Set function for penalised complexity priors ###################

local.inla.spde2.matern.new = function(mesh, alpha=2, prior.pc.rho, prior.pc.sig)
{
  # Call inla.spde2.matern with range and standard deviation parametrization
  d = INLA:::inla.ifelse(inherits(mesh, "inla.mesh"), 2, 1)
  nu = alpha-d/2
  kappa0 = log(8*nu)/2
  tau0   = 0.5*(lgamma(nu)-lgamma(nu+d/2)-d/2*log(4*pi))-nu*kappa0
  spde   = inla.spde2.matern(mesh = mesh,
                             B.tau   = cbind(tau0,   nu,  -1),
                             B.kappa = cbind(kappa0, -1, 0))
  
  # Change prior information
  param = c(prior.pc.rho, prior.pc.sig)
  spde$f$hyper.default$theta1$prior = "pcspdega"
  spde$f$hyper.default$theta1$param = param
  spde$f$hyper.default$theta1$initial = log(prior.pc.rho[1])+1
  spde$f$hyper.default$theta2$initial = log(prior.pc.sig[1])-1
  
  # End and return
  return(invisible(spde))  
}


# And then call the function with
sig0 = 1; rho0 = .3 #rho0 is typical range, sig0 typical sd
# - YOU MUST SET rho0 , e.g. to half the length/width of your space
# - You may reduce sig0 to have less spatial effect
# - You may increase rho0 additionally, to make the spatial effect smoother

################################# Set up data and model #############################

## Load data
# Counts of mosquitoes (bal)
# Location of  (sp)
# Spatial coordinates
# Environmental covariates
# Date/ time (tm)
# Number of person nights (nite)

## Create mesh
pts <- cbind(df$X,df$Y)
mesh <- inla.mesh.create.helper(points=pts, max.edge=c(0.5,2), cut=0.5)
plot(mesh)
points(pts[,1], pts[,2], pch=19, cex=1, col="red")

## Sample for independent validation - withhold 20%
df$new.bal <- df$bal
id.cross <- sample(c(1:nrow(df)),ceiling(nrow(df)*0.8))
df$new.bal[-id.cross] <- NA
link <- rep(NA,nrow(df))
link[which(is.na(df$new.bal))] <- 1

## Variables for model
sp <- df$sp # spatial ID
frd <- df$frd
tm <- df$tm
pop <- df$pop
evi <- df$evi
ml <- df$ml
Y <- df$new.bal
offset <- log(df$nite) 
inla.df <- data.frame(sp, tm, ml, pop, evi, frd, Y, offset)
covars <- inla.df[,c(2:6)]

########################## MODEL FITTING ##########################

## Set priors for fixed effects
fixed.priors <- list(mean.intercept = 0, prec.intercept=1/100, mean = list(pop=0, frd=0, evi= 0), 
                     prec=list(pop =1/100, evi=1/100, frd=1/100))

## Use penalised priors for spatial component
spde = inla.spde2.pcmatern(mesh,
                             prior.sigma = c(0.1, 0.01),
                             prior.range = c(10, 0.01))

## Set number of replicates as month
repl <- tm
A <- inla.spde.make.A(mesh, repl=repl, loc = pts)

## Create spatial index
ind <- inla.spde.make.index(name = 's', n.spde=spde$n.spde, n.repl = max(tm))

## Stack data
stk <- inla.stack(data=list(Y=Y), tag='est', A=list(A,1), 
                   effects=list(ind, list(data.frame(b0=1, covars))))

## Run model - recommended prior for rw (increasing u is weaker effect, decreasing stronger)
form <- Y~ 0 + b0 + pop + evi + frd + f(s, model=spde, replicate=s.repl) + 
  f(inla.group(tm), model='rw1', scale.model = TRUE, hyper = list(theta = list(prior="pc.prec", param=c(1,0.01)))) 

res <- inla(form, family = "nbinomial", data=inla.stack.data(stk), offset=offset,
             control.predictor = list(A=inla.stack.A(stk), compute=TRUE),
             control.inla=list(strategy='laplace'),
             control.fixed= fixed.priors,
             control.compute=list(dic=TRUE, cpo=TRUE))
summary(res)
res$dic$dic

## Model checks
str(sdat <- inla.stack.index(stk, tag = 'est')$data)
fitted.values <- res$summary.fitted.values$mean[sdat]/df$nite
observed.values <- df$bite
rmse <- sqrt(mean((observed.values-fitted.values)^2))
plot(fitted.values, observed.values, ylim = c(0,30), xlim = c(0,30))

## cross-validatory measures - compare with independent training data
str(sdat <- inla.stack.index(stk, tag = 'est')$data)
fitted.values <- res$summary.fitted.values$mean[sdat]
fitted.values <- fitted.values[is.na(link)==F]
observed.values <- df$bal[is.na(link)==F]
rmse <- sqrt(mean((observed.values-fitted.values)^2))

plot(fitted.values, observed.values, xlim = c(0,40), ylim=c(0,40))
plot(density(observed.values))
plot(density(fitted.values))


 