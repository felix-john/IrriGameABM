#-------------------------------------------------------------------------------------
# I. Prerequirements
#-------------------------------------------------------------------------------------

# load RNetLogo package 
# (if not installed already, execute install.packages("rJava") and install.packages("RNetLogo") )
# Sys.setenv(NOAWT=1)
require(RNetLogo)

# an R random seed (for beeing reproducible)
set.seed(1606350664)

# TODO: adapt these paths to your requirements
# the NetLogo installation path (where the NetLogo.jar is located)
nl.path     <- "/Applications/NetLogo 5.2"
# the path to the NetLogo model file
# model.path  <- "../../src/irrigame_comm.nlogo"
model.path  <- "/Users/felixx/Documents/UFZ-Cloud/IrriGameABM/src/irrigame_comm.nlogo"
# the simulation function
# simfun.path <- "../../src/sim_function1.R"
simfun.path <- "/Users/felixx/Documents/UFZ-Cloud/IrriGameABM/src/sim_function1.R"

# TODO: set values for those parameters that are not changes during the experiment
#       parameter names have to be the names of the parameters in the NetLogo model
fix.params <- list(
  'scenario'      = "\"social-values\"",
  'limcom'        = "false",
  'visioneffect'  = "false",
  'phase2?'       = "false"
)      

# TODO: give value ranges for each parameter 
#       parameter names has to been the names of the parameters in the NetLogo model
parameter.values <- list(
  'psat'          = list(min = 0.0,  max = 1.0, random.function="qunif"),
  'umin'          = list(min = 0.0,  max = 50.0, random.function="qunif"),
  'alpha'         = list(min = -1.0, max = 1.0, random.function="qunif"),
  'beta'          = list(min = -1.0, max = 1.0, random.function="qunif"),
  'mu'            = list(min = 0.0,  max = 100.0, random.function="qunif")
)

# TODO: adapt to your observation summary statistics (here: just the mean of the ranges)
calibration.ranges <- data.frame(
  'infrastructure' = c(83.38, 78.43, 74.19, 74.00, 72.57),
  'inequality' = c(0.283, 0.223, 0.241, 0.182, 0.257)
)

# TODO: number of parameter sets
sample.count <- 10

# how many repetitions for each parameter set should be run (to control stochasticity)?
# TODO: adapt the number of repititions, set to 1 if deterministic model
no.repeated.sim <- 5

# TODO: set tolerance value for ABC (required proportion of points accepted)
tol <- 0.3

# TODO: should R report the progress
trace.progress = TRUE

# initialize NetLogo
nl.sm9 <- "irrigame_comm.nlogo"
NLStart(nl.path, gui=FALSE, nl.obj=nl.sm9)
NLLoadModel(model.path,nl.obj=nl.sm9)
# NLQuit(all = T)

#-------------------------------------------------------------------------------------
# II. Definition of the simulation function to test a parameter set
#-------------------------------------------------------------------------------------

# load the code of the simulation function (name: simulate)
source(file=simfun.path)


#-------------------------------------------------------------------------------------
# III. Creation of the parameter sets by latin hypercube sampling
#-------------------------------------------------------------------------------------

# get names of parameters
parameter.names <- names(parameter.values)
fix.param.names <- names(fix.params)

#-------------------------------------------------------------------------------------
# III.A. Alternative 1: Package tgp
#-------------------------------------------------------------------------------------
# load required package tgp
#require(tgp)

# create random (latin hypercube) samples, it would be possible to use the ranges directly
# by we will use the value interval [0,1] and apply a random distribution function afterwards
#lhs.design <- lhs(sample.count, t(matrix(c(0,1),2,length(parameter.values))))

# transform the standardized random values to the real parameter value range
# and apply the desired random distribution
#lhs.design <- lapply(seq(1,length(parameter.values)), function(i) {
#    match.fun(parameter.values[[i]]$random.function)(lhs.design[,i], parameter.values[[i]]$min, parameter.values[[i]]$max)
#  })
#names(lhs.design) <- parameter.names


#-------------------------------------------------------------------------------------
# III.B. Alternative 2: Package lhs
#-------------------------------------------------------------------------------------
# NOTE: here, we can't give the real value range. 
# This package will create values from 0 to 1 and must be transformed afterwards.
# But this package is much more powerfull supplying different LHS methods.
require(lhs)

# create a random sample of parameter sets (Latin Hypercube Sampling)
lhs.design <- randomLHS(sample.count, length(parameter.values))

# transform the standardized random values to the real parameter value range
# and apply the desired random distribution
lhs.design <- lapply(seq(1,length(parameter.values)), function(i) {
  match.fun(parameter.values[[i]]$random.function)(lhs.design[,i], parameter.values[[i]]$min, parameter.values[[i]]$max)
})
names(lhs.design) <- parameter.names


#-------------------------------------------------------------------------------------
# IV. Run of the (LHS) simulation for all parameter sets
#-------------------------------------------------------------------------------------

# variable used for progress tracing 
# (global variable: not nice, but I see currently no other way in R)
already.processed <- 0

# simulate for all parameter sets and
# get results of all evalulation criteria as matrix
  sim.results.lhs <- apply(data.frame(lhs.design), 1, 
                           simulate,
                           no.repeated.sim=no.repeated.sim, nl.obj=nl.sm9, 
                           trace.progress=trace.progress,
                           parameter.names=parameter.names, 
                           iter.length=sample.count,
                           function.name="LHS",
                           fix.params, fix.param.names
  )