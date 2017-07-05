install_load <- function (package1, ...)  {   
  
  # convert arguments to vector
  packages <- c(package1, ...)
  
  # start loop to determine if each package is installed
  for(package in packages){
    
    # if package is installed locally, load
    if(package %in% rownames(installed.packages()))
      do.call('library', list(package))
    
    # if package is not installed locally, download, then load
    else {
      install.packages(package)
      do.call("library", list(package))
    }
  } 
}

install_load("reshape2", "dplyr", "sensitivity", "RNetLogo", "rJava")


#-------------------------------------------------------------------------------------
# I. Prerequirements
#-------------------------------------------------------------------------------------

# # load RNetLogo package
# # (if not installed already,
# #  execute install.packages("rJava") and install.packages("RNetLogo") )
# require(RNetLogo)

# an R random seed (for beeing reproducible)
set.seed(-402223867)

# TODO: adapt these paths to your requirements
# the NetLogo installation path (where the NetLogo.jar is located)
nl.path     <- "m:/"
# the path to the NetLogo model file
model.path  <- "m:/IrriGameABM/src/irrigame_comm.nlogo"
# the simulation function
simfun.path <- "m:/IrriGameABM/src/sim_Morris.R"

# set number of cores hard to cores assigned to job
processors <- 4L

# TODO: set values for those parameters that are not changes during the experiment
#       parameter names have to be the names of the parameters in the NetLogo model
fix.params_SV <- list(
  'scenario'      = "\"social-values\"",
  'limcom'        = "false",
  'visioneffect'  = "false",
  'phase2?'       = "false"
)      

fix.param.names_SV <- names(fix.params)

# TODO: give the value for each input factor, min. and max. values for the inputs      
input.values_SV <- list(
  'alpha'     = list(min = -1.0, max = 1.0),
  'beta'  = list(min = -1.0, max = 1.0),
  'mu' = list(min = 0.0, max = 25.0),
  'umin'        = list(min = 0.0, max = 30.0),
  'psat'    = list(min = 0.0, max = 1.0)
)

# TODO: give details for the sampling design of the Morris method (see help(morris) for details)
morris.design_SV <- list(type = "oat", levels = 5, grid.jump = 2)
# 'levels' indicates the number of factor levels that will be created; 
# 'grid.jump' refers to how strongly a parameter will varied from one iteration to the next (= how many parameters levels will be jumped over) - recommendation: levels / 2

# TODO: give the number of repetions of morris screening (argument r in function morris)
no.repetitions_SV <- 50

# TODO: names of output values
output.names <- c("Infrastructure3","Infrastructure6","Infrastructure10", 
                  "Gini_ext3", "Gini_ext6", "Gini_ext10")

# how many repetitions for each parameter set should be run (to control stochasticity)?
# TODO: adapt the number of repetitions, set to 1 if deterministic model
no.repeated.sim_SV <- 50
# refers to inter-run variability: repeats the sim the given number of times and reports the mean

# TODO: plot the results on the screen?
plot.on.screen <- FALSE

# TODO: plot the results in 3D, (3D requires package rgl)
plot.3d <- FALSE

# TODO: should R report the progress
trace.progress = TRUE


# TODO: set parallelization properties
parallel <- TRUE
gui <- FALSE # FALSE for headless mode

## parallelization ####
# implemented by FJ based on vignette "Parallel processing with the RNetLogo Package" by Jan C. Thiele

# load the parallel package
library(parallel)

# detect the number of cores available
# processors <- detectCores()


# the initialization function
prepro <- function(dummy, gui, nl.path, model.path, nl.obj) {
  library(RNetLogo)
  NLStart(nl.path, gui=gui, nl.obj = nl.obj)
  NLLoadModel(model.path, nl.obj = nl.obj)
}

# the quit function
postpro <- function(x) {
  NLQuit(all = T)
}


# initialize NetLogo
nl.sm13 <- "nl.sm13"
if (parallel == TRUE) 
{
  # create a cluster
  cl <- makeCluster(processors, outfile = "m:/IrriGameOutput/Clusterlog.txt")
  
  invisible(parLapply(cl, 1:processors, prepro, gui=gui,
                      nl.path = nl.path, model.path = model.path,
                      nl.obj=nl.sm13))
} else {
  NLStart(nl.path, gui=FALSE, nl.obj=nl.sm13)
  NLLoadModel(model.path,nl.obj=nl.sm13)
}

#-------------------------------------------------------------------------------------
# II. Definition of the simulation function to test a parameter set
#-------------------------------------------------------------------------------------

# load the code of the simulation function (name: simulate)
source(file=simfun.path)


#-------------------------------------------------------------------------------------
# III. Run of the simulation for all parameter sets
#-------------------------------------------------------------------------------------
require(sensitivity)

# variable used for progress tracing
already.processed <- 0  

# get names of parameters
input.names_SV = names(input.values_SV)

# calculate number of iterations
iter.length_SV <- no.repetitions_SV * (length(input.values_SV)+1)

# get the min and max values of the input factor ranges
mins <- sapply(seq(1,length(input.values_SV)), function(i) {
  input.values_SV[[i]]$min})
maxs <- sapply(seq(1,length(input.values_SV)), function(i) {
  input.values_SV[[i]]$max})

# create input sets
mo_SV <- morris(model = NULL, factors = input.names_SV, r = no.repetitions_SV, 
                design = morris.design_SV, binf = mins, bsup = maxs, scale=TRUE)

# simulate for all input sets
# get results of all evalulation criteria as matrix                                                    
if (parallel == TRUE)
{
  # export variables from parent environment (= workspace) to worker nodes
  clusterExport(cl, c("fix.params_SV", "fix.param.names_SV", "simulate",  
                      "input.names_SV", "iter.length_SV", "no.repeated.sim_SV",
                      "nl.sm13", "trace.progress", "already.processed")) 
  
  # run simulation on multiple cores
  sim.results.morris_SV <- parApply(cl, mo_SV$X, 1,
                                     function(x) {simulate(param.set=x,
                                                           no.repeated.sim=no.repeated.sim_SV,
                                                           nl.obj=nl.sm13, trace.progress=trace.progress,
                                                           parameter.names=input.names_SV,
                                                           iter.length=iter.length_SV,
                                                           function.name="Morris", 
                                                           fix.params = fix.params_SV,
                                                           fix.param.names = fix.param.names_SV)}
  )
} else {
  # run simulation on single core
  sim.results.morris_SV <- apply(mo_SV$X, 1, 
                                  function(x) {simulate(param.set=x,                            
                                                        no.repeated.sim=no.repeated.sim_SV, 
                                                        nl.obj=nl.sm13, trace.progress=trace.progress,
                                                        parameter.names=input.names_SV,
                                                        iter.length=iter.length_SV,
                                                        function.name="Morris", 
                                                        fix.params = fix.params_SV,
                                                        fix.param.names = fix.param.names_SV)}
  )
}


if(parallel == TRUE)
{
  # shut down NetLogo on all cores
  invisible(parLapply(cl, 1:processors, postpro))
  
  # terminate cluster
  stopCluster(cl)
} else {
  # shut down all open NetLogo instances
  NLQuit(all = T)
}

write.csv(sim.results.morris_SV, file = "%wcc%/johnf/IrriGameABM/data/output/sim.results.morris.csv")
write.csv(mo_SV, file = "%wcc%/johnf/IrriGameABM/data/output/mo.csv")