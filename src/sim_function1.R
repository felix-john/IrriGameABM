## adapted from Thiele et al. 2014, simulation_function1.R

# a function to handle a simulation
# gets a set of parameters
# returns results of evaluation criteria
# NOTE: It run repeated simulations for stochastic models.
#       To controll stochasticity it runs replicated simulations for current parameter combination
#       and calculates the mean simulation output.
#       If your model is deterministic, just set no.repeated.sim to 1.
simulate <- function(param.set, parameter.names, no.repeated.sim, nl.obj, trace.progress, iter.length, function.name) {
  # some security checks
  if (length(param.set) != length(parameter.names))
  { stop("Wrong length of param.set!") }
  if (no.repeated.sim <= 0)
  { stop("Number of repetitions must be > 0!") }
  if (length(parameter.names) <= 0)
  { stop("Length of parameter.names must be > 0!") }
  
  # an empty list to save the simulation results
  eval.values <- NULL
  
  # repeated simulations (to control stochasticity)
  for (i in 1:no.repeated.sim)
  {
    # create a random-seed for NetLogo from R, based on min/max of NetLogo's random seed
    # for NetLogo 4:
    #NLCommand("random-seed",runif(1,-9007199254740992,9007199254740992), nl.obj=nl.obj)
    # since NetLogo 5:
    NLCommand("random-seed",runif(1,-2147483648,2147483647), nl.obj=nl.obj)
    
    # TODO: adapt the following to your simulation model
    # This is the stuff for one simulation
    NLCommand("setupcleardata", nl.obj=nl.obj)
    
    # set NetLogo parameters to current parameter values
    lapply(seq(1:length(parameter.names)), function(x) {NLCommand("set ",parameter.names[x], param.set[x], nl.obj=nl.obj)})
    
    # run simulation
    # TODO: adapt to your simulation process
    # two warm-up years (12 months each)
    NLDoCommand(2,"repeat 12 [go]", nl.obj=nl.obj)
    # here: run 20 x 1 year (=12 months) and get results from month 11 (november) as calibration criterion
    cal.crit <- NLDoReport(20,"repeat 12 [go]",c("year","month-11-count","month-11-alpha"), as.data.frame=T, df.col.names=c("year","abund","alpha"), nl.obj=nl.obj)
    
    # TODO: adapt to your calibration criteria
    # number of patches (for calculation of percentage)
    patches.count <- NLReport("count patches", nl.obj=nl.obj)
    
    # calculate calibration criteria
    # mean abundance criterion
    abundance.criterion <- mean(cal.crit$abund)
    # variation criterion
    variation.criterion <- sd(cal.crit$abund)
    # vacancy criterion
    vacancy.criterion <- mean(cal.crit$alpha/patches.count)
    
    # merge calibration criteria
    calibration.criteria <- c(abundance.criterion,variation.criterion,vacancy.criterion)
    
    # append to former results
    eval.values <- rbind(eval.values,calibration.criteria)
  }
  
  # print the progress if requested
  if (trace.progress == TRUE)
  {
    already.processed <- get("already.processed",env=globalenv()) + 1
    assign("already.processed", already.processed, env=globalenv())
    print(paste("processed (",function.name,"): ", already.processed / iter.length * 100, "%", sep = ""))
  }
  
  # return the mean of the repeated simulation results
  if (no.repeated.sim > 1)
  {
    return(colMeans(eval.values))
  }
  else {
    return(eval.values)
  }
}
