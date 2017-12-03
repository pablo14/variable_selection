library(caret)
set.seed(468)
training <- twoClassSim(  300, noiseVars = 100,
                          corrVar = 100, corrValue = 0.95)
testing  <- twoClassSim(  300, noiseVars = 100,
                          corrVar = 100, corrValue = 0.75)
large    <- twoClassSim(10000, noiseVars = 100,
                        corrVar = 100, corrValue = 0.75)


initialSmall <- function(object, ...) 
{
  #object=ga_GA_1
  population <- sample(0:1, 
                       replace = TRUE, 
                       size = object@nBits * object@popSize, 
                       prob = c(0.9, 0.1))
  population <- matrix(population, 
                       nrow = object@popSize, 
                       ncol = object@nBits)
  return(population)
}

