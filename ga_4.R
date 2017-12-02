dataset <- data.frame(name = paste0("x",1:11),
                      Weight = c(2.14083022,7.32592911,0.50945094,4.94405846,12.02631340,14.59102403,0.07583312,0.36318323,10.64413370,3.54882187,1.79507759),
                      stringsAsFactors = F)


max_weight = 10

fitness_function2 <- function(x){
  current_weight <- x %*% dataset$Weight
  if ( current_weight > max_weight){
    return(0)
  } else {
    return(current_weight)
  }
}

ga_GA <- ga(type = "binary", fitness = fitness_function2, popSize = 100, pmutation = .1, nBits = 11)

ga_GA@solution 
