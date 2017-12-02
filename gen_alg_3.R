rm(list = ls(all = TRUE))
set.seed(123)
library(GA)
library(igraph)
# TSP problem example this is the data of 21 europian cities
data("eurodist", package = "datasets")
D <- as.matrix(eurodist)

# given a tour, calculate the total distance
tourLength <- function(tour, distMatrix) {
  tour <- c(tour, tour[1])
  route <- embed(tour, 2)[, 2:1]
  sum(distMatrix[route])
}

# inverse of thetotal distance is the fitness
tpsFitness <- function(tour, ...) 1/tourLength(tour, ...)


# run a GA algorithm
GA.fit <- ga(type = "permutation", fitness = tpsFitness, distMatrix = D, min = 1, 
             max = attr(eurodist, "Size"), popSize = 10, maxiter = 500, run = 100, pmutation = 0.2, 
             monitor = NULL)

  
GA.fit@names

getAdj <- function(tour) {
  n <- length(tour)
  from <- tour[1:(n - 1)]
  to <- tour[2:n]
  m <- n - 1
  A <- matrix(0, m, m)
  A[cbind(from, to)] <- 1
  A <- A + t(A)
  return(A)
}

# 2-d coordinates
mds <- cmdscale(eurodist)
x <- mds[, 1]
y <- -mds[, 2]
n <- length(x)

B <- 100
fitnessMat <- matrix(0, B, 2)
A <- matrix(0, n, n)

for (b in seq(1, B)) {
  # run a GA algorithm
  GA.rep <- ga(type = "permutation", fitness = tpsFitness, distMatrix = D, 
               min = 1, max = attr(eurodist, "Size"), popSize = 10, maxiter = 50, run = 100, 
               pmutation = 0.2, monitor = NULL)
  
  tour <- GA.rep@solution[1, ]
  tour <- c(tour, tour[1])
  fitnessMat[b, 1] <- GA.rep@solution[GA.rep@iter]
  fitnessMat[b, 2] <- gaSummary(GA.rep)$mean
  A <- A + getAdj(tour)
}


plot.tour <- function(x, y, A) {
  n <- nrow(A)
  for (ii in seq(2, n)) {
    for (jj in seq(1, ii)) {
      w <- A[ii, jj]
      if (w > 0) 
        lines(x[c(ii, jj)], y[c(ii, jj)], lwd = w, col = "lightgray")
    }
  }
}


plot(x, y, type = "n", asp = 1, xlab = "", ylab = "", main = "Tour after GA converged")
points(x, y, pch = 16, cex = 1.5, col = "grey")
abline(h = pretty(range(x), 10), v = pretty(range(y), 10), col = "lightgrey")
tour <- GA.fit@solution[1, ]
tour <- c(tour, tour[1])
n <- length(tour)
arrows(x[tour[-n]], y[tour[-n]], x[tour[-1]], y[tour[-1]], length = 0.15, angle = 45, 
       col = "steelblue", lwd = 2)
text(x, y - 100, labels(eurodist), cex = 0.8)

summary(GA.fit)



plot(GA.fit, main = "GA progression")
points(rep(50, B), fitnessMat[, 1], pch = 16, col = "lightgrey")
points(rep(55, B), fitnessMat[, 2], pch = 17, col = "lightblue")
title(main = "Best and Avg at 50th iteration over 100 simulations")


plot(x, y, type = "n", asp = 1, xlab = "", ylab = "", main = "Tours from 100 simulations")
plot.tour(x, y, A * 10/max(A))
points(x, y, pch = 16, cex = 1.5, col = "blue")
text(x, y - 100, labels(eurodist), cex = 0.8)
n <- length(tour)
lines(x[tour[-n]], y[tour[-n]], col = "red", lwd = 1)
