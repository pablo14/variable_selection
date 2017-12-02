install.packages("gaoptim")
require(gaoptim)

op <- par(mfrow = c(2, 1))
n = 10
R = 10
angs = seq(0, 2*pi, length = n)
xp = R * cos(angs) + rnorm(n)
yp = R * sin(angs) + rnorm(n)
xp = c(xp, xp[1])
yp = c(yp, yp[1])

base.M = matrix(c(xp, yp), ncol = 2)

dist.FUN = function(p)
{
  p = c(p, p[1])
  
  M.diff = diff(base.M[p, ])
  dists = apply(M.diff, 1, function(x)x[1]^2 + x[2]^2)
  a=1/sum(dists)
  print(paste(p, a))
  return(a)
}

ga1 = GAPerm(dist.FUN, n, popSize = 100, mutRate = 0.3)
ga1$evolve(100)
plot(ga1)
plot(xp, yp, type = 'n', xlab = '', ylab = '', main = 'Best Tour')
res = ga1$bestIndividual()
res = c(res, res[1])

i = 1:n
xi = base.M[res[i], 1]
yi = base.M[res[i], 2]
xf = base.M[res[i + 1], 1]
yf = base.M[res[i + 1], 2]

arrows(xi, yi, xf, yf, col = 'red', angle = 10)
text(base.M[res, 1], base.M[res, 2], 1:n, cex = 0.9, col = 'gray20')
par(op)
