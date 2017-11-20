# http://ipub.com/id3-with-data-tree/
IsPure <- function(data) {
  length(unique(data[,ncol(data)])) == 1
}

Entropy <- function( vls ) {
  res = vls/sum(vls) * log2(vls/sum(vls))
  res[vls == 0] = 0
  -sum(res)
}

entropy = function(edible) Entropy(c(edible, 100 - edible))
entropy = Vectorize(entropy)
curve( entropy, from = 0, to = 100, xname = 'edible')

Entropy(c(10, 0))

Entropy(c(10, 10))

Entropy(c(1, 0, 1))


####
InformationGain = function( tble ) {
  tble = as.data.frame.matrix(tble)
  entropyBefore = Entropy(colSums(tble))
  s = rowSums(tble)
  entropyAfter = sum (s / sum(s) * apply(tble, MARGIN = 1, FUN = Entropy ))
  informationGain = entropyBefore - entropyAfter
  return (informationGain)
}

install.packages("data.tree")
library(data.tree)
data(mushroom)
tble = table(mushroom[,c('color', 'edibility')])
tble

InformationGain(tble)
InformationGain(table(mushroom[,c('size', 'edibility')]))
InformationGain(table(mushroom[,c('points', 'edibility')]))

## da mas alto el IG, es mejor predictor
InformationGain(table(mushroom[,c('flag_true', 'edibility')]))


mushroom
