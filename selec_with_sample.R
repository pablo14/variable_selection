library(funModeling)
library(dplyr)
library(ggplot2)
library(reshape2)

data <- data.frame(a=rnorm(100), b = rnorm(100,.5,1.2))
data <- melt(data)
colnames(data) <- c("Category", "Intensity")


df_smp=data.frame(i=NULL, value=NULL) 
d_des=NULL
for(i in 1:10)
{
  smpl=sample_frac(mtcars, 0.9, replace = TRUE)
  df_smp=rbind(df_smp, data.frame(i=i, value=smpl$mpg))
  #print(mad(smpl$mpg))
  v=mad(smpl$mpg)
  print(v)
  d_des=rbind(d_des, v)
}

ggplot(df_smp, aes(x=value, colour=as.factor(i))) + geom_density(aes(group=i))

profiling_num(d_des, print_results = F)$variation_coef

# Variables con mas desvio? caoticas?  Con y sin target
# variables mas estables respecto a un target (aquellas q no varian mucho sus tazas 
# de reconversion en resampling)



##########################################################################################
##########################################################################################
##########################################################################################
# two independent random variables
library(entropy)
x1 = runif(10000)
x2 = runif(10000)
# binning by equal length
y2d = discretize2d(x1, x2, numBins1=10, numBins2=10)

# cantidad de casos 10000
sum(y2d) 

# la entropia da maxima, como el mismo random
H12 = entropy(y2d) 

# max entropia teorica
log(100, base = 2) 

# mutual information, almost 0...
mi.empirical(y2d, unit = "log2")

entropy(c(4/10,6/10))

y = c(4, 2, 3, 0, 2, 4, 0, 0, 2, 1, 1)  
y=c(9, 1)
entropy(y, method="ML")
entropy(y, method="MM")
entropy(y, method="Jeffreys")
entropy(y, method="Laplace")
entropy(y, method="SG")
entropy(y, method="minimax")
entropy(y, method="CS")
#entropy(y, method="NSB")
entropy(y, method="shrink")

y=c(5,5)
y=c(10,1)

entropy.empirical(y2d, unit="log2")

## ejemplos...
d_hd=df_categorical(heart_disease)
res=var_rank_info(d_hd, "has_heart_disease")

#entropy.empirical(, unit="log2")
entropy.empirical(table(d_hd$has_heart_disease), unit="log2")

recur=function(x)
{
  y=round(log(x,base = 10))
  print(y)
  if(y<=3) return()
  recur(y)
}

recur(1000)
fibonacci(20)

# propuesta: 
## 1) quedarse con el mejor 50%
## 2) unir en 1 var con paste iterativamente las q tengan el menor MI, con prioridad para las MI alto contra el target
## 3) cercano a 0

target="has_heart_disease"
d_hd_2=select(d_hd, -has_heart_disease)
nam=colnames(d_hd_2)

for(i in 1:ncol(d_hd))
{
  new_var=paste(d_hd_2$age, d_hd$gender)
  
  res_2=var_rank_info(d_hd_2, res[i,"var"])
  
}



