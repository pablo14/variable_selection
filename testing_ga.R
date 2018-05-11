library(caret)
library(caret)
library(mlbench)
#install.packages("mlbench")
library(Hmisc)
library(randomForest)
source("lib_ga.R")
source("models.R")


n <- 100
p <- 40
sigma <- 1
set.seed(1)
sim <- mlbench.friedman1(n, sd = sigma)
colnames(sim$x) <- c(paste("real", 1:5, sep = ""),
                     paste("bogus", 1:5, sep = ""))
bogus <- matrix(rnorm(n * p), nrow = n)
colnames(bogus) <- paste("bogus", 5+(1:ncol(bogus)), sep = "")
x <- cbind(sim$x, bogus)
y <- sim$y

# convert to cat
d_cat_x=convert_df_to_categoric(data = as.data.frame(x), n_bins = 20);
d_cat_y=convert_df_to_categoric(data = as.data.frame(y), n_bins = 10);

# parameters
param_nBits=ncol(d_cat_x)
col_names=colnames(d_cat_x)

# call 
ga_GA_1 <- ga(type = "binary", 
              fitness = fitness_gain_ratio, 
              data=d_cat_x, 
              target_var=d_cat_y[,1], 
              p_sampling=0.9,
              tot_vars = param_nBits,
              elitism = 0.3,
              popSize = 20, # the number of indivduals/solutions/var selected
              pmutation = .02, 
              nBits = param_nBits,
              run=10, # max iter w/impro stop criteria
              maxiter = 50, # total runs...
              monitor=plot,
              keepBest = TRUE,
              parallel = T,
              names=col_names,
              crossover=gabin_uCrossover
)

summary(ga_GA_1)

best_vars_ga=col_names[ga_GA_1@solution[1,]==1];best_vars_ga


fit_model_1=create_model_rf_select_vars_multi(data_tr_sample = as.data.frame(x),
                                              target = y,
                                              best_vars = best_vars_ga); fit_model_1

fit_model_2=create_model_rf_select_vars_multi(data_tr_sample = as.data.frame(x),
                                              target = y,
                                              best_vars =c("real4" ,"real5", "real2","real1")); fit_model_2

fit_model_all=create_model_rf_select_vars_multi(data_tr_sample = as.data.frame(x),
                                                target = y,
                                                best_vars =colnames(as.data.frame(x))); fit_model_all

resamp=resamples(list(ga = fit_model_1, caret = fit_model_2,all = fit_model_all));bwplot(resamp, layout = c(3, 1))


