library(caret)
library(caret)
library(mlbench)
library(Hmisc)
library(randomForest)
library(funModeling)
library(GA)
source("lib_ga.R")
source("models.R")

data(BreastCancer)

x=select(BreastCancer, everything(), -Id, -Class)
y=BreastCancer[["Class"]]

x=x %>% mutate_if(is.factor, as.numeric)
x=x %>% mutate_if(is.ordered, as.numeric)
x=convert_df_to_categoric(x, n_bins = 5);
# convert to cat
#d_cat_x=convert_df_to_categoric(x, n_bins = 10);
#d_cat_y=convert_df_to_char(data = as.data.frame(y), n_bins = 10);

# parameters
param_nBits=ncol(x)
col_names=colnames(x)

#x$Bare.nuclei=as.character(x$Bare.nuclei)
#x$Bare.nuclei=ifelse(is.na(x$Bare.nuclei), "NA.", x$Bare.nuclei)

ver_rank=funModeling::var_rank_info(data = BreastCancer,target = "Class")
  
# call 
ga_GA_1 <- ga(type = "binary", 
              fitness = fitness_gain_ratio, 
              data = x, 
              target_var=y, 
              tot_vars = param_nBits,
              p_sampling = 0.99,
              elitism = 0.1,
              popSize = 10, # the number of indivduals/solutions/var selected
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


fit_model_1=create_model_rf_select_vars(data_tr_sample = x,
                                              target = y,
                                              best_vars = best_vars_ga); fit_model_1

fit_model_2=create_model_rf_select_vars(data_tr_sample = x,
                                              target = y,
                                              best_vars = colnames(x)); fit_model_2

resamp=resamples(list(ga = fit_model_1, all = fit_model_2));bwplot(resamp, layout = c(3, 1))



