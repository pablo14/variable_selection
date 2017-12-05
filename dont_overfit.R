source("lib_ga.R")
source("lib_var_selection.R")
source("models.R")
#source("gen_sample_data.R")

#https://www.kaggle.com/c/overfitting

# max cant d genes= max 
data_overfit=read.csv("overfitting.csv", header = T,sep = ",", stringsAsFactors = F)

# keep trainable data
data=filter(data_overfit, train==1) %>% select(-Target_Leaderboard, -Target_Evaluate, -case_id, -train) %>% mutate_if(is.character, as.numeric)
data_test=filter(data_overfit, train==0)

d_cat=create_categ_df(data = data, n_bins = 10);
d_cat_test=create_categ_df(data = data_test, n_bins = 10);
target="Target_Practice"

## Rank best vars based on IG
#rank_var=var_rank_info(data = d_cat, target = target)

#cross_plot(training, str_input=c("TwoFactor2","Linear01", "Noise001"), str_target = "Class")

## Generic Data Prep
target_var=d_cat[[target]]
d_cat_in=d_cat %>% select(-one_of(target))

## Variation coef variable
d_cat_in_with_target=d_cat_in
d_cat_in_with_target[[target]]=target_var

variation_coef_rank=var_rank_sampling(data = d_cat_in_with_target, target = target)
d_cat_in=d_cat_in %>% select(-one_of(variation_coef_rank$var[1:100]))

############################

target_var_test=d_cat_test[[target]]
d_cat_in_test=d_cat_test %>% select(-one_of(target))

param_nBits=ncol(d_cat_in)
col_names=colnames(d_cat_in)

## Calling the GA function
system.time(ga_GA_1 <- ga(type = "binary", 
                          fitness = fitness_gain_ratio, 
                          data=d_cat_in, 
                          target_var=target_var, 
                          p_sampling=0.7,
                          tot_vars = param_nBits,
                          elitism = 0.1,
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
)


summary(ga_GA_1)

best_vars_ga=col_names[ga_GA_1@solution[1,]==1];best_vars_ga




#save(best_vars_ga, file="vars_ga.rds")
library(caret)
fit_model_1=create_model_rf_select_vars(data_tr_sample = d_cat_in, target = make.names(target_var),best_vars = best_vars_ga);fit_model_1
fit_model_2=create_model_rf_select_vars(data_tr_sample = d_cat_in_test, target = make.names(target_var_test),best_vars = best_vars_ga);fit_model_2

## no noisy vars
vars_no_noise=select(d_cat_in, -contains("Noise")) %>% colnames(.)
fit_model_2=create_model_rf_select_vars(data_tr_sample = d_cat_in, target = make.names(target_var),best_vars =vars_no_noise);fit_model_2


## Cheuing all
resamp=resamples(list(ga = fit_model_1, no_noise = fit_model_2,noise=fit_model_3));bwplot(resamp, layout = c(3, 1))

