library(GA)
#vignette("GA")
source("lib_var_selection.R")


#### Example hear_disease
heart_disease_cat=create_categ_df(heart_disease)

# calculo columna target
target="has_heart_disease"
target_var=heart_disease_cat[[target]]

# le saco el target
heart_disease_cat_2=select(heart_disease_cat, -has_heart_disease)


fitness_gain_ratio_aux <- function(vars, data, target_var)
{
  # data=heart_disease_cat_2
  # vars=ga_GA@solution[40,]
  
  names=colnames(data)
  names_2=names[vars==1]
  print(names_2)
  d_filtered=data[, names_2]
  
  all_x=concatenate_n_vars(data, names_2)
  
  gr=gain_ratio(all_x, target_var)
  print(gr)
  return(gr)
}


fitness_gain_ratio <- function(vars, ...)
{
  fitness_gain_ratio_aux(vars, ...) 
}



# gen: one-hot vector indicating variables
# fitness: calculates and returns Info Gain

# max cant d genes= max 
param_nBits=ncol(heart_disease_cat_2)


ga_GA <- ga(type = "binary", 
            fitness = fitness_gain_ratio, 
            data=heart_disease_cat_2, 
            target_var=target_var, 
            popSize = 10, 
            pmutation = .1, 
            nBits = param_nBits,
            run=10)

ga_GA <- ga(type = "binary", 
            fitness = fitness_gain_ratio, 
            data=heart_disease_cat_2, 
            target_var=target_var, 
            popSize = 10, 
            pmutation = .01, 
            nBits = param_nBits,
            maxiter = 10,
            run = 3, # q iter without improvement
            parallel = TRUE,
            elitism = 0.01,
            monitor = plot
)

plot(ga_GA)
ga_GA@solution


names=colnames(data)
vars=ga_GA@solution
names_2=names[vars==1];names_2


