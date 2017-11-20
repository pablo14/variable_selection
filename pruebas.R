source("lib_var_selection.R")

#### Example from: http://www.saedsayad.com/decision_tree.htm
entropy::entropy(c(5,9), unit = "log2")  # real
entropy::entropy(c(7,7), unit = "log2")  # max chaos
entropy::entropy(c(14,0), unit = "log2") # max order


#### Example hear_disease
heart_disease_cat=create_categ_df(heart_disease);df_status(heart_disease_cat)

## Entropy between two vars - case 1
tbl_2v=table(heart_disease_cat$has_heart_disease, heart_disease_cat$max_heart_rate)
sum(tbl_2v) # q rows

# joint entropy
H12 = entropy(tbl_2v, unit = "log2")
H12 # 3.17
# theoretical maximum for the given table
log(sum(tbl_2v)) 

## Entropy between two vars - case 1
tbl_2v_b=table(heart_disease_cat$has_heart_disease, heart_disease_cat$exter_angina)
sum(tbl_2v_b) # q rows, 303, idem before

# joint entropy
H12_b = entropy(tbl_2v_b, unit = "log2")
H12_b # 1.76 

# theoretical maximum for the given table, always the same no matter the variable
log(sum(tbl_2v_b)) 

## Comparing two input vars
H12; H12_b; # max_heart_rate has more entropy than exter_angina 

## Entropy of the target variable, almost 1, even class
Ht=entropy(table(heart_disease_cat$has_heart_disease), unit = "log2")

# Information gain is the E(target) - E(target, input)
Ht-H12




