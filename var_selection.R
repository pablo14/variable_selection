library(funModeling)
library(dplyr)
#library(ggplot2)
library(reshape2)
library(entropy)
#library(infotheo)


infor_magic=function(data, col1, col2)
{
  tbl_2v=table(data[[col1]], data[[col2]])
  
  # otra manera de calcular max entropia teorica...
  en=round(entropy::entropy(tbl_2v, unit = "log2") ,3)
  
  # max entropia teorica (maximo caos)
  #log(nrow(tbl_2v)*ncol(tbl_2v)) 
  
  # mutual entropy
  mi=round(mi.empirical(tbl_2v, unit = "log2"),3)
  #mi=mutinformation(data[[col1]],data[[col2]]) # package infotheo
  
  ratio=round(mi/en, 3)
  
  #print(paste(en, mi,, sep=", "))
  
  return(c(en, mi, ratio))
}

#infor_magic(data=heart_disease_2, col1="has_heart_disease", col2="age")

#infor_magic(data=heart_disease_2, col1="has_heart_disease", col2="max_heart_rate")


var_rank_info <- function(data, target)
{
  nam=colnames(data)
  nam=nam[nam!=target]
  df_res=data.frame(var=NULL, en=NULL, mi=NULL, stringsAsFactors = F)
  for(i in 1:length(nam))
  {
    r=infor_magic(data, nam[i], target)
    df_res=rbind(df_res, data.frame(var=nam[i], en=r[1],mi=r[2]))
  }
  
  df_res$var=as.character(df_res$var)
  return(df_res %>% arrange(-mi))
}

if(F)
{
  d_cuts=df_calculate_cuts(heart_disease, n_bins = 5)
  heart_disease_cat=df_recover_cuts(data = heart_disease, data_cuts = d_cuts, stringsAsFactors = F)
  
  #df_status(heart_disease_cat)
  df_rank=var_rank_info(heart_disease_cat, target="has_heart_disease") 
  
  
  v1=df_rank$var[1]
  v2=df_rank$var[2]
  heart_disease_cat$v_comp=concatenate_vars(heart_disease_cat, v1,v2)
  heart_disease_cat=heart_disease_cat %>% select(-one_of(c(v1,v2)))
  
  df_rank_2=var_rank_info(heart_disease_cat, target="has_heart_disease")
  information.gain(Species~., iris)
  
  var_rank_info(heart_disease_cat, target="heart_disease_severity") 
  
  
  for(i in 1:nrow(df_rank))
  {
    v1=df_rank$var[i]
    v2=df_rank$var[i+1]
    heart_disease_cat$v_comp=concatenate_vars(heart_disease_cat, v1,v2)
    heart_disease_cat=heart_disease_cat %>% select(-one_of(c(v1,v2)))
    
    df_rank_2=var_rank_info(heart_disease_cat, target="has_heart_disease") 
    
    var_rank_info(heart_disease_cat, target="heart_disease_severity") 
    
  }
  
}

concatenate_vars <- function(data, var1, var2)
{
  var1=data[[var1]]
  var2=data[[var2]]
  res=paste(var1, var2,sep="|")
  return(res)
}



create_categ_df <- function(data)
{
  d_cuts=df_calculate_cuts(data, n_bins = 5)
  data_cat=df_recover_cuts(data = data, data_cuts = d_cuts, stringsAsFactors = F)
  # algunas quedaron sin convertir...
  data_cat_2=data_cat %>% mutate_all(as.character); 
  return(data_cat_2)
}


concatenate_n_vars <- function(data, vars)
{
  df=data %>% select(one_of(vars))
  
  new_col=apply(df, 1, function(x) paste(x, collapse = " | ") )
  
  return(new_col)
}


entropy_bis <- function(x, y)
{
  tbl_x=table(x)
  probs_x=prop.table(tbl_x) # cell percentages
  
  tbl=table(x, y)  
  
  df_tbl=as.data.frame.matrix(tbl)
  res_entropy=data.frame(t(df_tbl)) %>% mutate_all(funs(entropy(., unit = "log2"))) %>% head(.,1)
  
  # final
  total_en=sum(probs_x*res_entropy)
  
  return(total_en)
}
