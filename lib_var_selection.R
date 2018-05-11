library(funModeling)
library(dplyr)
#library(ggplot2)
library(reshape2)
library(entropy)
#library(infotheo)


infor_magic=function(input, target)
{
  tbl_2v=table(input, target)
  
  # otra manera de calcular max entropia teorica...
  en=round(entropy::entropy(tbl_2v, unit = "log2") ,3)
  
  # max entropia teorica (maximo caos)
  #log(nrow(tbl_2v)*ncol(tbl_2v)) 
  
  # mutual entropy
  mi=round(mi.empirical(tbl_2v, unit = "log2"),3)
  #mi=mutinformation(data[[col1]],data[[col2]]) # package infotheo
  
  ig=information_gain(input, target)
  
  gr=gain_ratio(input, target)
  
  return(c(en, mi, ig , gr))
}


var_rank_variation_coef <- function(data, target, sample_size=0.3)
{
  df_gr=data.frame(i=NULL, var=NULL,  gr=NULL)
  
  for(i in 1:5)
  {
    ix=get_sample(data, percentage_tr_rows = sample_size, seed = i)
    d_samp=data[ix, ]
    df_info=var_rank_info(d_samp, target) %>% select(var, gr)
    
    df_gr=rbind(df_gr, data.frame(df_info, i=i))
  }
  
  
  stats=group_by(df_gr, var) %>% summarise(variation_coef=sd(gr)/mean(gr)) %>% arrange(-variation_coef);stats
  
}


var_rank_info <- function(data, target)
{
  nam=colnames(data)
  nam=nam[nam!=target]
  
  df_res=data.frame(var=NULL, en=NULL, mi=NULL, ig=NULL, stringsAsFactors = F)
  
  for(var in nam)
  {
    r=infor_magic(data[[var]], data[[target]])
    df_res=rbind(df_res, data.frame(var=var, en=r[1],mi=r[2],ig=r[3], gr=r[4]))
  }
  
  df_res$var=as.character(df_res$var)
  return(df_res %>% arrange(-gr))
}

if(F)
{
  d_cuts=discretize_get_bins(heart_disease, n_bins = 5)
  heart_disease_cat=discretize_df(data = heart_disease, data_bins = d_cuts, stringsAsFactors = F)
  
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



convert_df_to_char <- function(data, n_bins)
{
  d_cuts=discretize_get_bins(data = data, n_bins = n_bins)
  data_cat=discretize_df(data = data, data_bins = d_cuts, stringsAsFactors = F)
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


entropy_2 <- function(input, target)
{
  tbl_x=table(x)
  probs_x=prop.table(tbl_x) # cell percentages
  
  tbl=table(x, y)  
  
  df_tbl=as.data.frame.matrix(tbl)
  
  # not an elegant solution...
  res_entropy=data.frame(t(df_tbl)) %>% mutate_all(funs(entropy(., unit = "log2"))) %>% head(.,1)
  
  # final
  total_en=sum(probs_x*res_entropy)
  
  return(total_en)
}

information_gain <- function(x, y)
{
  tbl=table(y)
  en_y=entropy(tbl, unit = "log2")
  en=entropy_2(x, y)
  info_gain=en_y-en
  
  return(info_gain)
}


gain_ratio <- function(input, target)
{
  ig=information_gain(input, target)
  split=information_gain(input, input)
  
  gain_r=ig/split
  
  return(gain_r)
    
}




rank_iterator <- function(data_cat, rank_var, target)
{
  ## Time to plot the cost-benefit, top-down based on rank
  df_res_iter=data.frame(vars=NULL, en=NULL, mi=NULL, ig=NULL, gr=NULL)
  for(i in 1:length(rank_var))
  {
    vars=rank_var[1:i]
    data_cat$all_x=concatenate_n_vars(data_cat, vars)
    
    rx=infor_magic(data_cat$all_x, data_cat[[target]])
    
    #df_res_iter=rbind(df_res_iter, data.frame(paste(vars, collapse = ", "), rx[1], rx[2], rx[3]))
    df_res_iter=rbind(df_res_iter, data.frame(vars=paste(c(1,i), collapse=","), en=rx[1], mi=rx[2], ig=rx[3], gr=rx[4]))
  }
  
  return(df_res_iter)
}