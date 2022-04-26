get_stimulus <- function(n_stim,n_dim, embedding_path){
  em <- read_csv(embedding_path, col_names = FALSE)
  em[sample(nrow(em), n_stim), 1:n_dim]
}

get_within_concept_distance <- function(embedding_path){
  em <- read_csv(embedding_path, col_names = FALSE)
  
  # stim names
  stim_names = em['X1'] %>% pull()
  
  # get unity stim pairs
  unity_stims_idx = stim_names %>% str_detect('Left|Right')
  
  without_lr = stim_names[unity_stims_idx] %>% str_replace('Left|Right', '') 
  
  same_idx = match(without_lr, unique(without_lr))
  
  unity_stim_embeddings = em[unity_stims_idx,]
  unity_stim_embeddings['sames'] = same_idx
  unity_stim_distances = unity_stim_embeddings %>% group_by(sames) %>% summarize(pair_distance = abs(X2[1] - X2[2])) %>% pull(pair_distance)
  
  # spore stims
  spore_stims_idx = stim_names %>% str_detect('simple|complex')
  
  without_ltrs = stim_names[spore_stims_idx]  %>% substr(1,10)
  
  same_idx = match(without_ltrs, unique(without_ltrs))
  
  spore_stim_embeddings = em[spore_stims_idx,]
  spore_stim_embeddings['sames'] = same_idx
  spore_stim_distances = spore_stim_embeddings %>% group_by(sames) %>% summarize(pair_distance = abs(X2[1] - X2[2])) %>% pull(pair_distance)
  
  mean_distance = mean(c(unity_stim_distances, spore_stim_distances), na.rm = TRUE)
  
  return(mean_distance)
}