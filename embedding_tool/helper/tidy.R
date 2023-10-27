
get_visualization_for_animate <- function(euc_dist_path, embedding_df_path){
  
  euc_dist <- read.csv(euc_dist_path) 
  euc_dist <- euc_dist[,-1] %>% as.matrix()
  
  embeddings_pca <- read.csv(embedding_df_path, header=FALSE)
  
  embeddings_df <- embeddings_pca %>% select(V1,V2) %>% 
    mutate(stim_name = euc_dist %>% colnames(), 
           number = case_when(str_detect(stim_name, "pair") ~ "pair",
                              TRUE ~ "single"),
           animacy = case_when(str_detect(stim_name, "inanimate") ~ "inanimate",
                               str_detect(stim_name, "animate") ~ "animate"),
           pose = case_when(str_detect(stim_name, "left") ~ "left",
                            str_detect(stim_name, "right") ~ "right"))
  
  euc_dist = euc_dist %>% as.data.frame() 
  rownames(euc_dist) = euc_dist %>% colnames()
  
  animal_names = embeddings_df %>% filter(animacy == 'animate', !str_detect(stim_name, 'pair')) %>% pull(stim_name)
  
  distance_df_animals <- tibble(stim_name = vector(mode="character", length=length(animal_names)),
                                pose_dist = vector(mode="character", length=length(animal_names)),
                                animacy_dist = vector(mode="character", length=length(animal_names)), 
                                identity_dist = vector(mode="character", length=length(animal_names)),
                                number_dist = vector(mode="character", length=length(animal_names)))
  
  poses = c('left', 'right')
  
  
  idx = 0
  for (animal in animal_names) {
    idx = idx + 1
    cur_pose = embeddings_df %>% filter(stim_name == animal) %>% pull(pose)
    cur_animacy = embeddings_df %>% filter(stim_name == animal) %>% pull(animacy)
    cur_number = embeddings_df %>% filter(stim_name == animal) %>% pull(number)
    
    # find names of stim relevant for each violation
    # pose violation
    pose_violation = str_replace(animal, cur_pose, poses[poses != cur_pose])
    
    # identity violations (same number)
    identity_violations = embeddings_df %>% filter(pose == cur_pose, animacy == cur_animacy, number == cur_number) %>% pull(stim_name)
    identity_violations = identity_violations[identity_violations != animal]  # remove itself
    
    # animacy violation
    animacy_violations = embeddings_df %>% filter(pose == cur_pose, animacy != cur_animacy, number == cur_number) %>% pull(stim_name)
    
    # number violation
    if (cur_number == 'pair'){
      number_violations = str_replace(animal, "_pair", "")
    }else {
      number_violations = str_replace(animal, ".png", "_pair.png")
    }
    
    # get distances
    pose_violation_dist = euc_dist[animal, pose_violation] %>% as.numeric()
    identity_violation_dist = mean(euc_dist[animal, identity_violations]  %>% as.numeric())
    animacy_violations_dist =  mean(euc_dist[animal, animacy_violations] %>% as.numeric())
    number_violations_dist =  mean(euc_dist[animal, number_violations] %>% as.numeric())
    
    distance_df_animals$stim_name[idx] = animal
    distance_df_animals$pose_dist[idx] = pose_violation_dist
    distance_df_animals$identity_dist[idx] = identity_violation_dist
    distance_df_animals$animacy_dist[idx] = animacy_violations_dist
    distance_df_animals$number_dist[idx] = number_violations_dist
    
  }
  
  long_distance_df = pivot_longer(distance_df_animals, cols = c('pose_dist', 'identity_dist', 'animacy_dist', 'number_dist'), names_to = 'violation_type') %>% mutate(violation_type = factor(violation_type, levels = c('animacy_dist', 'identity_dist', 'pose_dist', 'number_dist')), number = ifelse(str_detect(stim_name, "pair"),"pair","single"), is_twelve = str_starts(stim_name, "animate_012")) 
  
  return (long_distance_df)  
}


