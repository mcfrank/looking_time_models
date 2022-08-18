model %>% 
  ggplot(aes(x = t, y = im, 
             color = as.factor(stimulus_idx))) + 
  geom_point() 


model %>% 
  group_by(stimulus_idx) %>% 
  summarise(sample_n = n()) %>% 
  ggplot(aes(x = stimulus_idx, 
             y = sample_n)) + 
  geom_point()+ 
  ylim(0, 20)
