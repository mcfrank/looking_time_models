library(tidyverse)

C <- 1
EIG <- seq(0, 20, by=0.1)


trial1std_EIG = 10 

dev_EIG = 12
std_EIG = 8


LTs_dev = vector(mode = "numeric", length = 50)
LTs_std = vector(mode = "numeric", length = 50)

# luce choice
sample_num = 0
subject_n = 1000

# deviant with higher EIG
for (i in 1:subject_n) {
  
  continue = TRUE
  sample_num = 0
  
  while(continue){
    p_choice_sample = dev_EIG / (dev_EIG + C)
    flip = rbernoulli(1, p=p_choice_sample)
    
    if (flip)
      sample_num = sample_num + 1
    else {
      LTs_dev[i] = sample_num
      continue = FALSE
    }
  }
}
  
## std with higher EIG

for (i in 1:subject_n) {
  
  continue = TRUE
  sample_num = 0
  
  while(continue){
    p_choice_sample = std_EIG / (std_EIG + C)
    flip = rbernoulli(1, p=p_choice_sample)
    
    if (flip)
      sample_num = sample_num + 1
    else {
      LTs_std[i] = sample_num
      continue = FALSE
    }
  }
  
}

LT_df <- tibble( background = LTs_std , deviant = LTs_dev) %>% pivot_longer(cols = c("background","deviant"), names_to = "trial_type", values_to = "sample_n")

ggplot(data = LT_df, aes(x = sample_n)) + geom_histogram() + facet_grid(~trial_type) 
