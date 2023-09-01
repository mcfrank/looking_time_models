library(tidyverse)

# let's define hunter and ames functions as a family of quadratic functions
# where y = beta_1 * x + beta_2 * x^2
# and beta_1 < 0 and beta_2 > 0 

t <- seq(0, 10, .1)

sim <- expand_grid(b1 = seq(-1, 0, .2), 
                   b2 = seq(0, .1, .02), 
                   t = t) %>%
  mutate(nov_pref = b1 * t + b2 * t^2) 

ggplot(sim, 
       aes(x = t, y = nov_pref, col = b1, group = b1)) + 
  geom_line() + 
  geom_hline(yintercept = 0, lty = 2) + 
  facet_wrap(~b2) + 
  ylab("Relative novelty preference") + 
  xlab("Time")

# now how do we turn those into looking times? 
# imagine a looking time function is some exponential decay
# so y \sim \alpha + \beta e^{-\gamma x}

qplot(t, exp(- t))

# LT = f(nov_pref) 

# f(x) = a+b+e^{-c t * inv_logit(x)}

# so you could then say something like:
# lt = a + b * e^{-c t * inv_logit(nov_pref)}, where
# pref = beta_1 * t + beta_2 * t^2

sim <- sim %>%
  mutate(novelty = exp(-t * boot::inv.logit(nov_pref)), 
         familiarity = exp(-t * boot::inv.logit(-nov_pref))) %>%
  pivot_longer(cols = c("novelty","familiarity"), 
               names_to = "preference",
               values_to = "looking_time")

ggplot(sim, 
       aes(x = t, y = looking_time, col = preference)) + 
  geom_line() +
  facet_grid(b1 ~ b2)