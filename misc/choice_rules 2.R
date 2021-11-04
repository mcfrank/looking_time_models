library(tidyverse)

C <- 10
EIG <- seq(0, 20, by=0.1)


# right now:
p_keeplooking = EIG / (EIG + C)
p_keeplooking2 = 1/(1+exp(-1*(EIG-C)))
p_keeplooking3 = 1/(1+exp(-0.5*(EIG-C)))


df <- tibble("EIG" = EIG, "luce_choice" = p_keeplooking,
             "exponential" = p_keeplooking2,
             "flatter_exponential" = p_keeplooking3) %>% 
          pivot_longer(cols = 2:4, names_to = "calculation_method",
                       values_to = "value")

ggplot(df, aes(EIG, value, fill='calculation_method')) + 
  geom_line() + ylim(0,1) + ylab('P(stay)') + facet_wrap(~calculation_method) + 
  geom_hline(yintercept=0.5, linetype = 'dashed', color='red') +
  geom_vline(xintercept=C, linetype = 'dashed', color='yellow')
