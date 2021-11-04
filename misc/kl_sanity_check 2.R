thetas <- c(0.3, 0.7)

p_theta_f1 <- c(0.5, 0.5)
p_theta_f2 <- c(0.6, 0.4)

p_theta_f1_tplus1 <- c(0.6, 0.4)
p_theta_f2_tplus1 <- c(0.6, 0.4)

# feature by feature
dkl(p_theta_f1, p_theta_f1_tplus1) + dkl(p_theta_f2, p_theta_f2_tplus1)

# all creature distribution
# p(f1 = 0.3 & f2 = 0.3), p(f1 = 0.3 & f2 = 0.7), p(f1 = 0.7 & f2 = 0.3), p(f1 = 0.7 & f2 = 0.7)

p_theta = c(0.5*0.6, 0.5*0.4, 0.5*0.6, 0.5*0.4)

p_theta_plus1 = c(0.6*0.6, 0.6*0.4, 0.4*0.6, 0.4*0.4)

dkl(p_theta, p_theta_plus1)


