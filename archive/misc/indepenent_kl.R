# test whether the KL of the independent features is the same as the KL of the conjunction

f1_t1 <- c(.8)
f2_t1 <- c(.7)

f1_t2 <- c(.9)
f2_t2 <- c(.8)

kl_f1 <- kl_div(c(f1_t2, 1-f1_t2), 
                c(f1_t1, 1-f1_t1))
kl_f2 <- kl_div(c(f2_t2, 1-f2_t2),
                c(f2_t1, 1-f2_t1))

f12_t1 <- c(f1_t1 * f2_t1, 
            (1-f1_t1) * f2_t1, 
            f1_t1 * (1-f2_t1),
            (1-f1_t1) * (1-f2_t1)) 

f12_t2 <- c(f1_t2 * f2_t2, 
            (1-f1_t2) * f2_t2, 
            f1_t2 * (1-f2_t2),
            (1-f1_t2) * (1-f2_t2)) 

kl_f12 <- kl_div(f12_t2, f12_t1)

