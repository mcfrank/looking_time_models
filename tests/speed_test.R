library(microbenchmark)

# check test vs non-test 
kl_div_test <- function (x, y) {
  sum(
    ifelse(all( (x == 0) == (y == 0) ),  # check that all the 0's are in the same position, if there are any
           x * log(x/y),
           stop('found lone 0 in distribution when calculating KL')
    )
  )
}

kl_div_no_test <- function (x, y) {
  sum(x * log(x/y))
}

microbenchmark::microbenchmark(
  kl_div_test(seq(0.01, 0.09, 0.02), seq(0.02, 0.02, 0.04)), 
  kl_div_no_test(seq(0.01, 0.09, 0.02), seq(0.02, 0.02, 0.04)), 
  times = 100000
)

