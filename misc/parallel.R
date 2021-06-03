library(here)
library(tidyverse)

source(here("adult_modeling/scripts/00_generate_stimuli.R"))
source(here("adult_modeling/scripts/01_get_learning_measure.R"))
source(here("adult_modeling/scripts/02_get_experiment_parameter.R"))
d <- read_csv(here("adult_modeling/data/processed_RTdata.csv"))

d_experiment_parameter <- get_experiment_parameter(d)

df_sequence <- generate_sequence_with_parameter(
  d_experiment_parameter, 
                   200, 
                   100, 
                   50, 
                   0.2, 
                   0.6)
library(furrr)
no_cores <- availableCores() - 1
future::plan(multicore, workers = no_cores)

test_beta_count <- function(df_sequence){
  get_beta_count <- function(block, feature_prior = c(3, 1)){
    prior <- replicate(length(block[[1]]), feature_prior, simplify = FALSE)
    
    beta_count <- list()
    beta_count[[1]] <- prior 
    for (trial in 1:length(block)){
      beta_count[[trial+1]] <- mapply(function(x, y) {
           x[y + 1] <- x[y + 1] + 1
           return(list(x))
         },
         beta_count[[trial]], 
         block[[trial]])
    }
    
      return(beta_count)

  }
 
      
  ptm <- proc.time()
  d_measure <- df_sequence %>% 
   mutate(beta = future_map(.x = sequence, 
                    .f = get_beta_count), 
          probability = future_map(.x = beta, 
                           .f = get_probability), 
          surprise = future_map2(.x = probability, 
                         .y = sequence, 
                         .f = get_surprise), 
          learning_progress = future_map(
           .x = probability, 
           .f = get_learning_progress
         )) 
  time <- proc.time() - ptm
  
  return(time)
  
}

test <- test_beta_count(df_sequence)
test

library(furrr)
future::plan(sequential)

test_beta_count <- function(df_sequence){
  get_beta_count <- function(block, feature_prior = c(3, 1)){
    prior <- replicate(length(block[[1]]), feature_prior, simplify = FALSE)
    
    beta_count <- list()
    beta_count[[1]] <- prior 
    for (trial in 1:length(block)){
      beta_count[[trial+1]] <- mapply(function(x, y) {
           x[y + 1] <- x[y + 1] + 1
           return(list(x))
         },
         beta_count[[trial]], 
         block[[trial]])
    }
    
      return(beta_count)

  }
 
      
  ptm <- proc.time()
  d_measure <- df_sequence %>% 
   mutate(beta = future_map(.x = sequence, 
                    .f = get_beta_count), 
          probability = future_map(.x = beta, 
                           .f = get_probability), 
          surprise = future_map2(.x = probability, 
                         .y = sequence, 
                         .f = get_surprise), 
          learning_progress = future_map(
           .x = probability, 
           .f = get_learning_progress
         )) 
  time <- proc.time() - ptm
  
  return(time)
  
}

test <- test_beta_count(df_sequence)
test