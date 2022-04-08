get_stimulus <- function(n_stim,n_dim, embedding_path){
  em <- read_csv(embedding_path, col_names = FALSE)
  em[sample(nrow(em), n_stim), 1:n_dim]
}