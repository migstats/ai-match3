library(purrr)
library(magrittr)

have_same_elements <- function(list){
  min(list) == max(list)
}

have_row_match <- function(list){
  if (length(list) < 3)
    FALSE
  else if (have_same_elements(list[1:3]))
    TRUE
  else
    have_row_match(list[-1])
}

have_no_combination <- function(matrix){
  check_row <- function(matrix){
    map(seq_len(ncol(matrix)), ~ matrix[,.]) %>% 
      every(~ !have_row_match(.))
  }
  check_row(matrix) & check_row(t(matrix))
}

generate_matrix <- function (ncols = 5, nrows = 5, ncolours = 4){
  sample(ncolours, ncols * nrows, replace = TRUE) %>% 
    matrix(nrows, ncols)
}

generate_valid_matrix <- function(ncols = 5, nrows = 5, ncolours = 4){
  gen_matrix <- matrix(0, nrows, ncols)
  while(!have_no_combination(gen_matrix)){
    gen_matrix <- generate_matrix(ncols, nrows, ncolours)
    have_no_combination(gen_matrix)
  }
  gen_matrix
}
