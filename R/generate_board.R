library(purrr)

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
  every(matrix, ~ !have_row_match(.))
}

generate_matrix <- function (ncols = 5, nrows = 5, ncolours = 4){
  
  
}