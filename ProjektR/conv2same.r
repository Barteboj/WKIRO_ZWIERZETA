conv2same <- function(A, B) {
  source('conv2.r')
  
  # Returns the central part of two-dimensional convolution of matrices A and B which is the same size as A.
  N.row <- (floor(NROW(B)/2) + (1:NROW(A)))
  N.col <- (floor(NCOL(B)/2) + (1:NCOL(A)))
  C = conv2(A, B)[N.row, N.col]
  
  return(C)
}