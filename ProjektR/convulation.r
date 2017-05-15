conv2 <- function(A, B){
  
  A.pad <- matrix(0, ncol = NCOL(A) + NCOL(B)-1, nrow = NROW(A)+NROW(B)-1);
  A.pad[1:NROW(A), 1:NCOL(A)] <- A
  B.pad <- matrix(0, ncol = NCOL(A) + NCOL(B)-1, nrow = NROW(A)+NROW(B)-1);
  B.pad[1:NROW(B), 1:NCOL(B)] <- B
  
  A.fft <- fft(A.pad);
  B.fft <- fft(B.pad);
  M <- fft(A.fft * B.fft, inverse = TRUE)/length(A.fft)
  
  C <- Re(M)
  return(C)
}

conv2same <- function(A, B) {
  # Returns the central part of two-dimensional convolution of matrices A and B which is the same size as A.
  N.row <- (floor(NROW(B)/2) + (1:NROW(A)))
  N.col <- (floor(NCOL(B)/2) + (1:NCOL(A)))
  C = conv2(A, B)[N.row, N.col]
  
  return(C)
}

conv2vector <- function(u, v, A) {
  # Returns matrix which is equvalent to:
  # Convolution each column of A with the vector u, and then convultion each row of the result with the vector v.
  
  B =  u %*% t(v)

  C <- conv2(B, A)
  
  C <- round(C, 5)
  
  return(C)
}