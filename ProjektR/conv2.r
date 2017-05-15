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