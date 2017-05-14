conv2same <- function(A, B) {
  # Returns the central part of two-dimensional convolution of matrices A and B which is the same size as A.
  
  a_size <- dim(A)
  b_size <- dim(B)
  ma <- a_size[1]
  na <- a_size[2]
  mb <- b_size[1]
  nb <- b_size[2]
  
  mc <- max(ma+mb-1, ma, mb)
  nc <- max(na+nb-1, na, nb)
  
  #calculating indexes central part of output matrix 
  j_i <- c()
  k_i <- c()
  #x index
  s_x <- 1
  e_x = mc#out x size
  l_x = e_x - s_x + 1#calc x length
  o_x = ma
  repeat {
    if (l_x == o_x) {
      break
    }
    s_x <- s_x + 1 #cut top edge
    l_x <- e_x - s_x + 1
    if (l_x == o_x) {
      break
    }
    e_x = e_x - 1#cut bottom edge
    l_x = e_x - s_x + 1
  }
  #y index
  s_y <- 1
  e_y <- nc#out y size
  l_y <- e_y - s_y + 1#calc y length
  o_y <- na
  repeat {
    if (l_y == o_y) {
      break
    }
    s_y <- s_y + 1#cut left edge
    l_y <- e_y - s_y + 1
    if (l_y == o_y) {
      break
    }
    e_y <- e_y - 1#cut right edge
    l_y <- e_y - s_y + 1
  }
  j_i <-c(s_x:e_x)
  k_i <-c(s_y:e_y)
  
  #source('conv2.r')
  #C <- conv2(A, B)
  source('myConv2.r')
  C <- myConv2(A, B)
  
  return(C[j_i,k_i])
}