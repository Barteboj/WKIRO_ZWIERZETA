myConv2 <- function(X, Y) {
  require('signal')
  
  m <- dim(X)[1]
  n <- dim(X)[2]
  k <- dim(Y)[1]
  l <- dim(Y)[2]
  
  z_y <- m + k - 1
  z_x <- n + l - 1
  
  y_m <- x_m <- matrix(data = 0, nrow = z_y,  ncol = z_x)
  
  l_x <- (n - 1)*(m + k - 1) + m
  l_y <- (l - 1)*(m + k - 1) + k
  l_z <- l_x + l_y - 1
  
  x_m[1:m, 1:n] <- X[,]
  y_m[1:k, 1:l] <- Y[,]
  
  x <- as.vector(x_m)
  y <- as.vector(y_m)
  
  #trunc
  x_t <- x[1:l_x]
  y_t <- y[1:l_y]
  
  
  z <- conv(x_t, y_t)
  Z <- matrix(data = z, nrow = z_y, ncol = z_x)
  
  return(Z)
}